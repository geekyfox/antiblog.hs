
--
-- Here we infer the tags by the following rules:
--
--  * every entry has it's own "natural" tags
--  * every "small" (read_more = false; meaning that teaser and
--    body are the same) entry is tagged "micro"
--  * every entry with "meta" link is tagged "meta"
--  * every entry has an empty ('') tag
--
CREATE OR REPLACE VIEW effective_entry_tag AS
SELECT DISTINCT * FROM (
    -- Generic tagged entries
    SELECT entry_id, tag FROM entry_tag
    UNION
    -- "Small" entries
    SELECT id, 'micro' FROM entry WHERE NOT read_more
    UNION
    -- Meta-entries
    SELECT entry_id, 'meta' FROM symlink WHERE kind = 'meta'
    UNION
    -- All entries 
    SELECT id, '' FROM entry
) sq;

--
-- This is the list of all known tags to display in tag cloud.
--
CREATE OR REPLACE VIEW known_tag_volatile AS
SELECT tag, count(1)::integer AS counter
FROM effective_entry_tag
WHERE tag <> ''
GROUP BY tag
ORDER BY counter DESC;

DROP MATERIALIZED VIEW known_tag;
CREATE MATERIALIZED VIEW known_tag
AS SELECT * FROM known_tag_volatile;

--
-- Here we do pagination.
--
-- Row (entry_id, tag, x, y) means that entry is present at
-- y-th position of x-th page of given tag.
--
CREATE OR REPLACE VIEW pagination AS
SELECT entry_id, tag
     , FLOOR(abs_index / 5) + 1 AS page_id
     , (abs_index % 5 + 1)::integer AS entry_index
FROM (
    SELECT entry_id, tag
         , RANK() OVER (PARTITION BY tag ORDER BY rank) - 1
           AS abs_index
    FROM (
        SELECT entry_id, tag, rank
        FROM effective_entry_tag t, entry e
        WHERE t.entry_id = e.id
    ) AS entry_tag_rank
) AS entry_tag_index;

CREATE OR REPLACE VIEW page AS
SELECT DISTINCT page_id, tag
FROM pagination;

CREATE OR REPLACE VIEW page_url AS
SELECT page_id, tag, '/page/'||tag||'/'||page_id AS href
     , (page_id <> 1) AS primary
FROM page WHERE tag <> ''
UNION
SELECT page_id, tag, '/page/'||tag AS href, true
FROM page WHERE tag <> '' AND page_id = 1
UNION
SELECT page_id, '', '/page/'||page_id AS href, (page_id <> 1)
FROM page WHERE tag = ''
UNION
SELECT 1, '', '/', true
FROM page WHERE tag = '' AND page_id = 1
UNION
SELECT last_page_id, tag, '/page/'||tag||'/last', false
FROM (
    SELECT MAX(page_id) AS last_page_id, tag
    FROM page WHERE tag <> '' GROUP BY tag
) sq1
UNION
SELECT last_page_id, tag, '/page/last', false
FROM (
    SELECT MAX(page_id) AS last_page_id, tag
    FROM page WHERE tag = '' GROUP BY tag
) sq2;

CREATE OR REPLACE VIEW orphaned_page AS
SELECT page_id, tag FROM page p
WHERE NOT EXISTS (
    SELECT 1 FROM page_url pu
    WHERE pu.tag = p.tag AND pu.page_id = p.page_id
);

--
-- Here we bind URLs to entries.
--
-- Row (url, entry_id, kind, x) means that entry is present
-- at x-th position of page at url.
--
CREATE OR REPLACE VIEW url_binding AS
    SELECT '/entry/'||id    AS href
         , id               AS entry_id
         , 'entry'::varchar AS kind
         , 1                AS index
    FROM entry WHERE NOT invisible
UNION
    SELECT '/entry/'||link, entry_id, 'entry', 1
    FROM symlink WHERE kind='normal'
UNION
    SELECT '/meta/'||link, entry_id, 'entry.meta', 1
    FROM symlink WHERE kind='meta'
UNION
    SELECT pu.href, p.entry_id
         , CASE WHEN p.tag = 'meta' THEN 'list.meta'
                ELSE 'list'
           END
         , p.entry_index
    FROM page_url pu, pagination p
    WHERE pu.tag = p.tag AND pu.page_id = p.page_id;

--
-- Here we join url_binding, entry and effective_entry_tag to
-- allow webserver to fetch whole dataset for rendering a page in a
-- single query.
--
CREATE OR REPLACE VIEW page_display_volatile AS
WITH agg_tags AS (
    SELECT entry_id, string_agg(tag, ' ') as tags
    FROM effective_entry_tag
    GROUP BY entry_id
)
SELECT
    p.entry_id,
    p.href,
    p.index AS page_index,
    e.title,
    p.kind,
    CASE WHEN p.kind = 'list' OR p.kind = 'list.meta'
         THEN e.teaser
         ELSE e.body
    END AS content,
    e.teaser AS teaser,
    CASE WHEN p.kind = 'list' OR p.kind = 'list.meta'
         THEN e.read_more
         ELSE FALSE
    END AS read_more,
    (SELECT '/entry/'||link FROM symlink s
     WHERE kind = 'normal' AND e.id = s.entry_id) AS symlink,
    (SELECT '/meta/'||link FROM symlink s
     WHERE kind = 'meta' AND e.id = s.entry_id) AS metalink,
    at.tags
FROM url_binding p, entry e
LEFT JOIN agg_tags at ON at.entry_id = e.id
WHERE p.entry_id = e.id;

--
-- Here we collect data for previous/next links.
--
CREATE OR REPLACE VIEW previous_next_volatile AS
SELECT pu.href
     , (SELECT pu2.href FROM page_url pu2
        WHERE pu.tag = pu2.tag
          AND pu.page_id = pu2.page_id + 1
          AND pu2.primary) AS previous
     , (SELECT pu2.href FROM page_url pu2
        WHERE pu.tag = pu2.tag
          AND pu.page_id = pu2.page_id - 1
          AND pu2.primary) AS next
FROM page_url pu;

CREATE OR REPLACE VIEW series_links_volatile AS
WITH indices AS (
    SELECT entry_id, s.series
         , index AS own_index
         , (
             SELECT MIN(index)
             FROM series_assignment ss
             WHERE s.series = ss.series
           ) AS min_index
         , (
             SELECT MAX(index)
             FROM series_assignment ss
             WHERE s.series = ss.series AND s.index > ss.index
           ) AS prev_index
         , (
             SELECT MIN(index)
             FROM series_assignment ss
             WHERE s.series = ss.series AND s.index < ss.index
           ) AS next_index
         , (
             SELECT MAX(index)
             FROM series_assignment ss
             WHERE s.series = ss.series
           ) AS max_index           
    FROM series_assignment s
),
refs AS (
    SELECT i.entry_id, i.series
         , CASE WHEN i.own_index = i.min_index
                THEN NULL
                ELSE (
                    SELECT format_entry_link(entry_id)
                    FROM series_assignment s
                    WHERE s.series = i.series
                    AND s.index = i.min_index
                )
           END AS first_entry_link
         , (
             SELECT format_entry_link(entry_id) 
             FROM series_assignment s
             WHERE s.series = i.series
             AND s.index = i.prev_index
           ) AS prev_entry_link
         , (
             SELECT format_entry_link(entry_id)
             FROM series_assignment s
             WHERE s.series = i.series
             AND s.index = i.next_index
           ) AS next_entry_link
         , CASE WHEN i.own_index = i.max_index
                THEN NULL
                ELSE (
                    SELECT format_entry_link(entry_id)
                    FROM series_assignment s
                    WHERE s.series = i.series
                    AND s.index = i.max_index
                )
           END AS last_entry_link                   
    FROM indices i
),
nontrivial AS (
    SELECT * FROM refs
    WHERE first_entry_link IS NOT NULL
       OR prev_entry_link  IS NOT NULL
       OR next_entry_link  IS NOT NULL
       OR last_entry_link  IS NOT NULL              
)
SELECT * FROM nontrivial;

DROP MATERIALIZED VIEW page_display;
CREATE MATERIALIZED VIEW page_display
AS SELECT * FROM page_display_volatile;

DROP MATERIALIZED VIEW previous_next;
CREATE MATERIALIZED VIEW previous_next
AS SELECT * FROM previous_next_volatile;

DROP MATERIALIZED VIEW series_links;
CREATE MATERIALIZED VIEW series_links
AS SELECT * FROM series_links_volatile;
