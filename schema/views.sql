
--
-- Here we infer the tags by the following rules:
--  * every entry has it's own "natural" tags
--  * every "small" (read_more = false; meaning that teaser and
--    body are the same) entry is tagged "micro"
--  * every entry with "meta" link is tagged "meta"
--
CREATE OR REPLACE VIEW effective_entry_tag AS
SELECT DISTINCT * FROM (
        SELECT entry_id, tag FROM entry_tag
    UNION
        SELECT id, 'micro' FROM entry
        WHERE NOT read_more
    UNION
        SELECT entry_id, 'meta' FROM symlink
        WHERE kind = 'meta'
) sq
ORDER BY tag;

--
-- Here we do pagination.
--
-- Row (entry_id, tag, x, y) means that entry is present at
-- y-th position of x-th page of given tag; front page list
-- is assumed to have tag = ''.
--
CREATE OR REPLACE VIEW pagination AS
WITH
entry_tag_rank AS (
    SELECT entry_id, tag, rank
    FROM effective_entry_tag t, entry e
    WHERE t.entry_id = e.id
UNION
    SELECT id AS entry_id, '' AS tag, rank
    FROM entry WHERE NOT invisible
), absolute_index AS (
    SELECT etr.entry_id, etr.tag AS tag, (
        SELECT COUNT(1) FROM entry_tag_rank tt
        WHERE tt.rank < etr.rank AND etr.tag = tt.tag
    ) AS abs_index
    FROM entry_tag_rank etr
)
SELECT
    ai.entry_id, ai.tag,
    floor(ai.abs_index / 5) + 1 AS page_id,
    (ai.abs_index % 5 + 1)::integer AS entry_index
FROM absolute_index ai;

--
-- Here we bind URLs to entries.
--
-- Row (url, entry_id, kind, x) means that entry is present
-- at x-th position of page at url.
--
CREATE OR REPLACE VIEW url_binding AS
    SELECT
        '/entry/'||id    AS href,
        id               AS entry_id,
        'entry'::varchar AS kind,
        1                AS index
    FROM entry WHERE NOT invisible
UNION
    SELECT
        '/entry/'||link, entry_id, 'entry', 1
    FROM symlink WHERE kind='normal'
UNION
    SELECT
        '/meta/'||link, entry_id, 'entry.meta', 1
    FROM symlink WHERE kind='meta'
UNION
    SELECT
        '/page/'||page_id, entry_id, 'list', entry_index
    FROM pagination WHERE tag = ''
UNION
    SELECT
        '/page/'||tag||'/'||page_id, entry_id, 'list', entry_index
    FROM pagination WHERE tag <> '' AND tag <> 'meta'
UNION
    SELECT
        '/page/meta/'||page_id, entry_id, 'list.meta', entry_index
    FROM pagination WHERE tag = 'meta'
UNION
    SELECT
        '/', entry_id, 'list', entry_index
    FROM pagination WHERE tag = '' AND page_id = 1;

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
WITH possibles AS (
    SELECT
        '/page/'||page_id AS href,
        CASE WHEN page_id = 2
             THEN '/'
              ELSE '/page/'||(page_id - 1) 
        END AS previous,
        '/page/'||(page_id + 1) AS next
    FROM pagination WHERE tag = ''
UNION
    SELECT '/page/'||tag||'/'||page_id AS href,
           '/page/'||tag||'/'||(page_id - 1) AS previous,
           '/page/'||tag||'/'||(page_id + 1) AS next
    FROM pagination WHERE tag <> ''
UNION
    SELECT '/' AS href,
           null AS previous,
           '/page/2' AS next
    FROM pagination WHERE tag = '' AND page_id = 1
)
SELECT DISTINCT
    href AS href,
    (SELECT DISTINCT href FROM url_binding
     WHERE href = previous) AS previous,
    (SELECT DISTINCT href FROM url_binding
     WHERE href = next) AS next
FROM possibles;

DROP MATERIALIZED VIEW page_display;
CREATE MATERIALIZED VIEW page_display
AS SELECT * FROM page_display_volatile;

DROP MATERIALIZED VIEW previous_next;
CREATE MATERIALIZED VIEW previous_next
AS SELECT * FROM previous_next_volatile;