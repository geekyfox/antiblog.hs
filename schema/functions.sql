
CREATE OR REPLACE FUNCTION strpos2(text VARCHAR, sub1 VARCHAR, sub2 VARCHAR)
RETURNS INTEGER AS $$
DECLARE
    x INTEGER;
    y INTEGER;
BEGIN
    x := STRPOS(text, sub1);
    y := STRPOS(text, sub2);

    IF x = 0 THEN
        RETURN y;
    ELSIF y = 0 THEN
        RETURN x;
    ELSE
        RETURN LEAST(x, y);
    END IF;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE OR REPLACE FUNCTION slide_entry_ranks(a_bottom_rank integer)
RETURNS VOID AS $$
DECLARE
    v_iterid integer;
BEGIN
    FOR v_iterid IN (
        SELECT id FROM entry
        WHERE rank >= a_bottom_rank ORDER BY rank DESC
    ) LOOP
        UPDATE entry SET rank = rank + 1 WHERE id = v_iterid;
    END LOOP;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION slide_feed_position()
RETURNS VOID AS $$
DECLARE
    v_iterid integer;
BEGIN
    FOR v_iterid IN (
        SELECT entry_id FROM rss_entry
        ORDER BY feed_position DESC
    ) LOOP
        UPDATE rss_entry SET feed_position = feed_position + 1
        WHERE entry_id = v_iterid;
    END LOOP;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION nightly_maintenance()
RETURNS TABLE(a_log varchar) AS $$
DECLARE
    v_max INTEGER;
    v_count INTEGER;
    v_raise_id INTEGER;
BEGIN
    SELECT MAX(rank) INTO v_max FROM entry;
    SELECT COUNT(1) INTO v_count FROM entry;

    IF v_count = 0 THEN
        a_log := 'Database is empty, nothing to regenerate';
        RETURN NEXT;
        RETURN;
    END IF;

    SELECT id INTO v_raise_id FROM entry WHERE rank = v_max;

    a_log := 'Promoted entry '||format_entry_link(v_raise_id);
    RETURN NEXT;

    PERFORM slide_entry_ranks(1);
    UPDATE entry SET rank = 1 WHERE id = v_raise_id;

    DELETE FROM rss_entry WHERE entry_id = v_raise_id;
    PERFORM slide_feed_position();
    INSERT INTO rss_entry(entry_id, feed_position)
    VALUES (v_raise_id, 1);
    DELETE FROM rss_entry WHERE feed_position > 10;

    IF v_max = v_count THEN
        UPDATE entry e SET rank = rank * 2
        WHERE MOD(e.rank, 2) = 1
          AND NOT EXISTS
            (SELECT 1 FROM rss_entry re WHERE e.id = re.entry_id)
          AND NOT EXISTS
            (SELECT 1 FROM entry ee WHERE ee.rank = e.rank * 2);
        a_log := 'Performed reshuffling';
        RETURN NEXT;
    END IF;

    PERFORM update_materialized_views();
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION update_materialized_views()
RETURNS VOID AS $$
BEGIN
    REFRESH MATERIALIZED VIEW known_tag;
    REFRESH MATERIALIZED VIEW page_display;
    REFRESH MATERIALIZED VIEW previous_next;
    REFRESH MATERIALIZED VIEW series_links;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION create_entry(
    a_title VARCHAR, a_body VARCHAR, a_summary VARCHAR, a_sign VARCHAR
)
RETURNS INTEGER AS $$
DECLARE
    v_inject INTEGER;
    v_tmp INTEGER;
    v_rank INTEGER;
    v_max INTEGER;
BEGIN
    LOOP
        v_inject := ROUND(RANDOM() * 9000000 + 1000000);
        IF NOT EXISTS (SELECT 1 FROM entry WHERE id = v_inject) THEN
            EXIT;
        END IF;
    END LOOP;

    SELECT MAX(rank) INTO v_max FROM entry;
            
    IF v_max IS NULL THEN
        v_rank := 1;
    ELSE
        v_rank := ROUND(RANDOM() * v_max) + 1;
        PERFORM slide_entry_ranks(v_rank);
    END IF;

    INSERT INTO entry
        (id, title, teaser, body, read_more, invisible, rank,
         md5_signature)
    VALUES
        (v_inject, '', '', '', FALSE, TRUE, v_rank, '');

    PERFORM update_entry(v_inject, a_title, a_body, a_summary, a_sign);

    RETURN v_inject;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION update_entry(
    a_entry_id INTEGER, a_title VARCHAR, a_body VARCHAR,
    a_summary VARCHAR, a_sign VARCHAR
)
RETURNS VOID AS $$
DECLARE
    v_teaser VARCHAR;
    v_more BOOLEAN;
    v_cutoff INTEGER;
    v_rank INTEGER;
    v_max INTEGER;
BEGIN
    IF a_summary IS NOT NULL THEN
        v_more := TRUE;
        v_teaser := a_summary;
    ELSIF LENGTH(a_body) < 600 THEN
        v_more := FALSE;
        v_teaser := a_body;
    ELSE
        v_more := TRUE;
        v_teaser := REVERSE(LEFT(a_body, 600));
        v_cutoff := STRPOS2(v_teaser, ' ', '\n');
        v_teaser := REVERSE(SUBSTR(v_teaser, v_cutoff));
    END IF;

    UPDATE entry SET title = a_title, 
                     teaser = v_teaser, 
                     body = a_body, 
                     read_more = v_more,
                     invisible = FALSE,
                     md5_signature = a_sign
    WHERE id = a_entry_id;
    
    PERFORM update_materialized_views();
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION delete_entry(a_entry_id INTEGER)
RETURNS VOID AS $$
BEGIN
    UPDATE entry SET invisible = TRUE WHERE id = a_entry_id;
    
    PERFORM update_materialized_views();    
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION assign_symlinks(
    a_id integer,
    a_symlink varchar,
    a_metalink varchar
) RETURNS VOID AS $$
BEGIN
    DELETE FROM symlink WHERE entry_id = a_id;

    IF a_symlink IS NOT NULL THEN
        DELETE FROM symlink
        WHERE link = a_symlink AND kind = 'normal';
        
        INSERT INTO symlink(link, kind, entry_id)
        VALUES (a_symlink, 'normal', a_id);
    END IF;
    
    IF a_metalink IS NOT NULL THEN
        DELETE FROM symlink
        WHERE link = a_metalink AND kind = 'meta';

        INSERT INTO symlink(link, kind, entry_id)
        VALUES (a_metalink, 'meta', a_id);
    END IF;
        
    PERFORM update_materialized_views();
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION format_entry_link(a_id int)
RETURNS varchar AS $$
DECLARE
    v_link varchar;
BEGIN
    SELECT link INTO v_link FROM symlink
    WHERE entry_id = a_id AND kind = 'normal';

    IF FOUND THEN
        RETURN '/entry/'||v_link;
    END IF;

    SELECT link INTO v_link FROM symlink
    WHERE entry_id = a_id AND kind = 'meta';

    IF FOUND THEN
        RETURN '/meta/'||v_link;
    END IF;
    
    RETURN '/entry/'||a_id;
END;
$$ LANGUAGE plpgsql STABLE;

CREATE OR REPLACE FUNCTION random_entry()
RETURNS varchar AS $$
DECLARE
    v_id int;
BEGIN
    SELECT id INTO v_id FROM entry
    ORDER BY random() LIMIT 1;
        
    IF NOT FOUND THEN
        RETURN '/';
    ELSE
        RETURN format_entry_link(v_id);
    END IF;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION random_entry(a_referer varchar)
RETURNS varchar AS $$
DECLARE
    v_id int;
BEGIN
    FOR v_id IN SELECT id FROM entry e ORDER BY random() LOOP
        IF NOT EXISTS
            (SELECT 1 FROM url_binding p
             WHERE a_referer LIKE '%'||href
             AND p.entry_id = v_id)
        THEN
            RETURN format_entry_link(v_id);
        END IF;
    END LOOP;

    IF NOT FOUND THEN
        RETURN '/';
    ELSE
        RETURN format_entry_link(v_id);
    END IF;
END;
$$ LANGUAGE plpgsql VOLATILE;

CREATE OR REPLACE FUNCTION promote_entry(a_entry_id INTEGER)
RETURNS VOID AS $$
DECLARE
    v_max int;
BEGIN
    SELECT MAX(rank) INTO v_max FROM entry;
    
    IF v_max IS NOT NULL THEN
        UPDATE entry SET rank = v_max + 1
        WHERE id = a_entry_id;
        
        PERFORM update_materialized_views();
    END IF;
END;
$$ LANGUAGE plpgsql VOLATILE;

