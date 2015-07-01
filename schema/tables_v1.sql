
CREATE TABLE IF NOT EXISTS entry (
    id        INTEGER UNIQUE PRIMARY KEY NOT NULL,
    title     VARCHAR(200) NOT NULL,
    teaser    VARCHAR(600) NOT NULL,
    body      TEXT NOT NULL,
    read_more BOOLEAN NOT NULL,
    invisible BOOLEAN NOT NULL,
    rank      INTEGER UNIQUE NOT NULL,
    md5_signature VARCHAR(32) NOT NULL
);

CREATE TABLE IF NOT EXISTS rss_entry (
    entry_id INTEGER PRIMARY KEY NOT NULL,
    feed_position INTEGER UNIQUE NOT NULL,
    date_posted TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,

    FOREIGN KEY (entry_id) REFERENCES entry (id) ON DELETE RESTRICT
);

CREATE TABLE IF NOT EXISTS symlink (
    link VARCHAR(255) NOT NULL,
    kind VARCHAR(32) NOT NULL,
    entry_id INTEGER NOT NULL,

    PRIMARY KEY (link, kind),
    FOREIGN KEY (entry_id) REFERENCES entry (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS entry_tag (
    entry_id INTEGER NOT NULL,
    tag VARCHAR(64) NOT NULL,

    PRIMARY KEY (entry_id, tag),
    FOREIGN KEY (entry_id) REFERENCES entry (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS series_assignment (
    entry_id INTEGER NOT NULL,
    series   VARCHAR(255) NOT NULL,
    index    INTEGER NOT NULL,
    
    PRIMARY KEY (entry_id, series),
    FOREIGN KEY (entry_id) REFERENCES entry (id) ON DELETE CASCADE
);

