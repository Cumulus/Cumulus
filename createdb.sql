SET client_encoding = 'UTF8';

CREATE USER cumulus PASSWORD 'cumulus';
CREATE DATABASE cumulus OWNER cumulus ENCODING 'UTF8';
\connect cumulus

CREATE TABLE options (
  name text PRIMARY KEY,
  value text NOT NULL
);
ALTER TABLE options OWNER TO cumulus;

CREATE TABLE users (
  id integer PRIMARY KEY,
  name text NOT NULL UNIQUE,
  password text NOT NULL,
  email text NOT NULL UNIQUE,
  is_admin boolean NOT NULL,
  feeds_per_page integer NOT NULL CHECK (feeds_per_page > 0)
);
ALTER TABLE users OWNER TO cumulus;

CREATE TABLE feeds (
    id integer PRIMARY KEY,
    url text UNIQUE,
    description text NOT NULL,
    timedate timestamp NOT NULL,
    author integer NOT NULL REFERENCES users (id),
    parent integer REFERENCES feeds (id) ON DELETE CASCADE,
    root integer REFERENCES feeds (id) ON DELETE CASCADE,
    leftBound integer NOT NULL,
    rightBound integer NOT NULL CHECK (rightBound > leftBound)
);
ALTER TABLE feeds OWNER TO cumulus;

CREATE TABLE feeds_tags (
    tag text NOT NULL,
    id_feed integer NOT NULL REFERENCES feeds (id) ON DELETE CASCADE
);
ALTER TABLE feeds_tags OWNER TO cumulus;

CREATE TABLE favs (
  id_user integer NOT NULL REFERENCES users (id),
  id_feed integer NOT NULL REFERENCES feeds (id) ON DELETE CASCADE
);
ALTER TABLE favs OWNER TO cumulus;

CREATE TABLE votes (
  id_user integer NOT NULL REFERENCES users (id),
  id_feed integer NOT NULL REFERENCES feeds (id) ON DELETE CASCADE,
  score integer NOT NULL
);
ALTER TABLE votes OWNER TO cumulus;

CREATE SEQUENCE users_id_seq;
ALTER SEQUENCE users_id_seq OWNER TO cumulus;

CREATE SEQUENCE feeds_id_seq;
ALTER SEQUENCE feeds_id_seq OWNER TO cumulus;

INSERT INTO options VALUES ('dbversion', '10');
