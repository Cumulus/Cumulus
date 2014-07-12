GRANT ALL PRIVILEGES ON DATABASE cumulus TO cumulus;
SET client_encoding = 'UTF8';

CREATE TABLE options (
  name text PRIMARY KEY,
  value text NOT NULL
);

CREATE TABLE users (
  id integer PRIMARY KEY,
  name text NOT NULL,
  password text NOT NULL,
  email text NOT NULL,
  is_admin boolean NOT NULL,
  feeds_per_page integer NOT NULL CHECK (feeds_per_page > 0)
);

CREATE TABLE feeds (
    id integer PRIMARY KEY,
    url text,
    description text NOT NULL,
    timedate timestamp NOT NULL,
    author integer NOT NULL REFERENCES users (id),
    parent integer REFERENCES feeds (id) ON DELETE CASCADE,
    root integer REFERENCES feeds (id) ON DELETE CASCADE,
    leftBound integer NOT NULL,
    rightBound integer NOT NULL CHECK (rightBound > leftBound)
);

CREATE TABLE feeds_tags (
    tag text NOT NULL,
    id_feed integer NOT NULL REFERENCES feeds (id) ON DELETE CASCADE
);

CREATE TABLE favs (
  id_user integer NOT NULL REFERENCES users (id),
  id_feed integer NOT NULL REFERENCES feeds (id) ON DELETE CASCADE
);

CREATE TABLE votes (
  id_user integer NOT NULL REFERENCES users (id),
  id_feed integer NOT NULL REFERENCES feeds (id) ON DELETE CASCADE,
  score integer NOT NULL
);

CREATE SEQUENCE users_id_seq;
CREATE SEQUENCE feeds_id_seq;

INSERT INTO options VALUES ('dbversion', '8');
