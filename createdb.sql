GRANT ALL PRIVILEGES ON DATABASE cumulus TO cumulus;
SET client_encoding = 'UTF8';

CREATE TABLE options (
  name text NOT NULL,
  value text NOT NULL
);

CREATE TABLE feeds (
    id integer NOT NULL,
    url text,
    description text NOT NULL,
    timedate timestamp NOT NULL,
    author integer NOT NULL,
    parent integer,
    root integer
);

CREATE TABLE feeds_tags (
    id integer NOT NULL,
    tag text NOT NULL,
    id_feed integer NOT NULL
);

CREATE TABLE users (
  id integer NOT NULL,
  name text NOT NULL,
  password text NOT NULL,
  email text NOT NULL,
  is_admin boolean NOT NULL DEFAULT(false),
  feeds_per_page integer NOT NULL DEFAULT(10)
);

CREATE SEQUENCE users_id_seq;
CREATE SEQUENCE feeds_id_seq;
CREATE SEQUENCE feeds_tags_id_seq;

INSERT INTO options VALUES ('dbversion', '2');
