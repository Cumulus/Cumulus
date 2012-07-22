SET client_encoding = 'UTF8';

CREATE TABLE feeds (
    id integer NOT NULL,
    url text NOT NULL,
    title text NOT NULL,
    timedate timestamp NOT NULL,
    author integer NOT NULL
);

CREATE TABLE feeds_tags (
    tag text NOT NULL,
    id_feed integer NOT NULL
);

CREATE TABLE users (
  id integer NOT NULL,
  name text NOT NULL,
  password text NOT NULL,
  email text NOT NULL
);

CREATE SEQUENCE users_id_seq;
CREATE SEQUENCE feeds_id_seq;
