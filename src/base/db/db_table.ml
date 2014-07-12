(*
Copyright (c) 2012 Enguerrand Decorne

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Batteries
open Eliom_lib.Lwt_ops

let feeds_id_seq = (<:sequence< serial "feeds_id_seq" >>)

let feeds = (<:table< feeds (
                     id integer NOT NULL DEFAULT(nextval $feeds_id_seq$),
                     url text,
                     description text NOT NULL,
                     timedate timestamp NOT NULL DEFAULT(localtimestamp ()),
                     author integer NOT NULL,
                     parent integer,
                     root integer,
                     leftBound integer NOT NULL,
                     rightBound integer NOT NULL
                     ) >>)

let feeds_tags = (<:table< feeds_tags (
                          tag text NOT NULL,
                          id_feed integer NOT NULL
                          ) >>)

let votes = (<:table< votes (
                     id_user integer NOT NULL,
                     id_feed integer NOT NULL,
                     score integer NOT NULL
                     ) >>)

let users_id_seq = (<:sequence< serial "users_id_seq" >>)

let users = (<:table< users (
                     id integer NOT NULL DEFAULT(nextval $users_id_seq$),
                     name text NOT NULL,
                     password text NOT NULL,
                     email text NOT NULL,
                     is_admin boolean NOT NULL DEFAULT(false),
                     feeds_per_page integer NOT NULL DEFAULT(10)
                     ) >>)

let favs = (<:table< favs (
                    (* id integer NOT NULL DEFAULT(nextval $favs_id_seq$), *)
                    id_user integer NOT NULL,
                    id_feed integer NOT NULL
                    ) >>)
