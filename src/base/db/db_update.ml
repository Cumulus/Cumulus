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

let options = (<:table< options (
                       name text NOT NULL,
                       value text NOT NULL
                       ) >>)

let name = (<:value< "dbversion" >>)

let current_version () =
  Db.view_one
    (<:view< {
            o.value;
            } | o in $options$;
            o.name = $name$;
     >>)
  >>= fun version ->
  Lwt.return (int_of_string version#!value)

let update_version value =
  let value = string_of_int value in
  Db.query
    (<:update< o in $options$ := {
              value = $string:value$;
              } | o.name = $name$; >>)

let update version f =
  current_version () >>= function
  | v when v < version ->
      Printf.eprintf "Updating Cumulus database to version %d\n" version;
      f () >>= fun () ->
      update_version version
  | _ -> Lwt.return ()

let () =
  Lwt_main.run begin
    update 2
      (fun () ->
         Db.alter "ALTER TABLE users ADD COLUMN \
                   feeds_per_page integer NOT NULL DEFAULT(10)"
      )
    >>= fun () ->
    update 3
      (fun () ->
         Db.alter "ALTER TABLE feeds ALTER url DROP NOT NULL" >>= fun () ->
         Db.alter "ALTER TABLE feeds ADD COLUMN parent integer" >>= fun () ->
         Db.alter "ALTER TABLE feeds ADD COLUMN root integer" >>= fun () ->
         Db.alter "ALTER TABLE feeds RENAME COLUMN title TO description"
      )
    >>= fun () ->
    update 4
      (fun () ->
         Db.alter "CREATE TABLE favs (\
                   id_user integer NOT NULL, \
                   id_feed integer NOT NULL);"
      )
    >>= fun () ->
    update 5
      (fun () ->
         Db.alter "CREATE TABLE votes (\
                   id_user integer NOT NULL, \
                   id_feed integer NOT NULL, \
                   score integer NOT NULL);"
      )
    >>= fun () ->
    update 6
      (fun () ->
         Db.alter "ALTER TABLE options ADD PRIMARY KEY (name)" >>= fun () ->

         Db.alter "ALTER TABLE users ADD PRIMARY KEY (id)" >>= fun () ->
         Db.alter "ALTER TABLE users ALTER is_admin DROP DEFAULT" >>= fun () ->
         Db.alter "ALTER TABLE users ALTER feeds_per_page DROP DEFAULT" >>= fun () ->
         Db.alter "ALTER TABLE users ADD CHECK (feeds_per_page > 0)" >>= fun () ->

         Db.alter "ALTER TABLE feeds ADD PRIMARY KEY (id)" >>= fun () ->
         Db.alter "ALTER TABLE feeds ADD FOREIGN KEY (author) REFERENCES users (id)" >>= fun () ->

         Db.alter "ALTER TABLE feeds_tags DROP COLUMN id" >>= fun () ->
         (* Because we didn't remove all the orphan tags *)
         Db.alter "DELETE FROM feeds_tags AS t WHERE NOT EXISTS (SELECT * FROM feeds AS f WHERE t.id_feed = f.id)" >>= fun () ->
         Db.alter "ALTER TABLE feeds_tags ADD FOREIGN KEY (id_feed) REFERENCES feeds (id) ON DELETE CASCADE" >>= fun () ->

         Db.alter "ALTER TABLE favs ADD FOREIGN KEY (id_user) REFERENCES users (id)" >>= fun () ->
         (* Because we didn't remove all the orphan favs *)
         Db.alter "DELETE FROM favs AS t WHERE NOT EXISTS (SELECT * FROM feeds AS f WHERE t.id_feed = f.id)" >>= fun () ->
         Db.alter "ALTER TABLE favs ADD FOREIGN KEY (id_feed) REFERENCES feeds (id) ON DELETE CASCADE" >>= fun () ->

         Db.alter "ALTER TABLE votes ADD FOREIGN KEY (id_user) REFERENCES users (id)" >>= fun () ->
         Db.alter "ALTER TABLE votes ADD FOREIGN KEY (id_feed) REFERENCES feeds (id) ON DELETE CASCADE" >>= fun () ->

         Db.alter "DROP SEQUENCE feeds_tags_id_seq"
      )
  end
