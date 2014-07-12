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

let compute_interval_representation () =
  let open Int32 in
  Db.view
    (<:view< {
             f.id;
             f.root;
             f.parent;
             }
             | f in $Db_table.feeds$;
             is_null f.root || is_null f.parent; >>)
  >>= fun feeds ->
  let rec update_child ~leftBound parent =
    Db.view
      (<:view< {
               f.id;
               f.root;
               f.parent;
               } order by f.id
               | f in $Db_table.feeds$;
                 f.parent = $int32:parent$ >>)
    >>= fun childs ->
    let rec aux bound = function
      | [] -> Lwt.return (bound)
      | x :: r ->
          update_child ~leftBound:(bound + one) x#!id
          >>= fun rightBound ->
          Db.query
            (<:update< row in $Db_table.feeds$ :=
                       { leftBound = $int32:bound$;
                         rightBound = $int32:rightBound$ }
                       | row.id = $int32:x#!id$ >>)
          >>= fun () -> aux (rightBound + one) r
    in aux leftBound childs
  in
  let rec update_root ?(leftBound = zero) = function
    | [] -> Lwt.return ()
    | x :: r ->
        update_child ~leftBound:(leftBound + one) x#!id
        >>= fun rightBound ->
        Db.query
          (<:update< row in $Db_table.feeds$ :=
                     { leftBound = $int32:leftBound$;
                       rightBound = $int32:rightBound$ }
                     | row.id = $int32:x#!id$ >>)
        >>= fun () -> update_root ~leftBound:(rightBound + one) r
  in update_root feeds

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
         Db.alter "DELETE FROM feeds_tags AS t WHERE t.id_feed NOT IN (SELECT id FROM feeds)" >>= fun () ->
         Db.alter "ALTER TABLE feeds_tags ADD FOREIGN KEY (id_feed) REFERENCES feeds (id) ON DELETE CASCADE" >>= fun () ->

         Db.alter "ALTER TABLE favs ADD FOREIGN KEY (id_user) REFERENCES users (id)" >>= fun () ->
         (* Because we didn't remove all the orphan favs *)
         Db.alter "DELETE FROM favs AS f WHERE f.id_feed NOT IN (SELECT id FROM feeds)" >>= fun () ->
         Db.alter "ALTER TABLE favs ADD FOREIGN KEY (id_feed) REFERENCES feeds (id) ON DELETE CASCADE" >>= fun () ->

         Db.alter "ALTER TABLE votes ADD FOREIGN KEY (id_user) REFERENCES users (id)" >>= fun () ->
         Db.alter "ALTER TABLE votes ADD FOREIGN KEY (id_feed) REFERENCES feeds (id) ON DELETE CASCADE" >>= fun () ->

         Db.alter "DROP SEQUENCE feeds_tags_id_seq"
      )
    >>= fun () ->
    update 7
      (fun () ->
         (* Because we didn't remove all the orphan feeds *)
         Db.alter "DELETE FROM feeds AS f WHERE f.parent NOT IN (SELECT id FROM feeds)" >>= fun () ->
         Db.alter "ALTER TABLE feeds ADD FOREIGN KEY (parent) REFERENCES feeds (id) ON DELETE CASCADE" >>= fun () ->
         Db.alter "ALTER TABLE feeds ADD FOREIGN KEY (root) REFERENCES feeds (id) ON DELETE CASCADE"
      )
    >>= fun () ->
    update 8
      (fun () ->
         Db.alter "ALTER TABLE feeds ADD COLUMN leftbound integer NOT NULL DEFAULT(0)" >>= fun () ->
         Db.alter "ALTER TABLE feeds ADD COLUMN rightbound integer NOT NULL DEFAULT(0)"
         >>= compute_interval_representation >>= fun () ->
         Db.alter "ALTER TABLE feeds ADD CHECK (rightBound > leftBound)"
      )
  end
