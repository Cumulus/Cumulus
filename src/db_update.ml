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

let (>>=) = Lwt.(>>=)

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
                score interger NOT NULL);"
      )
  end
