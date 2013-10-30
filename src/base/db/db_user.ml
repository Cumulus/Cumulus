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

module Option = Eliom_lib.Option

let (>>=) = Lwt.(>>=)

type password = Bcrypt.hash_t

let to_password x = Bcrypt.hash x
let check_password = Bcrypt.verify

let users_id_seq = (<:sequence< serial "users_id_seq" >>)

let users = (<:table< users (
                     id integer NOT NULL DEFAULT(nextval $users_id_seq$),
                     name text NOT NULL,
                     password text NOT NULL,
                     email text NOT NULL,
                     is_admin boolean NOT NULL DEFAULT(false),
                     feeds_per_page integer NOT NULL DEFAULT(10)
                     ) >>)

let get_user_id_with_name name =
  Db.view_one
    (<:view< {
            u.id
            } | u in $users$;
            u.name = $string:name$;
     >>)

let get_user_name_and_email_with_id id =
  Db.view_one
    (<:view< {
            u.name;
            u.email;
            } | u in $users$;
            u.id = $int32:id$;
     >>)

let get_user_with_name name =
  Db.view_opt
    (<:view< {
            u.id;
            u.name;
            u.password;
            u.email;
            u.is_admin;
            u.feeds_per_page;
            } | u in $users$;
            u.name = $string:name$;
     >>)
  >>= fun user ->
  Lwt.return
    (Option.map
       (fun x ->
          object
            method id = x#id;
            method name = x#name;
            method password = Bcrypt.hash_of_string x#!password
            method email = x#email;
            method is_admin = x#is_admin;
            method feeds_per_page = x#feeds_per_page;
          end
       )
       user
    )

let add_user ~name ~password ~email () =
  Db.query
    (<:insert< $users$ := {
              id = users?id;
              name = $string:name$;
              password = $string:Bcrypt.string_of_hash password$;
              email = $string:email$;
              is_admin = users?is_admin;
              feeds_per_page = users?feeds_per_page;
              } >>)

let update_user_password ~userid ~password () =
  Db.query
    (<:update< u in $users$ := {
              password = $string:Bcrypt.string_of_hash password$;
              } | u.id = $int32:userid$; >>)

let update_user_email ~userid ~email () =
  Db.query
    (<:update< u in $users$ := {
              email = $string:email$;
              } | u.id = $int32:userid$; >>)

let update_user_feeds_per_page ~userid ~nb_feeds () =
  Db.query
    (<:update< u in $users$ := {
              feeds_per_page = $int32:nb_feeds$;
              } | u.id = $int32:userid$; >>)
