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

module Password = struct
  type t = Bcrypt.hash_t

  let hash x = Bcrypt.hash x
  let check = Bcrypt.verify
end

type user =
  { id : int32
  ; name : string
  ; password : Password.t
  ; email : string
  ; email_digest : string
  ; is_admin : bool
  ; feeds_per_page : int32
  }

let to_user =
  let f x =
    { id = x#!id
    ; name = x#!name
    ; password = Bcrypt.hash_of_string x#!password
    ; email = x#!email
    ; email_digest = x#!email_digest
    ; is_admin = x#!is_admin
    ; feeds_per_page = x#!feeds_per_page
    }
  in
  Option.map f

let get_user_id_with_name name =
  Db.view_one
    (<:view< {
            u.id
            } | u in $Db_table.users$;
            u.name = $string:name$;
     >>)
  >|= fun x ->
  x#id

let get_user_with_name name =
  Db.view_opt
    (<:view<
      group {
        email_digest = md5[u.email];
      }
      by {
            u.id;
            u.name;
            u.password;
            u.email;
            u.is_admin;
            u.feeds_per_page;
            } | u in $Db_table.users$;
            u.name = $string:name$;
     >>)
  >|= to_user

let get_user_with_email email =
  Db.view_opt
    (<:view< group {
        email_digest = md5[u.email];
      }
      by {
        u.id;
        u.name;
        u.password;
        u.email;
        u.is_admin;
        u.feeds_per_page;
      } | u in $Db_table.users$; u.email = $string:email$; >>)
  >|= to_user

let add_user ~name ~password ~email =
  Db.query
    (<:insert< $Db_table.users$ := {
              id = $Db_table.users$?id;
              name = $string:name$;
              password = $string:Bcrypt.string_of_hash password$;
              email = $string:email$;
              is_admin = $Db_table.users$?is_admin;
              feeds_per_page = $Db_table.users$?feeds_per_page;
              } >>)

let update_user_password ~userid ~password =
  Db.query
    (<:update< u in $Db_table.users$ := {
              password = $string:Bcrypt.string_of_hash password$;
              } | u.id = $int32:userid$; >>)

let update_user_email ~userid ~email =
  Db.query
    (<:update< u in $Db_table.users$ := {
              email = $string:email$;
              } | u.id = $int32:userid$; >>)

let update_user_feeds_per_page ~userid ~nb_feeds =
  Db.query
    (<:update< u in $Db_table.users$ := {
              feeds_per_page = $int32:nb_feeds$;
              } | u.id = $int32:userid$; >>)
