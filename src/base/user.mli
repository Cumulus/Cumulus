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

type user_state = Already_connected | Ok | Bad_password | Not_found

type user = Db_user.user =
  { id : int32
  ; name : string
  ; password : Db_user.Password.t
  ; email : string
  ; is_admin : bool
  ; feeds_per_page : int32
  }

val add : string * (string * (string * string)) -> bool Lwt.t
val get_user : unit -> user option Lwt.t
val get_userid : unit -> int32 option Lwt.t
val is_connected : unit -> bool Lwt.t
val is_admin : unit -> bool Lwt.t
val connect : string -> string -> user_state Lwt.t
val disconnect : unit -> bool Lwt.t
val get_user_and_email : unit -> < email : string; name : string > option Lwt.t
val update_password : string * string -> bool Lwt.t
val update_email : string -> bool Lwt.t
val update_feeds_per_page : int32 -> bool Lwt.t

val get_offset : unit -> int32 Lwt.t

val send_reset_email :
  service:((Db_user.user ->
            (unit, unit, [< Eliom_service.get_service_kind ],
             [< Eliom_service.suff ], 'a, unit,
             [< Eliom_service.registrable ], 'b)
              Eliom_service.service)) ->
  string ->
  unit Lwt.t

val force_connect : Db_user.user -> unit Lwt.t
