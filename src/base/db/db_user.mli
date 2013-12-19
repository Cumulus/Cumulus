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

module Password : sig
  type t

  val hash : string -> t
  val check : string -> t -> bool
end

type user =
  { id : int32
  ; name : string
  ; password : Password.t
  ; email : string
  ; is_admin : bool
  ; feeds_per_page : int32
  }

val get_user_with_name : string -> user option Lwt.t
val get_user_id_with_name : string -> Sql.int32_t Sql.non_nullable_data Lwt.t
val get_user_with_email : string -> user option Lwt.t

val add_user :
  name:string ->
  password:Password.t ->
  email:string ->
  unit Lwt.t

val update_user_password :
  userid:int32 ->
  password:Password.t ->
  unit Lwt.t

val update_user_email :
  userid:int32 ->
  email:string ->
  unit Lwt.t

val update_user_feeds_per_page :
  userid:int32 ->
  nb_feeds:int32 ->
  unit Lwt.t
