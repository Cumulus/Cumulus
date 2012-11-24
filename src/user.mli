type user_state = Already_connected | Ok | Bad_password | Not_found

type user = {
  id : int32;
  name : string;
  password : Bcrypt.hash_t;
  email : string;
  is_admin : bool;
  feeds_per_page : int32;
}

val add : string * (string * (string * string)) -> bool Lwt.t
val get_user : unit -> (user option) Lwt.t
val get_userid : unit -> (int32 option) Lwt.t
val get_user_feeds_per_page : unit -> (int32 option) Lwt.t
val is_connected : unit -> bool Lwt.t
val is_admin : unit -> bool Lwt.t
val connect : string -> string -> user_state Lwt.t
val disconnect : unit -> bool Lwt.t
val get_user_and_email :
  unit ->
  < email: Sql.string_t Db.macaque_type Sql.t;
  name : Sql.string_t Db.macaque_type Sql.t >
    option Lwt.t
val update_password : string * string -> bool Lwt.t
val update_email : string -> bool Lwt.t
val update_feeds_per_page : int32 -> bool Lwt.t

val get_offset : unit -> int32 Lwt.t
