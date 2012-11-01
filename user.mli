type user
type user_state = Already_connected | Ok | Bad_password | Not_found

val user_new :
  < email : Sql.string_t Db.macaque_type Sql.t;
  id : Sql.int32_t Db.macaque_type Sql.t;
  name : Sql.string_t Db.macaque_type Sql.t;
  password : Sql.string_t Db.macaque_type Sql.t;
  is_admin : Sql.bool_t Db.macaque_type Sql.t > ->
  user
val add : string -> string -> string -> unit Lwt.t
val get_userid : unit -> (int32 option) Lwt.t
val is_connected : unit -> bool Lwt.t
val is_admin : unit -> bool Lwt.t
val connect : user -> string -> user_state Lwt.t
val disconnect : unit -> bool Lwt.t
val get_user_and_email :
  unit ->
  < email: Sql.string_t Db.macaque_type Sql.t;
  name : Sql.string_t Db.macaque_type Sql.t >
    option Lwt.t
val update_password : string -> unit Lwt.t
val update_email : string -> unit Lwt.t
