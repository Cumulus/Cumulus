type user
type user_state = Already_connected | Ok | Bad_password | Not_found

val user_new :
  < email : < get : 'a; nul : Sql.non_nullable; t : Sql.string_t > Sql.t;
  id : < get : 'b; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
  name : < get : 'c; nul : Sql.non_nullable; t : Sql.string_t > Sql.t;
  password : < get : 'd; nul : Sql.non_nullable; t : Sql.string_t > Sql.t; > ->
  user
val hash_password : string -> string
val check_password : user -> string -> bool
val get_userid : unit -> (int32 option) Lwt.t
val is_connected : unit -> bool Lwt.t
val connect : user -> user_state Lwt.t
val disconnect : unit -> bool Lwt.t
