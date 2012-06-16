type user
type user_state = Already_connected | Ok | Bad_password | Not_found

val check_password : user -> string -> bool
val is_connected : unit -> bool Lwt.t
val connect : string -> user_state Lwt.t
val disconnect : unit -> bool Lwt.t
