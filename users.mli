type users

val users_new : unit -> users
val connect_user : users -> string -> string -> User.user_state Lwt.t
val add_user : users -> string -> string -> string -> bool Lwt.t
