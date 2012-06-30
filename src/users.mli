val connect_user : string -> string -> User.user_state Lwt.t
val add_user : (string * (string * string)) -> bool Lwt.t
