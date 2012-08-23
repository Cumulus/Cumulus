val connect_user : string -> string -> User.user_state Lwt.t
val add_user : (string * (string * (string * string))) -> bool Lwt.t
val update_user_password : (string * string) -> bool Lwt.t
val update_user_mail : (string) -> bool Lwt.t
