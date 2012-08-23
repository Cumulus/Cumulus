let connect_user username password =
  Db.get_user_with_name username >>= function
    | None -> Lwt.return User.Not_found
    | (Some user) ->
      let user = User.user_new user in
      User.connect user password

let add_user (name, (email, (password, password_check))) =
  if password <> password_check then
    Lwt.return false
  else
    Db.get_user_with_name name >>= function
      | (Some _) -> Lwt.return false
      | None ->
        User.add name password email >>= fun () ->
        Lwt.return true


let update_user_password ((password, password_check)) =
  (* if password <> password_check then *)
    Lwt.return false
  (* else ( *)
  (*   Db.get_user_with_name name >>= (function *)
  (*     | (Some _) -> Lwt.return false *)
  (*     | None -> ( *)
  (*       User.add name password email >>= (fun () -> *)
  (*         Lwt.return true *)
  (*       ) *)
  (*     ) *)
  (*   ) *)
  (* ) *)

let update_user_mail (email) =
  Lwt.return false
