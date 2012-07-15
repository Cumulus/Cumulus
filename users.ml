let connect_user username password =
  Db.get_user_with_name username >>= (function
    | None -> Lwt.return User.Not_found
    | (Some user) -> (
      let user = User.user_new user in
      match User.check_password user password with
        | true -> User.connect user
        | false -> Lwt.return User.Bad_password
    )
  )

let add_user (name, (email, (password, password_check))) =
  if password <> password_check then
    Lwt.return false
  else (
    Db.get_user_with_name name >>= (function
      | (Some _) -> Lwt.return false
      | None -> (
        User.add name password email >>= (fun () ->
          Lwt.return true
        )
      )
    )
  )
