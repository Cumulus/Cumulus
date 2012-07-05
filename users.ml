let self =
  Ocsipersist.open_table "users"

let connect_user username password =
  Lwt.try_bind
    (fun () -> Ocsipersist.find self username)
    (fun user ->
      match User.check_password user password with
        | true -> User.connect username
        | false -> Lwt.return User.Bad_password
    )
    (fun _ -> Lwt.return User.Not_found)

let add_user (username, (password, email)) =
  Lwt.try_bind
    (fun () -> Ocsipersist.find self username)
    (fun _ -> Lwt.return false)
    (fun _ ->
      Ocsipersist.add self username
        (User.user_new password email) >>= (fun () ->
          Lwt.return true
         )
    )
