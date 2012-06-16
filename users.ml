type users = User.user Ocsipersist.table

let users_new () =
  Ocsipersist.open_table "users"

let connect_user self username password =
  Lwt.try_bind
    (fun () -> Ocsipersist.find self username)
    (fun user ->
      match User.check_password user password with
        | true -> User.connect username
        | false -> Lwt.return User.Bad_password
    )
    (fun _ -> Lwt.return User.Not_found)
