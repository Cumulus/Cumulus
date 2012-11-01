let connect_user username password =
  Db.get_user_with_name username >>= function
    | None -> Lwt.return User.Not_found
    | (Some user) ->
      let user = User.user_new user in
      User.connect user password

let add_user = function
  | ("", ((_ as email), ((_ as password), (_ as password_check))))
  | (_, (("" as email), ((_ as password), (_ as password_check))))
  | (_, ((_ as email), (("" as password), (_ as password_check))))
  | (_, ((_ as email), ((_ as password), ("" as password_check))))
  | (_, (email, (password, password_check)))
      when password <> password_check
        || Utils.is_invalid_email email ->
      Lwt.return false
  | (name, (email, (password, password_check))) ->
      Db.get_user_with_name name >>= function
        | (Some _) -> Lwt.return false
        | None ->
            User.add name password email >>= fun () ->
            Lwt.return true

let update_user_password = function
  | (("" as password), (_ as password_check))
  | ((_ as password), ("" as password_check))
  | (password, password_check) when password <> password_check ->
      Lwt.return false
  | (password, _) ->
      User.update_password password >>= fun () ->
      Lwt.return true

let update_user_mail = function
  | "" as email
  | email when Utils.is_invalid_email email ->
      Lwt.return false
  | email ->
      User.update_email email >>= fun () ->
      Lwt.return true
