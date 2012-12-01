type user = {
  id : int32;
  name : string;
  password : Db_user.password;
  email : string;
  is_admin : bool;
  feeds_per_page : int32;
}
type user_state = Already_connected | Ok | Bad_password | Not_found

let (>>=) = Lwt.(>>=)

let user_new data = {
  id = data#!id;
  name = data#!name;
  password = data#password;
  email = data#!email;
  is_admin = data#!is_admin;
  feeds_per_page = data#!feeds_per_page;
}

let add = function
  | ("", ((_ as email), ((_ as password), (_ as password_check))))
  | (_, (("" as email), ((_ as password), (_ as password_check))))
  | (_, ((_ as email), (("" as password), (_ as password_check))))
  | (_, ((_ as email), ((_ as password), ("" as password_check))))
  | (_, (email, (password, password_check)))
      when password <> password_check
        || Utils.is_invalid_email email ->
      Lwt.return false
  | (name, (email, (password, _))) ->
      Db_user.get_user_with_name name >>= function
        | Some _ -> Lwt.return false
        | None ->
            let password = Db_user.to_password password in
            Db_user.add_user ~name ~password ~email () >>= fun () ->
            Lwt.return true

let (get_user, set_user, unset_user) =
  let eref =
    Eliom_reference.eref ~scope:Eliom_common.default_session_scope None
  in
  ((fun () -> Eliom_reference.get eref),
   (fun user -> Eliom_reference.set eref (Some user)),
   (fun () -> Eliom_reference.unset eref)
  )

let get_userid () =
  get_user () >>= function
    | None -> Lwt.return None
    | (Some user) -> Lwt.return (Some user.id)

let get_user_feeds_per_page () =
  get_user () >>= function
    | None -> Lwt.return None
    | Some user -> Lwt.return (Some user.feeds_per_page)

let is_connected () =
  get_user () >>= function
    | None -> Lwt.return false
    | _ -> Lwt.return true

let is_admin () =
  get_user () >>= function
    | None -> Lwt.return false
    | Some user -> Lwt.return user.is_admin

let connect user password =
  Db_user.get_user_with_name user >>= function
    | None -> Lwt.return Not_found
    | (Some user) ->
      let user = user_new user in
      if Db_user.check_password password user.password then
        is_connected () >>= function
          | true -> Lwt.return Already_connected
          | false ->
              set_user user >>= fun () ->
              Lwt.return Ok
      else
        Lwt.return Bad_password

let get_user_and_email () =
  get_userid () >>= fun userid ->
  match userid with
    | None -> Lwt.return None
    | Some id ->
      Db_user.get_user_name_and_email_with_id id >>= fun user ->
      Lwt.return (Some user)

let disconnect () =
  is_connected () >>= function
    | true ->
        unset_user () >>= fun () ->
        Lwt.return true
    | false -> Lwt.return false

let update_password = function
  | (("" as password), (_ as password_check))
  | ((_ as password), ("" as password_check))
  | (password, password_check) when password <> password_check ->
      Lwt.return false
  | (password, _) ->
      get_userid () >>= function
        | None -> Lwt.return false
        | Some id ->
            let password = Db_user.to_password password in
            Db_user.update_user_password ~userid:id ~password ()
            >>= fun () ->
            Lwt.return true

let update_email = function
  | "" as email
  | email when Utils.is_invalid_email email ->
      Lwt.return false
  | email ->
      get_user () >>= function
        | None -> Lwt.return false
        | Some user ->
            set_user {user with email} >>= fun () ->
            Db_user.update_user_email ~userid:user.id ~email () >>= fun () ->
            Lwt.return true

let update_feeds_per_page feeds_per_page =
  get_user () >>= function
    | None -> Lwt.return false
    | Some user ->
        set_user {user with feeds_per_page} >>= fun () ->
        Db_user.update_user_feeds_per_page
          ~userid:user.id
          ~nb_feeds:feeds_per_page
          ()
        >>= fun () ->
        Lwt.return true

let get_offset () =
  get_user_feeds_per_page () >>= fun off ->
  Lwt.return (Eliom_lib.Option.get (fun () -> Utils.offset) off)
