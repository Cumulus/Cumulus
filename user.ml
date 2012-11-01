type user = {
  id : int32;
  name : string;
  password : Bcrypt.hash_t;
  email : string;
  is_admin : bool;
}
type user_state = Already_connected | Ok | Bad_password | Not_found

let (>>=) = Lwt.(>>=)

let user_new data = {
  id = data#!id;
  name = data#!name;
  password = Bcrypt.hash_of_string data#!password;
  email = data#!email;
  is_admin = data#!is_admin;
}

let hash_password password =
  (* Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) password *)
  (* Digest.to_hex (Digest.string password) *)
  (* Sha512.to_hex (Sha512.string password) *)
  Bcrypt.string_of_hash (Bcrypt.hash password)

let check_password self password = Bcrypt.verify password self.password

let add = function
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
        | Some _ -> Lwt.return false
        | None ->
            Db.add_user name (hash_password password) email >>= fun () ->
            Lwt.return true

let (current_user : (user option) Eliom_reference.eref) =
  Eliom_reference.eref ~scope: Eliom_common.default_session_scope None

let get_userid () =
  Eliom_reference.get current_user >>= function
    | None -> Lwt.return None
    | (Some user) -> Lwt.return (Some user.id)

let is_connected () =
  Eliom_reference.get current_user >>= function
    | None -> Lwt.return false
    | _ -> Lwt.return true

let is_admin () =
  Eliom_reference.get current_user
  >>= function
    | None -> Lwt.return false
    | Some user -> Lwt.return user.is_admin

let connect user password =
  Db.get_user_with_name user >>= function
    | None -> Lwt.return Not_found
    | (Some user) ->
      let user = user_new user in
      if check_password user password then
        is_connected () >>= function
          | true -> Lwt.return Already_connected
          | false ->
              Eliom_reference.set current_user (Some user) >>= fun () ->
              Lwt.return Ok
      else
        Lwt.return Bad_password

let get_user_and_email () =
  get_userid () >>= fun userid ->
  match userid with
    | None -> Lwt.return None
    | Some id ->
      Db.get_user_name_and_email_with_id id >>= fun user ->
      Lwt.return (Some user)

let disconnect () =
  is_connected () >>= function
    | true -> Eliom_reference.unset current_user >>= (fun () ->
      Lwt.return true)
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
            Db.update_user_password id (hash_password password)
            >>= fun () ->
            Lwt.return true

let update_email = function
  | "" as email
  | email when Utils.is_invalid_email email ->
      Lwt.return false
  | email ->
      get_userid () >>= function
        | None -> Lwt.return false
        | Some id ->
            Db.update_user_email id email >>= fun () ->
            Lwt.return true
