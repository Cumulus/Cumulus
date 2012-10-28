type user = {
  id : int32;
  name : string;
  password : Bcrypt.hash_t;
  email : string;
  is_admin : bool;
}
type user_state = Already_connected | Ok | Bad_password | Not_found

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

let add name password email =
  Db.add_user name (hash_password password) email

let (current_user : (user option) Eliom_reference.eref) =
  Eliom_reference.eref ~scope: Eliom_common.session None

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
