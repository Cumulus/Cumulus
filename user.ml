type user = {
  id : int32;
  name : string;
  password : Bcrypt.hash_t;
  email : string
}
type user_state = Already_connected | Ok | Bad_password | Not_found

let user_new data = {
  id = data#!id;
  name = data#!name;
  password = Bcrypt.hash_of_string data#!password;
  email = data#!email
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
  Eliom_reference.get current_user >>= (function
    | None -> Lwt.return None
    | (Some user) -> Lwt.return (Some user.id)
  )

let is_connected () =
  Eliom_reference.get current_user >>= (fun userid ->
    Lwt.return (
      match userid with
        | None -> false
        | _ -> true
    )
  )

let connect user =
  is_connected () >>= (fun state ->
    match state with
      | true -> Lwt.return Already_connected
      | false -> Eliom_reference.set
        current_user (Some user) >>= (fun () ->
          Lwt.return Ok
        )
  )

let disconnect () =
  is_connected () >>= (fun state ->
    match state with
      | true -> Eliom_reference.unset current_user >>= (fun () ->
        Lwt.return true)
      | false -> Lwt.return false
  )
