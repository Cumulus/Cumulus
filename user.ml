type user = {
  password : string;
  email : string
}
type user_state = Already_connected | Ok | Bad_password | Not_found

let check_password self password = self.password = password

let current_user =
  Eliom_reference.eref ~scope: Eliom_common.session None

let is_connected () =
  Eliom_reference.get current_user >>= (fun username ->
    Lwt.return (
      match username with
        | None -> false
        | _ -> true
    )
  )

let connect username =
  is_connected () >>= (fun state ->
    match state with
      | true -> Lwt.return Already_connected
      | false -> Eliom_reference.set current_user (Some username) >>= (fun () ->
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
