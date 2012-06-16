type user = (string option) Eliom_reference.eref
type user_state = Already_connected | Ok | Bad_password

let current_user =
  Eliom_reference.eref ~scope: Eliom_common.session None

let user_new () =
  current_user

let user_is_connected self =
  Eliom_reference.get self >>= (fun username ->
    Lwt.return (
      match username with
        | None -> false
        | _ -> true
    )
  )

let user_connect self username password =
  (* TODO: If passwd match *)
  user_is_connected self >>= (fun state ->
    match state with
      | true -> Lwt.return Already_connected
      | false -> Eliom_reference.set self (Some username) >>= (fun () ->
        Lwt.return Ok
      )
  )

let user_disconnect self =
  user_is_connected self >>= (fun state ->
    match state with
      | true -> Eliom_reference.unset self >>= (fun () -> Lwt.return true)
      | false -> Lwt.return false
  )
