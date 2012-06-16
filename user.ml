type user = (string option) Eliom_reference.eref

let current_user =
  Eliom_reference.eref ~scope: Eliom_common.session None

let user_new () =
  current_user

let user_connect self username password =
  (* TODO: If passwd match *)
  Eliom_reference.set self (Some username)

let user_disconnect self =
  Eliom_reference.unset self

let user_is_connected self =
  Eliom_reference.get self >>= (fun username ->
    Lwt.return (
      match username with
        | None -> false
        | _ -> true
    )
  )
