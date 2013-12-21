(*
Copyright (c) 2012 Enguerrand Decorne

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Batteries
open Eliom_lib.Lwt_ops

type user = Db_user.user =
  { id : int32
  ; name : string
  ; password : Db_user.Password.t
  ; email : string
  ; is_admin : bool
  ; feeds_per_page : int32
  }
type user_state = Already_connected | Ok | Bad_password | Not_found

let add = function
  | ("", ((_ as email), ((_ as password), (_ as password_check))))
  | (_, (("" as email), ((_ as password), (_ as password_check))))
  | (_, ((_ as email), (("" as password), (_ as password_check))))
  | (_, ((_ as email), ((_ as password), ("" as password_check))))
  | (_, (email, (password, password_check)))
    when not (String.equal password password_check)
      || Utils.is_invalid_email email ->
      Lwt.return false
  | (name, (email, (password, _))) ->
      let name = String.trim name in
      Db_user.get_user_with_name name >>= function
      | Some _ -> Lwt.return false
      | None ->
          let password = Db_user.Password.hash password in
          Db_user.add_user ~name ~password ~email >>= fun () ->
          Lwt.return true

let (get_user, set_user, unset_user) =
  let session_scope = Eliom_common.default_session_scope in
  let eref =
    Eliom_reference.eref
      ~scope:session_scope
      ~persistent:"cumulus_user_v1"
      None
  in
  ((fun () -> Eliom_reference.get eref),
   (fun user ->
      let cookie_scope =
        Eliom_common.cookie_scope_of_user_scope session_scope
      in
      Eliom_state.set_persistent_data_cookie_exp_date
        ~cookie_scope
        (Some (Int32.to_float Int32.max_int))
      >>= fun () ->
      Eliom_reference.set eref (Some user)
   ),
   (fun () -> Eliom_reference.unset eref)
  )

let get_userid () = get_user () >|= Option.map (fun x -> x.id)
let is_connected () = get_user () >|= Option.is_some
let is_admin () = get_user () >|= Option.map_default (fun x -> x.is_admin) false

let connect user password =
  Db_user.get_user_with_name user >>= function
  | None -> Lwt.return Not_found
  | Some user ->
      if Db_user.Password.check password user.password then
        is_connected () >>= function
        | true -> Lwt.return Already_connected
        | false ->
            set_user user >>= fun () ->
            Lwt.return Ok
      else
        Lwt.return Bad_password

(* TODO: Remove this after the merge of the new ui *)
let get_user_and_email () =
  get_user () >>= function
  | None -> Lwt.return None
  | Some user ->
      let user = object
        method name = user.name
        method email = user.email
      end in
      Lwt.return (Some user)

let disconnect () =
  is_connected () >>= function
  | true ->
      unset_user () >>= fun () ->
      Lwt.return true
  | false -> Lwt.return false

let update_password (password, password_check) =
  if String.is_empty password || not (String.equal password password_check) then
    Lwt.return false
  else
    get_user () >>= function
    | None -> Lwt.return false
    | Some user ->
        let password = Db_user.Password.hash password in
        Db_user.update_user_password ~userid:user.id ~password
        >>= fun () ->
        Lwt.return true

let update_email email =
  if String.is_empty email || Utils.is_invalid_email email then
    Lwt.return false
  else
    get_user () >>= function
    | None -> Lwt.return false
    | Some user ->
        set_user {user with email} >>= fun () ->
        Db_user.update_user_email ~userid:user.id ~email >>= fun () ->
        Lwt.return true

let update_feeds_per_page feeds_per_page =
  get_user () >>= function
  | None -> Lwt.return false
  | Some user ->
      Db_user.update_user_feeds_per_page
        ~userid:user.id
        ~nb_feeds:feeds_per_page
      >>= fun () ->
      set_user {user with feeds_per_page} >>= fun () ->
      Lwt.return true

let get_offset () =
  get_user () >|= Option.map_default (fun x -> x.feeds_per_page) Utils.offset

let send_reset_email ~service email =
  let f x =
    let service = Html.make_string_uri ~service:(service x) ~absolute:true () in
    Netsendmail.sendmail
      (Netsendmail.compose
         ~from_addr:("Cumulus no-reply", General_config.email)
         ~to_addrs:[("", email)]
         ~subject:"Cumulus: Réinitialisation du mot-de-passe"
         ("Si tu veux vraiment réinitaliser ton mot-de-passe, clique ici: "
          ^ service)
      )
  in
  Db_user.get_user_with_email email >|= Option.may f

let force_connect = set_user
