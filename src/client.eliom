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

{shared{
  open Eliom_lib.Lwt_ops
  open Eliom_content.Html5.F

  let get_upvote_inner ~upon ~up ~downon ~down ~vote ~score =
    let cl = "upvote_wrap_inner" :: (if score <= 0 then ["gray"] else []) in
    div
      ~a:[a_class cl]
      [ if vote = 1 then upon else up
      ; pcdata (string_of_int score)
      ; if vote = -1 then downon else down
      ]
}}

{client{
  let display_error ~error_frame =
    let id_timeout = ref None in
    id_timeout :=
      Some
        (Dom_html.window##setTimeout
           (Js.wrap_callback
              (fun () ->
                 Eliom_content.Html5.Manip.removeChildren error_frame;
                 match !id_timeout with
                 | None -> () (* It cannot happen *)
                 | Some id ->
                     Dom_html.window##clearTimeout (id)
              ),
            5_000.
           )
        )

  let waiting_for_reload get_feeds ~box =
    let event = %Feeds.event in
    let stream = Lwt_react.E.to_stream event in
    Lwt_stream.iter_s
      (fun id ->
         (* TODO: Except for ourself *)
         (* TODO: reload on delete *)
         get_feeds 0 >|= fun feeds ->
         Eliom_content.Html5.Manip.replaceChildren box feeds;
      )
      stream

  let feeds_actions ~content ~box =
    let link_next =
      Eliom_content.Html5.D.aside ~a:[a_class ["row"; "post"; "mod"]] []
    in
    let before =
      Eliom_content.Html5.D.section ~a:[a_class["line"]] [link_next]
    in
    let no_more_links =
      Raw.a ~a:[a_class ["link_next"]] [pcdata "No more links"]
    in
    let default_link_next_content f =
      Raw.a
        ~a:[ a_onclick (fun _ -> Lwt.async f)
           ; a_class ["link_next"; "link"]
           ]
        [pcdata "Get the next links"]
    in
    let loading =
      img
        ~a:[a_class ["loader"]]
        ~alt:"loader.gif"
        ~src:(make_uri
                ~service:(Eliom_service.static_dir ())
                ["loader.gif"]
             )
        ()
    in
    let get_next_page =
      let last_page = ref 0 in
      let rec aux () =
        let page = succ !last_page in
        Eliom_content.Html5.Manip.replaceChildren
          link_next
          [loading];
        content page >|= fun feeds ->
        begin match feeds with
        | [] ->
            Eliom_content.Html5.Manip.replaceChildren link_next [no_more_links];
        | feeds ->
            Eliom_content.Html5.Manip.appendChildren ~before box feeds;
            Eliom_content.Html5.Manip.replaceChildren
              link_next
              [default_link_next_content aux];
        end;
        last_page := page;
      in
      aux
    in
    Lwt.async
      (fun () ->
         Lwt_js_events.scrolls
           Dom_html.document
           (fun _ _ ->
              let doc = Dom_html.document##documentElement in
              let innerHeight = Dom_html.window##innerHeight in
              begin match Js.Optdef.to_option innerHeight with
              | None -> Lwt.return_unit
              | Some innerHeight ->
                  if doc##scrollTop >= doc##scrollHeight - innerHeight then
                    get_next_page ()
                  else
                    Lwt.return_unit
              end
           )
      );
    Eliom_content.Html5.Manip.appendChild box before;
    Eliom_content.Html5.Manip.replaceChildren
      link_next
      [default_link_next_content get_next_page];
    waiting_for_reload content ~box

  let fav_actions ~is_fav ~res ~del ~add ~feed_id =
    let is_fav = ref is_fav in
    let rec aux () =
      Lwt_js_events.click
        (Eliom_content.Html5.To_dom.of_element (if !is_fav then del else add))
      >>= fun _ ->
      let call service =
        Eliom_client.call_ocaml_service ~service feed_id ()
      in
      (if !is_fav then
         call %Services.del_fav_feed >|= function
         | `Ok ->
             Eliom_content.Html5.Manip.replaceChildren res [add]
         | `NotConnected -> () (* TODO: Display an error *)
       else
         call %Services.add_fav_feed >|= function
         | `Ok ->
             Eliom_content.Html5.Manip.replaceChildren res [del]
         | `NotConnected -> () (* TODO: Display an error *)
      )
      >>= fun () ->
      is_fav := not !is_fav; (* BUG ? *)
      aux ()
    in
    aux ()

  let upvotes_actions ~container ~upon ~up ~downon ~down ~vote ~feed_id =
    let vote = ref vote in
    let call service =
      Eliom_client.call_ocaml_service ~service feed_id ()
    in
    let action = function
      | `Ok (v, score) ->
          let content = get_upvote_inner ~upon ~up ~downon ~down ~vote:v ~score in
          Eliom_content.Html5.Manip.replaceChildren container [content];
          vote := v;
      | `NoRight
      | `NotConnected ->
          () (* TODO: Display an error *)
    in
    let rec aux () =
      let up_action =
        Lwt_js_events.click
          (Eliom_content.Html5.To_dom.of_element (if !vote = 1 then upon else up))
        >>= fun _ ->
        call (if !vote = 1 then %Services.cancelvote_feed else %Services.upvote_feed)
      in
      let down_action =
        Lwt_js_events.click
          (Eliom_content.Html5.To_dom.of_element (if !vote = -1 then downon else down))
        >>= fun _ ->
        call (if !vote = -1 then %Services.cancelvote_feed else %Services.downvote_feed)
      in
      Lwt.pick [up_action; down_action]
      >|= action
      >>= aux
    in
    aux ()
}}

(* Due to a known limitation of Eliom, we have to set the type here
 * through this module. (see: https://github.com/ocsigen/eliom/issues/51)
 *)
module ClientTypes : sig
  val feeds_actions :
    content:(int -> [`Section] Eliom_content.Html5.elt list Lwt.t) ->
    box:[`Aside] Eliom_content.Html5.elt ->
    unit

  val fav_actions :
    is_fav:bool ->
    res:[`Div] Eliom_content.Html5.elt ->
    del:[`A of [`Img]] Eliom_content.Html5.elt ->
    add:[`A of [`Img]] Eliom_content.Html5.elt ->
    feed_id:int32 ->
    unit

  val display_error :
    error_frame:[`Div] Eliom_content.Html5.elt ->
    unit

  val upvotes_actions :
    container:[`Div] Eliom_content.Html5.elt ->
    upon:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
    up:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
    downon:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
    down:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
    vote:int ->
    feed_id:int32 ->
    unit
end = struct

  let feeds_actions ~content ~box =
    let content = server_function Json.t<int> content in
    ignore {unit{
      let content = %content in
      let box = %box in
      Lwt.async (fun () -> feeds_actions ~content ~box)
    }}

  let fav_actions ~is_fav ~res ~del ~add ~feed_id =
    ignore {unit{
      let is_fav = %is_fav in
      let res = %res in
      let del = %del in
      let add = %add in
      let feed_id = %feed_id in
      Lwt.async (fun () -> fav_actions ~is_fav ~res ~del ~add ~feed_id)
    }}

  let display_error ~error_frame =
    ignore {unit{
      let error_frame = %error_frame in
      display_error ~error_frame
    }}

  let upvotes_actions ~container ~upon ~up ~downon ~down ~vote ~feed_id =
    ignore {unit{
      let container = %container in
      let upon = %upon in
      let up = %up in
      let downon = %downon in
      let down = %down in
      let vote = %vote in
      let feed_id = %feed_id in
      Lwt.async
        (fun () ->
           upvotes_actions ~container ~upon ~up ~downon ~down ~vote ~feed_id
        )
    }}
end

include ClientTypes
