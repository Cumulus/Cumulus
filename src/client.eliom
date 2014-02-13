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
}}

{client{
  open Eliom_content.Html5.F

  let display_error ~error_frame =
    let id_timeout = ref None in
    id_timeout :=
      Some
        (Dom_html.window##setTimeout
           (Js.wrap_callback
              (fun () ->
                 Eliom_content.Html5.Manip.removeAllChild error_frame;
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
         Eliom_content.Html5.Manip.replaceAllChild box feeds;
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
        Eliom_content.Html5.Manip.replaceAllChild
          link_next
          [loading];
        content page >|= fun feeds ->
        begin match feeds with
        | [] ->
            Eliom_content.Html5.Manip.replaceAllChild link_next [no_more_links];
        | feeds ->
            Eliom_content.Html5.Manip.appendChilds ~before box feeds;
            Eliom_content.Html5.Manip.replaceAllChild
              link_next
              [default_link_next_content aux];
        end;
        last_page := page;
      in
      aux
    in
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
    >|= fun () ->
    Eliom_content.Html5.Manip.appendChild box before;
    Eliom_content.Html5.Manip.replaceAllChild
      link_next
      [default_link_next_content get_next_page];
    waiting_for_reload content ~box

  let fav_actions ~is_fav ~res ~del ~add ~feed_id =
    let r = ref is_fav in
    let rec aux () =
      Lwt_js_events.click
        (Eliom_content.Html5.To_dom.of_element (if !r then del else add))
      >>= fun _ ->
      let call service =
        Eliom_client.call_ocaml_service ~service feed_id ()
      in
      (if !r then
         call %Services.del_fav_feed >|= function
         | `Ok ->
             Eliom_content.Html5.Manip.replaceAllChild res [add]
         | `NotConnected -> () (* TODO: Display an error *)
       else
         call %Services.add_fav_feed >|= function
         | `Ok ->
             Eliom_content.Html5.Manip.replaceAllChild res [del]
         | `NotConnected -> () (* TODO: Display an error *)
      )
      >>= fun () ->
      r := not !r;
      aux ()
    in
    aux ()
}}

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
