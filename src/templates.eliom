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
  let waiting_for_reload server_function ~box =
    let event = %Feeds.event in
    let stream = Lwt_react.E.to_stream event in
    Lwt.async
      (fun () ->
         Lwt_stream.iter_s
           (fun id ->
              (* TODO: Except for ourself *)
              (* TODO: reload on delete *)
              server_function 0 >|= fun feeds ->
              Eliom_content.Html5.Manip.replaceAllChild box feeds;
           )
           stream
      )
}}

open Batteries

let main_style_aux content =
  User.get_user () >>= fun user ->
  Errors.get_error () >>= fun error ->
  content user 0 >|= fun (content, server_function) ->
  Templates_feeds.main_style ~user ~error ~server_function content

let feed_list feeds =
  let rec content user page =
    let offset = User.get_feeds_per_page user in
    let starting = Int32.(Int32.of_int page * offset) in
    let userid = User.get_id user in
    feeds ~starting ~number:offset ~user:userid () >|= fun feeds ->
    let feeds = Templates_feeds.to_html ~user feeds in
    let server_function ~box =
      let server_function =
        let f page = content user page >|= fst in
        server_function Json.t<int> f
      in
      ignore {unit{
        let box = %box in
        let link_next =
          let open Eliom_content.Html5.F in
          Eliom_content.Html5.D.aside ~a:[a_class ["row"; "post"; "mod"]] []
        in
        let before =
          let open Eliom_content.Html5.F in
          Eliom_content.Html5.D.section ~a:[a_class["line"]] [link_next]
        in
        let no_more_links =
          let open Eliom_content.Html5.F in
          Raw.a ~a:[a_class ["link_next"]] [pcdata "No more links"]
        in
        let default_link_next_content f =
          let open Eliom_content.Html5.F in
          Raw.a
            ~a:[ a_onclick (fun _ -> Lwt.async f)
               ; a_class ["link_next"; "link"]
               ]
            [pcdata "Get the next links"]
        in
        let loading =
          let open Eliom_content.Html5.F in
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
            %server_function page >|= fun feeds ->
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
        Lwt.async
          (fun () ->
             let rec ev () =
               Lwt_js_events.scroll Dom_html.document >>= fun _ ->
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
               >>= ev
             in
             ev ()
          );
        Eliom_content.Html5.Manip.appendChild box before;
        Eliom_content.Html5.Manip.replaceAllChild
          link_next
          [default_link_next_content get_next_page];
        waiting_for_reload %server_function ~box;
      }};
    in
    (feeds, server_function)
  in
  main_style_aux content

let main_style content =
  let content user page =
    content user page >|= fun content ->
    (content, fun ~box:_ -> ())
  in
  main_style_aux content

let main_style_pure content =
  let content user _ = Lwt.return (content user) in
  main_style content

(* see TODO [1] *)
let main () = feed_list Feed.get_root_feeds

let user username = feed_list (Feed.get_feeds_with_author username)

let tag tag = feed_list (Feed.get_feeds_with_tag tag)

let fav_feed username = feed_list (Feed.get_fav_with_username username)

let feeds_comments_to_html ~user id =
  let userid = User.get_id user in
  Feed.get_feed_with_id ~user:userid id >>= fun root ->
  Feed.get_comments ~user:userid id >|= fun comments ->
  let result = Comments.tree_comments [Comments.Sheet root] comments
  in match result with
  | Some tree -> Templates_feeds.comments_to_html' ~user tree
  | None -> Templates_feeds.comments_to_html' ~user (Comments.Sheet root)

(* TODO: Fix it (doesn't seems to work) *)
let feeds_branch_to_html ~user id =
  let userid = User.get_id user in
  Feed.get_feed_with_id ~user:userid id >>= fun target ->
  (match target.Feed.root with
    | None -> Lwt.return (Comments.Sheet target)
    | Some root ->
      Feed.get_feed_with_id ~user:userid root >>= fun root ->
      Feed.get_comments ~user:userid root.Feed.id >|= fun comments ->
      (Comments.branch_comments (Comments.Sheet target) (root :: comments)))

(* Shows a specific link (TODO: and its comments) *)
let view_feed id =
  let content user _ =
    Feed.exist ~feedid:id () >>= fun exist ->
    if exist then
      feeds_comments_to_html ~user id >|= fun feeds ->
      [feeds]
    else
      Lwt.return
        (Templates_feeds.error_content "Ce lien n'existe pas.")
  in
  main_style content

let register () =
  let content _ = Templates_feeds.private_register () in
  main_style_pure content

let preferences () =
  let content user = Templates_feeds.private_preferences ~user in
  main_style_pure content

let comment id =
  let content user _ =
    Feed.exist ~feedid:id () >>= fun exist ->
    if not exist then
      Lwt.return (Templates_feeds.error_content "Ce commentaire n'existe pas.")
    else
      feeds_branch_to_html ~user id >|= fun branch ->
      Templates_feeds.private_comment ~user id branch
  in
  main_style content

let edit_feed id =
  let content user _ =
    let userid = User.get_id user in
    Feed.get_feed_with_id ~user:userid id >>= fun feed ->
    Feed.get_edit_infos feed.Feed.id >>= fun infos ->
    Feed.exist ~feedid:feed.Feed.id () >|= fun exist ->
    if not exist then
      Templates_feeds.error_content "Ce commentaire n'existe pas."
    else
      Templates_feeds.private_edit_feed ~user ~feed infos
  in
  main_style content

let reset_password () =
  let form _ = Templates_feeds.reset_password () in
  main_style_pure form
