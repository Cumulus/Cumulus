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

module Calendar = CalendarLib.Calendar
module Html = Eliom_content.Html5.F
module Uri = Eliom_uri

(* TODO: Remove duplicate *)
module H = Eliom_content.Html5.F.Raw
module M = MarkdownHTML.Make_html5(struct include H module Svg = Eliom_content.Svg.F.Raw end)

(* TODO: Remove duplicate *)
let links_of_tags tags =
  List.fold_left (fun acc tag ->
    let link =
      Html.a
        ~a:[Html.a_class ["tags"]]
        ~service:Services.tag_feed
        [Html.pcdata tag]
        (None, tag)
    in
    acc @ [Html.pcdata " "; link]
  ) [] tags

let feed_to_atom self =
  User.get_userid () >>= fun user ->
  Feed.get_root ~feedid:self.Feed.id ~user () >>= fun root_feed ->
  let title, root_infos = match root_feed with
    | Some root_feed' -> ("[RE: " ^ (Utils.troncate root_feed'.Feed.description) ^
                            "] " ^ (
                            match self.Feed.url with
                            | Some url -> self.Feed.description
                            | None -> Utils.troncate self.Feed.description
                          ),
                          [Html.pcdata "ce message est une réponse à : ";
                           Html.a ~service:Services.view_feed
                             [Html.pcdata root_feed'.Feed.description]
                             (Int32.to_int root_feed'.Feed.id,
                              Utils.troncate root_feed'.Feed.description)])
    | None -> (Utils.troncate' 200 self.Feed.description, [])
  in
  Lwt.return (
    Atom_feed.entry
      ~updated: self.Feed.date
      ~id:(Int32.to_string self.Feed.id)
      ~title: (Atom_feed.plain (title))
      [Atom_feed.authors [Atom_feed.author self.Feed.user#name];
       Atom_feed.links [Atom_feed.link (Uri.make_string_uri ~absolute:true
                                          ~service:Services.view_feed
                                          (Int32.to_int self.Feed.id, "")
                                       )];
       Atom_feed.summary (Atom_feed.html5 (
         (match self.Feed.url with
          | Some url -> Html.Raw.a ~a:
                          [Html.a_href
                             (Html.uri_of_string
                                (fun () -> url)
                             )
                          ]
                          [Html.pcdata self.Feed.description]
          | None ->
              let markdown = Markdown.parse_text self.Feed.description in
              let render_pre ~kind s = H.pre [H.pcdata s] in
              let render_link {Markdown.href_target; href_desc} =
                H.a ~a:[H.a_href (H.uri_of_string href_target)] [H.pcdata href_desc]
              in
              let render_img {Markdown.img_src; img_alt} =
                H.img ~src:(H.uri_of_string img_src) ~alt:img_alt ()
              in
              Html.div ~a:[Html.a_class ["lamalama"]] (M.to_html ~render_pre
                                                         ~render_link ~render_img markdown)
         )
         :: (Html.br ())
         :: (Html.a ~service:Services.atom_feed [Html.pcdata "Flux atom du lien"]
               (Int32.to_int self.Feed.id))
         :: (Html.br ())
         :: (Html.pcdata "Tags : ")
         :: (links_of_tags self.Feed.tags)
         @ [(Html.br ())]
         @ root_infos
       )
       )
      ]
  )

(* TODO: Remove duplicate *)
let to_somthing f data =
  Lwt_list.map_p (fun feed -> f feed) data

let tree_to_atom id () =
  User.get_userid () >>= fun user ->
  Feed.get_tree_feeds ~user id ~starting:0l ~number:Utils.offset ()
  >>= fun (feeds, _) ->
  to_somthing feed_to_atom feeds
  >>= (fun tmp ->
    Lwt.return (
      Atom_feed.feed
        ~updated: (Calendar.make 2012 6 9 17 40 30)
        ~id:"http://cumulus.org"
        ~title: (Atom_feed.plain ("Cumulus (id: " ^ Int32.to_string id ^ ")"))
        tmp
    )
  )

let tag_to_atom tag () =
  User.get_userid () >>= fun user ->
  Feed.get_feeds_with_tag ~user tag ~starting:0l ~number:Utils.offset ()
  >>= fun (feeds, _) ->
  to_somthing feed_to_atom feeds
  >>= (fun tmp ->
    Lwt.return (
      Atom_feed.feed
        ~updated: (Calendar.make 2012 6 9 17 40 30)
        ~id:"http://cumulus.org"
        ~title: (Atom_feed.plain ("Cumulus (tag: " ^ tag ^ ")"))
        tmp
    )
  )

(* FIXME? should atom feed return only a limited number of links ? *)
let to_atom () =
  User.get_userid () >>= fun user ->
  Feed.get_links_feeds ~user ~starting:0l ~number:Utils.offset ()
  >>= fun (feeds, _) ->
  to_somthing feed_to_atom feeds
  >>= (fun tmp ->
    Lwt.return (
      Atom_feed.feed
        ~updated: (Calendar.make 2012 6 9 17 40 30)
        ~id:"http://cumulus.org"
        ~title: (Atom_feed.plain "Cumulus")
        tmp
    )
  )

let comments_to_atom () =
  User.get_userid () >>= fun user ->
  Feed.get_comments_feeds ~user ~starting:0l ~number:Utils.offset ()
  >>= fun (feeds, _) ->
  to_somthing feed_to_atom feeds
  >>= (fun tmp ->
    Lwt.return (
      Atom_feed.feed
        ~updated: (Calendar.make 2012 6 9 17 40 30)
        ~id:"http://cumulus.org"
        ~title: (Atom_feed.plain "Cumulus")
        tmp
    )
  )