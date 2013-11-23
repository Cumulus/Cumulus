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
module Uri = Eliom_uri

type feed = {
  id : int32;
  url : string option;
  description : string;
  date : Calendar.t;
  author : int32;
  parent : int32 option;
  root : int32 option;
  tags: string list;
  score : int;
}

let feed_new data tags score = {
  id = data#!id;
  url = data#?url;
  description = data#!description;
  date = data#!timedate;
  author = data#!author;
  parent = data#?parent;
  root = data#?root;
  tags;
  score;
}

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

module H = Xhtml_f.Make(Eliom_content.Xml)
module M = MarkdownHTML.Make_xhtml(H)

let conv : 'a H.elt list -> 'a Html.elt list = fun x -> Html.totl (H.toeltl x)

let to_html self =
  let content = match self.url with
    | Some url -> Html.div ~a:[Html.a_class["line_title"]][
                    Html.Raw.a
                    ~a:[Html.a_class ["postitle"];
                        Html.a_href (Html.uri_of_string (fun () -> url));
                       ]
                    [Html.pcdata self.description]]
    | None ->
      let markdown = Markdown.parse_text self.description in
      let render_pre ~kind s = H.pre [H.pcdata s] in
      let render_link {Markdown.href_target; href_desc} =
        H.a ~a:[H.a_href (H.uri_of_string href_target)] [H.pcdata href_desc]
      in
      let render_img {Markdown.img_src; img_alt} =
        H.img ~src:(H.uri_of_string img_src) ~alt:img_alt ()
      in
      Html.div ~a:[Html.a_class ["lamalama"]] (conv (M.to_html ~render_pre ~render_link ~render_img markdown))
  in
  let tags = match self.url with
    | Some _ -> Html.div ~a:[Html.a_class["tag_line"]] (links_of_tags self.tags)
    | None -> Html.div ~a:[Html.a_class["error"]][] in
  User.is_connected () >>= fun state ->
  Db_user.get_user_name_and_email_with_id self.author >>= fun author ->
  User.get_userid () >>= (function
      | None -> Lwt.return false
      | Some userid ->
        Db_feed.is_feed_author ~feed:self.id ~userid ()
    )
  >>= fun is_author ->
  User.is_admin ()
  >>= fun is_admin ->
  Db_feed.count_comments self.id >>= fun comments ->
  User.get_userid () >>= (function
      | None -> Lwt.return false
      | Some userid -> Db_feed.is_fav ~feedid:self.id ~userid ()
    )
  >>= fun is_fav ->
  User.get_userid () >>= (function
      | None -> Lwt.return (Int32.of_int 0)
      | Some userid -> Db_feed.user_vote ~feedid:self.id ~userid ()
    )
  >>= fun user_score ->
  Lwt.return (
    [
      Html.div ~a: [Html.a_class["col";"w20"]]
         [Html.div ~a: [Html.a_class["post_avatar"]]
            [Html.img
               ~a: [Html.a_class ["postimg"]]
               ~alt: (author#!name)
               ~src: (
                 Html.make_uri
                   ~service: (Utils.get_gravatar (author#!email)) (65, "identicon")
               )
               ()]];
      Html.div ~a: [Html.a_class["col";"post_info"]][
       Html.div ~a: [Html.a_class["line_author"]][

      Html.pcdata ("Publié le " ^ (Utils.string_of_calendar self.date) ^ " par ");
      Html.a
        ~service:Services.author_feed
        [Html.pcdata author#!name]
        (None, author#!name);
       ];
      (*
      (if not state then
         (Html.pcdata "")
       else
         (if is_fav = true then
            (Html.a ~service:Services.del_fav_feed [Html.pcdata "★"] self.id)
          else (Html.a ~service:Services.add_fav_feed [Html.pcdata "☆"] self.id))
      );
      (if not state or is_author then
         (Html.pcdata "")
       else if user_score <> (Int32.of_int 1) then
         (Html.a ~service:Services.upvote_feed [Html.pcdata "⬆"] self.id)
       else
         (Html.a ~service:Services.cancelvote_feed [Html.pcdata "✕"] self.id)
      );
      (if not state or is_author then
         (Html.pcdata "")
       else if user_score <> (Int32.of_int (-1)) then
         (Html.a ~service:Services.downvote_feed [Html.pcdata "⬇"] self.id)
       else
         (Html.a ~service:Services.cancelvote_feed [Html.pcdata "✕"] self.id)
      );
      Html.pcdata ("[" ^ string_of_int self.score ^ "] ");*)
      content;
      tags;
      ];
      (*[
       (* TODO : afficher "n commentaire(s)" *)
       Html.a
         ~service:Services.view_feed
         (let n = Int64.to_int comments#!n
          in match n with
          | 0
          | 1 -> [Html.pcdata ((string_of_int n) ^ " commentaire")]
          | n -> [Html.pcdata ((string_of_int n) ^ " commentaires")])
         (* [Html.pcdata (string_to_int (Int64.to_int comments)) " commentaires "] *)
         (* url_of_title or url_of_desc ? *)
         (Int32.to_int self.id, Utils.troncate self.description);
       Html.a
         ~service:Services.comment
         [Html.pcdata " Poster un commentaire "]
         (Int32.to_int self.id, Utils.troncate self.description);
      ]; *)
     (* [Html.a ~service:Services.atom_feed
         [Html.pcdata " [Flux Atom du lien]"] (Int32.to_int self.id)];
      (if is_author or is_admin then
         [ Html.br ();
           Html.pcdata " (";
           Html.a ~service:Services.delete_feed [Html.pcdata "supprimer"] self.id ;
           Html.pcdata " | ";
           Html.a ~service:Services.edit_feed [Html.pcdata "editer"]
             (Int32.to_int self.id, Utils.troncate self.description);
           Html.pcdata ")"
         ]
       else []
      ); *)
    ]
  )

let to_atom self =
  Db_feed.get_root self.id () >>= fun root_feed ->
  Db_user.get_user_name_and_email_with_id self.author >>= fun author ->
  let title, root_infos = match root_feed with
    | Some root_feed' -> ("[RE: " ^ (Utils.troncate root_feed'#!description) ^
                            "] " ^ (
                            match self.url with
                            | Some url -> self.description
                            | None -> Utils.troncate self.description
                          ),
                          [Html.pcdata "ce message est une réponse à : ";
                           Html.a ~service:Services.view_feed
                             [Html.pcdata root_feed'#!description]
                             (Int32.to_int root_feed'#!id,
                              Utils.troncate root_feed'#!description)])
    | None -> (Utils.troncate' 200 self.description, [])
  in
  Lwt.return (
    Atom_feed.entry
      ~updated: self.date
      ~id:(Int32.to_string self.id)
      ~title: (Atom_feed.plain (title))
      [Atom_feed.authors [Atom_feed.author author#!name];
       Atom_feed.links [Atom_feed.link (Uri.make_string_uri ~absolute:true
                                          ~service:Services.view_feed
                                          (Int32.to_int self.id, "")
                                       )];
       Atom_feed.summary (Atom_feed.html5 (
         (match self.url with
          | Some url -> Html.Raw.a ~a:
                          [Html.a_href
                             (Html.uri_of_string
                                (fun () -> url)
                             )
                          ]
                          [Html.pcdata self.description]
          | None ->
              let markdown = Markdown.parse_text self.description in
              let render_pre ~kind s = H.pre [H.pcdata s] in
              let render_link {Markdown.href_target; href_desc} =
                H.a ~a:[H.a_href (H.uri_of_string href_target)] [H.pcdata href_desc]
              in
              let render_img {Markdown.img_src; img_alt} =
                H.img ~src:(H.uri_of_string img_src) ~alt:img_alt ()
              in
              Html.div ~a:[Html.a_class ["lamalama"]] (conv (M.to_html ~render_pre
                                                               ~render_link ~render_img markdown))
         )
         :: (Html.br ())
         :: (Html.a ~service:Services.atom_feed [Html.pcdata "Flux atom du lien"]
               (Int32.to_int self.id))
         :: (Html.br ())
         :: (Html.pcdata "Tags : ")
         :: (links_of_tags self.tags)
         @ [(Html.br ())]
         @ root_infos
       )
       )
      ]
  )

let get_edit_url feed =
  match feed#?url with
  | Some f -> f
  | None -> "Url"

let get_edit_tags tags =
  let tags_str = List.map (fun t -> t#!tag) tags in
  String.concat ", " tags_str

let get_edit_infos id =
  Db_feed.is_url ~feedid:id () >>= fun is_url ->
  Db_feed.get_feed_with_id id >>= fun (feed, tags, _) ->
  let desc = feed#!description in
  let url = get_edit_url feed in
  let tags_str = get_edit_tags tags in
  Lwt.return (is_url, desc, url, tags_str)

let delete_feed_check ~feed ~userid () =
  User.is_admin () >>= fun is_admin ->
  Db_feed.is_feed_author ~feed ~userid () >>= fun is_author ->
  if is_admin or is_author then
    Db_feed.delete_feed ~feed ~userid ()
  else
    Lwt.return ()

let exec_if_not_author f feedid =
  User.get_userid () >>= function
  | Some userid ->
      Db_feed.is_feed_author ~feed:feedid ~userid ()
      >>= fun is_author ->
      if not is_author then
        f ~feedid ~userid ()
      else
        Lwt.return ()
  | None -> Lwt.return ()

let get_userid f =
  User.get_userid () >>= BatOption.map_default f Lwt.return_unit

let add_fav feedid =
  get_userid (fun userid -> Db_feed.add_fav ~feedid ~userid ())
let del_fav feedid =
  get_userid (fun userid -> Db_feed.del_fav ~feedid ~userid ())

let upvote = exec_if_not_author Db_feed.upvote
let downvote = exec_if_not_author Db_feed.downvote
let cancel_vote = exec_if_not_author Db_feed.cancelvote

(* TODO: Remove the following functions *)
let get_root_feeds = Db_feed.get_root_feeds
let count_root_feeds () = Db_feed.count_root_feeds () >|= fun x -> x#!n
let get_feeds_with_author = Db_feed.get_feeds_with_author
let count_feeds_with_author x = Db_feed.count_feeds_with_author x >|= fun x -> x#!n
let get_feeds_with_tag = Db_feed.get_feeds_with_tag
let count_feeds_with_tag x = Db_feed.count_feeds_with_tag x >|= fun x -> x#!n
let get_fav_with_username = Db_feed.get_fav_with_username
let count_fav_with_username x = Db_feed.count_fav_with_username x >|= fun x -> x#!n
let exist = Db_feed.exist
let is_feed_author = Db_feed.is_feed_author
