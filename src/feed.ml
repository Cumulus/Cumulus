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

module Calendar = CalendarLib.Calendar

type feed = {
  id : int32;
  url : string option;
  description : string;
  date : Calendar.t;
  author : int32;
  parent : int32 option;
  root : int32 option;
  tags: string list
}

type tree = Sheet of feed | Node of feed * tree list
let (>>=) = Lwt.(>>=)

let feed_new data tags = {
  id = data#!id;
  url = data#?url;
  description = data#!description;
  date = data#!timedate;
  author = data#!author;
  parent = data#?parent;
  root = data#?root;
  tags = tags
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

let to_html self =
  let content = match self.url with
    | Some url -> Html.Raw.a
                    ~a:[Html.a_class ["postitle"];
                        Html.a_href (Html.uri_of_string (fun () -> url));
                       ]
                  [Html.pcdata self.description]
    | None -> Html.pcdata self.description in
  let tags = match self.url with
    | Some _ -> (Html.pcdata "Tags: ") :: links_of_tags self.tags
    | None -> [] in
  Db_user.get_user_name_and_email_with_id self.author >>= fun author ->
  User.get_userid () >>= (function
    | None -> Lwt.return false
    | Some userid ->
        Db_feed.is_feed_author ~feed:self.id ~userid ()
        >>= fun x ->
        User.is_admin ()
        >>= fun y ->
        Lwt.return (x || y)
  )
  >>= fun is_author ->
  Db_feed.count_comments self.id >>= fun comments ->
  Lwt.return (
    List.flatten [
      [Html.img
          ~a: [Html.a_class ["left"]]
          ~alt: (author#!name)
          ~src: (
            Html.make_uri
              ~service: (Utils.get_gravatar (author#!email)) (40, "identicon")
          )
          ();
       content;
       Html.br ();
       Html.pcdata ("Publié le " ^ (Utils.string_of_calendar self.date) ^ " par ");
       Html.a
         ~service:Services.author_feed
         [Html.pcdata author#!name]
         (None, author#!name);
      ];
      [Html.br ();
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
         (Int32.to_int self.id, Utils.strip self.description);
       Html.a
         ~service:Services.comment
         [Html.pcdata " Poster un commentaire "]
         (Int32.to_int self.id, Utils.strip self.description);
      ]; tags;
      [Html.a ~service:Services.atom_feed
        [Html.pcdata " [Flux Atom du lien]"] (Int32.to_int self.id)];
      (if is_author then
          [Html.a ~service:Services.delete_feed [Html.pcdata " (supprimer ?)"] self.id]
       else []
      );
    ]
  )

let to_atom self =
  Db_user.get_user_name_and_email_with_id self.author >>= fun author ->
  Lwt.return (
    Atom_feed.entry
      ~updated: self.date
      ~id:(Int32.to_string self.id)
      ~title: (Atom_feed.plain (self.description))
      [Atom_feed.authors [Atom_feed.author author#!name];
       (match self.url with
         | Some url ->
             Atom_feed.links [Atom_feed.link url]
         | _ -> Atom_feed.links []);
       Atom_feed.summary (Atom_feed.html5 (links_of_tags self.tags))
      ]
  )
