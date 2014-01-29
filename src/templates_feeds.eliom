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

{client{
  open Eliom_lib.Lwt_ops

  let display_error error_frame =
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

  (* Reloading feeds*)
  let () =
    let service = Eliom_service.void_coservice' in
    let event = %Feeds.event in
    let stream = Lwt_react.E.to_stream event in
    Lwt.async
      (fun () ->
         Lwt_stream.iter_s
           (fun () ->
              Eliom_client.change_page ~service () ()
           )
           stream
      )
}}

open Batteries

module Html = Eliom_content.Html5.F

let feed_to_html ~user self =
  let module H = Eliom_content.Html5.F.Raw in
  let module M = MarkdownHTML.Make_html5(struct include H module Svg = Eliom_content.Svg.F.Raw end) in
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
  in
  let get_image cls imgname =
   Html.img ~a: [Html.a_class cls]
                  ~alt: imgname
                  ~src:(Html.make_uri
                    ~service: (Eliom_service.static_dir ())
                  [imgname]
                    )() in
  let content = match self.Feed.url with
    | Some url -> Html.div ~a:[Html.a_class["line_title"]][
                    Html.Raw.a
                    ~a:[Html.a_class ["postitle"];
                        Html.a_href (Html.uri_of_string (fun () -> url));
                       ]
                    [Html.pcdata self.Feed.description]]
    | None ->
        let markdown = Markdown.parse_text self.Feed.description in
        let render_pre ~kind s = H.pre [H.pcdata s] in
        let render_link {Markdown.href_target; href_desc} =
          H.a ~a:[H.a_href (H.uri_of_string href_target)] [H.pcdata href_desc]
        in
        let render_img {Markdown.img_src; img_alt} =
          H.img ~src:(H.uri_of_string img_src) ~alt:img_alt ()
        in
        Html.div ~a:[Html.a_class ["lamalama"]] (M.to_html ~render_pre ~render_link ~render_img markdown)
  in
  let tags = match self.Feed.url with
    | Some _ -> Html.div ~a:[Html.a_class["tag_line"]] (links_of_tags self.Feed.tags)
    | None -> Html.div ~a:[Html.a_class["error"]][]
  in
  let is_author = Feed.is_author ~feed:self user in
  List.flatten
    [
      [
        Html.aside ~a: [Html.a_class ["row";"post";"mod"]; Html.a_id "post"] [
          Html.aside ~a: [Html.a_class["col";"avatarbox"]]
            [Html.div ~a: [Html.a_class["post_avatar"]]
               [Html.img
                  ~a: [Html.a_class ["postimg"]]
                  ~alt: (self.Feed.user#name)
                  ~src: (
                    Html.make_uri
                      ~service: (Utils.get_gravatar (self.Feed.user#email_digest)) (65, "identicon")
                  )
                  ()]];
          Html.aside ~a: [Html.a_class["col";"post_info"]][
            Html.div ~a: [Html.a_class["line_author"]]([

              Html.pcdata ("Publié le " ^ (Utils.string_of_calendar self.Feed.date) ^ " par ");
              Html.a
                ~service:Services.author_feed
                [Html.pcdata self.Feed.user#name]
                (None, self.Feed.user#name);
              Html.a
                ~service:Services.atom_feed
                [Html.pcdata "  Flux Atom du lien "]
                (Int32.to_int self.Feed.id);
            ]
              @
              (if is_author then
                 [
                   Html.a ~service:Services.delete_feed [Html.pcdata "- Supprimer "] self.Feed.id ;
                   Html.a ~service:Services.edit_feed [Html.pcdata "- Editer"]
                     (Int32.to_int self.Feed.id, Utils.troncate self.Feed.description);
                 ]
               else []
              ));
            content;
            tags;
          ];
          Html.div ~a: [Html.a_class["col";"post_int"]][


            Html.aside
              ~a: [Html.a_class["comment_block"]][
              Html.div ~a: [Html.a_class["com_wrap"]][
                Html.a
                  ~service:Services.view_feed
                  [if self.Feed.count <= 0 then
                     get_image ["circled";"gray";"comment_icon"] "comments.png"
                   else
                     get_image ["circled";"highlighted";"comment_icon"] "comments.png";
                  ]
                  (Int32.to_int self.Feed.id, Utils.troncate self.Feed.description)];
              Html.pcdata (string_of_int self.Feed.count)
            ];
            Html.div ~a: [Html.a_class ["fav_wrap"]][
              if self.Feed.fav = false then
                Html.a
                  ~service:Services.add_fav_feed
                  [get_image ["circled";"gray";] "fav.png"]
                  (self.Feed.id)
              else
                Html.a
                  ~service:Services.del_fav_feed
                  [get_image ["circled";"highlighted";"deletable"] "fav.png"]
                  (self.Feed.id);
            ];
            let cl = if self.Feed.score <= 0 then ["upvote_wrap_inner";"gray"] else
                ["upvote_wrap_inner"] in
            Html.div ~a: [Html.a_class["upvote_wrap"]][
              Html.div ~a: [Html.a_class cl][
                if self.Feed.score <> 1 then
                  (Html.a ~service:Services.upvote_feed [
                     get_image [] "up.png"] self.Feed.id)
                else
                  (Html.a ~service:Services.cancelvote_feed [
                     get_image [] "upon.png"] self.Feed.id);
                Html.pcdata (string_of_int self.Feed.score);
                if self.Feed.score <> -1 then
                  (Html.a ~service:Services.downvote_feed [
                     get_image [] "down.png"] self.Feed.id)
                else
                  (Html.a ~service:Services.cancelvote_feed [
                     get_image [] "downon.png"] self.Feed.id)
              ]];
          ]
        ]
      ]
    ]

let rec comments_to_html' ~user tree =
  match tree with
  | Comments.Sheet feed ->
      let elm = feed_to_html ~user feed in
      Html.div ~a: [Html.a_class ["line"]] elm
  | Comments.Node (feed, childs) ->
      let elm = feed_to_html ~user feed in
      let childs = List.map (comments_to_html' ~user) childs in
      Html.div ~a: [Html.a_class ["line"]] (elm @ childs)

let private_to_html ~user data =
  List.map
    (fun feed ->
       let elm = feed_to_html ~user feed in
       Html.section ~a: [Html.a_class["line"]] elm
    )
    data

let to_html = private_to_html

let string_input_box ?(a=[]) =
  Html.string_input ~a:(Html.a_class ["input-box"] :: a)

let submit_input ?(a=[]) =
  Html.string_input
    ~a:(Html.a_class ["btn-box"] :: a)
    ~input_type:`Submit

let user_form () =
  Html.aside
    ~a: [Html.a_class ["col";"w20";"userbox"; "right"; "bottom-box"]][
    Html.post_form
      ~a: [Html.a_class ["userboxcontent"]]
      ~service: Services.auth
      (fun (user_name, password_name) -> [
           Html.string_input
             ~a: [Html.a_placeholder "Pseudo"]
             ~input_type: `Text
             ~name: user_name ();
           Html.string_input
             ~a: [Html.a_placeholder "Mot de passe"]
             ~input_type: `Password
             ~name: password_name ();
           Html.div ~a: [Html.a_class ["loginlinks"]] [
             Html.string_input
               ~input_type: `Submit
               ~value: "Connexion" ();
             Html.a
               ~a: [Html.a_class ["nav"]]
               ~service: Services.registration
               [Html.pcdata "Inscription"] ();
           ]
         ]
      )
      ()
  ]

let user_logged user =
  Html.aside
    ~a:[Html.a_class ["col";"w10";"loggedbox"; "right"; "bottom-box"]]
    [ Html.post_form
        ~a:[Html.a_class ["userboxcontent"]]
        ~service:Services.disconnect
        (fun () ->
           [
             Html.string_input
               ~input_type:`Submit
               ~value:"Déconnexion"
               ();

             Html.div ~a: [Html.a_class ["loggedlink"]] [
               Html.a
                 ~a:[Html.a_class ["loggedentry"]]
                 ~service:Services.fav_feed
                 [Html.pcdata "Favoris"]
                 (user.User.name, Some 0)];

             Html.div ~a: [Html.a_class ["loggedlink"]] [
               Html.a
                 ~a:[Html.a_class ["loggedentry"]]
                 ~service:Services.preferences
                 [Html.pcdata "Préférences"]
                 ()];
             Html.img
               ~a: [Html.a_class ["loggedavatar";"right"]]
               ~alt:user.User.name
               ~src:(
                 Html.make_uri
                   ~service: (Utils.get_gravatar user.User.email_digest)
                   (50, "identicon")
               )
               ();
           ]
        )
        ()
    ]

let userbox = function
  | Some user -> user_logged user
  | None -> user_form ()

let header () =
  [ Html.section
      ~a:[Html.a_class ["flex flex-h header"]]
      [
        Html.aside
          ~a:[Html.a_class [""]]
          [ Html.img
              ~alt:("Cumulus Project")
              ~src:(
                Html.make_uri
                  ~service: (Eliom_service.static_dir ())
                  ["logo.png"]
              )
              ();
          ];
        Html.aside
          ~a:[Html.a_class ["w75 dash"]]
          ([ Html.post_form
               ~service:Services.append_feed
               (fun (url_name, (title_name, tags_name)) -> [
                    Html.string_input
                      ~a:[Html.a_placeholder "URL"; Html.a_class["url"]]
                      ~input_type:`Text
                      ~name:url_name
                      ();
                    Html.string_input
                      ~a:[Html.a_placeholder "Titre"]
                      ~input_type:`Text
                      ~name:title_name
                      ();
                    Html.string_input
                      ~a:[Html.a_placeholder "Tags"]
                      ~input_type:`Text
                      ~name:tags_name
                      ();
                    Html.string_input
                      ~a:[Html.a_class [""]]
                      ~input_type:`Submit
                      ~value: "ENVOYER !"
                      ()
                  ])
               ()
           ])
      ];
  ]

let main_style ~user ~error content footer =
  let userbox = userbox user in
  let header = header () in
  let base_error_frame =
    Eliom_content.Html5.D.div
      ~a:[Html.a_class ["msghandler"]]
  in
  let error_frame =
    match error with
    | Some error ->
        let error_frame =
          base_error_frame [Html.p [Html.pcdata error]]
        in
        ignore {unit{
          display_error %error_frame
        }};
        error_frame
    | None -> base_error_frame []
  in
  Html.html
    (Html.head
       (Html.title
          (Html.pcdata "Cumulus")
       )
       [ Html.css_link
           ~uri: (Html.make_uri
                    ~service: (Eliom_service.static_dir ())
                    ["knacss.css"]
                 ) ();
         Html.css_link
           ~uri: (Html.make_uri
                    ~service: (Eliom_service.static_dir ())
                    ["cumulus.css"]
                 ) ();
       ]
    )
    (Html.body
       [ Html.div
           ~a: [Html.a_class ["line"]]
           (header
            @ [Html.aside ~a: [Html.a_class["col";"w80"]] content]
            @ [ userbox;
                Html.div ~a: [Html.a_class ["navigation"]]footer;
                   (*Html.footer
                     ( [ Html.br ();
                         Html.br ();
                         Html.pcdata "(not so) Proudly propulsed by the inglorious ";
                         Html.Raw.a ~a:[Html.a_href
                                          (Html.uri_of_string
                                             (fun () ->
                                                "https://github.com/Cumulus/Cumulus"
                                             )
                                          )
                                       ]
                           [Html.pcdata "Cumulus Project"];
                         Html.pcdata ", with love, and the ";
                         Html.Raw.a ~a:[Html.a_href
                                          (Html.uri_of_string
                                             (fun () -> "http://ocsigen.org/")
                                          )
                                       ]
                           [Html.pcdata "OCaml web framework Ocsigen"];
                         Html.a ~service:Services.atom
                           [Html.pcdata "    (Flux Atom du site)"] ();
                         Html.pcdata ", ";
                         Html.a ~service:Services.comments_atom
                           [Html.pcdata "Flux Atom des commentaires"] ();
                         Html.pcdata ") Si t'as oublié ton mot-de-passe, clique ";
                         Html.a ~service:Services.reset_password_form [Html.pcdata "ici"] ();
                       ]
                     )*)
              ]
           )
       ]
    )

let link_footer ~link min max = function
  | page when page = min && page < max -> [ link "Suivant" (Some (page + 1)) ]
  | page when page = max && page > min -> [ link "Précédent" (Some (page - 1)) ]
  | page when page > min && page < max ->
      [ link "Précédent" (Some (page - 1)); link "Suivant" (Some (page + 1)) ]
  | _ -> []

let private_register () =
  main_style
    [Html.post_form
       ~a:[Html.a_class ["box"]]
       ~service:Services.add_user
       (fun (username_name, (email_name, (password_name, password_check))) -> [
            Html.h1 [Html.pcdata "Inscription"];
            Html.p [
              string_input_box
                ~a:[Html.a_placeholder "Pseudo"]
                ~input_type:`Text
                ~name:username_name
                ();
              Html.br ();
              string_input_box
                ~a:[Html.a_placeholder "Mot de passe"]
                ~input_type:`Password
                ~name:password_name
                ();
              Html.br ();
              string_input_box
                ~a:[Html.a_placeholder "Confirmation"]
                ~input_type:`Password
                ~name:password_check
                ();
              Html.br ();
              string_input_box
                ~a:[Html.a_placeholder "Email"]
                ~input_type:`Text
                ~name:email_name
                ();
              Html.br ();
              submit_input ~value:"Valider" ()
            ]
          ])
       ()
    ]
    []

let private_preferences ~user ~error =
  main_style
    ~user
    ~error
    (match user with
     | None ->
         [Html.div
            ~a:[Html.a_class ["box"]]
            [Html.pcdata "Veuillez vous connecter pour accéder aux préférences."]
         ]
     | Some usr ->
         [ Html.post_form
             ~a:[Html.a_class ["box"]]
             ~service:Services.update_user_password
             (fun (password_name, password_check) -> [
                  Html.h1 [Html.pcdata "Modifier le mot de passe"] ;
                  Html.p [
                    string_input_box
                      ~a:[Html.a_placeholder "Nouveau mot de passe"]
                      ~input_type:`Password
                      ~name:password_name
                      ();
                    Html.br ();
                    string_input_box
                      ~a:[Html.a_placeholder "Confirmer le nouveau mot de passe"]
                      ~input_type:`Password
                      ~name:password_check
                      ();
                    Html.br ();
                    submit_input ~value:"Valider" ()
                  ]
                ])
             ();
           Html.post_form
             ~a:[Html.a_class ["box"]]
             ~service:Services.update_user_mail
             (fun email_name -> [
                  Html.h1 [Html.pcdata "Changer d'adresse mail"];
                  Html.p [
                    string_input_box
                      ~a:[Html.a_placeholder User.(usr.email);
                          Html.a_id "new_email"
                         ]
                      ~input_type:`Text
                      ~name:email_name
                      ();
                    Html.br ();
                    submit_input ~value:"Valider" ()
                  ]
                ])
             ();
           Html.post_form
             ~a:[Html.a_class ["box"]]
             ~service:Services.update_user_feeds_per_page
             (fun nb_feeds_name -> [
                  Html.h1 [Html.pcdata "Changer le nombre de liens par page"];
                  Html.p [
                    Html.int_input
                      ~a:[Html.a_class ["input-box"];
                          Html.a_placeholder (Int32.to_string
                                                User.(usr.feeds_per_page))
                         ]
                      ~input_type:`Text
                      ~name:nb_feeds_name
                      ();
                    Html.br ();
                    submit_input ~value:"Valider" ()
                  ]
                ])
             ()
         ]
    )
    []

let private_comment ~user id =
  main_style
    ~user
    ( if Option.is_none user then
        [Html.div
           ~a:[Html.a_class ["box"]]
           [Html.pcdata "Veuillez vous connecter pour poster un commentaire."]
        ]
      else
        [ Html.post_form
            ~a:[Html.a_class ["box"]]
            ~service:Services.append_link_comment
            (fun (parent, (url, (desc, tags))) -> [
                 Html.h1 [Html.pcdata "Lien"] ;
                 Html.p [
                   string_input_box
                     ~a:[Html.a_placeholder "URL"]
                     ~input_type:`Text
                     ~name:url
                     ();
                   Html.br ();
                   string_input_box
                     ~a:[Html.a_placeholder "Titre"]
                     ~input_type:`Text
                     ~name:desc
                     ();
                   Html.br ();
                   string_input_box
                     ~a:[Html.a_placeholder "Tags"]
                     ~input_type:`Text
                     ~name:tags
                     ();
                   Html.br ();
                   Html.int_input
                     ~input_type:`Hidden
                     ~name:parent
                     ~value:(Int32.to_int id)
                     ();
                   submit_input ~value:"Envoyer !" ()
                 ]
               ])
            (Int32.to_int id, "");
          Html.post_form
            ~a:[Html.a_class ["box"]]
            ~service:Services.append_desc_comment
            (fun (parent, desc) -> [
                 Html.h1 [Html.pcdata "Commentaire"];
                 Html.p [
                   Html.textarea
                     ~a:[Html.a_class ["input-box"];
                         Html.a_placeholder "Texte"
                        ]
                     ~name:desc
                     ();
                   Html.int_input
                     ~input_type:`Hidden
                     ~name:parent
                     ~value:(Int32.to_int id)
                     ();
                   Html.br ();
                   submit_input ~value:"Envoyer !" ()
                 ]
               ])
            (Int32.to_int id, "")
        ]) []

let private_edit_feed ~user ~error ~feed (edit_desc, edit_url, edit_tags) =
  let is_author = Feed.is_author ~feed user in
  let id = Int32.to_int feed.Feed.id in
  main_style
    ~user
    ~error
    ( if not is_author then
        [Html.div
           ~a:[Html.a_class ["box"]]
           [Html.pcdata "Vous n'avez pas le droit d'editer ce lien."]
        ]
      else
        [
          match edit_url with
          | Some edit_url ->
              (Html.post_form
                 ~a:[Html.a_class ["box"]]
                 ~service:Services.edit_link_comment
                 (fun (parent, (url, (desc, tags))) -> [
                      Html.h1 [Html.pcdata "Lien"] ;
                      Html.p [
                        string_input_box
                          ~a:[ Html.a_placeholder "URL"]
                          ~input_type:`Text
                          ~name:url
                          ~value:edit_url
                          ();
                        Html.br ();
                        string_input_box
                          ~a:[ Html.a_placeholder "Titre" ]
                          ~input_type:`Text
                          ~name:desc
                          ~value:edit_desc
                          ();
                        Html.br ();
                        string_input_box
                          ~a:[ Html.a_placeholder "Tags" ]
                          ~input_type:`Text
                          ~name:tags
                          ~value:edit_tags
                          ();
                        Html.br ();
                        Html.int_input
                          ~input_type:`Hidden
                          ~name:parent
                          ~value:id
                          ();
                        submit_input ~value:"Envoyer !" ()
                      ]
                    ])
                 (id, ""))
          | None ->
              (Html.post_form
                 ~a:[Html.a_class ["box"]]
                 ~service:Services.edit_desc_comment
                 (fun (parent, desc) -> [
                      Html.h1 [Html.pcdata "Commentaire"];
                      Html.p [
                        Html.textarea
                          ~a:[Html.a_class ["input-box"];
                              Html.a_placeholder "Comment" ]
                          ~name:desc
                          ~value:edit_desc
                          () ;
                        Html.int_input
                          ~input_type:`Hidden
                          ~name:parent
                          ~value:id
                          ();
                        Html.br ();
                        submit_input ~value:"Envoyer !" ()
                      ]
                    ])
                 (id, ""))
        ]
    )
    []
