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
open Eliom_content.Html5.F

let feed_to_html ~user self =
  let get_image cls imgname =
   img ~a: [a_class cls]
                  ~alt: imgname
                  ~src:(make_uri
                    ~service: (Eliom_service.static_dir ())
                  [imgname]
                    )() in
  let content = match self.Feed.url with
    | Some url -> div ~a:[a_class["line_title"]][
                    Raw.a
                    ~a:[a_class ["postitle"];
                        a_href (uri_of_string (fun () -> url));
                       ]
                    [pcdata self.Feed.description]]
    | None ->
        let markdown = Markdown.parse_text self.Feed.description in
        let render_pre ~kind s = Raw.pre [Raw.pcdata s] in
        let render_link {Markdown.href_target; href_desc} =
          Raw.a ~a:[Raw.a_href (Raw.uri_of_string href_target)] [Raw.pcdata href_desc]
        in
        let render_img {Markdown.img_src; img_alt} =
          Raw.img ~src:(Raw.uri_of_string img_src) ~alt:img_alt ()
        in
        div ~a:[a_class ["lamalama"]] (Templates_common.Markdown.to_html ~render_pre ~render_link ~render_img markdown)
  in
  let tags = match self.Feed.url with
    | Some _ -> div ~a:[a_class["tag_line"]] (Templates_common.links_of_tags self.Feed.tags)
    | None -> div ~a:[a_class["error"]][]
  in
  let is_author = Feed.is_author ~feed:self user in
  List.flatten
    [
      [
        aside ~a: [a_class ["row";"post";"mod"]; a_id "post"] [
          aside ~a: [a_class["col";"avatarbox"]]
            [div ~a: [a_class["post_avatar"]]
               [img
                  ~a: [a_class ["postimg"]]
                  ~alt: (self.Feed.user#name)
                  ~src: (
                    make_uri
                      ~service: (Utils.get_gravatar (self.Feed.user#email_digest)) (65, "identicon")
                  )
                  ()]];
          aside ~a: [a_class["col";"post_info"]][
            div ~a: [a_class["line_author"]]([

              pcdata ("Publié le " ^ (Utils.string_of_calendar self.Feed.date) ^ " par ");
              a
                ~service:Services.author_feed
                [pcdata self.Feed.user#name]
                (None, self.Feed.user#name);
              a
                ~service:Services.atom_feed
                [pcdata "  Flux Atom du lien "]
                self.Feed.id;
            ]
              @
              (if is_author then
                 [
                   a ~service:Services.delete_feed [pcdata "- Supprimer "] self.Feed.id ;
                   a ~service:Services.edit_feed [pcdata "- Editer"]
                     (self.Feed.id, Utils.troncate self.Feed.description);
                 ]
               else []
              ));
            content;
            tags;
          ];
          div ~a: [a_class["col";"post_int"]][


            aside
              ~a: [a_class["comment_block"]][
              div ~a: [a_class["com_wrap"]][
                a
                  ~service:Services.view_feed
                  [if self.Feed.count <= 0 then
                     get_image ["circled";"gray";"comment_icon"] "comments.png"
                   else
                     get_image ["circled";"highlighted";"comment_icon"] "comments.png";
                  ]
                  (self.Feed.id, Utils.troncate self.Feed.description)];
              pcdata (string_of_int self.Feed.count)
            ];
            div ~a: [a_class ["fav_wrap"]][
              if self.Feed.fav = false then
                a
                  ~service:Services.add_fav_feed
                  [get_image ["circled";"gray";] "fav.png"]
                  (self.Feed.id)
              else
                a
                  ~service:Services.del_fav_feed
                  [get_image ["circled";"highlighted";"deletable"] "fav.png"]
                  (self.Feed.id);
            ];
            let cl = if self.Feed.score <= 0 then ["upvote_wrap_inner";"gray"] else
                ["upvote_wrap_inner"] in
            div ~a: [a_class["upvote_wrap"]][
              div ~a: [a_class cl][
                if self.Feed.score <> 1 then
                  (a ~service:Services.upvote_feed [
                     get_image [] "up.png"] self.Feed.id)
                else
                  (a ~service:Services.cancelvote_feed [
                     get_image [] "upon.png"] self.Feed.id);
                pcdata (string_of_int self.Feed.score);
                if self.Feed.score <> -1 then
                  (a ~service:Services.downvote_feed [
                     get_image [] "down.png"] self.Feed.id)
                else
                  (a ~service:Services.cancelvote_feed [
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
      div ~a: [a_class ["line"]] elm
  | Comments.Node (feed, childs) ->
      let elm = feed_to_html ~user feed in
      let childs = List.map (comments_to_html' ~user) childs in
      div ~a: [a_class ["line"]] (elm @ childs)

let private_to_html ~user data =
  List.map
    (fun feed ->
       let elm = feed_to_html ~user feed in
       section ~a: [a_class["line"]] elm
    )
    data

let to_html = private_to_html

let user_form () =
  aside
    ~a: [a_class ["col";"w20";"userbox"; "right"; "bottom-box"]][
    post_form
      ~a: [a_class ["userboxcontent"]]
      ~service: Services.auth
      (fun (user_name, password_name) -> [
           string_input
             ~a: [a_placeholder "Pseudo"]
             ~input_type: `Text
             ~name: user_name ();
           string_input
             ~a: [a_placeholder "Mot de passe"]
             ~input_type: `Password
             ~name: password_name ();
           div ~a: [a_class ["loginlinks"]] [
             string_input
               ~input_type: `Submit
               ~value: "Connexion" ();
             a
               ~a: [a_class ["nav"]]
               ~service: Services.registration
               [pcdata "Inscription"] ();
           ]
         ]
      )
      ()
  ]

let user_logged user =
  aside
    ~a:[a_class ["col";"w10";"loggedbox"; "right"; "bottom-box"]]
    [ post_form
        ~a:[a_class ["userboxcontent"]]
        ~service:Services.disconnect
        (fun () ->
           [
             string_input
               ~input_type:`Submit
               ~value:"Déconnexion"
               ();

             div ~a: [a_class ["loggedlink"]] [
               a
                 ~a:[a_class ["loggedentry"]]
                 ~service:Services.fav_feed
                 [pcdata "Favoris"]
                 (user.User.name, Some 0)];

             div ~a: [a_class ["loggedlink"]] [
               a
                 ~a:[a_class ["loggedentry"]]
                 ~service:Services.preferences
                 [pcdata "Préférences"]
                 ()];
             img
               ~a: [a_class ["loggedavatar";"right"]]
               ~alt:user.User.name
               ~src:(
                 make_uri
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
  [ section
      ~a:[a_class ["flex flex-h header"]]
      [
        aside
          ~a:[a_class [""]]
          [ img
              ~alt:("Cumulus Project")
              ~src:(
                make_uri
                  ~service: (Eliom_service.static_dir ())
                  ["logo.png"]
              )
              ();
          ];
        aside
          ~a:[a_class ["w75 dash"]]
          ([ post_form
               ~service:Services.append_feed
               (fun (url_name, (title_name, tags_name)) -> [
                    string_input
                      ~a:[a_placeholder "URL"; a_class["url"]]
                      ~input_type:`Text
                      ~name:url_name
                      ();
                    string_input
                      ~a:[a_placeholder "Titre"]
                      ~input_type:`Text
                      ~name:title_name
                      ();
                    string_input
                      ~a:[a_placeholder "Tags"]
                      ~input_type:`Text
                      ~name:tags_name
                      ();
                    string_input
                      ~a:[a_class [""]]
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
      ~a:[a_class ["msghandler"]]
  in
  let error_frame =
    match error with
    | Some error ->
        let error_frame =
          base_error_frame [p [pcdata error]]
        in
        ignore {unit{
          display_error %error_frame
        }};
        error_frame
    | None -> base_error_frame []
  in
  html
    (head
       (title
          (pcdata "Cumulus")
       )
       [ css_link
           ~uri: (make_uri
                    ~service: (Eliom_service.static_dir ())
                    ["knacss.css"]
                 ) ();
         css_link
           ~uri: (make_uri
                    ~service: (Eliom_service.static_dir ())
                    ["cumulus.css"]
                 ) ();
       ]
    )
    (body
       [ div
           ~a: [a_class ["line"]]
           (header
            @ [aside ~a: [a_class["col";"w80"]] content]
            @ [ userbox;
                div ~a: [a_class ["navigation"]]footer;
                   (*footer
                     ( [ br ();
                         br ();
                         pcdata "(not so) Proudly propulsed by the inglorious ";
                         Raw.a ~a:[a_href
                                          (uri_of_string
                                             (fun () ->
                                                "https://github.com/Cumulus/Cumulus"
                                             )
                                          )
                                       ]
                           [pcdata "Cumulus Project"];
                         pcdata ", with love, and the ";
                         Raw.a ~a:[a_href
                                          (uri_of_string
                                             (fun () -> "http://ocsigen.org/")
                                          )
                                       ]
                           [pcdata "OCaml web framework Ocsigen"];
                         a ~service:Services.atom
                           [pcdata "    (Flux Atom du site)"] ();
                         pcdata ", ";
                         a ~service:Services.comments_atom
                           [pcdata "Flux Atom des commentaires"] ();
                         pcdata ") Si t'as oublié ton mot-de-passe, clique ";
                         a ~service:Services.reset_password_form [pcdata "ici"] ();
                       ]
                     )*)
              ]
           )
       ]
    )

let link_footer ~service ~param min max =
  let link name x = a ~service [pcdata name] (param x) in
  function
  | page when page = min && page < max -> [ link "Suivant" (Some (page + 1)) ]
  | page when page = max && page > min -> [ link "Précédent" (Some (page - 1)) ]
  | page when page > min && page < max ->
      [ link "Précédent" (Some (page - 1)); link "Suivant" (Some (page + 1)) ]
  | _ -> []

let private_register () =
  [post_form
     ~a:[a_class ["box"]]
     ~service:Services.add_user
     (fun (username_name, (email_name, (password_name, password_check))) -> [
          h1 [pcdata "Inscription"];
          p [
            Templates_common.string_input_box
              ~a:[a_placeholder "Pseudo"]
              ~input_type:`Text
              ~name:username_name
              ();
            br ();
            Templates_common.string_input_box
              ~a:[a_placeholder "Mot de passe"]
              ~input_type:`Password
              ~name:password_name
              ();
            br ();
            Templates_common.string_input_box
              ~a:[a_placeholder "Confirmation"]
              ~input_type:`Password
              ~name:password_check
              ();
            br ();
            Templates_common.string_input_box
              ~a:[a_placeholder "Email"]
              ~input_type:`Text
              ~name:email_name
              ();
            br ();
            Templates_common.submit_input ~value:"Valider" ()
          ]
        ])
     ()
  ]

let private_preferences ~user =
  match user with
   | None ->
       [div
          ~a:[a_class ["box"]]
          [pcdata "Veuillez vous connecter pour accéder aux préférences."]
       ]
   | Some user ->
       [ post_form
           ~a:[a_class ["box"]]
           ~service:Services.update_user_password
           (fun (password_name, password_check) -> [
                h1 [pcdata "Modifier le mot de passe"] ;
                p [
                  Templates_common.string_input_box
                    ~a:[a_placeholder "Nouveau mot de passe"]
                    ~input_type:`Password
                    ~name:password_name
                    ();
                  br ();
                  Templates_common.string_input_box
                    ~a:[a_placeholder "Confirmer le nouveau mot de passe"]
                    ~input_type:`Password
                    ~name:password_check
                    ();
                  br ();
                  Templates_common.submit_input ~value:"Valider" ()
                ]
              ])
           ();
         post_form
           ~a:[a_class ["box"]]
           ~service:Services.update_user_mail
           (fun email_name -> [
                h1 [pcdata "Changer d'adresse mail"];
                p [
                  Templates_common.string_input_box
                    ~a:[a_placeholder user.User.email;
                        a_id "new_email"
                       ]
                    ~input_type:`Text
                    ~name:email_name
                    ();
                  br ();
                  Templates_common.submit_input ~value:"Valider" ()
                ]
              ])
           ();
         post_form
           ~a:[a_class ["box"]]
           ~service:Services.update_user_feeds_per_page
           (fun nb_feeds_name -> [
                h1 [pcdata "Changer le nombre de liens par page"];
                p [
                  int32_input
                    ~a:[a_class ["input-box"];
                        a_placeholder (Int32.to_string
                                         user.User.feeds_per_page)
                       ]
                    ~input_type:`Text
                    ~name:nb_feeds_name
                    ();
                  br ();
                  Templates_common.submit_input ~value:"Valider" ()
                ]
              ])
           ()
       ]

let private_comment ~user id =
  if Option.is_none user then
    [div
       ~a:[a_class ["box"]]
       [pcdata "Veuillez vous connecter pour poster un commentaire."]
    ]
  else
    [ post_form
        ~a:[a_class ["box"]]
        ~service:Services.append_link_comment
        (fun (parent, (url, (desc, tags))) -> [
             h1 [pcdata "Lien"] ;
             p [
               Templates_common.string_input_box
                 ~a:[a_placeholder "URL"]
                 ~input_type:`Text
                 ~name:url
                 ();
               br ();
               Templates_common.string_input_box
                 ~a:[a_placeholder "Titre"]
                 ~input_type:`Text
                 ~name:desc
                 ();
               br ();
               Templates_common.string_input_box
                 ~a:[a_placeholder "Tags"]
                 ~input_type:`Text
                 ~name:tags
                 ();
               br ();
               int32_input
                 ~input_type:`Hidden
                 ~name:parent
                 ~value:id
                 ();
               Templates_common.submit_input ~value:"Envoyer !" ()
             ]
           ])
        (id, "");
      post_form
        ~a:[a_class ["box"]]
        ~service:Services.append_desc_comment
        (fun (parent, desc) -> [
             h1 [pcdata "Commentaire"];
             p [
               textarea
                 ~a:[a_class ["input-box"];
                     a_placeholder "Texte"
                    ]
                 ~name:desc
                 ();
               int32_input
                 ~input_type:`Hidden
                 ~name:parent
                 ~value:id
                 ();
               br ();
               Templates_common.submit_input ~value:"Envoyer !" ()
             ]
           ])
        (id, "")
    ]

let private_edit_feed ~user ~feed (edit_desc, edit_url, edit_tags) =
  let is_author = Feed.is_author ~feed user in
  if not is_author then
    [div
       ~a:[a_class ["box"]]
       [pcdata "Vous n'avez pas le droit d'editer ce lien."]
    ]
  else
    [
      match edit_url with
      | Some edit_url ->
          (post_form
             ~a:[a_class ["box"]]
             ~service:Services.edit_link_comment
             (fun (parent, (url, (desc, tags))) -> [
                  h1 [pcdata "Lien"] ;
                  p [
                    Templates_common.string_input_box
                      ~a:[ a_placeholder "URL"]
                      ~input_type:`Text
                      ~name:url
                      ~value:edit_url
                      ();
                    br ();
                    Templates_common.string_input_box
                      ~a:[ a_placeholder "Titre" ]
                      ~input_type:`Text
                      ~name:desc
                      ~value:edit_desc
                      ();
                    br ();
                    Templates_common.string_input_box
                      ~a:[ a_placeholder "Tags" ]
                      ~input_type:`Text
                      ~name:tags
                      ~value:edit_tags
                      ();
                    br ();
                    int32_input
                      ~input_type:`Hidden
                      ~name:parent
                      ~value:feed.Feed.id
                      ();
                    Templates_common.submit_input ~value:"Envoyer !" ()
                  ]
                ])
             (feed.Feed.id, ""))
      | None ->
          (post_form
             ~a:[a_class ["box"]]
             ~service:Services.edit_desc_comment
             (fun (parent, desc) -> [
                  h1 [pcdata "Commentaire"];
                  p [
                    textarea
                      ~a:[a_class ["input-box"];
                          a_placeholder "Comment" ]
                      ~name:desc
                      ~value:edit_desc
                      () ;
                    int32_input
                      ~input_type:`Hidden
                      ~name:parent
                      ~value:feed.Feed.id
                      ();
                    br ();
                    Templates_common.submit_input ~value:"Envoyer !" ()
                  ]
                ])
             (feed.Feed.id, ""))
    ]

let error_content msg = [div ~a:[a_class ["box"]] [pcdata msg]]

let reset_password () =
  [post_form
     ~a:[a_class ["box"]]
     ~service:Services.reset_password
     (fun email_name -> [
          h1 [pcdata "Adresse mail associée au compte"];
          p [
            Templates_common.string_input_box
              ~a:[a_id "new_email"]
              ~input_type:`Text
              ~name:email_name
              ();
            br ();
            Templates_common.submit_input ~value:"Valider" ()
          ]
        ])
     ()
  ]
