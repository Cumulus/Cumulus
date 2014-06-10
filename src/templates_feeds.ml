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
open Eliom_content.Html5.F

let action_to_html ~user self =
  if Option.is_some user then
    [ pcdata " - ";
      a
        ~service:Services.comment
        [ span
            ~a:[a_class ["line_author_link"]]
            [pcdata "Commenter"]
        ]
        (self.Feed.id, Utils.troncate self.Feed.description);
    ]
  else
    []

let feed_to_html ?(padding=5) ?(is_child=false) ~user self =
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
        let render_pre ~kind:_ s = Raw.pre [Raw.pcdata s] in
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
  let fav_div =
    let add =
      Eliom_content.Html5.D.Raw.a
        ~a:[a_class ["link"]]
        [get_image ["circled";"gray";"fav_icon"] "fav.png"]
    in
    let del =
      Eliom_content.Html5.D.Raw.a
        ~a:[a_class ["link"]]
        [get_image ["circled";"highlighted";"deletable"] "fav.png"]
    in
    let is_fav = self.Feed.fav in
    let res =
      Eliom_content.Html5.D.div
        ~a:[a_class ["fav_wrap"]]
        [if self.Feed.fav then del else add]
    in
    let feed_id = self.Feed.id in
    Client.fav_actions ~is_fav ~res ~del ~add ~feed_id;
    res
  in
  let upvotes =
    let link name =
      Eliom_content.Html5.D.Raw.a ~a:[a_class ["link"]] [get_image [] name]
    in
    let upon = link "upon.png" in
    let up = link "up.png" in
    let downon = link "downon.png" in
    let down = link "down.png" in
    let vote = self.Feed.vote in
    let container =
      Eliom_content.Html5.D.div
        ~a:[a_class ["upvote_wrap"]]
        [Client.get_upvote_inner ~upon ~up ~downon ~down ~vote ~score:self.Feed.score]
    in
    let feed_id = self.Feed.id in
    Client.upvotes_actions ~container ~upon ~up ~downon ~down ~vote ~feed_id;
    container
  in
  List.flatten
    [
      [
        aside ~a: [a_class ["row";"post";"mod"]; a_id "post"; a_style ("margin-left: " ^ (string_of_int padding) ^ "px;")] [
          aside ~a: [a_class["col"; if is_child then "little" else ""; "avatarbox"]]
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
                ~a:[a_class ["line_author_link"]]
                ~service:Services.author_feed
                [pcdata self.Feed.user#name]
                self.Feed.user#name;
              pcdata " | ";
              a
                ~service:Services.atom_feed
                [span ~a:[a_class ["line_author_link"]]
                   [pcdata "Flux Atom du lien"]
                ]
                self.Feed.id;
            ]
              @
              (if is_author then
                 [
                   pcdata " - ";
                   a ~service:Services.delete_feed
                     [span ~a:[a_class ["line_author_link"]]
                        [pcdata "Supprimer"]
                     ]
                     self.Feed.id;
                   pcdata " - ";
                   a ~service:Services.edit_feed
                     [span ~a:[a_class ["line_author_link"]]
                        [pcdata "Éditer"]
                     ]
                     (self.Feed.id, Utils.troncate self.Feed.description);
                 ]
               else []
              ) @ (action_to_html ~user self));
            content;
            tags;
          ];
          div ~a: [a_class["col"; if is_child then "little" else ""; "post_int"]][


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
            fav_div;
            upvotes;
          ]
        ]
      ]
    ]

let rec comments_to_html' ?(padding=5) ?(is_child=false) ~user tree =
  match tree with
  | Comments.Sheet feed ->
      let elm = feed_to_html ~padding ~is_child ~user feed in
      div ~a: [a_class ["line"]] elm
  | Comments.Node (feed, childs) ->
      let elm = feed_to_html ~padding ~is_child ~user feed in
      let childs = List.map (comments_to_html' ~padding:(padding + 75) ~is_child:true ~user) childs in
      div ~a: [a_class ["line"]] (elm @ childs)

let to_html ~user data =
  List.map
    (fun feed ->
       let elm = feed_to_html ~user feed in
       section ~a: [a_class["line"]] elm
    )
    data

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
                 user.User.name];

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
          [ a
              ~service:Services.main
              [ img
                  ~alt:("Cumulus Project")
                  ~src:(
                    make_uri
                      ~service: (Eliom_service.static_dir ())
                      ["CUMULUS_Logo.svg"]
                  )
                  ();
              ]
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
                      ~a:[a_placeholder "Tags (séparés par des virgules)"]
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

let main_style ~user ~error ~server_function ~page_title content =
  let userbox = userbox user in
  let header = header () in
  let base_error_frame =
    Eliom_content.Html5.D.div ~a:[a_class ["msghandler"]]
  in
  let error_frame =
    match error with
    | Some error ->
        let error_frame =
          base_error_frame [p [pcdata error]]
        in
        Client.display_error ~error_frame;
        error_frame
    | None -> base_error_frame []
  in
  let content =
    Eliom_content.Html5.D.aside ~a:[a_class ["col"; "w80"]] content
  in
  server_function ~box:content;
  html
    (head
       (title
          (pcdata
             (match page_title with
              | None -> "Cumulus"
              | Some t -> "Cumulus — " ^ t
             )
          )
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
       [ noscript [pcdata "Your browser doesn't support javascript. Please pick one which does."];
         div
           ~a: [a_class ["line"]]
           (header
            @ [ error_frame;
                content;
                userbox;
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

let private_register () =
  [div
     ~a:[a_class ["simple-page"]]
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
  ]

let private_preferences ~user =
  match user with
   | None ->
       [div
          ~a:[a_class ["box"]]
          [pcdata "Veuillez vous connecter pour accéder aux préférences."]
       ]
   | Some user ->
       [div
          ~a:[a_class ["simple-page"]]
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
                       ~value:user.User.email
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
                   let nb_feeds = user.User.feeds_per_page in
                   p [
                     int32_input
                       ~a:[a_class ["input-box"];
                           a_placeholder (Int32.to_string nb_feeds)
                          ]
                       ~input_type:`Text
                       ~name:nb_feeds_name
                       ~value:nb_feeds
                       ();
                     br ();
                     Templates_common.submit_input ~value:"Valider" ()
                   ]
                 ])
              ()
          ]
       ]

let private_comment ~user id branch =
  let branch = comments_to_html' ~user branch in
  if Option.is_none user then
    [div
       ~a:[a_class ["box"]]
       [pcdata "Veuillez vous connecter pour poster un commentaire."]
    ]
  else
    [div
       ~a:[a_class [""]]
        [ branch;
          post_form
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
                      ~a:[a_placeholder "Tags (séparés par des virgules)"]
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
             id;
          post_form
            ~a:[a_class ["box"]]
            ~service:Services.append_desc_comment
            (fun (parent, desc) -> [
                 h1 [pcdata "Commentaire"];
                 p [
                   textarea
                     ~a:[a_class ["input-box";"comment-input"];
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
            id;
        ]
    ]

let private_edit_feed ~user ~feed (edit_desc, edit_url, edit_tags) =
  let is_author = Feed.is_author ~feed user in
  if not is_author then
    [div
       ~a:[a_class ["box"]]
       [pcdata "Vous n'avez pas le droit d'editer ce lien."]
    ]
  else
    [div
       ~a:[a_class ["simple-page"]]
       [match edit_url with
        | Some edit_url ->
            post_form
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
                       ~a:[ a_placeholder "Tags (séparés par des virgules)" ]
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
              feed.Feed.id
        | None ->
            post_form
              ~a:[a_class ["box"]]
              ~service:Services.edit_desc_comment
              (fun (parent, desc) -> [
                   h1 [pcdata "Commentaire"];
                   p [
                     textarea
                       ~a:[a_class ["input-box"; "comment-input"];
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
              feed.Feed.id
       ]
    ]

let error_content msg = [div ~a:[a_class ["box"]] [pcdata msg]]

let reset_password () =
  [div
     ~a:[a_class ["simple-page"]]
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
  ]
