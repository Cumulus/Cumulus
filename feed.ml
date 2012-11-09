module Calendar = CalendarLib.Calendar

type feed = {
  id : int32;
  url : string option;
  description : string;
  date : Calendar.t;
  author : int32;
  parent : int32 option;
  tags: string list
}

type tree = Sheet of feed | Node of feed * tree list 

let feed_new data tags = {
  id = data#!id;
  url = data#?url;
  description = data#!description;
  date = data#!timedate;
  author = data#!author;
  parent = data#?parent;
  tags = tags
}

let rec parent_in_tree_feed id = function
  | Sheet elm -> elm.id = id
  | Node (elm, childs) -> let l = List.map (fun x -> parent_in_tree_feed id x) childs in 
                          elm.id = id || (List.fold_left (fun x -> fun y -> x || y) false l)

let rec append_tree tree id = function
  | Sheet elm -> begin match id = elm.id with
                   | true -> Node (elm, [tree])
                   | false -> Sheet elm end
  | Node (elm, childs) -> match id = elm.id with
                            | true -> Node (elm, tree :: childs)
                            | false -> let childs = List.map (fun x -> append_tree tree id x) childs in 
                                       Node (elm, childs)

let string_of_tree root =
  let buffer = Buffer.create 16 in
  let rec aux = function
    | Sheet feed -> Buffer.add_string buffer ("[" ^ (string_of_int (Int32.to_int feed.id)) ^ "]" ^ feed.description)
    | Node (elm, childs) -> Buffer.add_string buffer ("([" ^ (string_of_int (Int32.to_int elm.id)) ^ "]" ^ elm.description);
                            List.map (fun x -> Buffer.add_char buffer ' '; aux x) childs;
                            Buffer.add_char buffer ')'
  in aux root; Buffer.contents buffer

let rec generate_tree_comments stack comments =
  let get = function
    | Sheet elm -> begin match elm.parent with
                     | None -> 0l (* OH LE GROS HACK *)
                     | Some n -> n end
    | Node (elm, _) -> match elm.parent with
                        | None -> 0l (* OH LE GROS HACK *)
                        | Some n -> n
  in let rec scan tree stack acc = match stack with
    | [] -> tree :: acc
    | x :: r -> if parent_in_tree_feed (get tree) x
                then (append_tree tree (get tree) x) :: r
                else scan tree r (x :: acc)
  in match comments with
    | [] -> begin match stack with
              | [] -> []
              | [ x ] -> [ x ]
              | x :: r -> scan x r [] end
    | x :: r -> generate_tree_comments (scan (Sheet x) stack []) r

let links_of_tags tags =
  List.fold_left (fun acc tag ->
    let link = Html.a ~a: [Html.a_class ["tags"]] ~service: Services.tag_feed [Html.pcdata tag] (None, tag) in
    acc @ [Html.pcdata " "; link]
  ) [] tags

let to_html self =
  let content = match self.url with
    | Some l -> let url_service =
                  Eliom_service.external_service
                    l []
                    Eliom_parameter.unit () in
                Html.a ~a: [Html.a_class ["postitle"]] ~service: url_service
                  [Html.pcdata self.description] ()
    | None -> Html.pcdata self.description in
  Db.get_user_name_and_email_with_id self.author >>= fun author ->
  User.get_userid () >>= (function
    | None -> Lwt.return false
    | Some userid ->
        Db.is_feed_author self.id userid
        >>= fun x ->
        User.is_admin ()
        >>= fun y ->
        Lwt.return (x || y)
  )
  >>= fun is_author ->
  Db.count_comments self.id >>= fun comments ->
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
       Html.a Services.author_feed [Html.pcdata (author#!name)] (None, author#!name);
      ];
      [Html.br ();
       (* TODO : afficher "n commentaire(s)" *)
       Html.a
         Services.view_feed
         (let n = Int64.to_int comments#!n
         in match n with
           | 0
           | 1 -> [Html.pcdata ((string_of_int n) ^ " commentaire ")]
           | n -> [Html.pcdata ((string_of_int n) ^ " commentaires ")])
         (* [Html.pcdata (string_to_int (Int64.to_int comments)) " commentaires "] *)
         (* url_of_title or url_of_desc ? *)
         (Int32.to_int self.id, Utils.strip self.description);
       Html.pcdata "Tags: "
      ];
      links_of_tags self.tags;
      (if is_author then
          [Html.a Services.delete_feed [Html.pcdata " (supprimer ?)"] self.id]
       else []
      );
    ]
  )

let rec html_from_tree = function
  | Sheet feed -> to_html feed >>= (fun elm -> Lwt.return (Html.div ~a: [Html.a_class ["line post"]] elm))
  | Node (feed, childs) -> to_html feed >>= (fun elm ->
                           Lwt_util.map html_from_tree childs >>= (fun childs ->
                              Lwt.return (Html.div ~a: [Html.a_class ["line post"]] (elm @ childs))
                           ))

let to_atom self =
  Db.get_user_name_and_email_with_id self.author >>= fun author ->
  Lwt.return (match self.url with
    | Some l -> Atom_feed.entry
                  ~updated: self.date
                  ~id: (Html.Xml.uri_of_string l)
                  ~title: (Atom_feed.plain self.description) (* title is description ? *)
                [Atom_feed.authors [Atom_feed.author author#!name]]
    | None -> Atom_feed.entry
                  ~updated: self.date
                  ~id: (Html.Xml.uri_of_string (Int32.to_string self.id))
                  ~title: (Atom_feed.plain self.description) (* title is description ? *)
                [Atom_feed.authors [Atom_feed.author author#!name]]
  )
