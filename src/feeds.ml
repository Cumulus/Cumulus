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
module UTF8 = CamomileLibraryDefault.Camomile.CaseMap.Make(CamomileLibrary.UTF8)

type append_state = Ok | Not_connected | Empty | Already_exist | Invalid_url

let (>>=) = Lwt.(>>=)

let feed_of_db (feed, tags, votes) =
  let tags =
    List.map
      (fun elm -> elm#!tag)
      (List.filter (fun elm -> elm#!id_feed = feed#!id) tags)
  in
  let votes =
    List.fold_left
      (fun acc elm -> acc + Int32.to_int elm#!score)
      0
      (List.filter (fun elm -> elm#!id_feed = feed#!id) votes)
  in
  Lwt.return (Feed.feed_new feed tags votes)

let feeds_of_db (feeds, tags, votes) =
  Lwt_list.map_s (fun x -> feed_of_db (x, tags, votes)) feeds

let to_somthing f data =
  Lwt_list.map_p (fun feed -> f feed) data

let private_to_html data =
  to_somthing
    (fun feed ->
      Feed.to_html feed >>= (fun elm ->
        Lwt.return (Html.div ~a: [Html.a_class ["line post"]] elm)
      )
    ) data

let comments_to_html id =
  Db_feed.get_feed_with_id id
  >>= feed_of_db
  >>= fun root ->
  Db_feed.get_comments id
  >>= feeds_of_db
  >>= fun comments ->
  let result = Comments.tree_comments [Comments.Sheet root] comments
  in match result with
    | Some tree -> Comments.to_html tree
    | None -> Comments.to_html (Comments.Sheet root)

let branch_to_html id =
  Db_feed.get_feed_with_id id
  >>= feed_of_db
  >>= fun sheet ->
  match sheet.Feed.root with
    | None -> Comments.to_html (Comments.Sheet sheet)
    | Some id ->
        Db_feed.get_feed_with_id id
        >>= feed_of_db
        >>= fun root ->
        Db_feed.get_comments id
        >>= feeds_of_db
        >>= fun comments ->
        let tree =
          Comments.branch_comments (Comments.Sheet sheet) (root :: comments)
        in
        Comments.to_html tree

let to_html feeds = feeds_of_db feeds >>= private_to_html

let feed_id_to_html id =
  Db_feed.get_feed_with_id id
  >>= feed_of_db
  >>= fun feed ->
  private_to_html [feed]

let tree_to_atom id () =
  Db_feed.get_tree_feeds id ~starting:0l ~number:Utils.offset ()
  >>= feeds_of_db
  >>= to_somthing Feed.to_atom
  >>= (fun tmp ->
    Lwt.return (
      Atom_feed.feed
        ~updated: (Calendar.make 2012 6 9 17 40 30)
        ~id:"http://cumulus.org"
        ~title: (Atom_feed.plain ("Cumulus (id: " ^ Int32.to_string id ^ ")"))
        tmp
    )
  )

(* FIXME? should atom feed return only a limited number of links ? *)
let to_atom () =
  Db_feed.get_links_feeds ~starting:0l ~number:Utils.offset ()
  >>= feeds_of_db
  >>= to_somthing Feed.to_atom
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
  Db_feed.get_comments_feeds ~starting:0l ~number:Utils.offset ()
  >>= feeds_of_db
  >>= to_somthing Feed.to_atom
  >>= (fun tmp ->
    Lwt.return (
      Atom_feed.feed
        ~updated: (Calendar.make 2012 6 9 17 40 30)
        ~id:"http://cumulus.org"
        ~title: (Atom_feed.plain "Cumulus")
        tmp
    )
  )

let (event, call_event) =
  let (private_event, call_event) = React.E.create () in
  let event = Eliom_react.Down.of_react private_event in
  (event, call_event)

let strip_and_lowercase x =
  (* (List.map (fun x -> String.lowercase (Utils.strip x)) (Str.split (Str.regexp "[,]+") tags)) *)
  UTF8.lowercase (Utils.strip x)

let updating_and_ret () =
  call_event ();
  Lwt.return Ok

let append_feed_aux_base ~description f =
  User.get_userid () >>= function
    | None -> Lwt.return Not_connected
    | Some author ->
        if Utils.string_is_empty description
        then Lwt.return Empty
        else f ~author ()

let append_feed_aux ~url ~description ~tags f =
  append_feed_aux_base ~description
    (fun ~author () ->
      if Utils.string_is_empty tags then
        Lwt.return Empty
      else if Utils.is_invalid_url url then
        Lwt.return Invalid_url
      else
        Db_feed.get_feed_url_with_url url >>= function
          | Some _ -> Lwt.return Already_exist
          | None -> f ~author () >>= updating_and_ret
    )

let append_feed (url, (description, tags)) =
  append_feed_aux ~url ~description ~tags
    (fun ~author () ->
      Db_feed.add_feed
        ~url
        ~description
        ~tags:(List.map strip_and_lowercase (Utils.split tags))
        ~userid:author
        ()
    )

let get_root_and_parent id =
  Db_feed.get_feed_with_id (Int32.of_int id) >>= fun (feed, _, _) ->
  let parent = feed#!id in
  let root = match feed#?root with
    | Some root -> root
    | None -> parent
  in
  Lwt.return (Db_feed.add_feed ~root ~parent)

let append_link_comment (id, (url, (description, tags))) =
  append_feed_aux ~url ~description ~tags
    (fun ~author () ->
      get_root_and_parent id >>= fun f ->
      f ~url
        ~description
        ~tags:(List.map strip_and_lowercase (Utils.split tags))
        ~userid:author
        ()
    )

let append_desc_comment (id, description) =
  append_feed_aux_base ~description
    (fun ~author () ->
      get_root_and_parent id >>= fun f ->
      f ~description
        ~tags:[]
        ~userid:author
        ()
      >>= updating_and_ret
    )

let edit_feed_aux ~id ~url ~description ~tags f =
  append_feed_aux_base ~description
    (fun ~author () ->
      if Utils.string_is_empty tags then
        Lwt.return Empty
      else if Utils.is_invalid_url url then
        Lwt.return Invalid_url
      else
        Db_feed.get_feed_with_id (Int32.of_int id) >>= (fun (feed, _, _) ->
	  if feed#?url <> Some url then
            Db_feed.get_feed_url_with_url url >>= function
            | Some _ -> Lwt.return Already_exist
            | None -> f () >>= updating_and_ret
	  else
	    f () >>= updating_and_ret)
    )

let edit_link_comment (id, (url, (description, tags))) =
  edit_feed_aux ~id ~url ~description ~tags
    (fun () ->
      Db_feed.update
	~feedid:(Int32.of_int id)
	~url:(Some url)
        ~description
        ~tags:(List.map strip_and_lowercase (Utils.split tags))
        ()
    )

let edit_desc_comment (id, description) =
  append_feed_aux_base ~description
    (fun ~author () ->
      Db_feed.update
	~feedid:(Int32.of_int id)
	~description
        ~tags:[]
	~url:None
        ()
      >>= updating_and_ret
    )
