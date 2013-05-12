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

module Option = Eliom_lib.Option

let (>>=) = Lwt.(>>=)

class type feed = object
  method author : (Sql.int32_t, Sql.non_nullable) Db.t
  method id : (Sql.int32_t, Sql.non_nullable) Db.t
  method timedate : (Sql.timestamp_t, Sql.non_nullable) Db.t
  method description : (Sql.string_t, Sql.non_nullable) Db.t
  method url : (Sql.string_t, Sql.nullable) Db.t
  method parent : (Sql.int32_t, Sql.nullable) Db.t
  method root : (Sql.int32_t, Sql.nullable) Db.t
end

class type tag = object
  method tag : (Sql.string_t, Sql.non_nullable) Db.t
  method id_feed : (Sql.int32_t, Sql.non_nullable) Db.t
end


class type fav = object
  (* method id : (Sql.int32_t, Sql.non_nullable) Db.t *)
  method id_user : (Sql.int32_t, Sql.non_nullable) Db.t
  method id_feed : (Sql.int32_t, Sql.non_nullable) Db.t
end


type feeds_and_tags = feed list * tag list

type feed_generator =
    starting:int32 ->
    number:int32 ->
    unit ->
    feeds_and_tags Lwt.t

let feeds_id_seq = (<:sequence< serial "feeds_id_seq" >>)

let feeds = (<:table< feeds (
  id integer NOT NULL DEFAULT(nextval $feeds_id_seq$),
  url text,
  description text NOT NULL,
  timedate timestamp NOT NULL DEFAULT(current_timestamp),
  author integer NOT NULL,
  parent integer,
  root integer
) >>)

let feeds_tags_id_seq = (<:sequence< serial "feeds_tags_id_seq" >>)

let feeds_tags = (<:table< feeds_tags (
  id integer NOT NULL DEFAULT(nextval $feeds_tags_id_seq$),
  tag text NOT NULL,
  id_feed integer NOT NULL
) >>)

(* let favs_id_seq = (<:sequence< serial "favs_id_seq" >>) *)

let favs = (<:table< favs (
  (* id integer NOT NULL DEFAULT(nextval $favs_id_seq$), *)
  id_user integer NOT NULL,
  id_feed integer NOT NULL
) >>)

let filter_tags_id f tags =
  (<:value< $Db.in'$ f.id $List.map (fun x -> x#id_feed) tags$ >>)
let filter_feeds_id t feeds =
  (<:value< $Db.in'$ t.id_feed $List.map (fun x -> x#id) feeds$ >>)

let get_id_feed_from_tag tag =
  Db.view
    (<:view< {
      t.id_feed;
     } | t in $feeds_tags$;
    t.tag = $string:tag$;
    >>)

let get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter () =
  Db.view
    (<:view< {
      f.id;
      f.url;
      f.description;
      f.timedate;
      f.author;
      f.parent;
      f.root;
     } order by f.id desc
        limit $int32:number$
        offset $int32:starting$ |
            f in $feeds$;
    $feeds_filter$ f;
    >>)
  >>= fun feeds ->
  Db.view
    (<:view< {
      t.tag;
      t.id_feed;
    } | t in $feeds_tags$;
    $tags_filter feeds$ t;
    >>)
  >>= fun tags ->
  Lwt.return (feeds, tags)

let get_tree_feeds feed_id ~starting ~number () =
  let feeds_filter f = (<:value< f.root = $int32:feed_id$ || f.parent = $int32:feed_id$ >>) in
  let tags_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ()

let get_links_feeds ~starting ~number () =
  let feeds_filter f = (<:value< is_not_null f.url >>) in
  let tags_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ()

let get_root_feeds ~starting ~number () =
  let feeds_filter f = (<:value< is_null f.root || is_null f.parent >>) in
  let tags_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ()

let get_feeds ~starting ~number () =
  let feeds_filter _ = (<:value< true >>) in
  let tags_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ()

let count_feeds_aux ~filter () =
  Db.view_one
    (<:view< group {
      n = count[f];
     } | f in $feeds$;
    $filter$ f;
    >>)

let count_feeds () =
  let filter _ = (<:value< true >>) in
  count_feeds_aux ~filter ()

let count_root_feeds () =
  let filter f = (<:value< is_null f.root || is_null f.parent >>) in
  count_feeds_aux ~filter ()


let get_feeds_with_author author ~starting ~number () =
  Db_user.get_user_id_with_name author >>= fun author ->
  let feeds_filter f = (<:value< f.author = $int32:author#!id$ >>) in
  let tags_filter feeds t = filter_feeds_id t feeds in
  get_feeds_aux ?starting ?number ~feeds_filter ~tags_filter ()

let count_feeds_with_author author =
  Db_user.get_user_id_with_name author >>= fun author ->
  let filter f = (<:value< f.author = $int32:author#!id$ >>) in
  count_feeds_aux ~filter ()

let get_feeds_with_tag tag ~starting ~number () =
  get_id_feed_from_tag tag >>= fun tags ->
  let feeds_filter f = filter_tags_id f tags in
  let tags_filter feeds t = filter_feeds_id t feeds in
  get_feeds_aux ?starting ?number ~feeds_filter ~tags_filter ()

let count_feeds_with_tag tag =
  get_id_feed_from_tag tag >>= fun tags ->
  let filter f = filter_tags_id f tags in
  count_feeds_aux ~filter ()

let get_feed_url_with_url url =
  Db.view_opt
    (<:view< {
      f.url
    } | f in $feeds$;
    f.url = $string:url$;
    >>)

let get_feed_with_url url =
  Db.view_opt
    (<:view< {
      f.id;
      f.url;
      f.description;
      f.timedate;
      f.author;
      f.parent;
      f.root;
    } | f in $feeds$;
    f.url = $string:url$;
    >>)

let get_feed_with_id id =
  Db.view_one
    (<:view< {
      f.id;
      f.url;
      f.description;
      f.timedate;
      f.author;
      f.parent;
      f.root;
     } | f in $feeds$;
    f.id = $int32:id$;
    >>)
  >>= fun feeds ->
  Db.view
    (<:view< {
      t.tag;
      t.id_feed;
     } | t in $feeds_tags$;
    t.id_feed = $int32:id$;
    >>)
  >>= fun tags ->
  Lwt.return (feeds, tags)

let count_comments root =
  let filter f = (<:value< f.root = $int32:root$ || f.parent = $int32:root$ >>) in
  count_feeds_aux ~filter ()

let add_feed ?root ?parent ?url ~description ~tags ~userid () =
  Db.value (<:value< feeds?id >>)
  >>= fun id_feed ->
  let feed =
    Db.query
      (<:insert< $feeds$ := {
        id = $int32:id_feed$;
        url = of_option $Option.map Sql.Value.string url$;
        description = $string:description$;
        timedate = feeds?timedate;
        author = $int32:userid$;
        parent = of_option $Option.map Sql.Value.int32 parent$;
        root = of_option $Option.map Sql.Value.int32 root$;
      } >>)
  and tag =
    Lwt_list.iter_p
      (fun tag ->
        Db.query
          (<:insert< $feeds_tags$ := {
            id = feeds_tags?id;
            tag = $string:tag$;
            id_feed = $int32:id_feed$;
          } >>)
      )
      tags
  in
  Lwt.join [feed; tag]

let is_feed_author ~feed ~userid () =
  try_lwt begin
    Db.view_one
      (<:view< f | f in $feeds$;
      f.id = $int32:feed$;
      f.author = $int32:userid$;
      >>)
    >>= fun _ ->
    Lwt.return true
  end
  with exn ->
    Ocsigen_messages.debug (fun () -> Printexc.to_string exn);
    Lwt.return false

let get_comments root =
  Db.view
    (<:view< {
      f.id;
      f.url;
      f.description;
      f.timedate;
      f.author;
      f.parent;
      f.root;
     } order by f.timedate desc |
        f in $feeds$;
    f.root = $int32:root$ || f.parent = $int32:root$;
    >>)
  >>= fun feeds ->
  Db.view
    (<:view< {
      t.tag;
      t.id_feed;
    } | t in $feeds_tags$;
    >>)
  >>= fun tags ->
  Lwt.return (feeds, tags)

let list_of_depend_feed id =
  let get_feeds_root_without_id root id =
    Db.view
      (<:view< {
        f.id;
        f.url;
        f.description;
        f.timedate;
        f.author;
        f.parent;
        f.root;
       } | f in $feeds$;
      f.root = $int32:root$; f.id <> $int32:id$;
      >>)
  in
  let rec aux root comments =
    let get = function
      | None -> 0l
      | Some n -> n
    in match comments with
      | [] -> [ root ]
      | l -> let childs = List.filter (fun x -> (get x#?parent) = root#!id) l in
             let others = List.filter (fun x -> (get x#?parent) <> root#!id) l in
             if 0 = List.length childs
             then [ root ]
             else (root) :: (List.flatten (List.map (fun x -> aux x others) childs))
  in
  get_feed_with_id id
  >>= fun (root, _) -> match root#?root with
    | None -> Lwt.return [ root ]
    | Some rootid -> get_feeds_root_without_id rootid (root#!id)
                     >>= fun comments ->
                     Lwt.return (aux root comments)

let delete_feed ~feed ~userid () =
  list_of_depend_feed feed
  >>= fun dfeeds ->
  let feeds_filter f =
    (<:value< $Db.in'$ f.id $List.map (fun x -> x#id) dfeeds$ >>) in
  Db.query
    (<:delete< f in $feeds$ | $feeds_filter$ f; >>)
  >>= fun () ->
  let feeds_filter f =
    (<:value< $Db.in'$ f.id_feed $List.map (fun x -> x#id) dfeeds$ >>) in
  Db.query
    (<:delete< f in $feeds_tags$ | $feeds_filter$ f >>)

let get_fav_aux ~starting ~number ~feeds_filter ~tags_filter () =
  Db.view
    (<:view< {
      (* f.id; *)
      f.id_user;
      f.id_feed;
     } order by f.id_feed desc
        limit $int32:number$
        offset $int32:starting$ |
            f in $favs$;
    $feeds_filter$ f;
    >>)
  >>= fun favs ->
  let feeds_filter f =
    (<:value< $Db.in'$ f.id $List.map (fun x -> x#id_feed) favs$ >>) in
  get_feeds_aux ?starting ?number ~feeds_filter ~tags_filter ()

let count_fav_aux ~filter () =
  Db.view_one
    (<:view< group {
      n = count[f];
     } | f in $favs$;
    $filter$ f;
    >>)

let get_fav_with_username name ~starting ~number () =
  Db_user.get_user_id_with_name name >>= fun author ->
  let feeds_filter f = (<:value< f.id_user = $int32:author#!id$ >>) in
  let tags_filter favs t = filter_feeds_id t favs in
  get_fav_aux ?starting ?number ~feeds_filter ~tags_filter ()

let count_fav_with_username name =
  Db_user.get_user_id_with_name name >>= fun author ->
  let filter f = (<:value< f.id_user = $int32:author#!id$ >>) in
  count_fav_aux ~filter ()

let is_url ~feedid () =
  Db.view_opt
    (<:view< {
      f.url;
    } | f in $feeds$;
    f.id = $int32:feedid$;
    >>) >>= function
      | Some d -> Lwt.return (if d#?url <> None then true else false)
      | None -> Lwt.return false

let is_root ~feedid () =
  Db.view_opt
    (<:view< {
      f.root;
    } | f in $feeds$;
    f.id = $int32:feedid$;
    >>) >>= function
    | Some feed -> Lwt.return (if feed#?root <> None then false else true)
    | None -> Lwt.return false

let get_root ~feedid () =
  Db.view_opt
    (<:view< {
      f.root;
    } | f in $feeds$;
    f.id = $int32:feedid$;
    >>) >>= function
    | Some feed -> (match feed#?root with
      | Some id -> Db.view_opt
        (<:view< {
          f.id;
          f.url;
          f.description;
          f.timedate;
          f.author;
          f.parent;
          f.root;
        } | f in $feeds$;
        f.id = $int32:id$;
        >>)
      | None -> Lwt.return None)
    | None -> Lwt.return None


let add_fav ~feedid ~userid () =
  Db.view_opt
    (<:view< {
      f.id_user;
      f.id_feed;
    } | f in $favs$;
    f.id_user = $int32:userid$ && f.id_feed = $int32:feedid$;
    >>) >>= function
      | Some _ -> Lwt.return ()
      | None ->
          Db.query
            (<:insert< $favs$ := {
      (* id; *)
              id_user = $int32:userid$;
              id_feed = $int32:feedid$;
            } >>)

let del_fav ~feedid ~userid () =
  Db.view_opt
    (<:view< {
      f.id_user;
      f.id_feed;
    } | f in $favs$;
    f.id_user = $int32:userid$ && f.id_feed = $int32:feedid$;
    >>) >>= function
      | None -> Lwt.return ()
      | Some _ ->
          Db.query
            (<:delete< f in $favs$ | f.id_feed = $int32:feedid$ && f.id_user = $int32:userid$; >>)

let is_fav ~feedid ~userid () =
  try_lwt begin
    Db.view_one
      (<:view< {
        f.id_user;
        f.id_feed;
      } | f in $favs$; f.id_feed = $int32:feedid$ && f.id_user = $int32:userid$; >>)
    >>= fun _ ->
    Lwt.return true
  end
  with exn ->
    Ocsigen_messages.debug (fun () -> Printexc.to_string exn);
    Lwt.return false

(* Il faut delete tous les tags du lien et ajouter les nouveaux *)
let update ~feedid ~url ~description ~tags () =
  match url with
  | Some u ->
    (Db.query
      (<:update< f in $feeds$ := {
        description = $string:description$;
        url = $string:u$;
      } | f.id = $int32:feedid$; >>)
     >>= fun _ ->
     Db.query
       (<:delete< t in $feeds_tags$ | t.id_feed = $int32:feedid$ >>)
     >>= fun _ ->
     Lwt_list.iter_p
       (fun tag ->
	 Db.query
           (<:insert< $feeds_tags$ := {
             id = feeds_tags?id;
             tag = $string:tag$;
             id_feed = $int32:feedid$;
           } >>)
       )
       tags
    )
  | None ->
    Db.query
      (<:update< f in $feeds$ := {
        description = $string:description$;
      } | f.id = $int32:feedid$; >>)
    
let exist ~feedid () =
  Db.view_opt
    (<:view< {
      f.id;
    } | f in $feeds$;
    f.id = $int32:feedid$;
    >>) >>= function
      | None -> Lwt.return false
      | Some _ -> Lwt.return true
