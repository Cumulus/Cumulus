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

type feed =
  { author : int32
  ; id : int32
  ; date : CalendarLib.Calendar.t
  ; description : string
  ; url : string option
  ; parent: int32 option
  ; root : int32 option
  ; tags : string list
  ; score : int
  ; user : < email_digest : string; name: string >
  ; fav : bool
  ; vote : int
  ; count : int
  }

type feeds = feed list

type feed_generator =
  starting:int32 ->
  number:int32 ->
  user:int32 option ->
  unit ->
  (feeds * int64) Lwt.t

let get_feeds_aux ?range
  ~feeds_filter
  ~users_filter
  ~user
  () =
  begin match range with
  | Some (limit, offset) ->
      Db.view
        (<:view< group {
          email_digest = md5[u.email];
        }
        by {
          f.id;
          f.url;
          f.description;
          f.timedate;
          f.author;
          f.parent;
          f.root;
          u.name;
          u.email;
        } order by f.id desc limit $int32:limit$ offset $int32:offset$
        | f in $Db_table.feeds$; $feeds_filter$ f;
          u in $Db_table.users$; $users_filter$ f u;
          f.author = u.id;
        >>)
  | None ->
      Db.view
        (<:view< group {
          email_digest = md5[u.email];
        }
        by {
          f.id;
          f.url;
          f.description;
          f.timedate;
          f.author;
          f.parent;
          f.root;
          u.name;
          u.email;
        } order by f.id desc
        | f in $Db_table.feeds$; $feeds_filter$ f;
          u in $Db_table.users$; $users_filter$ f u;
          f.author = u.id;
        >>)
  end
  >>= fun feeds ->
  let ids = List.map (fun x -> x#id) feeds in
  Db.view
    (<:view<
      group {
        tags = match array_agg[t.tag] with null -> $string_array:[||]$ | x -> x;
      }
      by { t.id_feed }
      | t in $Db_table.feeds_tags$;
      in' t.id_feed $ids$
    >>)
  >>= fun tags ->
  Db.view
    (<:view<
      group { c = count[f.root] } by { f.root }
      | f in $Db_table.feeds$;
      (match f.root with
       | null -> false
       | root -> in' root $ids$
      )
    >>)
  >>= fun count ->
  Db.view
    (<:view< {
            f.id_feed;
            f.id_user;
            f.score;
            } | f in $Db_table.votes$;
            in' f.id_feed $ids$
     >>)
  >>= fun votes ->
  begin match user with
    | Some user_id ->
        Db.view
          (<:view< {
                  f.id_feed;
                  } | f in $Db_table.favs$;
                  f.id_user = $int32:user_id$; in' f.id_feed $ids$
           >>)
        >|= fun favs ->
        (favs, List.filter (fun x -> Int32.equal x#!id_user user_id) votes)
    | None ->
      Lwt.return ([], [])
  end
  >>= fun (favs, user_votes) ->
  Db.view_one
    (<:view< group {
            n = count[f];
            } | f in $Db_table.feeds$;
            $feeds_filter$ f;
     >>)
  >>= fun total_count ->
  let favs = List.map (fun x -> x#!id_feed) favs in
  let new_object o =
    let id = o#!id in
    { author = o#!author
    ; id
    ; date = o#!timedate
    ; description = o#!description
    ; url = o#?url
    ; parent = o#?parent
    ; root = o#?root
    ; tags = Option.map_default (fun x -> Array.to_list x#!tags) [] (List.Exceptionless.find (fun x -> Int32.equal x#!id_feed id) tags)
    ; score = List.fold_left (fun acc x -> if Int32.equal x#!id_feed id then acc + Int32.to_int x#!score else acc) 0 votes
    ; user = object method name = o#!name method email_digest = o#!email_digest end
    ; fav = List.exists (Int32.equal id) favs
    ; vote = Option.map_default (fun x -> Int32.to_int x#!score) 0 (List.Exceptionless.find (fun x -> Int32.equal x#!id_feed id) user_votes)
    ; count =
      try Int64.to_int (List.find (fun e -> match e#?root with Some x -> Int32.equal x o#!id | None -> false) count)#!c
      with _ -> 0
    }
  in
  Lwt.return (List.map new_object feeds, total_count#!n)

let get_tree_feeds feed_id ~starting ~number ~user () =
  let feeds_filter f = (<:value< f.root = $int32:feed_id$ >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~range:(number, starting) ~feeds_filter ~users_filter ~user ()

let get_links_feeds ~starting ~number ~user () =
  let feeds_filter f = (<:value< is_not_null f.url >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~range:(number, starting) ~feeds_filter ~users_filter ~user ()

let get_comments_feeds ~starting ~number ~user () =
  let feeds_filter f = (<:value< is_null f.url >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~range:(number, starting) ~feeds_filter ~users_filter ~user ()

let get_root_feeds ~starting ~number ~user () =
  let feeds_filter f = (<:value< is_null f.root || is_null f.parent >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~range:(number, starting) ~feeds_filter ~users_filter ~user ()

let get_feeds ~starting ~number ~user () =
  let feeds_filter _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~range:(number, starting) ~feeds_filter ~users_filter ~user ()

(* HIT *)

let get_feeds_with_author author ~starting ~number ~user () =
  let feeds_filter _ = (<:value< true >>) in
  let users_filter _ u = (<:value< u.name = $string:author$ >>) in
  get_feeds_aux ~range:(number, starting) ~feeds_filter ~users_filter ~user ()

let get_feeds_with_tag tag ~starting ~number ~user () =
  Db.view
    (<:view< {t.id_feed} | t in $Db_table.feeds_tags$; t.tag = $string:tag$; >>)
  >>= fun ids ->
  let feeds_filter f = (<:value< in' f.id $List.map (fun x -> x#id_feed) ids$ >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~range:(number, starting) ~feeds_filter ~users_filter ~user ()

let get_feed_with_url ~user url =
  let feeds_filter f = (<:value< f.url = $string:url$ >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~feeds_filter ~users_filter ~user ()
  >|= fst >|= (function [] -> None | x :: _ -> Some x)

let get_feed_with_id ~user id =
  let feeds_filter f = (<:value< f.id = $int32:id$ >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~feeds_filter ~users_filter ~user ()
  >|= fst >|= (function [x] -> x | _ -> assert false)

let get_comments ~user root =
  let feeds_filter f =
    (<:value< f.root = $int32:root$ || f.parent = $int32:root$ >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~feeds_filter ~users_filter ~user () >|= fst

let get_root ~feedid ~user () =
  let feeds_filter f = (<:value< f.id = $int32:feedid$ >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~feeds_filter ~users_filter ~user ()
  >|= fst >|= (function [] -> None | x :: _ -> Some x)


let is_feed_author ~feedid ~userid () =
  Lwt.catch
    (fun () ->
       Db.view_one
         (<:view< f | f in $Db_table.feeds$;
                 f.id = $int32:feedid$;
                 f.author = $int32:userid$;
          >>)
       >>= fun _ ->
       Lwt.return true
    )
    (fun exn ->
       Ocsigen_messages.debug (fun () -> Printexc.to_string exn);
       Lwt.return false
    )

let get_fav_aux ~starting ~number ~feeds_filter ~user () =
  Db.view
    (<:view< {
            f.id_feed;
            } order by f.id_feed desc
            | f in $Db_table.favs$;
            $feeds_filter$ f;
     >>)
  >>= fun favs ->
  let feeds_filter f =
    (<:value< in' f.id $List.map (fun x -> x#id_feed) favs$ >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~range:(number, starting) ~feeds_filter ~users_filter ~user ()

let get_fav_with_username name ~starting ~number ~user () =
  Db_user.get_user_id_with_name name >>= fun author ->
  let feeds_filter f = (<:value< f.id_user = $author$ >>) in
  get_fav_aux ~starting ~number ~feeds_filter ~user ()

let get_id_feed_from_tag tag =
  Db.view
    (<:view< {
            t.id_feed;
            } | t in $Db_table.feeds_tags$;
            t.tag = $string:tag$;
     >>)

(*
 * TODO: optimization (depend of Feeds.tree_to_atom)
 *)

let get_feed_url_with_url url =
  Db.view_opt
    (<:view< {
            f.url
            } | f in $Db_table.feeds$;
            f.url = $string:url$;
     >>)

let add_feed ?root ?parent ?url ~description ~tags ~userid () =
  Db.value (<:value< $Db_table.feeds$?id >>)
  >>= fun id_feed ->
  Db.query
    (<:insert< $Db_table.feeds$ := {
                id = $int32:id_feed$;
                url = of_option $Option.map Sql.Value.string url$;
                description = $string:description$;
                timedate = $Db_table.feeds$?timedate;
                author = $int32:userid$;
                parent = of_option $Option.map Sql.Value.int32 parent$;
                root = of_option $Option.map Sql.Value.int32 root$;
                } >>)
  >>= fun () ->
  Lwt_list.iter_p
    (fun tag ->
       Db.query
         (<:insert< $Db_table.feeds_tags$ := {
                   tag = $string:tag$;
                   id_feed = $int32:id_feed$;
                   } >>)
    )
    tags

let delete_feed ~feedid () =
  Db.query
    (<:delete< f in $Db_table.feeds$ | f.id = $int32:feedid$ >>)

let count_fav_aux ~filter () =
  Db.view_one
    (<:view< group {
            n = count[f];
            } | f in $Db_table.favs$;
            $filter$ f;
     >>)

let count_fav_with_username name =
  Db_user.get_user_id_with_name name >>= fun author ->
  let filter f = (<:value< f.id_user = $author$ >>) in
  count_fav_aux ~filter ()

let add_fav ~feedid ~userid () =
  Db.view_opt
    (<:view< {
            f.id_user;
            f.id_feed;
            } | f in $Db_table.favs$;
            f.id_user = $int32:userid$ && f.id_feed = $int32:feedid$;
     >>) >>= function
  | Some _ -> Lwt.return ()
  | None ->
      Db.query
        (<:insert< $Db_table.favs$ := {
                  (* id; *)
                  id_user = $int32:userid$;
                  id_feed = $int32:feedid$;
                  } >>)

let del_fav ~feedid ~userid () =
  Db.view_opt
    (<:view< {
            f.id_user;
            f.id_feed;
            } | f in $Db_table.favs$;
            f.id_user = $int32:userid$ && f.id_feed = $int32:feedid$;
     >>) >>= function
  | None -> Lwt.return ()
  | Some _ ->
      Db.query
        (<:delete< f in $Db_table.favs$ | f.id_feed = $int32:feedid$ && f.id_user = $int32:userid$; >>)

let upvote ~feedid ~userid () =
  Db.view_opt
    (<:view< {
            f.id_user;
            f.id_feed;
            } | f in $Db_table.votes$;
            f.id_user = $int32:userid$ && f.id_feed = $int32:feedid$;
     >>) >>= function
  | Some _ ->
      Db.query
        (<:update< f in $Db_table.votes$ := {
                  score = $int32:Int32.of_int(1)$
                  } | f.id_user = $int32:userid$ && f.id_feed = $int32:feedid$; >>)
  | None ->
      Db.query
        (<:insert< $Db_table.votes$ := {
                  id_user = $int32:userid$;
                  id_feed = $int32:feedid$;
                  score = $int32:Int32.of_int(1)$
                  } >>)

let downvote ~feedid ~userid () =
  Db.view_opt
    (<:view< {
            f.id_user;
            f.id_feed;
            } | f in $Db_table.votes$;
            f.id_user = $int32:userid$ && f.id_feed = $int32:feedid$;
     >>) >>= function
  | Some _ ->
      Db.query
        (<:update< f in $Db_table.votes$ := {
                  score = $int32:Int32.of_int(-1)$
                  } | f.id_user = $int32:userid$ && f.id_feed = $int32:feedid$; >>)
  | None ->
      Db.query
        (<:insert< $Db_table.votes$ := {
                  id_user = $int32:userid$;
                  id_feed = $int32:feedid$;
                  score = $int32:Int32.of_int(-1)$
                  } >>)

let cancelvote ~feedid ~userid () =
  Db.view_opt
    (<:view< {
            f.id_user;
            f.id_feed;
            } | f in $Db_table.votes$;
            f.id_user = $int32:userid$ && f.id_feed = $int32:feedid$;
     >>) >>= function
  | None -> Lwt.return ()
  | Some _ ->
      Db.query
        (<:delete< f in $Db_table.votes$ | f.id_feed = $int32:feedid$ && f.id_user = $int32:userid$; >>)

(* Il faut delete tous les tags du lien et ajouter les nouveaux *)
let update ~feedid ~url ~description ~tags () =
  match url with
  | Some u ->
      (Db.query
         (<:update< f in $Db_table.feeds$ := {
                   description = $string:description$;
                   url = $string:u$;
                   } | f.id = $int32:feedid$; >>)
       >>= fun _ ->
       Db.query
         (<:delete< t in $Db_table.feeds_tags$ | t.id_feed = $int32:feedid$ >>)
       >>= fun _ ->
       Lwt_list.iter_p
         (fun tag ->
            Db.query
              (<:insert< $Db_table.feeds_tags$ := {
                        tag = $string:tag$;
                        id_feed = $int32:feedid$;
                        } >>)
         )
         tags
      )
  | None ->
      Db.query
        (<:update< f in $Db_table.feeds$ := {
                  description = $string:description$;
                  } | f.id = $int32:feedid$; >>)

let exist ~feedid () =
  Db.view_opt
    (<:view< {
            f.id;
            } | f in $Db_table.feeds$;
            f.id = $int32:feedid$;
     >>) >>= function
  | None -> Lwt.return false
  | Some _ -> Lwt.return true
