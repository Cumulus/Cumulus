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
  }

class type search = object
  method id_feed : Sql.int32_t Sql.non_nullable_data
end

type feeds = feed list

type feed_generator =
  starting:int32 ->
  number:int32 ->
  unit ->
  feeds Lwt.t

module OrderedData = struct
  type t = Sql.int32_t Sql.non_nullable_data
  let compare a b = (Int32.to_int (Sql.get a)) - (Int32.to_int (Sql.get b))
end

module VoteMap = Map.Make(OrderedData)
module TagMap = Map.Make(OrderedData)

let get_feeds_aux ~starting ~number
  ~feeds_filter
  ~tags_filter
  ~votes_filter
  ~users_filter
  () =
  let votes_of_list l =
    List.fold_left (fun a e -> VoteMap.add e#id_feed e a) VoteMap.empty l
  in let tags_of_list l =
    List.fold_left (fun a e -> TagMap.add e#id_feed e a) TagMap.empty l
  in
  Db.view
    (<:view< {
      f.id;
      f.url;
      f.description;
      f.timedate;
      f.author;
      f.parent;
      f.root;
    } order by f.id desc limit $int32:number$ offset $int32:starting$
    | f in $Db_table.feeds$; $feeds_filter$ f;
    >>)
  >>= fun feeds ->
  Db.view
    (<:view<
      group { score = match sum[v.score] with null -> 0 | x -> x } by { v.id_feed }
      | v in $Db_table.votes$;
      $Db.in'$ v.id_feed $List.map (fun x -> x#id) feeds$
    >>)
  >>= fun votes ->
  Db.view
    (<:view<
      group { tags = match string_array_agg[t.tag]
        with null -> $string_array:[||]$
        | x -> x }
      by { t.id_feed }
      | t in $Db_table.feeds_tags$;
      $Db.in'$ t.id_feed $List.map (fun x -> x#id) feeds$
    >>)
  >>= fun tags
    Lwt.return (feeds, tags_of_list tags, votes_of_list votes)

let filter_tags_feed f tags =
  (<:value< $Db.in'$ f.id $List.map (fun x -> x#id_feed) tags$ >>)
let filter_feed_tag tag t f =
  (<:value< t.tag = $string:tag$ && f.id = t.id_feed >>)
let filter_feed_author author u f =
  (<:value< u.name = $string:author$ && f.author = u.id >>)

(*
 *  Transform:
 *    |id| url | ... |vote| tag |    user    |
 *
 *    [ 1;  url; ... ;   1; toto;  dinosaure ]
 *    [ 1;  url; ... ;   1; tata;  dinosaure ]
 *    [ 1;  url; ... ;   1; tutu;  dinosaure ]
 *    [ 1;  url; ... ;   1; titi;  dinosaure ]
 *  To:
 *    [ 1;  url; ... ;   4; [toto, tata, tutu, titi]; dinosaure ]
 *
 *)

let reduce (feeds, tags, votes) =
  let split = Str.split (Str.regexp_string " ") in
  let new_object o = Lwt.return
    { author = o#!author
    ; id = o#!id
    ; date = o#!timedate
    ; description = o#!description
    ; url = o#?url
    ; parent = o#?parent
    ; root = o#?root
    ; tags = (try split (TagMap.find o#id tags)#tags with _ -> [])
    ; score = (try Int32.to_int (VoteMap.find o#id votes)#!score with _ -> 0)
    }
  in Lwt_list.map_s new_object feeds

(*
let reduce feeds =
  let new_object o =
    { author = o#!author
    ; id = o#!id
    ; date = o#!timedate
    ; description = o#!description
    ; url = o#?url
    ; parent = o#?parent
    ; root = o#?root
    ; tags = [o#!tag]
    ; user = object method name = o#!name method email = o#!email end
    ; score = Int32.to_int o#!score
    }
  in
  Lwt_list.fold_left_s
    (fun acc element ->
      element#!id)));
      Lwt.catch
        (fun () ->
          let value = Lwt_list.find_s
            (fun e -> Lwt.return (e.id = element#!id)) acc in
          let acc = Lwt_list.filter_s
            (fun e -> Lwt.return (e.id <> element#!id)) acc in
          value >>= (fun value -> acc >>= (fun acc ->
            Lwt.return (
              { value with
                tags = element#!tag :: value.tags
              ; score = value.score + Int32.to_int element#!score
              }
              :: acc
            )
          ))
        )
        (fun _ -> Lwt.return ((new_object element) :: acc))
    ) [] feeds
*)

(*
 * TODO: optimization (next gen. of Db_feed.get_tree_feeds)
 *)

let rec get_tree_feeds feed_id ~starting ~number () =
  let feeds_filter f = (<:value< f.parent = $int32:feed_id$ >>) in
  let tags_filter _ _ = (<:value< true >>) in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ~votes_filter ~users_filter ()
  >>= reduce >>= (fun feeds ->
    Lwt_list.fold_left_s
      (fun acc feed -> get_tree_feeds feed.id ~starting ~number () >>=
      (fun child -> Lwt.return (acc @ child))
      ) feeds feeds (* rajoute les feeds enfants après les feeds parent *)
  )

let get_links_feeds ~starting ~number () =
  let feeds_filter f = (<:value< is_not_null f.url >>) in
  let tags_filter _ _ = (<:value< true >>) in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ~votes_filter ~users_filter ()
  >>= reduce

let get_comments_feeds ~starting ~number () =
  let feeds_filter f = (<:value< is_null f.url >>) in
  let tags_filter _ _ = (<:value< true >>) in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ~votes_filter ~users_filter ()
  >>= reduce

let get_root_feeds ~starting ~number () =
  let feeds_filter f = (<:value< is_null f.root || is_null f.parent >>) in
  let tags_filter _ _ = (<:value< true >>) in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ~votes_filter ~users_filter ()
  >>= reduce

let get_feeds ~starting ~number () =
  let feeds_filter f = (<:value< true >>) in
  let tags_filter _ _ = (<:value< true >>) in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ~votes_filter ~users_filter ()
  >>= reduce

(* HIT *)

let get_feeds_with_author author ~starting ~number () =
  let feeds_filter _ = (<:value< true >>) in
  let tags_filter _ _ = (<:value< true >>) in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter f u = filter_feed_author author u f in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ~votes_filter ~users_filter ()
  >>= reduce

let get_feeds_with_tag tag ~starting ~number () =
  let feeds_filter _ = (<:value< true >>) in
  let tags_filter f t = filter_feed_tag tag t f in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ~votes_filter ~users_filter ()
  >>= reduce

let get_feed_with_url url =
  let feeds_filter f = (<:value< f.url = $string:url$ >>) in
  let tags_filter _ _ = (<:value< true >>) in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting:0l ~number:99999999l ~feeds_filter ~tags_filter ~votes_filter ~users_filter ()
  (* TODO:                ^          ^ to fix *)
  >>= reduce
  >>= (function | [] -> Lwt.return None | x :: _ -> Lwt.return (Some x))

let get_feed_with_id id =
  let feeds_filter f = (<:value< f.id = $int32:id$ >>) in
  let tags_filter _ _ = (<:value< true >>) in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting:0l ~number:99999999l ~feeds_filter ~tags_filter ~votes_filter ~users_filter ()
  (* TODO:                ^          ^ to fix *)
  >>= reduce
  >>= (function | [] -> Lwt.return None | x :: _ -> Lwt.return (Some x))

let get_comments root =
  let feeds_filter f =
    (<:value< f.root = $int32:root$ || f.parent = $int32:root$ >>) in
  let tags_filter _ _ = (<:value< true >>) in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting:0l ~number:99999999l ~feeds_filter ~tags_filter ~votes_filter ~users_filter ()
  (* TODO:                ^          ^ to fix *)
  >>= reduce

let get_root ~feedid () =
  let feeds_filter f = (<:value< f.id = $int32:feedid$ >>) in
  let tags_filter _ _ = (<:value< true >>) in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting:0l ~number:99999999l ~feeds_filter ~tags_filter ~votes_filter ~users_filter ()
  (* TODO:                ^          ^ to fix *)
  >>= reduce
  >>= (function | [] -> Lwt.return None | x :: _ -> Lwt.return (Some x))

(*
 *  Cette fonction va disparaître !
 *  Elle est là pour tester sans « modifier » get_fav_with_username
 *)

let of_feed (feeds, tags, votes) =
  let search id =
    let rec aux id acc = function
      | [] -> acc
      | x :: r when x#!id_feed = id -> aux id (x :: acc) r
      | _ :: r -> aux id acc r
    in aux id []
  in Lwt_list.map_s
    (fun value ->
      Db_user.get_user_name_and_email_with_id value#!author
      >>= (fun user ->
      Lwt.return
        { author = value#!author
        ; id = value#!id
        ; date = value#!timedate
        ; description = value#!description
        ; url = value#?url
        ; parent = value#?parent
        ; root = value#?root
        ; tags = List.map (fun e -> e#!tag) (search value#!id tags)
        ; score = List.fold_left
        (fun a _ -> succ a)
        0 (search value#!id votes)
        })) feeds

let get_fav_with_username name ~starting ~number () =
  Db_feed.get_fav_with_username name starting number ()
  >>= of_feed
