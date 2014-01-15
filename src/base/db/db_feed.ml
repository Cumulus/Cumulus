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
  feeds Lwt.t

let connect =
  let open Db_config in
  PGOCaml.connect
    ?database
    ?host
    ?port
    ?password
    ?user
    ?unix_domain_socket_dir

let get_feeds_aux ?range
  ~feeds_filter
  ~users_filter
  ~user
  () =
  let dbh = connect () in
  begin match range with
  | Some (limit, offset) ->
      let limit = Int64.of_int32 limit in
      let offset = Int64.of_int32 offset in
  Lwt.return (PGSQL(dbh) "user=cumulus" "password=h"
    "
SELECT
  f.id AS id,
  f.url AS url,
  f.date AS date,
  f.description AS description,
  f.author AS author,
  f.parent AS parent,
  f.root AS root,
  f.name AS name,
  f.email_digest AS email_digest,
  f.tags AS tags,
  f.score AS score,

  COUNT(g.root) AS c

FROM
 (SELECT
    sum(v.score) AS score,

    f.id AS id,
    f.url AS url,
    f.date AS date,
    f.description AS description,
    f.author AS author,
    f.parent AS parent,
    f.root AS root,
    f.name AS name,
    f.email_digest AS email_digest,
    f.tags AS tags

  FROM
    (SELECT
       array_agg(t.tag) AS tags,

       f.id AS id,
       f.url AS url,
       f.date AS date,
       f.description AS description,
       f.author AS author,
       f.parent AS parent,
       f.root AS root,
       f.name AS name,
       f.email_digest AS email_digest

     FROM
       (SELECT
          f.id AS id,
          f.url AS url,
          f.timedate AS date,
          f.description AS description,
          f.author AS author,
          f.parent AS parent,
          f.root AS root,

          u.name AS name,
          md5(u.email) AS email_digest

        FROM
          feeds AS f,
          users AS u

        WHERE
          f.author = u.id
          AND
          (f.parent IS NULL OR f.root IS NULL)

        GROUP BY
          f.id,
          f.url,
          f.timedate,
          f.description,
          f.author,
          f.parent,
          f.root,
          u.name,
          u.email

        ORDER BY
          f.id DESC

        LIMIT $limit
        OFFSET $offset
      ) AS f
      LEFT OUTER JOIN
      feeds_tags AS t
      ON
        f.id = t.id_feed

    GROUP BY
      f.id,
      f.url,
      f.date,
      f.description,
      f.author,
      f.parent,
      f.root,
      f.name,
      f.email_digest

    ORDER BY
      f.id DESC
    ) AS f
    LEFT OUTER JOIN
    votes AS v
    ON
      f.id = v.id_feed

  GROUP BY
    f.id,
    f.url,
    f.date,
    f.description,
    f.author,
    f.parent,
    f.root,
    f.name,
    f.email_digest,
    f.tags

  ORDER BY
    f.id DESC
  ) AS f
  LEFT OUTER JOIN
  feeds AS g
  ON
    f.id = g.root


GROUP BY
  f.id,
  f.url,
  f.date,
  f.description,
  f.author,
  f.parent,
  f.root,
  f.name,
  f.email_digest,
  f.tags,
  f.score

ORDER BY
  f.id DESC
")
  | None ->
  Lwt.return (PGSQL(dbh) "user=cumulus" "password=h"
    "
SELECT
  f.id AS id,
  f.url AS url,
  f.date AS date,
  f.description AS description,
  f.author AS author,
  f.parent AS parent,
  f.root AS root,
  f.name AS name,
  f.email_digest AS email_digest,
  f.tags AS tags,
  f.score AS score,

  COUNT(g.root) AS c

FROM
 (SELECT
    sum(v.score) AS score,

    f.id AS id,
    f.url AS url,
    f.date AS date,
    f.description AS description,
    f.author AS author,
    f.parent AS parent,
    f.root AS root,
    f.name AS name,
    f.email_digest AS email_digest,
    f.tags AS tags

  FROM
    (SELECT
       array_agg(t.tag) AS tags,

       f.id AS id,
       f.url AS url,
       f.date AS date,
       f.description AS description,
       f.author AS author,
       f.parent AS parent,
       f.root AS root,
       f.name AS name,
       f.email_digest AS email_digest

     FROM
       (SELECT
          f.id AS id,
          f.url AS url,
          f.timedate AS date,
          f.description AS description,
          f.author AS author,
          f.parent AS parent,
          f.root AS root,

          u.name AS name,
          md5(u.email) AS email_digest

        FROM
          feeds AS f,
          users AS u

        WHERE
          f.author = u.id
          AND
          (f.parent IS NULL OR f.root IS NULL)

        GROUP BY
          f.id,
          f.url,
          f.timedate,
          f.description,
          f.author,
          f.parent,
          f.root,
          u.name,
          u.email

        ORDER BY
          f.id DESC
      ) AS f
      LEFT OUTER JOIN
      feeds_tags AS t
      ON
        f.id = t.id_feed

    GROUP BY
      f.id,
      f.url,
      f.date,
      f.description,
      f.author,
      f.parent,
      f.root,
      f.name,
      f.email_digest

    ORDER BY
      f.id DESC
    ) AS f
    LEFT OUTER JOIN
    votes AS v
    ON
      f.id = v.id_feed

  GROUP BY
    f.id,
    f.url,
    f.date,
    f.description,
    f.author,
    f.parent,
    f.root,
    f.name,
    f.email_digest,
    f.tags

  ORDER BY
    f.id DESC
  ) AS f
  LEFT OUTER JOIN
  feeds AS g
  ON
    f.id = g.root


GROUP BY
  f.id,
  f.url,
  f.date,
  f.description,
  f.author,
  f.parent,
  f.root,
  f.name,
  f.email_digest,
  f.tags,
  f.score

ORDER BY
  f.id DESC
")
end
  >>= fun feeds ->
  begin match user with
  | Some user_id ->
      Db.view
        (<:view< {
          f.id_feed;
        } | f in $Db_table.favs$;
        f.id_user = $int32:user_id$ && in' f.id_feed $List.map (fun (id, _, _, _, _, _, _, _, _, _, _, _) -> Sql.Value.int32 id) feeds$
        >>)
  | None ->
      Lwt.return []
  end
  >>= fun favs ->
  let new_object (id, url, date, description, author, parent, root, name', email_digest', tags, score, count) =
    { author
    ; id
    ; date
    ; description
    ; url
    ; parent
    ; root
    ; tags = Option.map_default Array.to_list [] tags
    ; score = Option.map_default Int64.to_int 0 score
    ; user = object method name = name' method email_digest = Option.get email_digest' end
    ; fav = List.exists (fun e -> e#!id_feed = id) favs
    ; vote = 0(*Option.map_default (fun user -> Int32.to_int (Option.default 0l (List.Exceptionless.find_map (fun e -> if e#?id_feed_of_votes = Some id && e#?id_user_of_votes = Some user then e#?score else None) feeds))) 0 user*)
    ; count = Option.map_default Int64.to_int 0 count
    }
  in
  Lwt.return (List.map new_object feeds)

let rec get_tree_feeds feed_id ~starting ~number ~user () =
  let feeds_filter f = (<:value< f.parent = $int32:feed_id$ >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~range:(number, starting) ~feeds_filter ~users_filter ~user ()
  >>= (fun feeds ->
    Lwt_list.fold_left_s
      (fun acc feed -> get_tree_feeds feed.id ~starting ~number ~user () >>=
      (fun child -> Lwt.return (acc @ child))
      ) feeds feeds (* rajoute les feeds enfants après les feeds parent *)
  )

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
  >>= (function | [] -> Lwt.return None | x :: _ -> Lwt.return (Some x))

let get_feed_with_id ~user id =
  let feeds_filter f = (<:value< f.id = $int32:id$ >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~feeds_filter ~users_filter ~user ()
  >|= (function [x] -> x | _ -> assert false)

let get_comments ~user root =
  let feeds_filter f =
    (<:value< f.root = $int32:root$ || f.parent = $int32:root$ >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~feeds_filter ~users_filter ~user ()

let get_root ~feedid ~user () =
  let feeds_filter f = (<:value< f.id = $int32:feedid$ >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~feeds_filter ~users_filter ~user ()
  >>= (function | [] -> Lwt.return None | x :: _ -> Lwt.return (Some x))


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
            (* f.id; *)
            f.id_user;
            f.id_feed;
            } order by f.id_feed desc
            limit $int32:number$
            offset $int32:starting$ |
            f in $Db_table.favs$;
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

let filter_tags_id f tags =
  (<:value< in' f.id $List.map (fun x -> x#id_feed) tags$ >>)

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

let count_feeds_aux ~filter () =
  Db.view_one
    (<:view< group {
            n = count[f];
            } | f in $Db_table.feeds$;
            $filter$ f;
     >>)

let count_feeds () =
  let filter _ = (<:value< true >>) in
  count_feeds_aux ~filter ()

let count_root_feeds () =
  let filter f = (<:value< is_null f.root || is_null f.parent >>) in
  count_feeds_aux ~filter ()

let count_feeds_with_author author =
  Db_user.get_user_id_with_name author >>= fun author ->
  let filter f = (<:value< f.author = $author$ >>) in
  count_feeds_aux ~filter ()

let count_feeds_with_tag tag =
  get_id_feed_from_tag tag >>= fun tags ->
  let filter f = filter_tags_id f tags in
  count_feeds_aux ~filter ()

let get_feed_url_with_url url =
  Db.view_opt
    (<:view< {
            f.url
            } | f in $Db_table.feeds$;
            f.url = $string:url$;
     >>)

let count_comments root =
  let filter f = (<:value< f.root = $int32:root$ || f.parent = $int32:root$ >>) in
  count_feeds_aux ~filter ()

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
