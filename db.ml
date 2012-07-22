module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

let connect () =
  Lwt_PGOCaml.connect
    ~database: "cumulus"
    ~host: "localhost"
    (* ~port: 5432 *)
    ~user: "cumulus" ()

let validate db =
  Lwt.try_bind
    (fun () -> Lwt_PGOCaml.ping db)
    (fun () -> Lwt.return true)
    (fun _ -> Lwt.return false)

let pool = Lwt_pool.create 16 ~validate connect

let transate_sql f =
  Lwt_pool.use pool (fun db ->
    Lwt_PGOCaml.begin_work db >>= (fun () ->
      Lwt.try_bind
        (fun () -> f db)
        (fun r ->
          Lwt_PGOCaml.commit db >>= (fun () ->
            Lwt.return r
          )
        )
        (fun e ->
          Lwt_PGOCaml.rollback db >>= (fun () ->
            Lwt.fail e
          )
        )
    )
  )

let feeds_id_seq = (<:sequence< serial "feeds_id_seq" >>)

let feeds = (<:table< feeds (
  id integer NOT NULL DEFAULT(nextval $feeds_id_seq$),
  url text NOT NULL,
  title text NOT NULL,
  timedate timestamp NOT NULL DEFAULT(current_timestamp),
  author integer NOT NULL
) >>)

let feeds_tags = (<:table< feeds_tags (
  tag text NOT NULL,
  id_feed integer NOT NULL
) >>)

let users_id_seq = (<:sequence< serial "users_id_seq" >>)

let users = (<:table< users (
  id integer NOT NULL DEFAULT(nextval $users_id_seq$),
  name text NOT NULL,
  password text NOT NULL,
  email text NOT NULL
) >>)

let get_user_id_with_name name =
  transate_sql (fun db ->
    Lwt_Query.view_one db (<:view< {
      a.id
    } | a in $users$; a.name = $string:name$ >>)
  )

let get_user_name_with_id id =
  transate_sql (fun db ->
    Lwt_Query.view_one db (<:view< {
      a.name
    } | a in $users$; a.id = $int32:id$ >>)
  )

let get_user_with_name name =
  transate_sql (fun db ->
    Lwt_Query.view_opt db (<:view< a |
        a in $users$; a.name = $string:name$ >>)
  )

(* TODO: Hashtlb ? *)
let get_tags_from_feeds db feeds =
  Lwt_list.map_p (fun feed ->
    Lwt.return
      (feed#!id,
       Lwt_Query.view db (<:view< {
         t.tag
       } | t in $feeds_tags$; t.id_feed = $feed#id$ >>)
      )
  ) feeds

let get_feeds () =
  transate_sql (fun db ->
    Lwt_Query.view db (<:view< f |
        f in $feeds$ >>)
    >>= (fun feeds ->
      Lwt.return (feeds, get_tags_from_feeds db feeds)
    )
  )

let get_feeds_with_author author =
  transate_sql (fun db ->
    get_user_id_with_name author >>= (fun author ->
      Lwt_Query.view db (<:view< f |
          f in $feeds$; f.author = $int32:author#!id$ >>)
      >>= (fun feeds ->
        Lwt.return (feeds, get_tags_from_feeds db feeds)
      )
    )
  )

let get_feed_url_with_url url =
  transate_sql (fun db ->
    Lwt_Query.view_opt db (<:view< {
      f.url
    } | f in $feeds$; f.url = $string:url$ >>)
  )

let add_feed url title tags userid =
  transate_sql (fun db ->
    let id_feed = (<:value< feeds?id >>) in
    let feed = Lwt_Query.query db (<:insert< $feeds$ := {
      id = $id_feed$;
      url = $string:url$;
      title = $string:title$;
      timedate = feeds?timedate;
      author = $int32:userid$
    } >>)
    and tag = Lwt_list.iter_p
      (fun tag -> Lwt_Query.query db (<:insert< $feeds_tags$ := {
        tag = $string:tag$;
        id_feed = $id_feed$
       } >>)
      ) tags in
    Lwt.join [feed; tag]
  )

let add_user name password email =
  transate_sql (fun db ->
    Lwt_Query.query db (<:insert< $users$ := {
      id = users?id;
      name = $string:name$;
      password = $string:password$;
      email = $string:email$
    } >>)
  )
