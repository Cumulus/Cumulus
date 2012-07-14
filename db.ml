module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

let get_db () = Lwt_PGOCaml.connect ~database: "cumulus" ~host: "localhost" (*~port: 5432*) ~user: "root" ()

let feeds_id_seq = (<:sequence< serial "feeds_id_seq" >>)

let feeds = (<:table< feeds (
  id integer NOT NULL DEFAULT(nextval $feeds_id_seq$),
  url text NOT NULL,
  title text NOT NULL,
  timedate timestamp NOT NULL DEFAULT(current_timestamp),
  author integer NOT NULL,
  tags text NOT NULL
) >>)

let users_id_seq = (<:sequence< serial "users_id_seq" >>)

let users = (<:table< users (
  id integer NOT NULL DEFAULT(nextval $users_id_seq$),
  name text NOT NULL,
  password text NOT NULL,
  email text NOT NULL
) >>)

let get_users_id_with_name name =
  get_db () >>= (fun db ->
    Lwt_Query.view_one db (<:view< {
      a.id
    } | a in $users$; a.name = $string:name$ >>)
  )

let get_users_name_with_id id =
  get_db () >>= (fun db ->
    Lwt_Query.view_one db (<:view< {
      a.name
    } | a in $users$; a.id = $int32:id$ >>)
  )

let get_users_with_name name =
  get_db () >>= (fun db ->
    Lwt_Query.view_opt db (<:view< a |
        a in $users$; a.name = $string:name$ >>)
  )

let get_feeds () =
  get_db () >>= (fun db ->
    Lwt_Query.view db (<:view< f | f in $feeds$ >>)
  )

let get_feeds_with_author author =
  get_db () >>= (fun db ->
    get_users_id_with_name author >>= (fun author ->
      Lwt_Query.view db (<:view< f |
          f in $feeds$; f.author = $int32:author#!id$ >>)
    )
  )

(* TODO: To improve... (Called in Feeds.add_feed) *)
let get_feeds_url () =
  get_db () >>= (fun db ->
    Lwt_Query.view db (<:view< {f.url} | f in $feeds$ >>)
  )

let add_feed url title tags userid =
  get_db () >>= (fun db ->
    Lwt_Query.query db (<:insert< $feeds$ := {
      id = feeds?id;
      url = $string:url$;
      title = $string:title$;
      timedate = feeds?timedate;
      author = $int32:userid$;
      tags = $string:tags$
    } >>)
  )

let add_user name password email =
  get_db () >>= (fun db ->
    Lwt_Query.query db (<:insert< $users$ := {
      id = users?id;
      name = $string:name$;
      password = $string:password$;
      email = $string:email$
    } >>)
  )
