module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

class type feed = object
  method author : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t
  method id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t
  method timedate : < get : unit; nul : Sql.non_nullable; t : Sql.timestamp_t > Sql.t
  method title : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t
  method url : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t
end

class type tag = object
  method tag : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t
  method id_feed : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t
end

type feeds_and_tags = feed list * tag list

let (>>=) = Lwt.(>>=)

let connect () =
  Lwt_PGOCaml.connect
    ~database: "cumulus"
    ~host: "localhost"
    (* ~port: 5432 *)
    ~password: "mdp"
    ~user: "cumulus" ()

let validate db =
  Lwt.try_bind
    (fun () -> Lwt_PGOCaml.ping db)
    (fun () -> Lwt.return true)
    (fun _ -> Lwt.return false)

let pool = Lwt_pool.create 16 ~validate connect

let feeds_id_seq = (<:sequence< serial "feeds_id_seq" >>)

let feeds = (<:table< feeds (
  id integer NOT NULL DEFAULT(nextval $feeds_id_seq$),
  url text NOT NULL,
  title text NOT NULL,
  timedate timestamp NOT NULL DEFAULT(current_timestamp),
  author integer NOT NULL
) >>)

let feeds_tags_id_seq = (<:sequence< serial "feeds_tags_id_seq" >>)

let feeds_tags = (<:table< feeds_tags (
  id integer NOT NULL DEFAULT(nextval $feeds_tags_id_seq$),
  tag text NOT NULL,
  id_feed integer NOT NULL
) >>)

let users_id_seq = (<:sequence< serial "users_id_seq" >>)

let users = (<:table< users (
  id integer NOT NULL DEFAULT(nextval $users_id_seq$),
  name text NOT NULL,
  password text NOT NULL,
  email text NOT NULL,
  is_admin boolean NOT NULL DEFAULT(false)
) >>)

let rec in' value = function
  | [] -> (<:value< false >>)
  | x::xs -> (<:value< $x$ = $value$ || $in' value xs$ >>)

let get_user_id_with_name name =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view_one db (<:view< {
      a.id
    } | a in $users$; a.name = $string:name$ >>)
  )

let get_user_name_and_email_with_id id =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view_one db (<:view< {
      a.name; a.email
    } | a in $users$; a.id = $int32:id$ >>)
  )

let get_user_with_name name =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view_opt db (<:view< a |
        a in $users$; a.name = $string:name$ >>)
  )

let get_feeds ?(starting=0l) ?(number=Utils.offset) () =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view db (
      <:view< {
        f.id;
        f.url;
        f.title;
        f.timedate;
        f.author;
      } order by f.id desc
        limit $int32:number$
        offset $int32:starting$ |
          f in $feeds$ >>)
    >>= fun feeds ->
    Lwt_Query.view db (<:view< {
      t.tag;
      t.id_feed;
    } | t in $feeds_tags$ >>)
    >>= fun tags ->
    Lwt.return (feeds, tags)
  )

let get_feeds_with_author ?(starting=0l) ?(number=Utils.offset) author =
  Lwt_pool.use pool (fun db ->
    get_user_id_with_name author
    >>= (fun author ->
      Lwt_Query.view db (
        <:view< {
          f.id;
          f.url;
          f.title;
          f.timedate;
          f.author;
        } order by f.id desc limit $int32:number$ offset $int32:starting$ |
          f in $feeds$;
          f.author = $int32:author#!id$ >>)
    )
    >>= fun feeds ->
    Lwt_Query.view db (<:view< {
      t.tag;
      t.id_feed;
    } | t in $feeds_tags$;
        $in'$ t.id_feed $List.map (fun x -> x#id) feeds$ >>)
    >>= fun tags ->
    Lwt.return (feeds, tags)
  )

let get_feeds_with_tag ?(starting=0l) ?(number=Utils.offset) tag =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view db (
      <:view< {
        f.id;
        f.url;
        f.title;
        f.timedate;
        f.author;
      } order by f.id desc limit $int32:number$ offset $int32:starting$ |
        f in $feeds$; t in $feeds_tags$;
        t.tag = $string:tag$;
        t.id_feed = f.id >>)
    >>= fun feeds ->
    Lwt_Query.view db (<:view< {
      t.tag;
      t.id_feed;
    } | t in $feeds_tags$;
        $in'$ t.id_feed $List.map (fun x -> x#id) feeds$ >>)
    >>= fun tags ->
    Lwt.return (feeds, tags)
  )

let get_feed_url_with_url url =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view_opt db (<:view< {
      f.url
    } | f in $feeds$; f.url = $string:url$ >>)
  )

let get_feed_with_id id =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view db (
      <:view< {
        f.id;
        f.url;
        f.title;
        f.timedate;
        f.author;
      } | f in $feeds$;
          f.id = $int32:id$ >>)
    >>= fun feeds ->
    Lwt_Query.view db (<:view< {
      t.tag;
      t.id_feed;
    } | t in $feeds_tags$;
        t.id_feed = $int32:id$ >>)
    >>= fun tags ->
    Lwt.return (feeds, tags)
  )

let count_feeds () =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view_one db (<:view< group { n = count[f] } | f in $feeds$ >>
    )
  )

let add_feed url title tags userid =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.value db (<:value< feeds?id >>)
  )
  >>= fun id_feed ->
  let feed =
    Lwt_pool.use pool (fun db ->
      Lwt_Query.query db (<:insert< $feeds$ := {
        id = $int32:id_feed$;
        url = $string:url$;
        title = $string:title$;
        timedate = feeds?timedate;
        author = $int32:userid$;
      } >>)
    )
  and tag =
    Lwt_list.iter_p
      (fun tag ->
        Lwt_pool.use pool (fun db ->
          Lwt_Query.query db (<:insert< $feeds_tags$ := {
            id = feeds_tags?id;
            tag = $string:tag$;
            id_feed = $int32:id_feed$;
          } >>)
        )
      )
      tags
  in
  Lwt.join [feed; tag]

let add_user name password email =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.query db (<:insert< $users$ := {
      id = users?id;
      name = $string:name$;
      password = $string:password$;
      email = $string:email$;
      is_admin = users?is_admin;
    } >>)
  )

let is_feed_author feed userid =
  try_lwt begin
    Lwt_pool.use pool (fun db ->
      Lwt_Query.view_one db (<:view< f |
        f in $feeds$;
        f.id = $int32:feed$;
        f.author = $int32:userid$; >>)
    )
    >>= fun _ ->
    Lwt.return true
  end
  with exn ->
    Ocsigen_messages.debug (fun () -> Printexc.to_string exn);
    Lwt.return false

let delete_feed feed userid =
  is_feed_author feed userid >>= function
    | true ->
        Lwt_pool.use pool (fun db ->
          Lwt_Query.query db (<:delete< f in $feeds$ |
              f.id = $int32:feed$; >>)
          >>= fun () ->
          Lwt_Query.query db (<:delete< f in $feeds_tags$ |
              f.id_feed = $int32:feed$ >>)
        )
    | false ->
        Lwt.return ()
