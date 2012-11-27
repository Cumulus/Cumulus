module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

class type ['a] macaque_type = object
  method get : unit
  method nul : Sql.non_nullable
  method t : 'a
end

class type feed = object
  method author : Sql.int32_t macaque_type Sql.t
  method id : Sql.int32_t macaque_type Sql.t
  method timedate : Sql.timestamp_t macaque_type Sql.t
  method description : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t
  method url : < get : unit; nul : Sql.nullable; t : Sql.string_t > Sql.t
  method parent : < get : unit; nul : Sql.nullable; t : Sql.int32_t > Sql.t
  method root : < get : unit; nul : Sql.nullable; t : Sql.int32_t > Sql.t
end

class type tag = object
  method tag : Sql.string_t macaque_type Sql.t
  method id_feed : Sql.int32_t macaque_type Sql.t
end

type feeds_and_tags = feed list * tag list

let (>>=) = Lwt.(>>=)

type feed_generator =
    starting:int32 ->
    number:int32 ->
    unit ->
    feeds_and_tags Lwt.t

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

(** Updating the database *)

let options = (<:table< options (
  name text NOT NULL,
  value text NOT NULL
) >>)

let current_version db =
  Lwt_Query.view_one db (<:view< {
    o.value;
  } | o in $options$;
      o.name = "dbversion" >>)
  >>= fun version ->
  Lwt.return (int_of_string version#!value)

let update_version db value =
  let value = string_of_int value in
  Lwt_Query.query db (<:update< o in $options$ := {
    value = $string:value$;
  } | o.name = "dbversion" >>)

let update version f =
  Lwt_pool.use pool (fun db ->
    current_version db >>= fun current_version ->
    if current_version < version then
      Lwt_pool.use pool (fun db ->
        Printf.eprintf "Updating Cumulus database to version %d\n" version;
        f db >>= fun () ->
        update_version db version
      )
    else
      Lwt.return ()
  )

let alter db query =
  let name = "query" in
  Lwt_PGOCaml.prepare db ~query ~name () >>= fun () ->
  Lwt_PGOCaml.execute db ~name ~params:[] () >>= fun _ ->
  Lwt_PGOCaml.close_statement db ~name ()

let () =
  Lwt_main.run begin
    update 2 (fun db ->
      alter db "ALTER TABLE users ADD COLUMN \
                feeds_per_page integer NOT NULL DEFAULT(10)"
    )
    >>= fun () ->
    update 3 (fun db ->
      alter db "ALTER TABLE feeds ALTER url TYPE text" >>= fun () ->
      alter db "ALTER TABLE feeds ADD COLUMN parent integer" >>= fun () ->
      alter db "ALTER TABLE feeds ADD COLUMN parent integer" >>= fun () ->
      alter db "ALTER TABLE feeds RENAME COLUMN title TO description"
    )
  end

(** Tables description *)

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

let users_id_seq = (<:sequence< serial "users_id_seq" >>)

let users = (<:table< users (
  id integer NOT NULL DEFAULT(nextval $users_id_seq$),
  name text NOT NULL,
  password text NOT NULL,
  email text NOT NULL,
  is_admin boolean NOT NULL DEFAULT(false),
  feeds_per_page integer NOT NULL DEFAULT(10)
) >>)

let rec in' value = function
  | [] -> (<:value< false >>)
  | x::xs -> (<:value< $x$ = $value$ || $in' value xs$ >>)

let filter_tags_id f tags =
    (<:value< $in'$ f.id $List.map (fun x -> x#id_feed) tags$ >>)
let filter_feeds_id t feeds =
    (<:value< $in'$ t.id_feed $List.map (fun x -> x#id) feeds$ >>)

let get_id_feed_from_tag tag =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view db (<:view< {
      t.id_feed;
    } | t in $feeds_tags$;
        t.tag = $string:tag$ >>)
  )

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

let get_feeds_aux
    ~starting
    ~number
    ~feeds_filter
    ~tags_filter
    () =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view db (
      <:view< {
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
          $feeds_filter$ f >>)
    >>= fun feeds ->
    Lwt_Query.view db (<:view< {
      t.tag;
      t.id_feed;
    } | t in $feeds_tags$;
        $tags_filter feeds$ t >>)
    >>= fun tags ->
    Lwt.return (feeds, tags)
  )

let get_root_feeds ~starting ~number () =
  let feeds_filter f = (<:value< is_null f.root || is_null f.parent >>) in
  let tags_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ()

let get_feeds ~starting ~number () =
  let feeds_filter _ = (<:value< true >>) in
  let tags_filter _ _ = (<:value< true >>) in
  get_feeds_aux ~starting ~number ~feeds_filter ~tags_filter ()

let count_feeds_aux ~filter () =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view_one db (<:view< group {
      n = count[f];
    } | f in $feeds$;
        $filter$ f >>)
  )

let count_feeds () =
  let filter _ = (<:value< true >>) in
  count_feeds_aux ~filter ()

let get_feeds_with_author author ~starting ~number () =
  get_user_id_with_name author >>= fun author ->
  let feeds_filter f = (<:value< f.author = $int32:author#!id$ >>) in
  let tags_filter feeds t = filter_feeds_id t feeds in
  get_feeds_aux ?starting ?number ~feeds_filter ~tags_filter ()

let count_feeds_with_author author =
  get_user_id_with_name author >>= fun author ->
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
        f.description;
        f.timedate;
        f.author;
        f.parent;
        f.root;
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

let count_comments parent =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view_one db (<:view< group { n = count[f] } | f in $feeds$; f.parent = $int32:parent$ >>
    )
  )

let add_feed url description tags userid =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.value db (<:value< feeds?id >>)
  )
  >>= fun id_feed ->
  let feed =
    Lwt_pool.use pool (fun db ->
      Lwt_Query.query db (<:insert< $feeds$ := {
        id = $int32:id_feed$;
        url = $string:url$;
        description = $string:description$;
        timedate = feeds?timedate;
        author = $int32:userid$;
        parent = null;
        root = null;
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
      feeds_per_page = users?feeds_per_page;
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

let get_comments root =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.view db (
      <:view< {
        f.id;
        f.url;
        f.description;
        f.timedate;
        f.author;
        f.parent;
        f.root;
      } order by f.timedate |
        f in $feeds$;
        f.root = $int32:root$ || f.parent = $int32:root$; >>)
    >>= fun feeds ->
    Lwt_Query.view db (<:view< {
      t.tag;
      t.id_feed;
    } | t in $feeds_tags$ >>)
    >>= fun tags ->
    Lwt.return (feeds, tags)
  )

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

let update_user_password userid password =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.query db (<:update< u in $users$ := {
      password = $string:password$;
    } | u.id = $int32:userid$ >>)
  )

let update_user_email userid email =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.query db (<:update< u in $users$ := {
      email = $string:email$;
    } | u.id = $int32:userid$ >>)
  )

let update_user_feeds_per_page userid nb_feeds =
  Lwt_pool.use pool (fun db ->
    Lwt_Query.query db (<:update< u in $users$ := {
      feeds_per_page = $int32:nb_feeds$;
    } | u.id = $int32:userid$ >>)
  )
