let (>>=) = Lwt.(>>=)

let users_id_seq = (<:sequence< serial "users_id_seq" >>)

let users = (<:table< users (
  id integer NOT NULL DEFAULT(nextval $users_id_seq$),
  name text NOT NULL,
  password text NOT NULL,
  email text NOT NULL,
  is_admin boolean NOT NULL DEFAULT(false),
  feeds_per_page integer NOT NULL DEFAULT(10)
) >>)

let get_user_id_with_name name =
  Db.view_one
    (<:view< {
      u.id
     } | u in $users$;
    u.name = $string:name$;
    >>)

let get_user_name_and_email_with_id id =
  Db.view_one
    (<:view< {
      u.name;
      u.email;
     } | u in $users$;
    u.id = $int32:id$;
    u.id = $int32:id$;
    >>)

let get_user_with_name name =
  Db.view_opt
    (<:view< {
      u.id;
      u.name;
      u.password;
      u.email;
      u.is_admin;
      u.feeds_per_page;
     } | u in $users$;
    u.name = $string:name$;
    >>)

let add_user ~name ~password ~email () =
  Db.query
    (<:insert< $users$ := {
      id = users?id;
      name = $string:name$;
      password = $string:password$;
      email = $string:email$;
      is_admin = users?is_admin;
      feeds_per_page = users?feeds_per_page;
    } >>)

let update_user_password ~userid ~password () =
  Db.query
    (<:update< u in $users$ := {
      password = $string:password$;
    } | u.id = $int32:userid$; >>)

let update_user_email ~userid ~email () =
  Db.query
    (<:update< u in $users$ := {
      email = $string:email$;
    } | u.id = $int32:userid$; >>)

let update_user_feeds_per_page ~userid ~nb_feeds () =
  Db.query
    (<:update< u in $users$ := {
      feeds_per_page = $int32:nb_feeds$;
    } | u.id = $int32:userid$; >>)
