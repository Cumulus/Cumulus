val get_user_name_and_email_with_id :
  int32 ->
  < email : (Sql.string_t, Sql.non_nullable) Db.t;
  name : (Sql.string_t, Sql.non_nullable) Db.t >
    Lwt.t

val get_user_with_name :
  string ->
  < email : (Sql.string_t, Sql.non_nullable) Db.t;
  id : (Sql.int32_t, Sql.non_nullable) Db.t;
  name : (Sql.string_t, Sql.non_nullable) Db.t;
  password : (Sql.string_t, Sql.non_nullable) Db.t;
  is_admin : (Sql.bool_t, Sql.non_nullable) Db.t;
  feeds_per_page : (Sql.int32_t, Sql.non_nullable) Db.t >
    option Lwt.t

val get_user_id_with_name :
  string ->
  < id : (Sql.int32_t, Sql.non_nullable) Db.t > Lwt.t

val add_user :
  string -> string -> string -> unit Lwt.t

val update_user_password : int32 -> string -> unit Lwt.t

val update_user_email : int32 -> string -> unit Lwt.t

val update_user_feeds_per_page : int32 -> int32 -> unit Lwt.t
