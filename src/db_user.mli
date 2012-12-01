type password

val to_password : string -> password
val check_password : string -> password -> bool

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
  password : password;
  is_admin : (Sql.bool_t, Sql.non_nullable) Db.t;
  feeds_per_page : (Sql.int32_t, Sql.non_nullable) Db.t >
    option Lwt.t

val get_user_id_with_name :
  string ->
  < id : (Sql.int32_t, Sql.non_nullable) Db.t > Lwt.t

val add_user :
  name:string ->
  password:password ->
  email:string ->
  unit ->
  unit Lwt.t

val update_user_password :
  userid:int32 ->
  password:password ->
  unit ->
  unit Lwt.t

val update_user_email :
  userid:int32 ->
  email:string ->
  unit ->
  unit Lwt.t

val update_user_feeds_per_page :
  userid:int32 ->
  nb_feeds:int32 ->
  unit ->
  unit Lwt.t
