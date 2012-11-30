class type ['a, 'b] macaque_type = object
  method get : unit
  method nul : 'b
  method t : 'a
end

type ('a, 'b) t = ('a, 'b) macaque_type Sql.t

val view : ('a, 'b) Sql.view -> 'a list Lwt.t

val view_one : ('a, 'b) Sql.view -> 'a Lwt.t

val view_opt : ('a, 'b) Sql.view -> 'a option Lwt.t

val query : unit Sql.query -> unit Lwt.t

val value :
  < nul : Sql.non_nullable; t : 'a #Sql.type_info; .. > Sql.t ->
  'a Lwt.t

val alter : string -> unit Lwt.t

val in' :
  < nul : 'a; t : 'b; .. > Sql.t ->
  < nul : 'a; t : 'b; .. > Sql.t list ->
  < nul : 'a; t : Sql.bool_t > Sql.t
