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
