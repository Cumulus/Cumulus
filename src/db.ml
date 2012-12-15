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

let (>>=) = Lwt.(>>=)

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

class type ['a, 'b] macaque_type = object
  method get : unit
  method nul : 'b
  method t : 'a
end

type ('a, 'b) t = ('a, 'b) macaque_type Sql.t

let connect () =
  Lwt_PGOCaml.connect
    ~database:"cumulus"
    ~host:"localhost"
    ~password:"mdp"
    ~user:"cumulus"
    ()

let validate db =
  Lwt.try_bind
    (fun () -> Lwt_PGOCaml.ping db)
    (fun () -> Lwt.return true)
    (fun _ -> Lwt.return false)

let pool = Lwt_pool.create 16 ~validate connect

let rec in' value = function
  | [] -> (<:value< false >>)
  | x::xs -> (<:value< $x$ = $value$ || $in' value xs$ >>)

let exec f x = Lwt_pool.use pool (fun db -> f db x)

let view x = exec (fun db x -> Lwt_Query.view db x) x
let view_one x = exec (fun db x -> Lwt_Query.view_one db x) x
let view_opt x = exec (fun db x -> Lwt_Query.view_opt db x) x
let query x = exec (fun db x -> Lwt_Query.query db x) x
let value x = exec (fun db x -> Lwt_Query.value db x) x

let alter =
  let name = "query" in
  let low_level_exec db query =
    Lwt_PGOCaml.prepare db ~query ~name () >>= fun () ->
    Lwt_PGOCaml.execute db ~name ~params:[] () >>= fun _ ->
    Lwt_PGOCaml.close_statement db ~name ()
  in
  exec low_level_exec
