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

module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

let connect =
  let open Configfile in
  Lwt_PGOCaml.connect
    ?database
    ?host
    ?port
    ?password
    ?user
    ?unix_domain_socket_dir

let pool = Lwt_pool.create 16 ~validate:Lwt_PGOCaml.alive connect

let use f = Lwt_pool.use pool f

let view x = use (fun db -> Lwt_Query.view db x)
let view_opt x = use (fun db -> Lwt_Query.view_opt db x)
let view_one x = use (fun db -> Lwt_Query.view_one db x)
let query x = use (fun db -> Lwt_Query.query db x)
let value x = use (fun db -> Lwt_Query.value db x)
let value_opt x = use (fun db -> Lwt_Query.value_opt db x)
let alter x = use (fun db -> Lwt_PGOCaml.alter db x)

let rec in' value = function
  | [] -> Sql.Value.bool false
  | x::xs -> Sql.Op.(||) (Sql.Op.(=) x value) (in' value xs)
