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

type obj =
(*  | Comment of int *)
  | Score of int
  | Feed of Html5_types.div_content_fun Eliom_content.Html5.elt

type t = (int32 * obj)

val call_event : t -> unit

val error_frame : [> Html5_types.div ] Eliom_content.Html5.elt

val get_score_div : score:int -> [> Html5_types.div ] Eliom_content.Html5.F.elt

val get_upvote_inner :
  score_div:Html5_types.div_content_fun Eliom_content.Html5.elt ->
  upon:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
  up:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
  downon:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
  down:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
  vote:int ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt

val feeds_actions :
  content:(int -> [`Section] Eliom_content.Html5.F.elt list Lwt.t) ->
  box:[`Aside] Eliom_content.Html5.F.elt ->
  unit

val fav_actions :
  is_fav:bool ->
  res:[`Div] Eliom_content.Html5.elt ->
  del:[`A of [`Img]] Eliom_content.Html5.elt ->
  add:[`A of [`Img]] Eliom_content.Html5.elt ->
  feed_id:int32 ->
  unit

val upvotes_actions :
  container:[`Div] Eliom_content.Html5.elt ->
  score_div:Html5_types.div_content_fun Eliom_content.Html5.elt ->
  upon:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
  up:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
  downon:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
  down:Html5_types.div_content_fun Eliom_content.Html5.F.elt ->
  vote:int ->
  feed_id:int32 ->
  unit

val set_first_feed :
  Html5_types.div_content_fun Eliom_content.Html5.elt option ->
  unit

val set_error_from_string : string -> unit

val actions_submit_link :
  submit:Html5_types.input Eliom_content.Html5.elt ->
  url:Html5_types.input Eliom_content.Html5.elt ->
  title:Html5_types.input Eliom_content.Html5.elt ->
  tags:Html5_types.input Eliom_content.Html5.elt ->
  unit
