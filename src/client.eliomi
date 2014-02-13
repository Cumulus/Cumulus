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

val display_error :
  error_frame:[`Div] Eliom_content.Html5.elt ->
  unit
