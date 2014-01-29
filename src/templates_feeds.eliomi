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

val to_html :
  user:User.user option ->
  Feed.feed list ->
  [> Html5_types.section ] Eliom_content.Html5.F.elt list
val main_style :
  user:User.user option ->
  error:string option ->
  [< Html5_types.aside_content_fun ] Eliom_content.Html5.F.elt list ->
  [< Html5_types.div_content_fun ] Eliom_content.Html5.F.elt list ->
  [> `Html ] Eliom_content.Html5.F.elt
val link_footer :
  link:(string -> int option -> 'a) ->
  int ->
  int ->
  int ->
  'a list
val comments_to_html' :
  user:User.user option ->
  Comments.tree ->
  [< Html5_types.div_content_fun > `Aside `Div ] Eliom_content.Html5.F.elt
val private_register :
  unit ->
  user:User.user option ->
  error:string option ->
  [> `Html ] Eliom_content.Html5.F.elt
val private_preferences :
  user:User.user option ->
  error:string option ->
  [> `Html ] Eliom_content.Html5.F.elt
val private_comment :
  user:User.user option ->
  int32 ->
  error:string option ->
  [> `Html ] Eliom_content.Html5.F.elt
val private_edit_feed :
  user:User.user option ->
  error:string option ->
  feed:Feed.feed ->
  string * string option * string ->
  [> `Html ] Eliom_content.Html5.F.elt
