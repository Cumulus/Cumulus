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

val string_input_box :
  ?a:Html5_types.input_attrib Eliom_content.Html5.F.attrib list ->
  input_type:[< `Button
             | `Checkbox
             | `Color
             | `Date
             | `Datetime
             | `Datetime_local
             | `Email
             | `File
             | `Hidden
             | `Image
             | `Month
             | `Number
             | `Password
             | `Radio
             | `Range
             | `Reset
             | `Search
             | `Submit
             | `Tel
             | `Text
             | `Time
             | `Url
             | `Week ] ->
  ?name:[< string Eliom_parameter.setoneradio ] Eliom_parameter.param_name ->
  ?value:string ->
  unit ->
  [> Html5_types.input ] Eliom_content.Html5.elt

val submit_input :
  ?a:Html5_types.input_attrib Eliom_content.Html5.F.attrib list ->
  ?name:[< string Eliom_parameter.setoneradio ] Eliom_parameter.param_name ->
  ?value:string ->
  unit ->
  [> Html5_types.input ] Eliom_content.Html5.elt

val links_of_tags :
  string list ->
  [> `A of [> `PCDATA ] | `PCDATA ] Eliom_content.Html5.F.elt list

module Markdown : module type of MarkdownHTML.Make_html5(struct
  include Eliom_content.Html5.F.Raw
  module Svg = Eliom_content.Svg.F.Raw
end)
