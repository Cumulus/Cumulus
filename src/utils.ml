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

module Calendar = struct
  include CalendarLib.Calendar
  module Printer = CalendarLib.Printer.Calendar
end

let offset = 10l

let string_of_calendar cal = Calendar.Printer.sprint "%d/%m/%Y à %T" cal

let string_is_empty str =
  let length = String.length str in
  let rec inner i =
    if i < length then
      (str.[i] = ' ') && inner (i + 1)
    else
      true in
  inner 0

let msg str = [
  Html.p [
    Html.pcdata str
  ]
]

let is_invalid_url =
  let regexp_match_url =
    let legit_chars = "[]0-9A-Za-z_~().,+=&%-|]" in
    let num = "[0-9]?" in
    "^\\(https?\\|ftp\\)://" ^                         (* Protocol *)
      "\\(" ^ legit_chars ^ "*?" ^                     (* Username *)
      "\\(:" ^legit_chars ^ "*\\)?@\\)?" ^             (* Password *)
      "[A-Za-z0-9._-]+" ^                              (* Domain name *)
      "\\(:" ^ num ^ num ^ num ^ num ^ num ^ "\\)?" ^  (* Port *)
      "\\(/"   ^ legit_chars ^ "*\\)*" ^               (* Arborescence *)
      "\\(\\?" ^ legit_chars ^ "*\\)?" ^               (* Parameters *)
      "\\(#"   ^ legit_chars ^ "*\\)?$"                (* Anchor *) in
  (fun input ->
    not (Str.string_match (Str.regexp regexp_match_url) input 0)
  )

let is_invalid_email =
  let email_regexp = Str.regexp "\\([^<>(),; \t]+@[^<>(),; \t]+\\)" in
  (fun email ->
    not (Str.string_match email_regexp email 0)
  )

let get_gravatar email =
  let md5_email = Digest.to_hex (Digest.string (String.lowercase email)) in
  Eliom_service.external_service
    ~prefix:"http://www.gravatar.com"
    ~path:[ "avatar"; md5_email ]
    ~get_params:Eliom_parameter.((int "s") ** (string "d"))
    ()

let strip =
  let regexp_begin = Str.regexp "^[ ]+" in
  let regexp_end = Str.regexp "[ ]+$" in
  (fun str ->
    let str = Str.replace_first regexp_begin "" str in
    Str.replace_first regexp_end "" str
  )

let split =
  let regexp = Str.regexp "[,]+" in
  (fun str ->
    Str.split regexp str
  )

let troncate str =
  let str = strip str in
  String.sub str 0 (min 40 (String.length str))
