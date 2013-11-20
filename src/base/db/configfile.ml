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

open Batteries

let fmt = Printf.sprintf

let fail_attrib ~tag x =
  let msg = fmt "Unexpected attribute '%s' inside '%s'" x tag in
  raise (Ocsigen_extensions.Error_in_config_file msg)

let fail_content ~tag =
  let msg = fmt "Unexpected content inside '%s'" tag in
  raise (Ocsigen_extensions.Error_in_config_file msg)

let fail_tag ~tag =
  let msg = fmt "Unexpected tag '%s'" tag in
  raise (Ocsigen_extensions.Error_in_config_file msg)

let fail_pcdata x =
  let msg = fmt "Unexpected pcdata '%s' inside cumulus" x in
  raise (Ocsigen_extensions.Error_in_config_file msg)

let fail_missing ~tag x =
  let msg = fmt "Missing attribute '%s' inside '%s'" x tag in
  raise (Ocsigen_extensions.Error_in_config_file msg)
