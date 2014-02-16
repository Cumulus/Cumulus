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
open Eliom_lib.Lwt_ops

module UTF8 = CamomileLibraryDefault.Camomile.CaseMap.Make(CamomileLibrary.UTF8)

type append_state = Ok | Not_connected | Empty | Already_exist | Invalid_url

let strip_and_lowercase x =
  UTF8.lowercase (Utils.strip x)

let append_feed_aux_base ~update ?root ?parent ?url ~description ~tags () =
  User.get_userid () >>= function
  | None -> Lwt.return Not_connected
  | Some userid ->
      if Utils.string_is_empty description then
        Lwt.return Empty
      else
        Db_feed.add_feed ?root ?parent ?url ~description ~tags ~userid ()
        >|= update

let append_feed_aux ~update ?root ?parent ~url ~description ~tags () =
  let tags = List.map strip_and_lowercase (Utils.split tags) in
  if List.is_empty tags then
    Lwt.return Empty
  else if Utils.is_invalid_url url then
    Lwt.return Invalid_url
  else
    Db_feed.exists_with_url ~url >>= function
    | true -> Lwt.return Already_exist
    | false ->
        append_feed_aux_base ~update ?root ?parent ~url ~description ~tags ()

let append_feed ~update (url, (description, tags)) =
  append_feed_aux ~update ~url ~description ~tags ()

let get_root_and_parent id =
  Db_feed.get_feed_with_id ~user:None id >>= fun feeds ->
  let parent = feeds.Feed.id in
  let root = match feeds.Feed.root with
    | Some root -> root
    | None -> parent
  in
  Lwt.return (root, parent)

let append_link_comment ~update (id, (url, (description, tags))) =
  get_root_and_parent id >>= fun (root, parent) ->
  append_feed_aux ~update ~root ~parent ~url ~description ~tags ()

let append_desc_comment ~update (id, description) =
  get_root_and_parent id >>= fun (root, parent) ->
  append_feed_aux_base ~update ~root ~parent ~description ~tags:[] ()

let edit_feed_aux_base ~feedid ~url ~description ~tags =
  User.get_userid () >>= function
  | None -> Lwt.return Not_connected
  | Some userid ->
      if Utils.string_is_empty description then
        Lwt.return Empty
      else
        Db_feed.update ~feedid ~url ~description ~tags () >|= fun () ->
        Ok

let edit_feed_aux ~feedid ~url ~description ~tags =
  let tags = List.map strip_and_lowercase (Utils.split tags) in
  if List.is_empty tags then
    Lwt.return Empty
  else if Utils.is_invalid_url url then
    Lwt.return Invalid_url
  else
    Db_feed.get_feed_with_id ~user:None feedid >>= fun feed ->
    if feed.Feed.url <> Some url then
      Db_feed.exists_with_url ~url >>= function
      | true -> Lwt.return Already_exist
      | false -> edit_feed_aux_base ~feedid ~url:(Some url) ~description ~tags
    else
      edit_feed_aux_base ~feedid ~url:(Some url) ~description ~tags

let edit_link_comment (feedid, (url, (description, tags))) =
  edit_feed_aux ~feedid ~url ~description ~tags

let edit_desc_comment (feedid, description) =
  edit_feed_aux_base ~feedid ~url:None ~description ~tags:[]
