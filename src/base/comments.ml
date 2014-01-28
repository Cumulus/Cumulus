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

type tree =
  | Sheet of Feed.feed
  | Node of Feed.feed * tree list

let get = function
  | Sheet elm
  | Node (elm, _) -> Option.map_default identity 0l elm.Feed.parent

let is_root = function
  | Sheet elm
  | Node (elm, _) -> Option.is_none elm.Feed.parent

let rec has_parent id = function
  | Sheet elm -> Int32.equal elm.Feed.id id
  | Node (elm, childs) ->
      let l = List.map (has_parent id) childs in
      Int32.equal elm.Feed.id id || List.fold_left (||) false l

let rec append_tree tree id = function
  | Sheet elm -> begin match Int32.equal id elm.Feed.id with
    | true -> Node (elm, [tree])
    | false -> Sheet elm
  end
  | Node (elm, childs) ->
      match Int32.equal id elm.Feed.id with
      | true -> Node (elm, tree :: childs)
      | false ->
          let childs = List.map (append_tree tree id) childs in
          Node (elm, childs)

let string_of_tree root =
  let buffer = Buffer.create 16 in
  let rec aux = function
    | Sheet feed ->
        Buffer.add_string buffer
          ("[" ^ (string_of_int (Int32.to_int feed.Feed.id)) ^ "]" ^ feed.Feed.description)
    | Node (elm, childs) ->
        Buffer.add_string buffer
          ("([" ^ (string_of_int (Int32.to_int elm.Feed.id)) ^ "]" ^ elm.Feed.description);
        List.iter (fun x -> Buffer.add_char buffer ' '; aux x) childs;
        Buffer.add_char buffer ')'
  in
  aux root;
  Buffer.contents buffer

let rec tree_comments stack comments =
  let rec scan tree stack acc = match stack with
    | [] -> acc @ [ tree ]
    | x :: r ->
        if has_parent (get tree) x
        then scan (append_tree tree (get tree) x) r acc
        else scan tree r (x :: acc)
  in
  match comments with
  | [] -> begin match stack with
    | [] -> None
    | [ x ] -> Some x
    | x :: r -> tree_comments (scan x r []) []
  end
  | x :: r -> tree_comments (scan (Sheet x) stack []) r

let rec branch_comments root = function
  | [] -> root
  | l when is_root root -> root
  | x :: r ->
      if Int32.equal x.Feed.id (get root)
      then branch_comments (Node (x, [root])) r
      else branch_comments root (r @ [x])

let rec to_html tree =
  match tree with
  | Sheet feed ->
      Feed.to_html feed >>= fun elm ->
      Lwt.return (Html.div ~a: [Html.a_class ["line"]] elm)
  | Node (feed, childs) ->
      Feed.to_html feed >>= fun elm ->
      Lwt_util.map to_html childs >>= fun childs ->
      Lwt.return (Html.div ~a: [Html.a_class ["line"]] (elm @ childs))
