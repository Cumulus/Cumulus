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

type node =
  { id : int32
  ; leftBound : int32
  ; rightBound : int32
  }

type tree =
  | Sheet of Feed.feed
  | Node of Feed.feed * tree list

let make feed =
  { id = feed.Feed.id
  ; leftBound = feed.Feed.leftBound
  ; rightBound = feed.Feed.rightBound
  }

let tree_of_node ~user node =
  let open Int32 in
  let rec traverse = function
    | feed :: rest when feed.Feed.leftBound + one = feed.Feed.rightBound ->
      (rest, Sheet feed)
    | feed :: rest ->
      let rec reduce childs = function
        | feed' :: rest when
          feed'.Feed.leftBound > feed.Feed.leftBound
          && feed'.Feed.rightBound < feed.Feed.rightBound ->
          let (rest, node) = traverse (feed' :: rest) in
          reduce (node :: childs) rest
        | rest -> (rest, childs)
      in let (rest, childs) = reduce [] rest
      in (rest, Node (feed, List.rev childs))
    | [] -> assert false
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | lst ->
      let (rest, node) = traverse lst
      in aux (node :: acc) rest
  in
  Feed.get_feed_with_id ~user node.id
  >>= fun root ->
  Feed.get_feeds_of_interval ~user node.leftBound node.rightBound
  >>= fun childs ->
    List.sort
      (fun feed feed' -> to_int (feed.Feed.leftBound - feed'.Feed.leftBound))
      (root :: childs)
    |> Lwt.return
  >|= aux []
  >|= List.hd

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

let rec branch_comments target = function
  | [] -> target
  | _ when is_root target -> target
  | x :: r ->
      if Int32.equal x.Feed.id (get target)
      then branch_comments (Node (x, [target])) r
      else branch_comments target (r @ [x])
