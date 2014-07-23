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
