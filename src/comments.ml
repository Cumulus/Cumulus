type tree = Sheet of Feed.feed | Node of Feed.feed * tree list
let (>>=) = Lwt.(>>=)

let rec has_parent id = function
  | Sheet elm -> elm.Feed.id = id
  | Node (elm, childs) ->
      let l = List.map (fun x -> has_parent id x) childs in
      (elm.Feed.id = id) || (List.fold_left (fun x -> fun y -> x || y) false l)

let rec append_tree tree id = function
  | Sheet elm -> begin match id = elm.Feed.id with
                   | true -> Node (elm, [tree])
                   | false -> Sheet elm
                 end
  | Node (elm, childs) ->
    match id = elm.Feed.id with
      | true -> Node (elm, tree :: childs)
      | false ->
          let childs = List.map (fun x -> append_tree tree id x) childs in
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
  let _ = aux root in
  Buffer.contents buffer

let rec tree_comments stack comments =
  let get = function
    | Sheet elm
    | Node (elm, _) ->
        match elm.Feed.parent with
          | None -> 0l
          | Some n -> n
  in
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
              | x :: r -> Some (List.hd (scan x r []))
            end
    | x :: r -> tree_comments (scan (Sheet x) stack []) r

let rec branch_comments root comments =
  let get = function
    | Sheet elm
    | Node (elm, _) -> match elm.Feed.parent with
        | None -> 0l
        | Some n -> n
  in
  let is_root = function
    | Sheet elm
    | Node (elm, _) -> match elm.Feed.parent with
        | None -> true
        | Some _ -> false
  in
  match comments with
    | [] -> root
    | l when (is_root root) -> root
    | x :: r ->
        if x.Feed.id = (get root)
        then branch_comments (Node (x, [root])) r
        else branch_comments root (r @ [x])

let rec to_html = function
  | Sheet feed ->
      Feed.to_html feed >>= fun elm ->
      Lwt.return (Html.div ~a: [Html.a_class ["line post"]] elm)
  | Node (feed, childs) ->
      Feed.to_html feed >>= fun elm ->
      Lwt_util.map to_html childs >>= fun childs ->
      Lwt.return (Html.div ~a: [Html.a_class ["line post"]] (elm @ childs))
