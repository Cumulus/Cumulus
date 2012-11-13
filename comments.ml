open Feed

type tree = Sheet of Feed.feed | Node of Feed.feed * tree list

let rec has_parent id = function
  | Sheet elm -> elm.id = id
  | Node (elm, childs) -> let l = List.map (fun x -> has_parent id x) childs in 
                          elm.id = id || (List.fold_left (fun x -> fun y -> x || y) false l)

let rec append_tree tree id = function
  | Sheet elm -> begin match id = elm.id with
                   | true -> Node (elm, [tree])
                   | false -> Sheet elm end
  | Node (elm, childs) -> match id = elm.id with
                            | true -> Node (elm, tree :: childs)
                            | false -> let childs = List.map (fun x -> append_tree tree id x) childs in 
                                       Node (elm, childs)

let string_of_tree root =
  let buffer = Buffer.create 16 in
  let rec aux = function
    | Sheet feed -> Buffer.add_string buffer ("[" ^ (string_of_int (Int32.to_int feed.id)) ^ "]" ^ feed.description)
    | Node (elm, childs) -> Buffer.add_string buffer ("([" ^ (string_of_int (Int32.to_int elm.id)) ^ "]" ^ elm.description);
                            List.iter (fun x -> Buffer.add_char buffer ' '; aux x) childs;
                            Buffer.add_char buffer ')'
  in let _ = aux root in Buffer.contents buffer

let rec tree_comments stack comments =
  let get = function
    | Sheet elm
    | Node (elm, _) -> match elm.parent with
                        | None -> 0l
                        | Some n -> n
  in let rec scan ?(end_of_scan=false) tree stack acc = match stack with
    | [] -> tree :: acc
    | x :: r -> if has_parent (get tree) x
                then if end_of_scan
                    then (append_tree tree (get tree) x) :: r
                    else scan (append_tree tree (get tree) x) r acc
                else scan tree r (x :: acc)
  in match comments with
    | [] -> begin match stack with
              | [] -> []
              | [ x ] -> [ x ]
              | x :: r -> scan ~end_of_scan:(true) x r [] end
    | x :: r -> tree_comments (scan (Sheet x) stack []) r

let rec branch_comments root comments =
  let get = function
    | Sheet elm
    | Node (elm, _) -> match elm.parent with
                        | None -> 0l
                        | Some n -> n
  in let is_root = function
    | Sheet elm
    | Node (elm, _) -> match elm.parent with
                        | None -> true
                        | Some _ -> false
  in let rec search id = function
    | [] -> None
    | x :: r -> if (get x) = id then Some x else search id r
  in match comments with
    | [] -> root
    | l when (is_root root) -> root
    | x :: r -> if x.id = (get root)
                then branch_comments (Node (x, [root])) r
                else branch_comments root (r @ [x])

let rec to_html = function
  | Sheet feed -> Feed.to_html feed >>= (fun elm -> Lwt.return (Html.div ~a: [Html.a_class ["line post"]] elm))
  | Node (feed, childs) -> Feed.to_html feed >>= (fun elm ->
                           Lwt_util.map to_html childs >>= (fun childs ->
                              Lwt.return (Html.div ~a: [Html.a_class ["line post"]] (elm @ childs))
                           ))

