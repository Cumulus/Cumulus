exception FeedError

(* Does it need Mutex ??? *)
let feed_new n =
  let eref = Eliom_references.eref_from_fun
    ~scope: Eliom_common.site
    ~persistent: (string_of_int n)
    (fun () -> raise FeedError) in
  Eliom_references.get eref
