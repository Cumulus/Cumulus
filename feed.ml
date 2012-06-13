exception FeedError

(* Does it need Mutex ??? *)
let feed_new n =
  let eref = Eliom_references.eref_from_fun
    ~scope: Eliom_common.site
    ~persistent: ("feed_" ^ (string_of_int n))
    (fun () -> None) in
  Eliom_references.get eref
