(* TODO: Add a mutex *)
let feeds_new () =
  let table = Ocsipersist.open_table "feeds"
  and ret = ref [] in
  Ocsipersist.iter_table (fun a b ->
    (* FIXME: PLLEEAAASSE I need a non-ugly example for that *)
    ret := (!ret) @ [Feed.feed_new b];
    Lwt.return ()) table >>= (fun () -> Lwt.return (!ret))
