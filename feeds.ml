(* Does it need Mutex ??? *)
let feeds_new () =
  let nb = Eliom_references.eref
    ~scope: Eliom_common.site
    ~persistent: "nlink"
    0 in
  let rec inner n tmp =
    if n >= 0 then
      inner (n - 1) (
        try (tmp @ [Feed.feed_new n])
        with Feed.FeedError -> tmp
      )
    else
      tmp
  and packed_nb = Eliom_references.get nb in
  packed_nb >>= (fun number -> Lwt.return (inner number []))
