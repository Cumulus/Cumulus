type feeds = (Feed.feed list) Lwt.t

let feeds_new () =
  let table = Ocsipersist.open_table "feeds"
  and ret = ref [] in
  Ocsipersist.iter_table (fun url data ->
    (* FIXME: PLLEEAAASSE I need a non-ugly example for that *)
    ret := (!ret) @ [Feed.feed_new url data];
    Lwt.return ()) table >>= (fun () -> Lwt.return (!ret))

let to_html self =
  self >>= (fun self ->
    let content tmp feed =
      tmp @ [
        Html.p (Feed.to_html feed)
      ] in
      let rec f tmp = function
        | [] ->
          Lwt.return
            (Html.html
               (Html.head (Html.title (Html.pcdata "Hello World")) [])
               (Html.body tmp)
            )
        | [x] -> f (content tmp x) []
        | x::xs -> f (content tmp x) xs in
      f [] self)
