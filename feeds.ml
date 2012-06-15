module Html = Eliom_content.Html5.D

let feeds_new () =
  let table = Ocsipersist.open_table "feeds"
  and ret = ref [] in
  Ocsipersist.iter_table (fun url data ->
    (* FIXME: PLLEEAAASSE I need a non-ugly example for that *)
    ret := (!ret) @ [Feed.feed_new url data];
    Lwt.return ()) table >>= (fun () -> Lwt.return (!ret))

let to_html self =
  self >>= (fun self ->
    let content tmp x =
      tmp @ [
        Html.p [
          Html.pcdata ("url: " ^ x.Feed.url);
          Html.br ();
          Html.pcdata ("title: " ^ x.Feed.title);
          Html.br ();
          Html.pcdata ("date: " ^ (Utils.string_of_calendar x.Feed.date));
          Html.br ();
          Html.pcdata ("author: " ^ x.Feed.author)
        ]
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
