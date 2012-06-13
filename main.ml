module Html = Eliom_pervasives.HTML5
module Calendar = CalendarLib.Calendar

let atom_service =
  Eliom_atom.Reg.register_service
    ~path: ["atom"]
    ~get_params: Eliom_parameters.unit
    (fun () () ->
      Lwt.return
        (Atom_feed.feed
           ~updated: (Calendar.make 2012 6 9 17 40 30)
           ~id: (Html.uri_of_string "http://cumulus.org")
           ~title: (Atom_feed.plain "An Atom flux")
           []
        )
    )

let init_test_ervice =
  Eliom_output.Html5.register_service
    ~path: ["init_test"]
    ~get_params: Eliom_parameters.unit
    (fun () () ->
        let nb = Eliom_references.eref
          ~scope: Eliom_common.site
          ~persistent: "nlink"
          0
        and eref = Eliom_references.eref
          ~scope: Eliom_common.site
          ~persistent: ("feed_1")
          None in
        ignore (Eliom_references.set eref (Some "TEST !") >>= (fun () ->
          Eliom_references.set nb 1));
        Lwt.return
          (Html.html
             (Html.head (Html.title (Html.pcdata "Hello World")) [])
             (Html.body [])
          )
    )

let test_service =
  Eliom_output.Html5.register_service
    ~path: ["test"]
    ~get_params: Eliom_parameters.unit
    (fun () () ->
      Feeds.feeds_new () >>= (fun feeds ->
        let rec f tmp = function
          | [] ->
            Lwt.return
              (Html.html
                 (Html.head (Html.title (Html.pcdata "Hello World")) [])
                 (Html.body tmp)
              )
          | [x] -> x >>= (fun maybe_str ->
            match maybe_str with
              | None -> f tmp []
              | (Some str) -> f (tmp @ [Html.p [Html.pcdata str]]) [])
          | x::xs -> x >>= (fun maybe_str ->
            match maybe_str with
              | None -> f tmp xs
              | (Some str) -> f (tmp @ [Html.p [Html.pcdata str]]) xs) in
        f [] feeds)
    )

let main_service =
  Eliom_output.Html5.register_service
    ~path: [""]
    ~get_params: Eliom_parameters.unit
    (fun () () ->
      Lwt.return
        (Html.html
           (Html.head (Html.title (Html.pcdata "Hello World")) [])
           (Html.body [
             Html.p [
               Html.pcdata "Hello World"
             ]
           ])
        )
    )
