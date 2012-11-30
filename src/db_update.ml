let (>>=) = Lwt.(>>=)

let options = (<:table< options (
  name text NOT NULL,
  value text NOT NULL
) >>)

let name = (<:value< "dbversion" >>)

let current_version () =
  Db.view_one
    (<:view< {
      o.value;
     } | o in $options$;
    o.name = $name$;
    >>)
  >>= fun version ->
  Lwt.return (int_of_string version#!value)

let update_version value =
  let value = string_of_int value in
  Db.query
    (<:update< o in $options$ := {
      value = $string:value$;
    } | o.name = $name$; >>)

let update version f =
  current_version () >>= function
    | v when v < version ->
        Printf.eprintf "Updating Cumulus database to version %d\n" version;
        f () >>= fun () ->
        update_version version
    | _ -> Lwt.return ()

let () =
  Lwt_main.run begin
    update 2
      (fun () ->
        Db.alter "ALTER TABLE users ADD COLUMN \
                  feeds_per_page integer NOT NULL DEFAULT(10)"
      )
    >>= fun () ->
    update 3
      (fun () ->
        Db.alter "ALTER TABLE feeds ALTER url DROP NOT NULL" >>= fun () ->
        Db.alter "ALTER TABLE feeds ADD COLUMN parent integer" >>= fun () ->
        Db.alter "ALTER TABLE feeds ADD COLUMN root integer" >>= fun () ->
        Db.alter "ALTER TABLE feeds RENAME COLUMN title TO description"
      )
  end
