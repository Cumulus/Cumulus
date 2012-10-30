module Calendar = CalendarLib.Calendar

let offset = 10l

let string_of_calendar cal =
  (string_of_int (Calendar.day_of_month cal)) ^ "/" ^
    (string_of_int (Calendar.Date.int_of_month (Calendar.month cal))) ^ "/" ^
    (string_of_int (Calendar.year cal)) ^ " à " ^
    (string_of_int (Calendar.hour cal)) ^ ":" ^
    (string_of_int (Calendar.minute cal)) ^ ":" ^
    (string_of_int (Calendar.second cal))

let string_is_empty str =
  let length = String.length str in
  let rec inner i =
    if i < length then
      (str.[i] = ' ') && inner (i + 1)
    else
      true in
  inner 0

let msg str = [
  Html.p [
    Html.pcdata str
  ]
]

let is_invalid_url input =
  let regexp_match_url =
    let legit_chars = "[]0-9A-Za-z_~().,+=&%-]" in
    let num = "[0-9]?" in
    "^\\(https?\\|ftp\\)://" ^                         (* Protocol *)
      "\\(" ^ legit_chars ^ "*?" ^                     (* Username *)
      "\\(:" ^legit_chars ^ "*\\)?@\\)?" ^             (* Password *)
      "[A-Za-z0-9._-]+" ^                              (* Domain name *)
      "\\(:" ^ num ^ num ^ num ^ num ^ num ^ "\\)?" ^  (* Port *)
      "\\(/"   ^ legit_chars ^ "*\\)*" ^               (* Arborescence *)
      "\\(\\?" ^ legit_chars ^ "*\\)?" ^               (* Parameters *)
      "\\(#"   ^ legit_chars ^ "*\\)?$"                (* Anchor *) in
  not (Str.string_match (Str.regexp regexp_match_url) input 0)

let get_gravatar email =
  let md5_email = Digest.to_hex (Digest.string (String.lowercase email))
  in Eliom_service.external_service
    "http://www.gravatar.com" [ "avatar"; md5_email ]
    Eliom_parameter.((int "s") ** (string "d")) ()

let strip str =
  let str = Str.replace_first (Str.regexp "^[ ]+") "" str in
  Str.replace_first (Str.regexp "[ ]+$") "" str
