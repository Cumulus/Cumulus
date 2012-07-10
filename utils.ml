#load "str.cma" ;;
module Calendar = CalendarLib.Calendar

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

let is_url input =
  let regexp_match_url =
    let legit_chars = "[]0-9A-Za-z_~ ().,+=&-]" in
    "^\\(https?\\|ftp\\)://" ^                                   (* Protocole *)
    "[A-Za-z0-9.]+" ^                                       (* Nom de domaine *)
    "\\(/"   ^ legit_chars ^ "*\\)*" ^                        (* Arborescence *)
    "\\(\\?" ^ legit_chars ^ "*\\)?" ^                          (* Paramètres *)
    "\\(#"   ^ legit_chars ^ "*\\)$"                                 (* Ancre *)
  in Str.string_match (Str.regexp regexp_match_url) input 0 ;;


