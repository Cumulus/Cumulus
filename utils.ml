module Calendar = CalendarLib.Calendar

let string_of_calendar cal =
  (string_of_int (Calendar.day_of_month cal)) ^ "/" ^
    (string_of_int (Calendar.Date.int_of_month (Calendar.month cal))) ^ "/" ^
    (string_of_int (Calendar.year cal)) ^ " à " ^
    (string_of_int (Calendar.hour cal)) ^ ":" ^
    (string_of_int (Calendar.minute cal)) ^ ":" ^
    (string_of_int (Calendar.second cal))
