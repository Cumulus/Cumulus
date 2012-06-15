type feed = {
  url : string;
  title : string;
  date : CalendarLib.Calendar.t;
  author : string
}

let feed_new url (title, date, author) = {
  url = url;
  title = title;
  date = date;
  author = author
}

let to_html self = [
  Html.pcdata ("url: " ^ self.url);
  Html.br ();
  Html.pcdata ("title: " ^ self.title);
  Html.br ();
  Html.pcdata ("date: " ^ (Utils.string_of_calendar self.date));
  Html.br ();
  Html.pcdata ("author: " ^ self.author)
]
