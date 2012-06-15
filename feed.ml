module Calendar = CalendarLib.Calendar

type feed = {
  url : string;
  title : string;
  date : Calendar.t;
  author : string
}

let feed_new url (title, date, author) =
  {
    url = url;
    title = title;
    date = date;
    author = author
  }
