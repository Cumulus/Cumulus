type feed = {
  url : string;
  title : string;
  date : int;
  author : string
}

(* TODO: Add a mutex *)
let feed_new url (title, date, author) =
  {
    url = url;
    title = title;
    date = date;
    author = author
  }
