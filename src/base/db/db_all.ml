(*
Copyright (c) 2012 Enguerrand Decorne

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

module Option = Eliom_lib.Option

let (>>=) = Lwt.(>>=)

class type feed = object
  method author : Sql.int32_t Sql.non_nullable_data
  method id : Sql.int32_t Sql.non_nullable_data
  method timedate : Sql.timestamp_t Sql.non_nullable_data
  method description : Sql.string_t Sql.non_nullable_data
  method url : Sql.string_t Sql.nullable_data
  method parent: Sql.int32_t Sql.nullable_data
  method root : Sql.int32_t Sql.nullable_data
  method tags : Sql.string_t Sql.non_nullable_data list
  method user :
    <
      email : Sql.string_t Sql.non_nullable_data;
      name : Sql.string_t Sql.non_nullable_data;
    >
  method score : < nul : Sql.non_nullable; t : Sql.int32_t > Sql.t
end

type feeds = feed list

type feed_generator =
  starting:int32 ->
  number:int32 ->
  unit ->
  feeds Lwt.t

let feeds_id_seq = (<:sequence< serial "feeds_id_seq" >>)

let feeds = (<:table< feeds (
                     id integer NOT NULL DEFAULT(nextval $feeds_id_seq$),
                     url text,
                     description text NOT NULL,
                     timedate timestamp NOT NULL DEFAULT(current_timestamp),
                     author integer NOT NULL,
                     parent integer,
                     root integer
                     ) >>)

let feeds_tags_id_seq = (<:sequence< serial "feeds_tags_id_seq" >>)

let feeds_tags = (<:table< feeds_tags (
                          id integer NOT NULL DEFAULT(nextval $feeds_tags_id_seq$),
                          tag text NOT NULL,
                          id_feed integer NOT NULL
                          ) >>)

let votes = (<:table< votes (
                     id_user integer NOT NULL,
                     id_feed integer NOT NULL,
                     score integer NOT NULL
                     ) >>)

let users_id_seq = (<:sequence< serial "users_id_seq" >>)

let users = (<:table< users (
                     id integer NOT NULL DEFAULT(nextval $users_id_seq$),
                     name text NOT NULL,
                     password text NOT NULL,
                     email text NOT NULL,
                     is_admin boolean NOT NULL DEFAULT(false),
                     feeds_per_page integer NOT NULL DEFAULT(10)
                     ) >>)

let get_feeds ~starting ~number
  ~feeds_filter
  ~tags_filter
  ~votes_filter
  ~users_filter
  () =
  Db.view
    (<:view< {
      f.id;
      f.url;
      f.description;
      f.timedate;
      f.author;
      f.parent;
      f.root;
      t.tag;
      u.name;
      u.email;
      v.score;
    } order by f.id desc limit $int32:number$ offset $int32:starting$
    | f in $feeds$; $feeds_filter$ f;
      t in $feeds_tags$; $tags_filter feeds$ t;
      v in $votes$; $votes_filter feeds$ v;
      u in $users$; $users_filter feeds$ u;
    >>)

let get_root_feeds ~starting ~number () =
  let new_object o = object
    method author = o#author
    method id = o#id
    method timedate = o#timedate
    method description = o#description
    method url = o#url
    method parent = o#parent
    method root = o#root
    method tags = [o#tag]
    method user = object method name = o#name method email = o#email end
    method score = (Sql.Op.(+) o#score <:value<0>>)
  end in
  let feeds_filter f = (<:value< is_null f.root || is_null f.parent >>) in
  let tags_filter _ _ = (<:value< true >>) in
  let votes_filter _ _ = (<:value< true >>) in
  let users_filter _ _ = (<:value< true >>) in
  get_feeds ~starting ~number ~feeds_filter ~tags_filter ~votes_filter
  ~users_filter ()
  >>= (fun feeds_and_tags ->
    Lwt_list.fold_left_s
      (fun acc element ->
        Lwt.catch
          (fun () ->
            let value = Lwt_list.find_s
              (fun e -> Lwt.return (e#!id = element#!id)) acc in
            let acc = Lwt_list.filter_s
              (fun e -> Lwt.return (e#!id <> element#!id)) acc in
            value >>= (fun value -> acc >>= (fun acc ->
              Lwt.return ((object
                method id = value#id
                method author = value#author
                method timedate = value#timedate
                method description = value#description
                method url = value#url
                method parent = value#parent
                method root = value#root
                method tags = (element#tag :: value#tags)
                method user = value#user
                method score =  (Sql.Op.(+) value#score <:value<1>>)
              end) :: acc))))
          (fun _ -> Lwt.return ((new_object element) :: acc)))
  [] feeds_and_tags)

