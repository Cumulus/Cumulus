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

val get_root_feeds : feed_generator
