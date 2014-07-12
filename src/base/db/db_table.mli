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

val feeds : (
  < author : Sql.int32_t Sql.non_nullable_data;
    description : Sql.string_t Sql.non_nullable_data;
    id : Sql.int32_t Sql.non_nullable_data;
    parent : Sql.int32_t Sql.nullable_data;
    root : Sql.int32_t Sql.nullable_data;
    leftBound : Sql.int32_t Sql.non_nullable_data;
    rightBound : Sql.int32_t Sql.non_nullable_data;
    timedate : Sql.timestamp_t Sql.non_nullable_data;
    url : Sql.string_t Sql.nullable_data >,
    < id : < nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
      timedate : < nul : Sql.non_nullable; t : Sql.timestamp_t > Sql.t >
    Sql.writable
) Sql.view

val feeds_tags : (
  < id_feed : Sql.int32_t Sql.non_nullable_data;
    tag : Sql.string_t Sql.non_nullable_data >,
  < >
  Sql.writable
) Sql.view

val votes : (
  < id_feed : Sql.int32_t Sql.non_nullable_data;
    id_user : Sql.int32_t Sql.non_nullable_data;
    score : Sql.int32_t Sql.non_nullable_data >,
  < > Sql.writable
) Sql.view

val users : (
  < email : Sql.string_t Sql.non_nullable_data;
    feeds_per_page : Sql.int32_t Sql.non_nullable_data;
    id : Sql.int32_t Sql.non_nullable_data;
    is_admin : Sql.bool_t Sql.non_nullable_data;
    name : Sql.string_t Sql.non_nullable_data;
    password : Sql.string_t Sql.non_nullable_data >,
  < feeds_per_page : < nul : Sql.non_nullable; t : Sql.int32_t > Sql.t ;
    id : < nul : Sql.non_nullable; t : Sql.int32_t > Sql.t ;
    is_admin : < nul : Sql.non_nullable; t : Sql.bool_t > Sql.t >
  Sql.writable
) Sql.view

val favs : (
  < id_feed : Sql.int32_t Sql.non_nullable_data;
    id_user : Sql.int32_t Sql.non_nullable_data >,
  < > Sql.writable
) Sql.view
