(* Copyright 2004 b8_bavard, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open CommonTypes

type graph =
  {
   mutable quarter : (float * float) list;
   mutable hour    : (float * float) list;
   mutable halfday : (float * float) list;
   mutable day     : (float * float) list;
   mutable week    : (float * float) list;
   mutable month   : (float * float) list;
   mutable year    : (float * float) list;
  }

type graph_record =
  GraphDownloads
| GraphUploads
| GraphFile of (uid_type * graph_record)

type graph_time =
  GraphQuarter
| GraphHour
| GraphHalfDay
| GraphDay
| GraphWeek
| GraphMonth
| GraphYear
