

type url = {
    proto : string;
    server : string;
    port : int;
    full_file : string;
    file : string;
    user : string;
    passwd : string;
    args : (string * string) list;
    
    string : string;
  }
  
val create  :    ?proto:string ->
    ?server:string ->
    ?port:int -> ?user:string -> ?pass:string -> string -> url
    (*d create an already parsed url *)
  
val of_string : string -> url
    (*d [url_of_string s] returns the url corresponding to the given [s].
       this string should start by ["http://"]. [raise Invalid_argument]
       if the string is not an url *)

val to_string : url -> string
    (*d [string_of_url u] returns a string representing u. the args are not
        put in this string *)

val cut_args : string -> (string * string) list
