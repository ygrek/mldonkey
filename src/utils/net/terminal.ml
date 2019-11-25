(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
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

(*
type command =
  | Message of string
  | Command of string


module Vt100 = struct

(* permet de manipuler le terminal *)

    let width = ref 80
    let  height = ref 24
        
  let cx = ref 0
  let cy = ref 0
      
  let in_update = ref false
      
  let gotoxy x y =
    if not !in_update then begin
      cx := x; 
      cy := y
    end;
    Printf2.lprintf "\027[%d;%dH" (y+1) (x+1) 
      
  let stdvid () = print_string "\027[m" (* "]" *);;
      
  let revvid () = print_string "\027[7m"
                                           
  let clrscr () = print_string "\027[2J"

let vtflush () = flush stdout

(* begin_update / end_update :                               *)
(* Sert uniquement à sauvegarder la position du curseur.     *)
(* N'assure pas d'exclusion mutuelle pour l'accès à l'écran. *)

let begin_update () = in_update := true
let end_update () = gotoxy !cx !cy; in_update := false

let beep () = print_char '\007'; flush stdout

end

open Vt100
open Printf

  type etat = {
      mutable durty_line : bool ; (* la ligne courante est-elle a jour ? *)
      mutable durty_info : bool ; (* le bandeau d'info est-il a jour ? *)
      mutable ligne : string ;    (* ligne courante *)
      mutable pos   : int ;       (* position dans cette ligne *) 
      mutable trailer : string list;
      mutable header : string list;
    } 

  (* initialisatino de l'etat *)
  let etat = { ligne = ""; pos = 0; trailer = [];
header = [];
 durty_line = true ; durty_info = true }

  (* ajoute les espaces a la fin de la chaine s pour atteindre la taille w *)
  let align w s =
    let l = String.length s in
    if l < w then s^String.make (w-l) ' ' else String.sub s 0 w

  let prompt = ref "> "
  let off = ref (String.length !prompt)


type control =
  | CHANGE_LINE of string * int
 (* Commande générique *)
  | COMMAND of string
 (* message en entree *)
  | MES_IN of string
 (* message recu d'un canal *)
  | MES_OUT of string (* canal * message *)
 (* message d'information (resultat d'une commande...) *)
  | INFO of string

let reader = ref (fun _ -> ())
let set_reader f = reader := f

  (* met a jour la ligne *)
  let change_line () =
    gotoxy 0 (!height - 1);
    print_string (!prompt^align (!width - !off) etat.ligne);
    gotoxy (!off + etat.pos) (!height - 1);
    flush stdout ;
    etat.durty_line <- false

  (* meet a jour le bandeau d'info *)
  let change_info () =
    revvid ();
    let rec iter i list =
      match list with
        [] -> ()
      | line :: tail ->
         gotoxy 0 i;
         print_string (align !width line);
  iter (i+1) tail
    in
    iter 0 etat.header;
    let rec iter i list =
      match list with
        [] -> ()
      | line :: tail ->
         gotoxy 0 (!height - i - 1);
         print_string (align !width line);
  iter (i-1) tail
    in
    iter (List.length etat.trailer) etat.trailer;
    stdvid () ;
    etat.durty_info <- false

  (* ajoute une ligne au-dessus du bandeau *)
  let print_line s =
    gotoxy 0 (!height - (List.length etat.trailer) -1) ;
    print_string (align !width s);
    gotoxy 0 (!height-1) ;
    print_newline ();
    etat.durty_line <- true ;
    etat.durty_info <- true


let set_prompt p = prompt := p; off := String.length p

let set_header lines = 
  etat.header <- lines;
  etat.durty_info <- true;
  change_info ()

let set_trailer lines = 
  etat.trailer <- lines;
  etat.durty_info <- true;
  change_info ()




module Output = struct

    
  (* recoit les commandes l'affichage, il y a un mutex pour qu'il
     n'y ait qu'une seule instance a la fois, c'est un mutex sur l'ecran *)
  let rec control c = 
    let info m =  control (INFO m) in begin
      (match c with
      | CHANGE_LINE (s,i) ->
   etat.ligne <- s;
   etat.pos <- i;
   etat.durty_line <- true
 (* une commande est recue *)
      | COMMAND (cmd) -> !reader (Command cmd)
(*
   let lexbuf = Lexing.from_string cmd in
   try
     (* on la parse *)
     match Parser.cmd Lexer.cmd lexbuf with
            | Help -> info ("ceci est l'aide")
     | Join chan -> begin
       try
  let canal = List.assoc chan !chanmap in
  etat.chan <- Some (chan, canal);
  info (sprintf "connecte au canal '%s'" chan);
  (* { | canal ALL } *)
       with Not_found -> (* pas encore de recepteur pour ce canal *)
  try (* on en cree un *)
    let serveur = nsrecord.get_loc chan in
    etat.chan <- Some (chan, create_channel chan serveur control) ;
    info (sprintf "connecte au canal '%s'" chan)
  with Not_found ->
    info (sprintf "canal '%s' inexistant" chan)
     end
     | Create chan -> begin
  let chan = String.sub cmd 7 (String.length cmd - 7) in
  try
    let _ = nsrecord.get_loc chan in
    info (sprintf "canal '%s' deja existant" chan)
      with Not_found ->
    let serveur = create_server chan in
    nsrecord.add_channel (chan,serveur);
    info (sprintf "canal '%s' cree" chan);
        control (COMMAND ("join "^chan)) (* on join le canal *)
     end
     | GetAll ->
  List.iter (fun s -> info s) (nsrecord.get_all_chan ())
   with Parsing.Parse_error ->
   info ("mauvaise commande, taper aide pour la "^
  "liste des commandes");
      end *)
        (* un message est recu depuis le clavier *)
      | MES_IN m -> 
        !reader (Message m)
 (* un message est recu depuis le gestionnaire du canal *)
      | MES_OUT m -> (
     (* c'est bien le canal actif, on l'affiche *)
       print_line m
     (* sinon, rien *)
  )
 (* un message d'info a afficher *)
      | INFO m -> print_line ("$ "^m)
      );
      (* on met a jour ce qu'il faut *)
      if etat.durty_info then change_info ();
      if etat.durty_line then change_line ()
    end

end

let output c = Output.control c

let print m = output (MES_OUT m)

(* input *)
module Input = struct
  open String
  type seq_state =
    | NORMAL
    | ESC
    | SQUARE
                                         | TILDE

  type state = {
      mutable history : string list ; (* l'historique des commandes *)
      mutable curline : int ;         (* la ligne visible *)
      mutable nbline : int ;          (* le nombre de lignes en tout *)
      mutable line : string ;         (* la ligne courante *)
      mutable pos : int ;             (* la position courante sur cette ligne *)
      mutable seq : seq_state;        (* l'etat pour les sequences de caracteres *)
    }

  (* initialisation de l'etat *)
  let s = { history = []; curline = 0; nbline = 0 ;
        line = ""; pos = 0; seq = NORMAL }

 

  let init () = 
  let sock = TcpBufferedSocket.create "stdin" Unix.stdin
    (fun _ _ -> ()) in

    TcpBufferedSocket.set_reader sock (
       fun sock nr ->
    let change () = output (CHANGE_LINE (s.line,s.pos)) in
    (* on lit un paquet de touches au clavier *)
    let b = TcpBufferedSocket.buf sock in
    let buf = String.sub b.TcpBufferedSocket.buf b.TcpBufferedSocket.pos nr in
    TcpBufferedSocket.buf_used sock nr;
    (* on les traite *)
    for i = 0 to nr - 1 do
      let c = buf.[i] in
      match s.seq with
      | NORMAL ->
   begin match c with
   | '\n' ->
       let line = s.line in
       (* est-ce une commande (debut par /) *)
       if length line >= 2 && line.[0] = '/' && line.[1] <> '/' then
   output (COMMAND
      (sub (lowercase line) 1 (length line - 1)))
       else if line <> "" then
   output (MES_IN line);
       if line <> "" && ((s.nbline > 0 && List.hd s.history <> line)
         || s.nbline = 0)
       then begin
  (* on ajoute la ligne a l'historique si ce n'est pas la meme 
     que la precedente et qu'elle n'est pas vide *)
  s.history <- (s.line :: s.history);
  s.nbline <- s.nbline + 1;
       end;
       (* reset *)
       s.curline <- 0;
       s.line <- "";
       s.pos <- 0;
(*   | c -> print_string (Char.escaped c); flush stdout*)
       (* DEL *)
   | '\008' | '\127' when s.pos > 0 ->
       let l = length s.line in
       s.line <- sub s.line 0 (s.pos-1) ^ sub s.line s.pos (l-s.pos) ;
       s.pos <- s.pos - 1
      (* DEL ou EOF *)
   | '\008' | '\127' | '\004' -> ()
      (* HOME *)
   | '\001' -> s.pos <- 0
      (* END *)
   | '\005' -> s.pos <- String.length s.line
      (* debut d'une sequence *)
   | '\027' -> s.seq <- ESC
      (* on ajoute le caractere a la ligne la ou l'on est *)
   | c ->
       s.line <- sub s.line 0 s.pos ^ (make 1 c) ^
  sub s.line s.pos (length s.line - s.pos) ;
       s.pos <- s.pos + 1
   end;
   change ()
      | ESC ->
   begin match c with
   | '[' -> s.seq <- SQUARE
   | _ -> s.seq <- NORMAL
   end
      | SQUARE ->
(*   gotoxy 1 1;
   printf "%d %d" s.curline s.nbline; *)
   begin match c with
     (* fleche droite *)
   | 'C' when s.pos < String.length s.line ->
       s.pos <- s.pos + 1;
       (* fleche gauche *)
   | 'D' when s.pos > 0 ->
       s.pos <- s.pos - 1;
       (* haut *)
   | 'A' when s.curline = 0 && s.line <> "" && s.nbline > 0 
     && List.hd s.history <> s.line ->
       (* on doit ajouter la ligne courante et il y en a
   deja au moins une dans l'historique *)
       s.history <- s.line :: s.history;
       s.nbline <- s.nbline + 1;
       s.line <- List.nth s.history 1;
       s.curline <- 1 ;
       s.pos <- length s.line
   | 'A' when s.curline = 0 && s.line <> "" && s.nbline > 1 ->
       (* on ne doit pas ajouter la ligne courante car c'est la 
   meme que la premiere ligne de l'historique et il y en a
   deja au moins une dans l'historique *)
       s.line <- List.nth s.history 1;
       s.curline <- 1 ;
       s.pos <- length s.line
   | 'A' when s.curline = 0 && s.nbline > 0 ->
       (* on ne doit pas ajouter la ligne courante *)
       s.line <- List.nth s.history 0;
       s.pos <- length s.line
   | 'A' when s.curline < s.nbline - 1 ->
       (* cas normal : on est en train de parcourir la liste *)
  s.curline <- s.curline + 1;
  s.line <- List.nth s.history s.curline;
       s.pos <- length s.line
   | 'A' when s.curline = s.nbline - 1 ->
       (* on arrive en bout de liste *)
       s.line <- List.nth s.history s.curline;
       s.pos <- length s.line
       (* bas *)
   | 'B' when s.curline > 0 ->
       (* on parcourt la liste dans l'autre sens *)
       s.curline <- s.curline - 1;
       s.line <- List.nth s.history s.curline;
       s.pos <- length s.line
   | 'B' (* curline = 0 *) ->
       s.line <- "";
       s.pos <- 0
    (* home *)
   | '1' | 'H' ->
       s.pos <- 0;
       if c = '1' then s.seq <- TILDE
    (* end *)
   | '4' | 'F' ->
       s.pos <- length s.line;
       if c = '4' then s.seq <- TILDE
    (* DELETE *)
   | '3' ->
       let l = length s.line in
       if s.pos < l then
  s.line <- sub s.line 0 s.pos ^ 
    sub s.line (s.pos+1) (l-s.pos-1) ;
       s.seq <- TILDE
   | _ -> ()
   end;
   if s.seq = SQUARE then s.seq <- NORMAL;
   change ()
 (* les sequences se terniment souvent pas un ~ qu'il faut faire 
    disparaitre *)
      | TILDE ->
   s.seq <- NORMAL
    done)

end

let init () =
  begin
    try 
      width := int_of_string (Sys.getenv "COLUMNS");
      height :=  int_of_string (Sys.getenv "LINES");
    with _ ->
      print_endline "$LINES/$COLUMNS not defined.";
      print_endline "try 'export LINES COLUMNS'"
  end;

  
  (* mise en place des paramettres du terminal *)
  let attr_save = Unix.tcgetattr Unix.stdin in
  let restore () = Unix.tcsetattr Unix.stdin Unix.TCSANOW attr_save in
  at_exit restore;
  let attr = Unix.tcgetattr Unix.stdin in
  attr.Unix.c_icanon <- false;
  attr.Unix.c_echo <- false;
 attr.Unix.c_vmin <- 1;
  Unix.tcsetattr Unix.stdin Unix.TCSANOW attr;
  Vt100.clrscr ();
  Input.init ()

let update () = change_info (); flush stdout

(*
let _ =
  init ();
  set_reader (fun cmd ->
    match cmd with
      Message m -> print m
    | Command cmd -> print (sprintf "COMMAND %s" cmd)
  );
  set_trailer ["TRAILER"];
  set_header ["HEADER"];
  update ();
  BasicSocket.loop ()
*)

*)

module ANSI = struct
    
    let esc =  "\027" (* 033 *)
    let esc_CHAR = '\027'

(*  Puts everything back to normal *)
    
    let ansi_NORMAL = esc ^ "[2;37;0m"     

    let ansi_CLRSCR =   esc ^ "[2J"
    let ansi_CLREOL =   esc ^ "[2K"
      
(*  Non-color based font changes *)
    
    let ansi_BOLD   = esc ^ "[1m" (* Turn on bold mode *)
    let ansi_BLINK   = esc ^ "[5m" 
(* Initialize blink mode *)
    let ansi_UNDERLINE   = esc ^ "[4m"
(* Initialize underscore mode *)
    let ansi_REVERSE  = esc ^ "[7m" (* Turns reverse video mode on *)
    let ansi_HIGH_REVERSE  = esc ^ "[1,7m" (* Hi intensity reverse video  *)

(*   Foreground Colors   *)
    
    let ansi_BLACK  = esc ^ "[30m"
    let ansi_RED  = esc ^ "[31m"
    let ansi_GREEN  = esc ^ "[32m"
    let ansi_YELLOW  = esc ^ "[33m"
    let ansi_BLUE  = esc ^ "[34m"
    let ansi_MAGENTA  = esc ^ "[35m"
    let ansi_CYAN  = esc ^ "[36m"
    let ansi_WHITE  = esc ^ "[37m"

(*   Hi Intensity Foreground Colors   *)
    
    let ansi_HIGH_RED   = esc ^ "[1;31m" 
    let ansi_HIGH_GREEN  = esc ^ "[1;32m" 
    let ansi_HIGH_YELLOW  = esc ^ "[1;33m" 
    let ansi_HIGH_BLUE   = esc ^ "[1;34m" 
    let ansi_HIGH_MAGENTA  = esc ^ "[1;35m" 
    let ansi_HIGH_CYAN   = esc ^ "[1;36m" 
    let ansi_HIGH_WHITE  = esc ^ "[1;37m" 

(*  Background Colors *)
    
    let ansi_BACKGROUND_BLACK  = esc ^ "[40m"
    let ansi_BACKGROUND_RED  = esc ^ "[41m"
    let ansi_BACKGROUND_GREEN  = esc ^ "[42m"
    let ansi_BACKGROUND_YELLOW  = esc ^ "[43m"
    let ansi_BACKGROUND_BLUE  = esc ^ "[44m"
    let ansi_BACKGROUND_MAGENTA = esc ^ "[45m"
    let ansi_BACKGROUND_CYAN  = esc ^ "[46m"
    let ansi_BACKGROUND_WHITE  = esc ^ "[47m"

(*  High Intensity Background Colors  *)
    
    let ansi_HIGH_BACKGROUND_RED  = esc ^ "[41;1m"
    let ansi_HIGH_BACKGROUND_GREEN  = esc ^ "[42;1m"
    let ansi_HIGH_BACKGROUND_YELLOW  = esc ^ "[43;1m"
    let ansi_HIGH_BACKGROUND_BLUE  = esc ^ "[44;1m"
    let ansi_HIGH_BACKGROUND_MAGENTA  = esc ^ "[45;1m"
    let ansi_HIGH_BACKGROUND_CYAN  = esc ^ "[46;1m"
    let ansi_HIGH_BACKGROUND_WHITE  = esc ^ "[47;1m"
  
  
  end
  
  (*
external get_screen_size : Unix.file_descr -> bool = "ml_get_screen_size"
external screen_width : unit -> int = "ml_screen_width"
external screen_height : unit -> int = "ml_screen_height"
*)
  
(* How do you get the size associated with a socket ? I have no book here,
I think we have to associate a pseudo-tty with the socket, but I don't
remember exactly... so, keep it simple. *)
    
    
let gotoxy x y =  Printf.sprintf "\027[%d;%dH" (y+1) (x+1) 
