%{
open GuiProto
%}
%token EOF
%token <string> STRING
%token <string> IDENT
%token SEMI
%token LBRACE
%token RBRACE
%start main
%type <GuiProto.gift_command> main
%%
main:
| EOF      { GiftCommand("", None, []) }
| SEMI      { GiftCommand("", None, []) }
| IDENT opt_arg keys SEMI { GiftCommand(String.lowercase $1, $2, List.rev $3) }
  ;
  
opt_arg:
| STRING    { Some $1 }
|  { None }
  ; 
  
keys:
  key keys  { $1 :: $2 }
|   { [] }
;
  
key:
  IDENT args {
    let (opt_arg, opt_keys) = $2 in
    GiftCommand(String.lowercase $1, opt_arg, opt_keys)
  }
;

args:
  STRING opt_args { (Some $1), (List.rev $2) }
| LBRACE keys RBRACE opt_arg { $4, List.rev $2 }
;
  
opt_args:
  LBRACE keys RBRACE { List.rev $2 }
|                    { [] }
;
  
  