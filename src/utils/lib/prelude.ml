(** Useful shortcuts *)

let ($) f g = fun x -> f (g x)
let ($$) f g = fun x y -> f (g x) (g y)

(** reverse apply : [x |> f |> g] is equivalent to [g (f x)] *)
let (|>) x f = f x
(* external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply" *)
(** apply : [g \@\@ f \@\@ x] is equivalent to [g (f x)] *)
let (@@) f x = f x
(* external (\@\@) : ('a -> 'b) -> 'a -> 'b = "%apply" *)

external id : 'a -> 'a = "%identity"
let curry f a b = f (a, b)
let uncurry f (a,b) = f a b
let flip f x y = f y x
let cons x y = x :: y
let tuck l x = l := x :: !l
let some x = Some x
