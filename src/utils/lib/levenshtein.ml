(* Levenshtein distance between two arrays of elements
   It uses O(C.length s1) space.
   The function can be curried on last argument to factorize array
   allocation costs.

   Complexity O(C.length s1 * C.length s2)
*)

type levenshtein_costs = {
  insert_cost : int;
  delete_cost : int;
  replace_cost : int;
}

module type Chain = sig
  type t
  type element
  val length : t -> int
  val get : t -> int -> element
  val equal : element -> element -> bool
end

module Make (C : Chain) = struct

  let distance lc =
    fun s1 ->
      let l1 = C.length s1 in
      let m = Array.make (l1 + 1) 0 in
      let n = Array.make (l1 + 1) 0 in
      fun s2 ->
	let l2 = C.length s2 in
	(* invariant:
	   matrix.(a).(b) = 
	   levenshtein.distance lc (String.sub s1 0 a) (String.sub s2 0 b) 
	 
	   m.(i) = matrix.(i).(j)
	   n.(i) = matrix.(i).(j+1) *)
	m.(0) <- 0;
	for i = 1 to l1 do
	  m.(i) <- m.(i - 1) + lc.delete_cost
	done;
	let rec aux j m n =
	  if j = l2 then m.(l1)
	  else
	    let c2 = C.get s2 j in
	    n.(0) <- m.(0) + lc.insert_cost;
	    for i = 1 to l1 do
	      let i1 = i - 1 in
	      n.(i) <-
		min
	        (min 
		  (n.(i1) + lc.delete_cost)
		  (m.(i) + lc.insert_cost))
	        (m.(i1) + 
		  (if C.equal (C.get s1 i1) c2 then 0 else lc.replace_cost))
	    done;
	    aux (j + 1) n m in
	aux 0 m n
end

module ChainString = struct
  type t = string
  type element = char
  let length = String.length
  let get = String.get
  let equal c1 c2 = c1 = c2
end

module ForString = Make(ChainString)

module ChainWords = struct
  type t = string array
  type element = string
  let length = Array.length
  let get = Array.get
  let equal s1 s2 = String.compare s1 s2 = 0
end

module ForWords = Make(ChainWords)
