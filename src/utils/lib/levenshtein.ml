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
      let current_row = Array.make (l1 + 1) 0 in
      let next_row = Array.make (l1 + 1) 0 in
      fun s2 ->
        let l2 = C.length s2 in
        (* invariant:
           matrix.(a).(b) = 
           levenshtein.distance lc (String.sub s1 0 a) (String.sub s2 0 b) 
         
           current_row.(i) = matrix.(i).(j)
           next_row.(i) = matrix.(i).(j+1) *)
        current_row.(0) <- 0;
        for i = 1 to l1 do
          current_row.(i) <- current_row.(i - 1) + lc.delete_cost
        done;
        let min_3int a b c : int =
          let min = if a <= b then a else b in
          if min <= c then min else c in
        let rec aux j current_row next_row =
          if j = l2 then current_row.(l1)
          else
            let c2 = C.get s2 j in
            next_row.(0) <- current_row.(0) + lc.insert_cost;
            for i = 1 to l1 do
              let i1 = i - 1 in
              next_row.(i) <-
                min_3int
                  (next_row.(i1) + lc.delete_cost)
                  (current_row.(i) + lc.insert_cost)
                  (if C.equal (C.get s1 i1) c2 then current_row.(i1) 
                   else current_row.(i1) + lc.replace_cost)
            done;
            aux (j + 1) next_row current_row in (* swap rows *)
        aux 0 current_row next_row
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
