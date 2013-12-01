
let (@@) f x = f x
let (|>) x f = f x

let pr fmt = Printf.ksprintf print_endline fmt
let fail fmt = Printf.ksprintf failwith fmt
