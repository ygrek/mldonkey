
let home =
  try Sys.getenv "HOME" with _ -> ""
      
let path = 
  try Sys.getenv "PATH" with _ -> ""
      
let user =
  try Sys.getenv "USER" with _ -> ""
      