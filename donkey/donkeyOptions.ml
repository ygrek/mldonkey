open Options
open CommonOptions
  
  
let protocol_version = 
  define_option downloads_ini ["protocol_version"] 
    "The version of the protocol that should be sent to servers (need restart) "
    int_option 59
  
let queued_timeout = 
  define_option downloads_ini ["queued_timeout"] 
    "How long should we wait in the queue of another client"
    float_option 3600. 
  
    
let upload_timeout = 
  define_option downloads_ini ["upload_timeout"] 
    "How long can a silent client stay in the upload queue"
    float_option 3600. 
  
let upload_power = define_option downloads_ini ["upload_power"]
  "The weight of upload on a donkey connection compared to upload on other
  peer-to-peer networks. Setting it to 5 for example means that a donkey 
  connection will be allowed to send 5 times more information per second than
  an Open Napster connection. This is done to favorise donkey connections
  over other networks, where upload is less efficient, without preventing
  upload from these networks." int_option 5

let propagate_servers = define_option downloads_ini ["propagate_servers"]
  "Send an UDP packet to a central servers with the list of servers you
  are currently connected to, for the central server to be able to
    generate accurate server lists." bool_option true

let files_queries_per_minute = define_option downloads_ini
    ["files_queries_per_minute"] 
  "Maximal number of localisation queries that can be sent to
  one server per minute. Some servers kick clients when this
  value is greater than 1" int_option 1

let files_queries_initial_delay = define_option downloads_ini
    ["files_queries_initial_delay"] 
  "Initial delay after sending the first localisation queries to
  a server, before sending other localisation queries." int_option 20
  
let _ = 
(* Clients should never send more than 5 localisations queries
per minute. Greater values are bad for server ressources.  *)
  option_hook files_queries_per_minute (fun _ ->
      if !!files_queries_per_minute > 5 then 
        files_queries_per_minute =:= 5)
      
  