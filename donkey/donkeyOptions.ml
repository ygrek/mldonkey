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

  