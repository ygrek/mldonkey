open Options
open CommonOptions
  
  
let protocol_version = 
  define_option downloads_ini ["protocol_version"] 
    "The version of the protocol that should be sent to servers (need restart) "
    int_option Mftp_server.protocol
  
let queued_timeout = 
  define_option downloads_ini ["queued_timeout"] 
    "How long should we wait in the queue of another client"
    float_option 3600. 
  
    
let upload_timeout = 
  define_option downloads_ini ["upload_timeout"] 
    "How long can a silent client stay in the upload queue"
    float_option 3600. 
  
  
  
  
let download_sample_rate = define_option downloads_ini ["download_sample_rate"]
  "The delay between one glance at a file and another" float_option 1.
 
let download_sample_size = define_option downloads_ini ["download_sample_size"]
    "How many samples go into an estimate of transfer rates" int_option 10
  

  