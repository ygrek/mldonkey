open CommonTypes
open DonkeyTypes

val count_seen : DonkeyTypes.client -> unit
val count_banned : DonkeyTypes.client -> unit
val count_filerequest : DonkeyTypes.client -> unit
val count_download : DonkeyTypes.client -> DonkeyTypes.file -> Int64.t -> unit
val count_upload : DonkeyTypes.client -> DonkeyTypes.file -> Int64.t -> unit


val brand_to_string : brand -> string
val print_stats : Buffer.t -> unit

