val home_dir : string
val installer_ini : Options.options_file
val installer_section : Options.options_section
val mldonkey_directory : string Options.option_record
val file_basedir : string
val home_basedir : string
val cmd_basedir : string
val html_themes_dir : string
val downloads_ini : Options.options_file
val servers_ini : Options.options_file
val searches_ini : Options.options_file
val files_ini : Options.options_file
val friends_ini : Options.options_file
val servers_section : Options.options_section
val ip_list_option : Ip.t list Options.option_class
val dir_option : (string * int) Options.option_class
val allow_browse_share_option : int Options.option_class
val addr_option : (string * int) Options.option_class
val is_not_spam : (string -> bool) ref
val random_letter : unit -> char
val new_name : unit -> string
val main_section : Options.options_section
val interfaces_section : Options.options_section
val bandwidth_section : Options.options_section
val networks_section : Options.options_section
val network_section : Options.options_section
val html_section : Options.options_section
val debug_section : Options.options_section
val download_section : Options.options_section
val startup_section : Options.options_section
val mail_section : Options.options_section
val path_section : Options.options_section
val mlchat_section : Options.options_section
val security_section : Options.options_section
val other_section : Options.options_section
val current_section : Options.options_section
val client_name : string Options.option_record
val current_section : Options.options_section
val allowed_ips : Ip.t list Options.option_record
val gui_port : int Options.option_record
val gift_port : int Options.option_record
val http_port : int Options.option_record
val telnet_port : int Options.option_record
val http_bind_addr : Ip.t Options.option_record
val gui_bind_addr : Ip.t Options.option_record
val telnet_bind_addr : Ip.t Options.option_record
val current_section : Options.options_section
val max_hard_upload_rate : int Options.option_record
val max_hard_download_rate : int Options.option_record
val max_opened_connections : int Options.option_record
val max_upload_slots : int Options.option_record
val dynamic_slots : bool Options.option_record
val max_connections_per_second : int Options.option_record
val current_section : Options.options_section
val enable_server : bool Options.option_record
val enable_overnet : bool Options.option_record
val enable_bittorrent : bool Options.option_record
val enable_donkey : bool Options.option_record
val enable_opennap : bool Options.option_record
val enable_soulseek : bool Options.option_record
val enable_audiogalaxy : bool Options.option_record
val enable_gnutella : bool Options.option_record
val enable_gnutella2 : bool Options.option_record
val enable_fasttrack : bool Options.option_record
val enable_directconnect : bool Options.option_record
val enable_openft : bool Options.option_record
val current_section : Options.options_section
val html_mods : bool Options.option_record
val html_mods_style : int Options.option_record
val html_mods_human_readable : bool Options.option_record
val html_mods_use_relative_availability : bool Options.option_record
val html_mods_vd_network : bool Options.option_record
val html_mods_vd_active_sources : bool Options.option_record
val html_mods_vd_age : bool Options.option_record
val html_mods_vd_last : bool Options.option_record
val html_mods_vd_prio : bool Options.option_record
val html_mods_vd_queues : bool Options.option_record
val html_mods_show_pending : bool Options.option_record
val html_mods_load_message_file : bool Options.option_record
val html_mods_max_messages : int Options.option_record
val html_mods_bw_refresh_delay : int Options.option_record
val html_mods_theme : string Options.option_record
val use_html_mods : CommonTypes.ui_conn -> bool
val html_checkbox_file_list : bool Options.option_record
val current_section : Options.options_section
val set_client_ip : Ip.t Options.option_record
val force_client_ip : bool Options.option_record
val web_infos : (string * int * string) list Options.option_record
val tcpip_packet_size : int Options.option_record
val mtu_packet_size : int Options.option_record
val minimal_packet_size : int Options.option_record
val network_update_url : string Options.option_record
val mlnet_redirector : (string * int) Options.option_record
val current_section : Options.options_section
val smtp_server : string Options.option_record
val smtp_port : int Options.option_record
val mail : string Options.option_record
val add_mail_brackets : bool Options.option_record
val filename_in_subject : bool Options.option_record
val current_section : Options.options_section
val auto_commit : bool Options.option_record
val commit_unverified_files : bool Options.option_record
val max_concurrent_downloads : int Options.option_record
val delete_original : bool Options.option_record
val file_completed_cmd : string Options.option_record
val current_section : Options.options_section
val run_as_user : string Options.option_record
val run_as_useruid : int Options.option_record
val ask_for_gui : bool Options.option_record
val start_gui : bool Options.option_record
val current_section : Options.options_section
val temp_directory : string Options.option_record
val incoming_directory_prio : int Options.option_record
val incoming_directory : string Options.option_record
val bin_dir : string
val previewer : string Options.option_record
val mldonkey_bin : string Options.option_record
val mldonkey_gui : string Options.option_record
val shared_directories : (string * int) list Options.option_record
val shared_extensions : string list Options.option_record
val current_section : Options.options_section
val allowed_commands : (string * string) list Options.option_record
val users : (string * Md4.Md4.t) list Options.option_record
val empty_password : string -> bool
val allow_browse_share : int Options.option_record
val messages_filter : string Options.option_record
val current_section : Options.options_section
val chat_app_port : int Options.option_record
val chat_app_host : string Options.option_record
val chat_port : int Options.option_record
val chat_bind_addr : Ip.t Options.option_record
val chat_console_id : string Options.option_record
val chat_warning_for_downloaded : bool Options.option_record
val current_section : Options.options_section
val buffer_writes : bool Options.option_record
val buffer_writes_delay : float Options.option_record
val buffer_writes_threshold : int Options.option_record
val client_timeout : float Options.option_record
val interface_buffer : int Options.option_record
val max_name_len : int Options.option_record
val term_ansi : bool Options.option_record
val update_gui_delay : float Options.option_record
val http_realm : string Options.option_record
val use_html_frames : bool Options.option_record
val commands_frame_height : int Options.option_record
val filter_search_delay : float Options.option_record
val motd_html : string Options.option_record
val compaction_delay : int Options.option_record
val vd_reload_delay : int Options.option_record
val minor_heap_size : int Options.option_record
val min_reask_delay : int Options.option_record
val max_reask_delay : int Options.option_record
val display_downloaded_results : bool Options.option_record
val filter_table_threshold : int Options.option_record
val client_buffer_size : int Options.option_record
val save_options_delay : float Options.option_record
val server_connection_timeout : float Options.option_record
val new_print_search : bool Options.option_record
val download_sample_rate : float Options.option_record
val download_sample_size : int Options.option_record
val calendar : (int list * int list * string) list Options.option_record
val ip_cache_timeout : int Options.option_record
val compaction_overhead : int Options.option_record
val max_displayed_results : int Options.option_record
val options_version : int Options.option_record
val current_section : Options.options_section
val log_size : int Options.option_record
val log_file : string Options.option_record
val verbosity : string Options.option_record
val current_section : Options.options_section
val last_high_id : Ip.t ref
val client_ip : TcpBufferedSocket.t option -> Ip.t
val start_running_plugins : bool ref
val verbose_msg_servers : bool ref
val verbose_msg_clients : bool ref
val verbose_src_manager : bool ref
val verbose_src_prop : bool ref
val verbose : bool ref
val verbose_download : bool ref
val verbose_upload : bool ref
val verbose_unknown_messages : bool ref
val verbose_overnet : bool ref
val verbose_location : bool ref
val verbose_share : bool ref
val verbose_md4 : bool ref
val verbose_connect : bool ref
val http_proxy :  (string * int) option ref
  