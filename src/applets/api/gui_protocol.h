/* Copyright 2002 b8_fange */
/*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef _GUI_PROTOCOL_H
#define _GUI_PROTOCOL_H

#include "endianess.h"


/* The info necessary to connect to mldonkey. login and password can be NULL.
*/
typedef struct mldonkey_config {
  char *mldonkey_hostname;
  int mldonkey_port;
  char *mldonkey_login;
  char *mldonkey_password;
} mldonkey_config;

/* The MLDonkey status returned */

typedef struct mldonkey_info {
  int64  upload_counter;
  int64 download_counter;
  int nshared_files;
  int64 shared_counter;
  int  tcp_upload_rate;
  int  tcp_download_rate;
  int  udp_upload_rate;
  int  udp_download_rate;
  int  ndownloaded_files;
  int ndownloading_files;
  int nconnected_networks;
  int connected_networks[1];
} mldonkey_info;

/* Call this function to update the information about mldonkey.
Note that the function will not reconnect to mldonkey if the
pointer to the mldonkey_config has not changed. As it uses static
data, it cannot be used in a multithreaded env. 

Returns 1 if connected and info filled, 0 if connected but not filled,
-1 otherwise.
*/


enum to_gui {
  CoreProtocol, /* 0 */
  Options_info,
  RESERVED2,
  DefineSearches,
  Result_info,
  Search_result,
  Search_waiting,
  File_info,
  File_downloaded,
  File_availability,
  File_source, /* 10 */
  Server_busy,
  Server_user,
  Server_state,
  Server_info,
  Client_info,
  Client_state,
  Client_friend,
  Client_file,
  Console,
  Network_info, /* 20 */
  User_info,
  Room_info,
  Room_message,
  Room_add_user,
  Client_stats,
  Server_info_v2,
  MessageFromClient,
  ConnectedServers,
  DownloadFiles,
  DownloadedFiles, /* 30 */
  Room_info_v2,
  Room_remove_user,
  Shared_file_info,
  Shared_file_upload,
  Shared_file_unshared,
  Add_section_option,
  Client_stats_v2,
  Add_plugin_option,
  Client_stats_v3,
  File_info_v2,      /* 40 */
  DownloadFiles_v2,
  DownloadedFiles_v2,
  File_info_v3,
  DownloadFiles_v3,
  DownloadedFiles_v3,
  File_downloaded_v2,
  BadPassword,
  Shared_file_info_v2,
  Client_stats_v4, /* 49 */
};

extern int get_mldonkey_status(mldonkey_config *, mldonkey_info *);

#define MLDONKEY_DISCONNECTED   0
#define MLDONKEY_CONNECTING     1
#define MLDONKEY_AUTHENTICATING 2             
#define MLDONKEY_CONNECTED      3


#endif
