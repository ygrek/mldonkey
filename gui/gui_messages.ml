(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
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
*)

(** GUI labels. *)

let password = "Password"
let address = "Address"
let percent = "%"
let rate = "Rate"
let state = "State"
let servers = "Servers"
let downloads = "Downloads"
let friends = "Friends"
let searches = "Searches"
let options = "Options"
let not_connected = "Not connected"
let connected_to_servers x y = 
  Printf.sprintf "Connected to %d/%d server(s)" x y
let downloading_files n = Printf.sprintf "Downloading %d file(s)" n
let no_current_search = "No current search"
let connection_port = "Connection port"
let control_port = "Control port"
let gui_port = "GUI port"
let ports = "Ports"
let delays = "Delays"
let save_options_delay = "Save options delay"
let check_client_connections = "Check client connections"
let check_server_connection = "Check server connection"
let check_serverDB_connection = "Check serverDB connection"
let small_retry_delay = "Small retry delay"
let medium_retry_delay = "Medium retry delay"
let long_retry_delay = "Long retry delay"
let gui_refresh_delay = "Gui refresh delay"
let general = "General"
let name = "Name"
let max_connected_servers = "Max connected servers"
let max_connected_clients = "Max connected clients"
let disconnect = "Disconnect"
let download_limit = "Download limit (kB/s)"
let upload_limit = "Upload limit (kB/s)"
let timeouts = "Timeouts"
let server_connection = "Server connection"
let client_connection = "Client connection"
let save_and_apply_options = "Apply and save options"
let query = "Query"
let search = "Search"
let min_size = "Min size"
let max_size = "Max size"
let media = "Media"
let format = "Format"
let album = "Album"
let artist = "Artist"
let title = "Title"
let min_bitrate = "Min bitrate"
let stop_search = "Stop"
let close_search = "Close"
let mp3_options = "Mp3 options"
let submit = "Submit"
let results = "Results"
let friends = "Friends"
let ip = "IP"
let port = "Port"
let remove = "Remove"
let view_files = "View files"
let view_users = "View users"
let files = "Files"
let filename = "Filename"
let size = "Size"
let properties = "Properties"
let md4 = "MD4"
let download_selected_files = "Download selected files"
let download = "Download"
let downloaded = "Downloaded"
let availability = "Availability"
let cancel = "Cancel"
let retry_connect = "Retry connect"
let disconnect_all = "Disconnect all"
let connected_to_locations x y = 
  Printf.sprintf "Connected to %d / %d locations" x y
let add_to_friends = "Add to friends"
let add_server = "Add server"
let add_friend = "Add friend"
let connect_more_servers = "Connect more servers"
let users = "Users"
let status = "Status"
let connect = "Connect"
let disconnect = "Disconnect"
let files_to_download = "Downloading Files"
let downloaded_files n m = Printf.sprintf "Downloaded Files: %d/%d" n m
let files_downloaded n = Printf.sprintf  "%d Downloaded Files" n
let downloaded_save = "Save All Files"
let server_name = "Name"
let server_desc = "Description"
let server_nusers = "Users"
let server_nfiles = "Files"
let ed2k = "ed2k:"
let recover_md4 = "Recover MD4:"
let find_friend = "Find Friend"
let remove_old_servers = "Remove old servers"
let max_server_age = "Max server age (days)"
let help = "Help"
let max_hits = "Max Hits"
let features = "Features"
let hostname = "Client hostname"
let option_name = "Option:"
let option_value = "Value:"
let set_option = "Set Option"
let command = "Command"
let console = "Console"
let clear_console = "Clear Console"
let friend_kind = "Kind"
let friend_status = "Status"
let friend_name = "Name"
let dialog = "Chat"

(** Menus labels. *)

let mConnections = "Connections"
let kill_server = "Kill core"
let mFile = "File"
let mReconnect = "Reconnect"
let mDisconnect = "Disconnect"
let mQuit = "Quit"
let mHelp = "?"

let mes_save = "Save file as" 
let mes_edit_mp3 = "Edit mp3 tags" 


(** Messages and string constants. *)

let software = "mp3tag"
let software_version = "1.0"

(** {2 Command line messages} *)

let usage = Sys.argv.(0)^" [options] <files>\n"
let options_are = "Options are :"


(** {2 Messages} *)

let title = "Title"
let artist = "Artist"
let album = "Album"
let year = "Year"
let tracknum = "Track number"
let comment = "Comment"
let genre = "Genre"

let local_search = "Local Search"
let extended_search = "Extended Search"
    
let help_text = "

                      MLDonkey
                      ========

Release: 1.11
Authors: [b8]_bavard (Communication engine) and [b8]_FeeCarabine (GUI)

 MLDonkey is a door to the 'donkey' network, a decentralized network used to
exchange big files on the Internet. It is written in a wonderful language,
called Objective-Caml, and present most features of the basic Windows donkey
client, plus some more:
 - It should work on most UNIX-compatible platforms.
 - You can remotely command your client, either by telnet, on a WEB browser,
   or with the GTK interface. 
 - You can connect to several servers, and each search will query all the 
    connected servers.
 - You can select mp3s by bitrates in queries (useful ?).
 - You can select the name of a downlaoded file before moving it to your
   incoming directory.
 - You can have several queries in the graphical user interface at the same
  moment. 
 - You can remember your old queries results in the command-line interface.
 - You can search in the history of all files you have seen on the network.


USAGE:
======

  This package contains three files: 'mldonkey', 'mldonkey_gui' and
'downloads.ini'.

'mldonkey' is the main program, a daemon which is used to download files.
It takes no argument, and outputs some debugging messages on his terminal,
so you should not close it. So, to start your program in the background, 
you can use:

prompt> ./mldonkey > mldonkey.log &

  'mldonkey' expects to find a file called 'downloads.ini' in the directory 
where it is started. You can use the file provided in this package, or your
old one if you already used 'mldonkey' before. It contains a list of servers
that were available when the release was done. You can edit 'downloads.ini' 
to modify its parameters before starting 'mldonkey', but you can also 
modify the parameters in the graphical user interface 'mldonkey_gui'.

 'mldonkey_gui', the graphical user interface, can be started by:

prompt> ./mldonkey_gui &

 'mldonkey_gui' doesn't need to be started in the same directory as
'mldonkey'. Without parameters, it expects to find 'mldonkey' running on the
same computer on the default port. If you started 'mldonkey' on another
computer, you should specify the hostname (myhost.mydomain.mydot for example)
on the command line:

prompt> ./mldonkey_gui myhost.mydomain.mydot

 If you also modified the GUI port, you can also specify it (here 9999 for
example) on the command line:

prompt>  ./mldonkey_gui myhost.mydomain.mydot 9999

 You can also start the GUI, and modify these settings in the Options panel,
and then try to reconnect. This is anyway necessary if you have put a
password.

 Instead of using the GUI, you can also telnet to the daemon:

prompt> telnet localhost 4000

(on your local computer) or 

prompt> telnet myhost.mydomain.mydot

(if the client was started on that host).

Using my old config:
===================
  Connect to mldonkey by telnet:

prompt> telnet localhost 4000

  Use the 'import' command with the donkey directory containing your old
config as parameter.

import '/.../donkey2000'

  All the files which were currently downloading will be moved to mldonkey
temp directory, and the server list will be imported too. 

  If you only want to import the server.met file, use the 'servers' command:

servers '/.../donkey2000/server.met'

  Filenames containing special characters (such as spaces) should be put
inside ''.


Frequently Asked Questions
==========================

*) How can I find new versions ? 
--------------------------------

 Simply search for mldonkey on the donkey network or forums ... However, we
don't plan to work a lot on this software now it is working correctly, so new
versions will probably be seldom, after probably some bug fixes in the first
release.

*) How can I contact the authors ? 
----------------------------------

You can reach us for a short time on 'mldonkey@monsieurcinema.com'. Please
don't bother us too much with questions on how to use mldonkey. We prefer bug
reports, containing USEFUL information to find the bug. 

 We will not make a forum for mldonkey. We are looking for a volonteer to do
that, probably on one of the current donkey sites. We will probably
anonymously subscribe to the list to hear about users problems.

  We are also looking for a volonteer to rewrite/correct the documentation.
French people are not native english speakers and writers.

*) What about the sources ? What about the protocol ?
-----------------------------------------------------

 We will not release the sources nor the protocol yet, because selfish guys
could easily write their own donkey software, disabling upload. We want to
make this as hard as possible. So you will have to do like us, find the
protocol on your own.

*) I added a password, and now the connection between the client and
  the GUI is immediatly aborded at startup. What should I do ?
--------------------------------------------------------------

  Start the GUI. In the Options panel, type your password, and ENTER.
 Then reconnect to the client (menu or CTRL-R).

*) How can I see the upload informations ?
------------------------------------------

You can't. For psychological reasons, we didn't want to display the upload
informations. However, you can change your maximal upload rate in the Options
panel. Minimum is 1 kB/s.

You can also disable upload for short periods of time with the 'nu' command.
Before you must have ran mldonkey at least 5*m minutes if you want to 
disable upload for m minutes. You cannot disable upload for more than 
5 hours per day. Your credit can be displayed with the 'vu' command.

*) How can I modify parameters that don't appear in the GUI nor in the
command-line client ? 
----------------------

mldonkey uses a file called 'downloads.ini'. You must stop your donkey client
(use the Kill menu in the GUI or the 'kill' command in the telnet client).
Then, edit this file and change the values of the options you want.

Some options can also be modified by the 'set' command (in the command-line
client or in the console of the GUI). These options appear when you use
the 'vo' command.

*) How can I communicate with the client ?
------------------------------------------

There are three ways to communicate with the client. In all the cases, you 
can run the client on one computer and control it from another computer.
In these examples, we suppose they both run on the local computer 'localhost':

  1) Use the GUI, called 'mldonkey_gui':

prompt> ./mldonkey_gui localhost

  2) Use the command-line client:

prompt> telnet localhost 4000

  3) Use a WEB browser:

prompt> lynx http://localhost:4080/

*) How can use the GUI on MacOS X ?
-----------------------------------

First, you need to have an X server installed. See for more information:

  http://fink.sourceforge.net/doc/x11/index.php

You will find a script in the tar file to start the GUI, that set the
correct library path for most systems. If it doesn't work, you need to
modify it according to your non-standard configuration.

*) Previewing doesn't work ?
----------------------------

mldonkey tries to start a script, specified by the 'previewer' option.
By default, this option calls 'mldonkey_previewer', which must be in your
path. Its first arg is the name of the file on the local disk, which its
second arg is the name of the file on the donkey network.

Help on the command-line interface
==================================

In a different shell, telnet to your application by:

prompt> telnet localhost 4000

4000 is the default port for connecting with the command-line client. 
Command-line client is disabled if you have set a password.

Then, use ? to find some help on available commands. Some commands are only 
available in the graphical interface (setting options for example), others
only in the command-line interface (import of old donkey config for example).

Here is the output of the help command:
n  <ip> [<port>]: add a server
vu  : view upload credits
nu  <m> : disable upload during <m> minutes (multiple of 5)
import  <dirname> : import the config from dirname
x  <num> : disconnect from server
servers  <filename> : add the servers from a server.met file
commit : move downloaded files to incoming directory
vd <num>: view file info
reshare  : check shared files for removal
vm : list connected servers
vma : list all known servers
q : close telnet
kill : save and kill the server
save : save
d <size> <md4> : download this file
upstats  : statistics on upload
port  <port> : change connection port
vo  : print options
set  <option_name> <option_value> : change option value
vr   [<num>]: view results of a search
forget  <num> : forget search <num>
ls  <query> : local search
s  <query> : search for files

        With special args:
        -minsize <size>
        -maxsize <size>
        -media <Video|Audio|...>
        -Video
        -Audio
        -format <format>
        -field <field> <fieldvalue> :

vs : view all queries
cancel  <num> : cancel download
xs : extended search
clh  : clear local history
c  [<num>]: connect to more servers (or to server <num>)


Help on the graphical user interface
====================================
1. Servers page:

The server page presents the list of known servers. For each server, the list
provides information on its IP address, port, connexion status, name, 
description, number of users and number of files.

The list is automatically updated when new servers are discovered. MLDonkey 
tries to automatically connect to servers until enough (see the
'Max connected servers' option) of them have replied.

To add a server: enter its IP and Port, and press the 'Add server' button.
To remove a server: select the server, and click the 'Remove' button.
To connect a server: select the server, and click the 'Connect' button.
To view server users: select the server, and click the 'View Users' button.
  If the list of users is already available, it is automatically displayed in
  the list on the right when the server is selected.
To add some user to your friend list: select the users, and click the
  'Add to friends' button. Direct users (those with a valid IP address)
  are immediatly added, while others are only added when they are
  connected.
The 'Connect more servers' button: if not enough servers are connected, 
  you can click this button to speed connections to servers.
The 'Remove old servers' button: this button allows to clean the list of
  servers which have not been connected for a while (see the
  'Max server age' option).

2. Downloads page:

When you select a file, its full name is displayed under the list of
files. A bar indicates which chunks of the file are available:
red means the chunks is not available and blue is for chunks which are
available on only one client. Black is for chunks that are available on 
several clients, whereas green means you already have the chunk.
Chunks are 9mb of consecutive bytes in the file.

You can put ed2k:// URLs (those found on www.sharereactor.com) in the 
upper entry (after the ed2k: label). Press ENTER then.

If you started a download and lost the config files for some reason, you can
recover it in the upper right entry (Recover MD4) if it is still present under
this MD4 in your temp/ directory.

When selecting a file in the 'Downloading' list, the GUI will display the list
of its locations on the right side. You can select some of these locations,
and click the 'Add to friends' button to add them to your friends.

When a download is finished, files are displayed in the 'Downloaded' list.
You can use the 'Save all files' button to move these files to your
incoming/ directory. This is not done automatically. You can also click on the
right button of the mouse to have a contextual menu where you can select the
name of the file. You can edit the tags of mp3 too in this menu.

3. Friends page:

Friends are displayed on the left, with the list of their files on the
right. You can remove a friend by selecting it and clicking the 'Remove' 
button. You can also search all connected servers for a friend by
putting its name in the 'Find Friend' entry and pressing ENTER.

You can select files in the list on the right, then click 
'Download Selected Files' to add them to your download list.

4. Queries page:

'Max Hits' is not working yet. 

Use the 'Stop' button to stop receiving results for a query. 

5. Options page:

This page can be used to update the simple options of the configuration
file 'downloads.ini', which can be found in the directory where 
'donkey_downloads' was started.

Interesting options are:

'Name': your name on the donkey network.

'Max Connected Servers': the maximum number of servers you can remain 
connected to. This is an old option, that was useful before UDP packets.
1 is now enough, since all servers will eventually be searched by UDP.

'Upload limit': default is 30 kB/s (good for ADSL/Cable). You can't set it
  under 10kB/s. If you have a large bandwith, set it to 500 kB/s or +

'Client hostname': the name of the host were your client is running if not
  the same host as the graphical interface.

'Password': the password used to control your client.

The different ports that can be modified in the Option panel can be used to 
allow several clients to run on the same computer. Moreover, several user
interfaces can connect to the same client at the same time (command-line
interfaces and graphical interfaces).

6. Console

In the console, you have access to the command-line commands.

TODO list
=========
  * Chat.
  * Set 'Connected locations' label.
  * Plugins.
  * Correct display of availability.
  * Add sleep and wakeup commands.
  * More options in GUI
  * Popup et historique des dialogues.
  * Admin open file descrs.
  * Keep (server_ip, server_port, id) for indirect connections.
  * Manager of shared files/direcxtories.

Known bugs:
===========
  * When clicking on the columns it sorts on that column, when
   clicking again it should do a reverse sort on it
  * the layout is not saved

ChangeLog
=========

Release 1.11:
  * Option 'shared_directories' to specify other directories where files to
   share can be found.
  * Setting 'telnet_port', 'http_port' or 'gui_port' to 0 disables the
      corresponding access.
  * Console: 
   - Sort results by size in vr.
   - When 'd' fails with Already_done, you can use 'force_download' to force
      an already downloaded file to be restarted.
   - New command 'dllink' taking an ed2k URL to download as argument.
   - Command 'set' can be used to set the allowed_ips option:
       set allowed_ips '127.0.0.1 128.91.255.255' 
     255 is for networks.
   - shorten names in 'vd' output (see 'max_name_len' option). 
  * WEB:
   - New Cancel links in 'vd' output.
   - New 'Complex Search' link in interface.
   - 'View Options' output can be edited to modify options.
  * GUI:
   - The GUI tries to connect to the core every second until it succeeds.
   - Corrected MD4 columns
   - In search panel, 'Max hits' combo set the maximal number of replies
      per server.
  * Bug fixes:
   - Don't add twice the incoming dir to shared file names.
   - Cross-partition moves fail, but filename is renamed in temp dir.

Release 1.10:
  * Default timeout for server connection set to 5 for faster connection.
  * Options are now divided into four configuration files: downloads.ini,
      servers.ini, friends.ini and files.ini.
  * Console: better display of downloads.
  * Verification of IP addresses for admin ('allowed_ips' option).
  * WEB interface: added pragma no-cache. 
  * Keep checking servers in the background.
  * GUI: Auto-resize is now delayed. Default selection
     mode is EXTENDED. Contextual menu items to switch between
    auto-resize/manual resize and EXTENDED/MULTIPLE selection.
  * Send a mail when download is finished. You must complete the
    'smtp_server', 'smtp_port' and 'mail' options.
  * Bug fixes: partial page in WEB interface fixed (often with vr).
      Removed two minutes timeout on good client connections.
      NoSpaceLeft causes pause in download.

Release 1.09:
  * Bug fixes: fixed bad md4 computation of the first chunk, preventing
     completion of downloads.
  * New command 'recover_temp' to recover all lost files from the temp
     directory.
  * See  http://www.freesoftware.fsf.org/mldonkey/ for porting mldonkey on new OSes.

Release 1.08:
  * Remove use of the 'mmap' system call.
  * Better management of delays between connection attempts.
  * Local searches use minsize/maxsize information.
  * More information on availability on WEB/console interface.
  * Small improvement in MD4 computation. Shared files MD4s are saved in
     a 'shared_files.ini' file to avoid computating them again.
  * Filters on replies: set the 'filters' option to a string containing
     sub-words you want to remove, separated by spaces.
  * Maximal download rate can be specified in 'max_download_rate' option.
   (need probably to be rewritten more carefully).
  * Bug fixes: files greater than 9mb in incoming/ can now be shared,
      ftruncate replaced by lseek (for vfat FS), multiple connections to
      the same client fixed.

Release 1.07:
  * Exchange of sets of sources between mldonkey clients.
  * Started implementing chat between friends.
  * GUI allows adding friends by specifying IP and port. New 
     'Connect friend' menu. Better properties of files.
  * New menu item 'preview' in GUI, associated with option 'previewer'.
  * New menu 'Select All' in lists.
  * Bug fixes: in mp3 tags edition, in connected servers count, in
    displaying friends files, in passive connections.

Release 1.06:
  * History of seen files. New commands: 'ls' for local search on the history,
    of search on servers, 'clh' for clear local history. New options: 
    'use_file_history' to allow history on files, 'save_file_history' to
    save the history on disk. For now, search only by words (not size).
  * Authentification by the 'auth' command on the telnet client.
  * Bug fixes: servers disconnected viewed as connected.

Release 1.05:
 * UDP protocol implemented: extended searches to all servers.
 * Improved HTTP interface (port 4080).
 * Limitation on minimal upload dropped to 1 kB/s.
 * Fixed bug preventing queries on multiple words.

Release 1.04:
  * Improved WEB interface (download files by clicking).
  * Stats on upload.

Release 1.03:
  * Upload can be disabled temporarily, if your client has been running long
   enough.

Release 1.02:
  * max_upload_rate is not modified automically anymore.
  * command-line client available from the GUI.

Release 1.01:
  * New command 'set' in command-line client to change options values
  * Command-line client accessible from the GUI or any 
   WEB browser (at http://localhost:4080/)

Release 1.00:
  * Upload/download works
  * Friends works
  * Complex queries works
  * Command-line interface works
  * Graphical interface works
  * Import old config works



"   (* PUT NOTHING AFTER THIS LINE !!!!!!! *)
