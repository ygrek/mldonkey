
                      MLDonkey
                      ========

Release: 1.15
Authors: [b8]_bavard (Communication engine) and [b8]_Zoggy (GUI)

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
 - You can select the name of a downloaded file before moving it to your
   incoming directory.
 - You can have several queries in the graphical user interface at the same
    time. 
 - You can remember your old queries results in the command-line interface.
 - You can search in the history of all files you have seen on the network.


USAGE:
======

  This package contains three files: 'mldonkey', 'mldonkey_gui' and
servers.ini'.

'mldonkey' is the main program, a daemon which is used to download files.
It takes no argument, and outputs some debugging messages on his terminal,
so you should not close it. So, to start your program in the background, 
you can use:

prompt> ./mldonkey > mldonkey.log &

  'mldonkey' expects to find several .ini files in the directory 
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

Using the config the standard core :
===================================
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

*) How can I contact the authors ?
----------------------------------

You can reach us for a short time on 

      mldonkey@monsieurcinema.com

Please don't bother us too much with questions on how to use mldonkey. We
prefer bug reports, containing USEFUL information to find the bug. You can
also submit bugs on the savannah WEB site:

  http://www.freesoftware.fsf.org/mldonkey/

For advices on how to use mldonkey, you can check several forums.
In particular, 

  http://forums.edonkey2000.com/

contains an interesting linux topics.

*) What about the sources ? What about the protocol ?
-----------------------------------------------------

Most sources are available on the savannah WEB site, except the protocol
files:

  http://www.freesoftware.fsf.org/mldonkey/

However, the distribution contains enough information to port mldonkey
on all systems where Objective-Caml has been ported. In the future, we will
provide additionnal files with a pseudo-protocol, so that contributors can
test their changes on a local network before sending them to us.

We will not release the protocol, because we don't want to be accused of
being the murderers of the eDonkey network. However, since our knowledge of
the protocol is only partial, we are interested in any information on it.

*) The GUI/telnet/WEB can't connect to the core.
-----------------------------------------------

Since version 1.12, there is an option to control which computers can 
connect to mldonkey: allowed_ips

By default, this option only allows your local computer to connect to the 
core. You can change this either by editing the downloads.ini file, or, in 
the console, using the

set allowed_ips '127.0.0.1 A.B.C.D ...'

command, where A.B.C.D ... are IP addresses separated by spaces.

*) I added a password, and now the connection between the client and
  the GUI is immediatly aborded at startup. What should I do ?
--------------------------------------------------------------

  Start the GUI. In the Options panel, type your password, and ENTER.
 Then reconnect to the client (menu or CTRL-R).

*) How can I see the upload information ?
-----------------------------------------

In mldonkey, you cannot directly know the current upload state of your
core. There is a 'upstats' command which can be used to get sorted
information on the files which have been requested.

Two options are used to control upload:
- 'max_upload_rate' controls the maximal bandwidth you accept to provide
on upload. The minimun is 1 (kB/s).
- 'shared_directories' is the list of directories that you want to share.
By default, the list is empty, since the temp/ and incoming/ dirs are always
shared.

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
path. Its first arg is the name of the file on the local disk, while its
second arg is the name of the file on the donkey network.

*) I started mldonkey, and it didn't connect to any server !
-----------------------------------------------------------

Depending on your list of servers, the process of finding a server available
can take a while. You can try to modify some options, such as the server
connection timeout or the delay between connections attempts. If you know
a good server, use 'c 34' if 34 is for example the number of the server
in the list 'vma'. You can also select some servers in the GUI, and use the
connect button/menu.

*) Which ports should I open on my firewall ?
--------------------------------------------

By default, mldonkey uses ports 4662 for tcp connections, and port 4666 for
udp connections. If you change the tcp port, udp port will be tcp_port + 4.
Therefore, you should allow your firewall to send incoming connections and
messages on these ports to your local network.


Help on the command-line interface
==================================

In a different shell, telnet to your application by:

prompt> telnet localhost 4000

4000 is the default port for connecting with the command-line client. 
Command-line client is disabled if you have set a password.

Then, use ? to find some help on available commands. Some commands are only 
available in the graphical interface (setting options for example), others
only in the command-line interface (import of old donkey config for example).

Here is the output of the help command for version 1.12:
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
        -field <field> <fieldvalue>
        -not <word>
        -and <word> 
        -or <word> :

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
  under 1kB/s. If you have a large bandwith, set it to 500 kB/s or +

'Client hostname': the name of the host were your client is running if not
  the same host as the graphical interface.

'Password': the password used to control your client.

The different ports that can be modified in the Option panel can be used to 
allow several clients to run on the same computer. Moreover, several user
interfaces can connect to the same client at the same time (command-line
interfaces and graphical interfaces).

6. Console

In the console, you have access to the command-line commands.

Using auxiliary programs for local indexation (in development)
=============================================

mldonkey now uses auxiliary programs to help find results to search.
Currently, two types of programs are supported:

- Finder (set by the 'local_index_find_cmd' option)
 The finder receives a query on its standard input, and replies by
 the results on its standard output.

Query format: the query finishes with 'end_query' on one line. On each line 
of the query, there is one keyword, a colon :, and a value. Keywords are:
 words, minsize, maxsize, minrate, media, format, title, album, artist

Result format: the result finishes with 'end result' on one line. On each line
of the result, there is one keyword, a colon :, and a value.
Keywords are:
  Required: md4, size
  Optional: name, format, type, string_tag, int_tag

There can be several name, string_tag, int_tag lines. The value on the 
string_tag line should be the name of the tag, a colon : and the value of
the tag. Idem for int_tag, but the value should be an integer.

- Indexer (set by the 'local_index_add_cmd' option)
The indexer is called each time a new result is received by mldonkey,
and the result is given on its standard input in the same format as specified
above. It can be used to add the result to the index that is used by the
Finder.

TODO list for client
====================
  * Plugins.
  * Correct display of availability.
  * Popup
  * Keep (server_ip, server_port, id) for indirect connections.
  * Manager of shared files/directories.
  * Add Friends in console and WEB. 
  * Allow hostname instead of IPs in servers and friends.
  * Queue uploads per chunk.
  * Add prefered servers.
  * Save options in a modular way (each server in a file ?).
  * Recommandation for upload.
  * Download priorities (what does it mean ?).
  * Use source groups for local downloads.
  * Use a cache of data to help diffusing files.
  * Check that program exists before trying to execute.
  * Correct boolean searches
  * List of prefered servers
  * Fix BUFFER OVERFLOW bug !!!!!
  * Customized queries in GUI
  * Clean the clients_by_num table from clients which are not useful anymore.
  * verify that interesting_clients that can't connect are correctly removed.
  * remove locations of downloaded files.
  * check that 'cancelled' files cannot be shared also in incoming/
  * don't add sources to files already downloaded.
  * efficient management of buffers
  * background searches: 
   - search and automatically download files


TODO list for server
====================
* Send a ping to all clients every minute
* Implement the whole protocol for TCP clients
* Implement the whole protocol for UDP clients
* Implement the protocol for UDP servers
* Implement indexation
* Implement localisation

Known bugs:
===========
  * When clicking on the columns it sorts on that column, when
   clicking again it should do a reverse sort on it, but it doesn't.

ChangeLog
=========

Release 1.15:
  * Started implementing mldonkey_s (eDonkey compatible server)
  * GUI:
    - Should fit in 640x480 screens.
    - Server can be specified by ip:port in Add Server entry.
    - Removed some non-tailrecursive functions.
  * Core acts now with all (direct) clients as a server to diffuse sources
     for files. Indirect mldonkey clients can also receive information.
  * WEB interface:
    - New option 'customized_searches' that can be used to add new
       queries in the WEB interface (see new option file searches.ini
       for examples).
    - Improved searches (work also in the GTK GUI): 
       * In the keyword/artist/title/album fields, a '-' in front of
        a word means 'without'. Words separated by
        spaces must appear in the reply. 
       * In the format field: you can specify several formats, separated by
        spaces. A '-' in front of a format means you don't want that format.
    - New options 'web_header' and 'web_header_frame' to customize the header
       displayed by the Web interface.
    - Generate correct HTML with HEAD and TITLE :)
    - Downloads can be sorted by name/percent/downloaded/size/rate.
    - Paused downloads are correctly displayed.
    - Use tables instead of spaces (for non-fixed fonts).
    - Added option 'use_html_frames'. When frames are enabled, clicking
       on a result to download will only modify the second frame.
    - Checklist of downloads and results can be activated/desactivated with
      the option 'html_checkbox_file_list'.
    - The number of days since the file was last seen complete is now displayed
  * Core:
    - New option 'file_completed_cmd' for a command which is called when 
     a file download is completed with as arguments: 
     <filename on disk> <md4> <size> <names on the edonkey networks>*
    - Started moving local indexation from mldonkey to an auxiliary program,
     controled by options 'local_index_find_cmd' and 'local_index_add_cmd'.
     See the 'Using auxiliary programs for local indexation' section in
     the help (in progress, doesn't work).
    - New command line option -client_ip that can be used to force the
     IP used by mldonkey (if you have a firewall). Be careful with
     dynamic IPs.
    - Delay between md4 computation can be set by 'compute_md4_delay' option.
     Chunks downloaded improve to reduce long md4 computations.
  * Most problems with ed2k:// links should be fixed (/ at the end, and
     spaces in the middle). MLdonkeySubmit is distributed for Konqueror users.
  * Improved implementation of indexation. Documents description are
     stored on disk. Different choices of indexation.
  * Server black list: 'bs <ip1> <ip2> ...' in console to add IPs to the
    server blacklist. Servers on this black list are eventually removed,
    and will never be added again.
  * New option 'master_server_min_users' that prevent mldonkey from remaining
     connected to a server with too few users.
  * New option 'dont_update_server_list' to disable automatic update of
     server lists.
  * Telnet interface: 
     - New command 'remove_old_servers'
  * New (less-aggressive) management of sources:
     - New option 'max_source_age' (in days) to remove old sources.
     - New option 'max_clients_per_second' to prevent bursts of connections.
  * Faster computation of md4s for i486/i586/i686 Linux systems.
  * Fixed bugs:  
   - #100662: only valid server IPs are accepted.
   - Use IP returned by getsockname instead of the one from gethostbyname.
   - #100761: new option 'update_server_list' to choose whether you want 
     mldonkey to automatically add new servers to your server list.
   - Fixed bug preventing sharing of in-download files after restart.
   - Fixed bug making history.met always increase.

Release 1.14:
  * Bandwidth is now controled by the 'max_hard_download_rate' and
      'max_hard_upload_rate' options. Old rate options are obsolete.
  * All simple options can now be set by arguments on the command line (use 
     -help to show arguments).
  * Bug fixes:
    - Removed non-tailrecursive function in options.ml causing segfault when
       loading files.ini.
    - Connected clients without names in GUI should not appear anymore.
    - Remove 2 minutes timeout for queued clients.

Release 1.13:
  * Annotations on files in file 'comments.met'. Commands 'comments <filename>'
    to load a file, and 'comment <md4> <comment>' to add a new comment.
    Such comments could be downloaded from web sites (sharereactor.com).
  * New option 'web_infos' and new command 'add_url' on console: allow
    to download a file from the WEB automatically everyday (or less 
    frequently). Currently, only server.met and comments.met files are
    supported.
  * GUI:
     - New config file in $HOME/.mldonkey_gui.ini with GUI options
     - colors added in lists (see .mldonkey_gui.ini)
     - Layout is saved.
     - File locations are updated.
     - Improved protocol to decrease bandwidth
     - Comments are displayed in searches, and can be added in contextual menu.
     - Shortcuts can be configured by options.
  * Mailer accept non-canonnical addresses.
  * Option 'verbose': '#' for download, 'U' for upload, and other messages.
  * When possible, find extra names for files in history.
  * Console:
      - New command 'close_fds' to close all open files, to free space on disk 
        after remove.
      - New command 'pause' and 'resume' to pause and resume downloads (use
         arg 'all' to apply to all files).
  * Some control on how many connections can be opened per minute.
  * Better scheduling of compaction (new option 'compaction_overhead')
     in both core and GUI that caused delay when exiting.
  * Bug fixes:
    - Remove block change causing many 'exceeding block boundaries'
    - history.dat changed to history.met, using more secure marshaling.
    - OVERFLOW with GUI should not appear anymore.
    - Compaction makes GUI memory usage better.

Release 1.12:
  * Bug fixes:
    - Remove error 'bad file descriptor' when saving files.
    - Remove possible infinite loop in upload function.

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
   - Limited open fds for files to 50 (doesn't limit shared files
       and downloaded files).
   - Execute entry on WEB interface always displayed.
   - Removed a timeout closing all client connection after 2 minutes.
   - Shared files are opened in read-only mode.

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

KNOWN COMMENTS (from savannah and forum):
========================================
Browser: Mozilla/4.75 [en] (X11; U; Linux 2.4.17 i686; Nav) On the help page, it still says you can't set uploads
under 1K. 
If a server IP along with a colon seperated port number
is specified in the Server IP box, and enter hit, then
it gets added. This would make copy/paste from a list
of servers lots easier. Saving upstats would be nice, as well as some way of
resetting it, to get a better idea of which files to
keep shared. The downloads panel is a little busy, and not good for
small screens (some people still have to use 640x480) Perhaps move the 'save files' to an incoming tab?
(which can flash when it's got files in) On the same lines, why not show the filename/details
(the one above the colour bar) in a line, with name in
one box, size in the next (perhaps with bytes/Mb
added), and the format in another, with the colour bar
as the last item in the line, taking up the whole width
of the client, with the 'connections' panel only
popping up over the right-hand-side of the screen when
a filename is double-clicked (or when show connections
is picked from the menu)
The MD4 recover might move to the incoming tab, and the
ed2k file to below the filename/colourbar line, and
automatically change when a new file is clicked,
to show the ed2k: link.
Maybe the cancel/retry-connect could move to the
connections panel? On the search results tab, it would be nice to indicate
number of results per file, to give at least an
indication of availability.

===========================================
The only problem is that since I use it (.10) it eats lots of memory and cpu
resources.. this is the 'top' line, using 90.3% of the cpu and 22.5% RAM 32294 runa      14   0 86612  84M   796 R    90.3 22.5 275:55 mldonkey
============================================


============================================

============================================

