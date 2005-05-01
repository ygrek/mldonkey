        *** THIS FILE IS OUTDATED.  PLEASE HAVE A LOOK AT THE ***
        *** WIKI PAGE FOR BASIC  INFORMATION ABOUT INSTALLING ***


                      MLDonkey
                      ========

Release: 1.16
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

      mldonkey@mldonkey.net
      mldonkey-users@non-gnu.org

Please don't bother us too much with questions on how to use mldonkey. We
prefer bug reports, containing USEFUL information to find the bug. You can
also submit bugs on the savannah WEB site:

  http://www.freesoftware.fsf.org/mldonkey/

For advices on how to use mldonkey, you can check several forums:

  http://www.mldonkeyworld.com/
  http://www.mldonkey.org/      (German forum)

*) What about the sources ? What about the protocol ?
-----------------------------------------------------

All sources are available on the savannah WEB site:

  http://www.freesoftware.fsf.org/mldonkey/

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

Known bugs:
===========
  * When clicking on the columns it sorts on that column, when
   clicking again it should do a reverse sort on it, but it doesn't.

