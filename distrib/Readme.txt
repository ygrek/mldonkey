
                      MLDonkey
                      ========

Release 2.04rc1
---------------

MLdonkey is a multi-network file-sharing client, written in Objective-Caml.
It can run as a daemon on your computer, and can be controlled via 3 
interfaces: telnet, WEB and GUIs.

FOR HELP:
=========
Mailing-Lists available from: http://www.mldonkey.net/

IRC channel: irc.freenode.net, chat #mldonkey

Web sites:
http://www.mldonkey.net/         Official site, bug reports
http://www.mldonkeyworld.com/    English forum
http://www.mldonkey.org/         German forum

Mailing-lists:
mldonkey-users@nongnu.org
Archives: http://mail.nongnu.org/mailman/listinfo/mldonkey-users

USAGE:
======

You can find three kind of binaries in MLdonkey distributions, their names
are of the form:

* mldonkey, mlgnut,... : the clients to access the different networks
* mldonkey_gui: the Graphical Interface ONLY 
* mldonkey+gui, mlgnut+gui,...: the client with the GUI integrated

Among the different networks clients, you can find:
  mldonkey: eDonkey + Overnet support
  mlgnut : Gnutella (+ Gnutella2 in the future) support
  mlslsk: Soulseek support
  mlbt: BitTorrent support
  mldc: Direct-Connect support
and mlnet: all the networks in one binary

To start, create an empty directory where mldonkey will store its 
configuration files, its temporary files and its downloaded files.
Then, just start mldonkey. It will create a temp/ and an incoming/ 
directories, and many .ini configuration files.

Now, to communicate with him, you have the choice:
1) WEB: open http://127.0.0.1:4080/

2) Telnet: telnet 127.0.0.1 4000

3) The GUI: mldonkey_gui

Normally, you can use CTRL-C to stop it, even if the best way is still
to use the 'kill' internal command in one of the 3 interfaces.

************************************************************************

     NOTE: THIS PART OF THE DOCUMENTATION IS OBSOLETE. You can still
    find information inside, but the most up-to-date information is
    available in the Wiki of http://www.mldonkeyworld.com/ 


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

