opennap v0.44 (BETA)
the open source (TM) napster server
===================================
September 30, 2001

http://opennap.sourceforge.net
IRC: #opennap on EFNet
Email: opennap-dev@lists.sourceforge.net

opennap is an Open Source(TM) server implementation of the popular
Napster protocol.  It is written in ANSI C and was designed to run on Unix
platforms.  So far this server has been tested on:
	* Linux (i386, alpha, sparc, ppc)
	* BSDI
	* FreeBSD
	* Solaris
	* IRIX
	* OS/2
	* Windows 95/98/NT/2000 (MS VC++, CYGWIN)
Minimal porting should be required for compilation on other Unix variants
(if you do a port, please send patches to drscholl@users.sourceforge.net)

Since Napster is not an open specification, much of the internals of the
servers are specific to this implementation.  In particular, I don't expect
that this server will interoperate with the official Napster servers once they
are linked together.  Given that I can't run tcpdump between those servers,
I can't see how they are communicating.  I'm hoping that with the proliferation
of servers outside of napster.com, they might be willing to open up their
specification to make sure all the clients can interoperate.  However, this
server should be compliant with the several currently available Unix Napster
clients.

Main differences from the official napster servers:
* servers can be linked together fully (chat and search)
* channel operators can be defined to moderate specific channels
* source code is freely available

What's included?
* opennap server
* metaserver for directing clients to server groups
* protocol specification
* SOURCE CODE!

There is also a mailing list opennap-admin@lists.sourceforge.net for people
running live servers to share information not directly related to the
development of the server.

* For a description of the napster protocol, please see the file 'napster.txt'
  included with this distribution.

* INSTALLATION

  To install, simply run the `setup' utility (provided with the binary
  distributions, or built from source).

  The installation process involves picking a directory where to install
  OpenNap and creating an initial account for administering the server.  By
  default, OpenNap will look for its configuration files in:

	C:\opennap			Win32 platforms (Win 95/98/NT/2000)

	-or-

	/usr/local/share/opennap	Unix platforms

  If you wish to install OpenNap in another location, simply enter the new
  directory when `setup' prompts you.

  The next step is to create an initial user account for the owner of the
  server.  `setup' will prompt you for the nickname to use, the password for
  the account and your email address (note that you ARE NOT required to
  enter a real email address if you don't want to--simply use the default
  email@here.com if you like)

  IF you installed OpenNap in a different directory than the default,
  `setup' will create a config file for you so that opennap knows that it
  is installed in the directory you chose.  However, in order for opennap to
  find this config file, you will have to tell it where the config file is
  when you start the server.  You do this by specifyin the -c command line
  option as such:

	opennap -c PATH

  where PATH is the _full_ path to the config file.

  (NOTE: for Win32 users, `setup' will create a `launch.bat' script in the
  directory you chose to install in which contains the above command so that
  you can simply run the batch file to start the server.  Just make sure you
  copy the opennap.exe executable into that directory first).
  
* Linking Servers

  1.  you need to add the dns name of the server you wish to allow to connect
      to your server in the database.  This needs to be done on both
      ends, as it does mutual authentication to prevent bogus servers from
      being able to join.

	edit /usr/local/share/opennap/servers (c:\opennap\servers on Win32)
	add:
		<server name> <password> <local_password>

	where <server name> is the dns name of the server, and <password>
	is the password for that server to gain access.  <local_password> is
	the password your server uses to authenticate itself to the other
	server (so the two password fields should be swapped on the
	other server).

   2.  connect to your server and gain elite access, and execute the
       `server connect' command (currently only BWap has support for
       the opennap commands), specifying the host and port to connect to.

       In BWap, you would do:
	/admin connect <server>
       In the windows client or other client which does not natively support
       the opennap extensions, you can do:
	/msg operserv connect <server>

* Server to Server messages

There are many cases where client commands need to be passed to peer servers
in order to maintain database consistency across all servers.  However, most
of the commands that the clients use don't specify the user who performed
the action.  This implementation borrows from the IRC protcol and prefixes
client commands with
	:user
to indicate to peer servers which user issued the command.  Commands which
already specify the user name, such as the login request (2), are not
prefixed.  Each request handler should have a comment at the beginning
specifying what input it expects.

* Extensions to messages

200	search [CLIENT]

	TYPE <mime-type>

		OpenNap supports other media types besides mp3.  By default,
		searches will only match mp3 files.  A client can however
		search for other media types by specifying a
		partial MIME content-type (audio, video, text, application,
		image).  (See message 10300 for adding media to the database).
		The special keyword `any' will match any media type in the
		database.

		The results of the search are returned as with mp3, except
		the fields for bitrate, sample frequency and length are
		meaningless (and are set to 0 in this implmentation).  The
		client can then download as they would any other mp3 file.

	FILENAME EXCLUDES "..."

		allows filtering search results by excluded all files which
		match words in an exclude list.  this must be used in
		conjunction with FILENAME CONTAINS "...".
		(NOTE: as of Napster 2.0 BETA 8, words in the
		FILENAME CONTAINS string which are prefixed with a minus
		sign (-) are considered to be exclusive.  You are better
		off implementing it this way for compatibility.)

	[ SIZE | DURATION ] ["AT LEAST" | "EQUAL TO" | "AT BEST"] <integer>

		allows matching on the file size and song length in addition
		to the other file attributes

612	ban user/ip [CLIENT]

	Format: <user|ip> [ "reason" [timeout] ]

	opennap adds the optional `timeout' fields which specifies the time,
	in seconds, that the ban should be enforced.  Note that if `timeout'
	is present, the `reason' field MUST be present, even if empty.

* Non-standard messages

The following messages are not present in the official napster servers, but
are implemented as additional functionality in the opennap server.

10000	client quit [SERVER]

	<nick>

	the message is sent to peer servers when a client connection has
	closed

10010	server login [SERVER]

	<server-name> <nonce> <compression>

	<server-name>	the dns name of the server wishing to connect
	<nonce>		a random string to use for authentication
	<compression>	the maximum zip (LZ77) compression level

	this message is sent when a connection wishes to identify itself as
	a peer server.  when a server receives this message, it will send its
	own login command to the peer to initial mutual authentication

10011	server login ack [SERVER]

	<hash-response>

	to authenticate itself, the server will hash the value
		<peer-nonce><nonce><server-pass>
	using the MD5 algorithm.  this allows the servers to mutually
	authenticate without the plaintext passwords traversing the network

10012	request server link list [CLIENT]

	Format: <empty>

	client requests the current server link info

10013	user ip [SERVER] (deprecated, this info is now appended to msg 2)

	<user> <ip> <port> <server>

	this message is used for a server to pass the ip address of a
	locally connected client to its peer servers, since that information
	is not available in the login message.  <ip> is an unsigned long
	integer specifying the ip address for <user>

10014	registration info [SERVER]

	<user> <pass> <level> <email> <created> <last-seen>

	<pass>		user's password
	<level>		user's default level
	<email>		user's email address
	<created>	time at which the account was created
	<last-seen>	last time the user logged into a server

	When a server detects that the user table is out of sync between
	servers, it will send its notion of what the entry should look like
	to all the other servers.  If a receiving server has a matching
	user, it checks the <created> time to see which is the oldest
	entry and updates accordingly.

10018	encapsulated message [SERVER]

	:<sender> <recip> <message>

	This command allows a server to send a private message to a user on
	a remote server.  <message> should be a well-formed Napster message,
	complete with tag and length header.

10019	server link info [SERVER]

	<server> <port> <server> <port> <hops>

	This message is used by servers to share information about the
	topology of the linked servers.  when a server joins, a message is
	sent to all other nodes in the cluster.  <hops> is incremented each
	time the message is relayed so that each server knows how far away
	the others are.

10020	server quit [SERVER]

	:<server> <server> "<reason>"

	This message is used to notify other servers in the group that a
	server has delinked.  The server with the peer connection to the
	server that has quit should send this message.

10021	notify mods [SERVER]

	:<server> <loglevel> "<message>"

	allows a server to send a message to all logged in moderators.
	primarily used to propogate ban messages when not all servers
	see the connection or login.

10022	server to server pong [SERVER]

	Format: :<server> <recip> [args]

	This numeric is generated by a server in response to a 750 command
	from another server.  The 750 was originally intended for a client
	to ping its local server, but doesn't contain enough information to
	use it as a pong response when crossing a server link.  [args] is
	optionally copied from the [args] in the 750 message, typically used
	to keep client state.

10023	time check [SERVER]

	Format: <time>

	upon linking, servers should send their idea of the current local
	time to the peer server in order to them to make sure they are not
	too far out of sync.

10024	remote whois notification [SERVER]

	Format: :<sender> <target>

	This message is used to notify a remote client that a local client
	has performed a whois request.  Mods+ are notified when a client
	does a whois request, and using a separate message allows the
	recipient client to turn off the notifications if requested.

10100	server connect [CLIENT]

	<server> [ <remote_server> ]

	Attempts to link the current server to <server>.  If
	<remote_server> is given, then that server attempts to make the
	link instead of the local server.

10101	server disconnect [CLIENT]

	<server> <reason> [ "<remote-server>" ]

	delink current server from <server>.
	must be admin or high level to execute this command

10110	kill server [CLIENT]

	<server> "<reason>"

	cause the server to shutdown.  must be elite level to execute
	this command.

10111	remove server [CLIENT] (DEPRECATED, not currently used)

	<server> "<reason>"

	reqeusts that <server> be removed from the table of allowed links.
	must be elite to execute this command

10112	show server links [CLIENT, SERVER]

	client: no data
	server: <host> <host> <hops>

	This command is used to show information about the links a
	server has to other servers.  The list is terminated by a 10112
	message with no data (0 length).

10115	show server stats [CLIENT, SERVER]

	client: no data
	server: <clients> <servers> <files> <gigs> <channels> <time>
	<uptime> <memory> <numusers>

	This command is used by administrators to get information about the
	state of the server.

	<clients>	number of locally connected clients
	<servers>	number of locally connected servers
	<users>		total number of global users
	<files>		number of files in the db
	<gigs>		total size of all files shared
	<channels>	number of active channels
	<time>		time at which the server was started
	<uptime>	number of seconds of uptime
	<memory>	if debugging is enabled, this will show the memory
			currently in use, otherwise it will be -1
	<numusers>	number of registered users		

10116	server ping [DEPRECATED]

	note: there is now a defined command for this in napster.txt

10117	rehash (reload) configuration files [CLIENT]

	Format: [server]

	Causes [server] to reread its configuration file and reload the
	motd into memory.  If [server] is not specified, the server to which
	the client is connected is affected.

10118	display client information statistics [CLIENT, SERVER]

	If compiled with --enable-version-stats, the server will dump its
	lists of known clients and the number of logins for each.  It does
	not keep track of unique ips, so this just gives a rough estimate
	on the popularity of various clients.

	Format (server): "<version>" <count>

	The server's list is terminated by a 10118 message with a 0 length.

10119	display which server a user is on [CLIENT]

	Format: <user>

	server will return an message detailing which server
	the particular user is on.  only mods+ can execute this command.

10120	ping all peer servers [CLIENT]

	Format: (empty)

    	Causes a server to sping all of its peer servers and report the
	results.

10121	who was [CLIENT]

	Format: <nick>

	Displays info about a user that has recently logged out.

10122	mass kill by ip [CLIENT]

	Format: <ip> ["reason"]

	Disconnect all connections originating from <ip>

10123	server command histogram [CLIENT, SERVER]

	Client: {empty}
	Server: <numeric> <count> <bytes>

	Displays a histogram of the number of commands and bytes received
	for each supported protocol numeric.

10124	end server histogram [SERVER]

	Format: <last_unknown> <unknown_count> <unknown_bytes> <total_count>
		<total_bytes>

	Terminates the list of 10123 messages.

10203	set user mode [CLIENT]

	[-]{ALL,MUZZLE,BAN,KILL,PORT,WALLOP} [ ... ]

10204	set channel op [CLIENT]

	Format: <channel> <user> [user ...]

	enable <user> to kick/ban users on <channel>

10205	remove channel op [CLIENT]

	Format: <channel> <user> [user ...]

	remove <user> as operator on <channel>

10206	channel op list [CLIENT]

	Format: <channel>

	returns the list of defined channel operators for the given channel

10207	drop channel [CLIENT]

	Format: <channel> [ "<reason>" ]

	marks the specified channel as being user created such that its
	state is not stored in the persistent channels file and will
	disappear once all users part from it.

10208	send message to channel ops [CLIENT]

	Format: <channel> <message>

	sends <message> to all operators and mods+ on <channel>

10209	change channel mode [CLIENT, SERVER]

	Format: <channel> [mode]

	if specified, [mode] is of the form

		(+|-)STRING

	where STRING is one of

		PRIVATE		- makes the channel not show up in the list
		MODERATED	- only ops and mods+ can speak in public
		INVITE		- channel is invite only
		TOPIC		- if set, any user may change the topic

10210	invite user to a channel [CLIENT, SERVER]

	Format: <channel> <user>

	when a channel is +INVITE, this sends an invitation to a user to
	join the specified channel.  the user issuing the invite must be
	a member of the channel.

10211	give voice to speak in moderated channel [CLIENT]

	Format: <channel> [user [user ...]]

10212	remove voice to speak in moderated channel [CLIENT]

	Format: <channel> [user [user ...]]

10213	muzzle a user in a specific channel [CLIENT]

	Format: <channel> <user> ["reason"]

10214	unmuzzle a user in specific channel [CLIENT]

	Format: <channel> <user> ["reason"]

10300	share generic media file [CLIENT]

	Format: "<filename>" <size> <md5> <major-content-type>

	<content-type> is the major MIME type defined for what the data is.
			should be one of:
				audio, video, text, image, application, mp3
			NOTE: mp3 is a separate type here since the original
			napster protocol was designed for sharing mp3 files.
			"audio" refers to other types of audio files

	Examples:

	"C:\IMAGES\Grand Canyon.jpg" 54187 bc938fdc0ef63772bfbdbf57aabb0860-54187 image
	"\home\drscholl\src\opennap-0.11.tar.gz" 102161 51c07734811a26853b1a2a87b67c68a1-102161 application

10301	new browse [CLIENT] (deprecated)

	Format: <nick>

10302	new browse result [CLIENT] (deprecated)

	Format: <nick> "<path>" <filename> <md5> <size> <bitrate>
		<frequency> <time> [ <filename> ... ]

* References

RFC1459, the IRC protocol was helpful in implementing many features.

http://www.onelist.com/community/napdev/ is a useful community (mailing list)
for discussion of the napster protocol.
