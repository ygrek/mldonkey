To compile:

  - You need:

     - GNU make

     - Objective-Caml 3.04 or higher

          http://pauillac.inria.fr/caml

     - LablGTK 1.2.3 or higher

          http://wwwfun.kurims.kyoto-u.ac.jp/soft/olabl/lablgtk.html

  - Run the ./configure script. The configure script will detect if you
     have a copy of the eDonkey protocol to compile the client. If you 
     don't have one, you can only build the GUI.

  - Build dependencies: 

          make depend

  - Build bytecode version (slower):

          make byte

  - Build native code version:

          make opt

  - Build static version:

          make static
To use:

1) Start the server in a directory containing the initial downloads.ini file.

  ./mldonkey

2) Start the GUI:

  ./mldonkey_gui [machine [port]]

3) Command line client:

  telnet machine 4000
