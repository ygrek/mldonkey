Bugs mentioned as #<N> are referencing github issues
at https://github.com/ygrek/mldonkey/issues

Changelog
=========

3.2.1 - 2024/08/19
------------------

* upgrade autoconf build system files
* commit configure to git so that source archive is identical to the tag checkout
* disable automatic invocation of autoconf

3.2.0 - 2024/08/17
------------------

Supported OCaml versions now are from 4.03 up to 4.14, as a consequence local (in-the-tree) build of OCaml is not supported anymore, you must have OCaml installed to build.

NB newgui2 build is finicky, see README.md for details (tl;dr system packaged OCaml and lablgtk2 or --enable-batch with OCaml 4.05)

* fix duplicated Content-Length header (#82)
* Support miniupnpc 2.2.8 (#101, Sergey Fedorov)
* upgrade upnp/natpmp libraries versions for in-the-tree build (--enable-force-upnp-natpmp)
* fix setting network-specific options from command-line (#34)
* fix build with newer (safe-string) OCaml (#98, #99, Luca Carlon)
* some internal code and build system cleanup
* CryptoPP build fixes (#62, #63, #66, Hin-Tak Leung, Jesús Pérez Rey (Chuso))
* dark theme for the web interface (Luca Carlon)

3.1.7-2 - 2020/07/05
--------------------

Patch release to fix build, no code changes (version in binary unchanged)

* fix links to website throughout
* fix build when num library is missing (#47)

3.1.7 - 2020/06/21
------------------

* improvements to build process
* fix build with OCaml up to 4.09.0
* BT: drop isoHunt link

NB When building with OCaml >= 4.06 and Bittorrent network enabled - ocaml num library has to be present

3.1.6 - 2017/01/22
------------------

* BT: better diagnostics of wrong udp tracker reply
* BT/DHT: update public router addresses
* fix buffer overflows in DNS resolution
* update default blocklist url (Closes #4)
* fix build with OCaml 4.02 (closes #6)
+ extend command conditions (pause, resume, priority) (ref #9)
* Fix compilation errors with gcc5 (closes #12) (Christopher Meng)
* BT: relaxed parsing of bencoded numbers
* fix build with libminiupnpc 1.9 (closes #13)
* escape filename based on filesystem, not OS type
* Fix compilation with OCaml 4.03.0 (Bastien Dejean)

See distrib/ChangeLog for older entries
