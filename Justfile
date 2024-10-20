
build:
  make

config:
  ./configure --enable-debug --enable-upnp-natpmp

autoconf:
  cd config && autoconf
