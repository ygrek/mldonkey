#!/bin/sh

set -e

if [ ! -d ~/.mldonkey ]; then
 echo "ERROR: ~/.mldonkey is missing. Please create the directory and" && \
 echo "       copy the examples from /usr/share/doc/mldonkey-cvs-byte/examples/" && \
 exit 1;
fi

cd ~/.mldonkey && exec /usr/lib/mldonkey-cvs-byte/mldonkey
