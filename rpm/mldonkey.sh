#!/bin/sh

set -e

if [ ! -d ~/.mldonkey ]; then
	 echo "Creating $HOME/.mldonkey"
	 mkdir $HOME/.mldonkey
   fi

   cd ~/.mldonkey && exec /usr/lib/mldonkey/mldonkey-bin
   
