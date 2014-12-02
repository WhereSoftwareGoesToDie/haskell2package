#!/bin/bash

if [ $# -ne 1 ] ; then
	echo "Need a single .cabal file as an argument."
	exit 2
fi

CABAL_FILE=$1
if [ ! -f "$CABAL_FILE" ] ; then
	echo "Cabal file ${CABAL_FILE} does not exist."
	exit 2
fi


sed -r -n -e '/^  build-depends:     /,/^$/ {
	s/^  build-depends: / / ;
	s/^ +// ;
	s/ .*|,// ;
	/./p
}' "$CABAL_FILE" | sort | uniq
