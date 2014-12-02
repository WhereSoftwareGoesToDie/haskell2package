#!/bin/bash

if ! ls *.cabal 2>/dev/null >/dev/null ; then
	echo "No .cabal files found"
	exit 2
fi

# `find` would be nicer: http://stackoverflow.com/a/4264351/806927
for CABAL in *.cabal ; do
	echo "Found $CABAL"

	PKG_NAME=$( grep "^name:"     "$CABAL" | sed -r 's/^[a-zA-Z-]+:[[:blank:]]+//' )
	PKG_VER=$(  grep "^version:"  "$CABAL" | sed -r 's/^[a-zA-Z-]+:[[:blank:]]+//' )
	PKG_SYN=$(  grep "^synopsis:" "$CABAL" | sed -r 's/^[a-zA-Z-]+:[[:blank:]]+//' )

	#[ -z "$PKG_SYN" ] && echo "synopsis is empty"

	echo "Package name is $PKG_NAME"
	echo "Package version is $PKG_VER"
	echo "Package synopsis is $PKG_SYN"

	sed -r -n -e '/^  build-depends:     /,/^$/ {
		s/^  build-depends: / / ;
		s/^ +// ;
		s/ .*|,// ;
		/./p
	}' "$CABAL" | sort | uniq > cabal_build_deps

	# Definitely only ever parse one .cabal file
	break
done
