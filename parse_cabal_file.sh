#!/bin/bash

if [ $# -ne 1 ] ; then
	echo "Need a single project name as an argument."
	exit 2
fi

PROJECT_NAME=$1
CABAL_FILE="${PROJECT_NAME}.cabal"
if [ ! -f "$CABAL_FILE" ] ; then
	echo "Cabal file ${CABAL_FILE} does not exist."
	exit 2
fi


#echo "Found $CABAL_FILE"

PKG_NAME=$( grep "^name:"     "$CABAL_FILE" | sed -r 's/^[a-zA-Z-]+:[[:blank:]]+//' )
PKG_VER=$(  grep "^version:"  "$CABAL_FILE" | sed -r 's/^[a-zA-Z-]+:[[:blank:]]+//' )
PKG_SYN=$(  grep "^synopsis:" "$CABAL_FILE" | sed -r 's/^[a-zA-Z-]+:[[:blank:]]+//' )

#echo "Package name is $PKG_NAME"
#echo "Package version is $PKG_VER"
#echo "Package synopsis is $PKG_SYN"

(cat <<EOF
dnl -*- m4 -*-
changequote(<<, >>)dnl
dnl
define(<<NAME>>, <<${PKG_NAME}>>)dnl
define(<<VERSION>>, <<${PKG_VER}>>)dnl
EOF
) | m4 - TEMPLATE.spec
