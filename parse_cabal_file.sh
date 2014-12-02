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

PKG_NAME=$(  grep -i "^name:"        "$CABAL_FILE" | sed -r 's/^[a-zA-Z-]+:[[:blank:]]+//' )
PKG_VER=$(   grep -i "^version:"     "$CABAL_FILE" | sed -r 's/^[a-zA-Z-]+:[[:blank:]]+//' )
PKG_SYN=$(   grep -i "^synopsis:"    "$CABAL_FILE" | sed -r 's/^[a-zA-Z-]+:[[:blank:]]+//' )
PKG_DESC=$(  grep -i "^description:" "$CABAL_FILE" | sed -r 's/^[a-zA-Z-]+:[[:blank:]]+//' )
PKG_MAINT=$( grep -i "^maintainer:"  "$CABAL_FILE" | sed -r 's/^[a-zA-Z-]+:[[:blank:]]+//' )
PKG_EXECS=$( grep -i "^executable"   "$CABAL_FILE" | awk '{print $2}')
CHANGELOG_DATE=$(date '+%a %b %d %Y')

#Dump version into file for rest of build process
echo $PKG_VER > pkg_ver

COPY_STRINGS=""
BIN_STRINGS=""
for x in $PKG_EXECS; do
    COPY_STRINGS=${COPY_STRINGS}"cp -v dist/build/${x}/${x} %{buildroot}%{_bindir}
"
    BIN_STRINGS=${BIN_STRINGS}"%{_bindir}/${x}
"
done

SOURCES=$(comm -12 cabal-build-deps <(sort repos.list) | grep -v ${PROJECT_NAME})
let i=1
for x in $SOURCES; do
    SANDBOX_STRINGS=${SANDBOX_STRINGS}"cabal sandbox add-source ../${x}
"
    SETUP_STRINGS=${SETUP_STRINGS}"%setup -n ${x} -T -D -b ${i}
"
    SRC_STRINGS=${SRC_STRINGS}"Source${i}:	${x}.tar.gz
"
    (( i += 1 ))
done

echo $SOURCES > anchor_github_deps

(cat <<EOF
dnl -*- m4 -*-
changequote(<<, >>)dnl
dnl
define(<<NAME>>, <<${PKG_NAME}>>)dnl
define(<<VERSION>>, <<${PKG_VER}>>)dnl
define(<<SUMMARY>>, <<${PKG_SYN}>>)dnl
define(<<DESCRIPTION>>, <<${PKG_DESC}>>)dnl
define(<<COPYS>>, <<${COPY_STRINGS}>>)dnl
define(<<FILES>>, <<${BIN_STRINGS}>>)dnl
define(<<SRCS>>, <<${SRC_STRINGS}>>)dnl
define(<<ADD_SRCS>>, <<${SANDBOX_STRINGS}>>)dnl
define(<<SETUP>>, <<${SETUP_STRINGS}>>)dnl
define(<<CHANGELOG_HEADING>>, <<${CHANGELOG_DATE} ${PKG_MAINT} ${PKG_VER}-0.0anchor${BUILD_NUMBER}>>)dnl
EOF
) > m4defs
