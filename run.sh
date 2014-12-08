#!/bin/sh

set -eux

yum update -y
yum install -y python-requests m4 zlib-devel gmp-devel

cd haskell2package
cabal update && cabal install --only-dependencies && cabal build
cd ..
./haskell2package/dist/build/haskell2package/haskell2package $@
