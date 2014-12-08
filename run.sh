#!/bin/sh

set -eux

yum update -y
yum install -y python-requests m4 zlib-devel gmp-devel 

git clone git@github.com:anchor/haskell2package ../haskell2package
cd ../haskell2package
cabal update && cabal install --only-dependencies && cabal build
cd ..
exec ./haskell2package/dist/build/haskell2package/haskell2package $@
