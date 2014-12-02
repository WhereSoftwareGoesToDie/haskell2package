#!/bin/bash
set -eux

export LANG=en_US.UTF-8

sudo yum update -y
sudo yum install -y python-requests m4

python anchor_repos.py
cat repos.list
./find_cabal_build_deps.sh "${JOB_NAME}.cabal" > cabal-build-deps
cat cabal-build-deps
./parse_cabal_file.sh "${JOB_NAME}"

VERSION=$(cat pkg_ver)
ROOT=$(pwd)
SYS_DEPS=$@

echo
echo "BUILDING VERSION ${VERSION} - ${BUILD_NUMBER}"
echo

# Pre

BUILD_REQS=""
RUN_REQS=""
for x in $SYS_DEPS; do
    BUILD_REQS=${BUILD_REQS}"BuildRequires:	${x}-devel
"
    RUN_REQS=${RUN_REQS}"Requires: ${x}
"
    sudo yum install -y ${x}-devel
done

(cat <<EOF
define(<<BUILD_REQS>>, <<${BUILD_REQS}>>)dnl
define(<<RUN_REQS>>, <<${RUN_REQS}>>)dnl
EOF
) >> m4defs

m4 m4defs TEMPLATE.spec > $ROOT/${JOB_NAME}.spec
cat $ROOT/${JOB_NAME}.spec

SRCS=$(cat anchor_github_deps)
for x in $SRCS; do
    git clone git@github.com:anchor/${x}.git ../${x}
    cd ../${x}
    git archive --prefix=${x}/ -o ../${x}.tar.gz master
done

cd ../${JOB_NAME}
git archive --prefix=${JOB_NAME}/ -o ../${JOB_NAME}.tar.gz HEAD

mkdir $HOME/rpmbuild/SOURCES/ -p
mv ../*.tar.gz $HOME/rpmbuild/SOURCES/

rpmdev-setuptree

# Make rpmbuild stop with the debuginfo packages.

echo '%debug_package %{nil}' > $HOME/.rpmmacros
rpmbuild -bb --define "build_number $BUILD_NUMBER" --define "dist .el7" \
        $ROOT/${JOB_NAME}.spec

mkdir packages

cp $HOME/rpmbuild/RPMS/x86_64/*.rpm packages/
