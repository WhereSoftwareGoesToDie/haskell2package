dnl vim: ts=8
Name:		PKGNAME()
Version:	VERSION()
Release:	0.0anchor%{?build_number}%{!?build_number:1}%{?dist}
Summary:	SUMMARY()

Group:		Development/Libraries
License:	BSD
URL:		https://github.com/anchor/NAME()
Source0:	NAME().tar.gz
SRCS()
BuildRoot:	%(mktemp -ud %{_tmppath}/NAME()-%{version}-%{release}-XXXXXX)

BuildRequires:	zlib-devel
BuildRequires:	gmp-devel
BUILD_REQS()
Requires:	zlib
Requires:	gmp
RUN_REQS()

%description
DESCRIPTION()
%prep
SETUP()%setup -T -D -n NAME() -b 0

%build
export LC_ALL=en_US.UTF-8

cabal update
cabal sandbox init
ADD_SRCS()
cabal install --only-dependencies --enable-tests -j
cabal configure --enable-tests
cabal test
cabal build

%install
mkdir -p %{buildroot}%{_bindir}
mkdir -p %{buildroot}%{_datadir}
COPYS()
%files
%defattr(-,root,root,-)
%{_datadir}/*
FILES()
%changelog
* CHANGELOG_HEADING()
- Automated RPM release
