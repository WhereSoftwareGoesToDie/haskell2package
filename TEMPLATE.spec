Name:		NAME()
Version:	VERSION()
Release:	0.0anchor%{?build_number}%{!?build_number:1}%{?dist}
Summary:	SUMMARY()

Group:		Development/Libraries
License:	BSD
URL:		https://github.com/anchor/NAME()
Source0:	%{name}.tar.gz
SRCS()
BuildRoot:	%(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

BuildRequires:  ghc >= 7.8.3
BuildRequires:  cabal-install
BUILD_REQS()
RUN_REQS()

%description
DESCRIPTION()
%prep
SETUP()%setup -T -D -n %{name} -b 0

%build
export LC_ALL=en_US.UTF-8
cabal list > /dev/null
sed -r -i "s,^(remote-repo: hackage.haskell.org.*)$,\1\nremote-repo: hackage.syd1.anchor.net.au:http://hackage.syd1.anchor.net.au/packages/archive," /home/jenkins/.cabal/config

cabal update
cabal sandbox init
ADD_SRCS()
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal test
cabal build

%install
mkdir -p %{buildroot}%{_bindir}
COPYS()
%files
%defattr(-,root,root,-)
FILES()   
%changelog
* CHANGELOG_HEADING()
- Automated RPM release
