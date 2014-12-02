Name:		NAME()
Version:	VERSION()
Release:	0.0anchor%{?dist}
Summary:	SYNOPSIS()

Group:		
License:	FREE AS IN BEER
URL:		https://github.com/anchor/NAME()
Source0:	

BuildRequires:	
Requires:	

%description


%prep
%setup -q


%build
%configure
make %{?_smp_mflags}


%install
make install DESTDIR=%{buildroot}


%files
%doc



%changelog

