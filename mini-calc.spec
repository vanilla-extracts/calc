%define __spec_install_post %{nil}
%define __os_install_post %{_dbpath}/brp-compress
%define debug_package %{nil}

Name: mini-calc
Summary: A fully-featured minimalistic configurable rust calculator
Version: 3.0.1
Release: 3.0.1
License: GPLv3+
Group: Applications/System
Source0: %{name}-%{version}.tar.gz
URL: https://calc.nwa2coco.fr

BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description
%{summary}

%prep
%setup -q

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}
cp -a * %{buildroot}

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root,-)
%{_bindir}/*
