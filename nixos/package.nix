{ config, pkgs, lib, stable, rolling, ...}:
with lib;
{
	nixpkgs.config.allowUnfree = true;
	environment.systemPackages =
	(with stable; [
		# Apperence
		arc-theme
		arc-icon-theme
		gnome3.gnome_themes_standard
		gtk_engines
		gtk-engine-murrine
		polybar
		tango-icon-theme
		rofi
		trayer
		xorg.xrandr

		# Mail
		isync
		notmuch
		python27Packages.afew

		# Software
		beancount
		calibre
		colordiff
		emacs
		dwarf-fortress
		dwarf-fortress-packages.phoebus-theme
		fava
		gparted
		htop
		i3lock
		keepass
		less
		libreoffice
		#lxappearance
		neovim
		openvpn
		pavucontrol
		pcmanfm
		roxterm
		screenfetch
		seafile-client
		shutter
		ssvnc
		steam
		speedcrunch
		taskwarrior
		thunderbird
		tig
		tldr
		xarchiver
		x11vnc
		zathura  # PDF Viewer

		# System
		arandr
		bashmount
		#busybox
		curl
		davfs2
		exfat
		fam
		ghc
		git
		jdk
		jre
		shared_mime_info
		lxmenu-data
		networkmanagerapplet
		ntfs3g
		numlockx
		pciutils
		python
		python27Packages.pip
		python27Packages.setuptools
		python3
		python35Packages.pip
		python35Packages.setuptools
		unrar
		unzip
		xbrightness
		xdotool
		xorg.libXt
		xorg.libXtst

		# Writing
		aspell
		aspellDicts.de
		aspellDicts.en
		# texlive.combined.scheme-full
	]) ++ (with rolling; [
		chromium
		lxappearance
		qutebrowser
	]);
}


