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

		# Games
		tome4

		# Dev
		# C
		cmake
		gnumake

		# Software
		beancount
		calibre
		colordiff
    darktable
		emacs
		dwarf-fortress
		dwarf-fortress-packages.phoebus-theme
		fava
		gajim
    
		htop
		i3lock
		keepass
		less
		libreoffice
    lxappearance
#    (lxappearance.overrideAttrs(old:
#    rec {
#      name = "lxappearance-0.6.2";
#      src = fetchurl {
#        url = "mirror://sourceforge/project/lxde/LXAppearance/${name}.tar.xz";
#        sha256 = "07r0xbi6504zjnbpan7zrn7gi4j0kbsqqfpj8v2x94gr05p16qj4";
#      };
#    }))
    mumble
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
#		busybox
		curl
    cryptsetup
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
		#lxappearance
		qutebrowser
	]);
}


