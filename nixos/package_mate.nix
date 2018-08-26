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

    # Software
		#calibre
		colordiff
   # darktable
		#emacs
		i3lock
		less
#		libreoffice
    lxappearance
    mirage
    mumble
    mpv
		neovim
    openconnect
    openvpn
		pavucontrol
    ranger
		roxterm
		#screenfetch
		seafile-client
		#shutter
    sshuttle
    ssvnc
		#steam
	#	speedcrunch
		thunderbird
		tig
		tldr
		xarchiver
		x11vnc
		zathura  # PDF Viewer

		# System
		arandr
    acpi
		bashmount
#    breeze-icons
		curl
    cryptsetup
		davfs2
		exfat
		fam
		ghc
		git
    go-sct
#		jdk
#		jre
		shared_mime_info
		lxmenu-data
		networkmanagerapplet
		ntfs3g
		numlockx
 #   redshift
    pciutils
    python
		unrar
		unzip
    wget
		xbrightness
		xdotool
		xorg.libXt
		xorg.libXtst
    cifs_utils

    #System Monitoring
		htop # task/process-manager
    psensor # monitor temperature
    screenfetch # system information
    s-tui # monitor CPU temperature, frequency, power
	  xfce.xfce4-taskmanager


		# Writing
		aspell
		aspellDicts.de
		aspellDicts.en
    #texlive.combined.scheme-basic
#		texlive.combined.scheme-full

    # Thinkpad
    linuxPackages.acpi_call
	]) ++ (with rolling; [
	discord
  #lxappearance
  dino
  emacs
	libreoffice
  speedcrunch
  firefox
  samsung-unified-linux-driver
  bspwm
	]);
}


