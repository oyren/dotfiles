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
    msmtp
    #notmuch
		afew

		# Games
		tome4

		# Dev
		# C
#    clang
    cmake
    gcc
    gnumake
    ## Libs
    #ncurses

    # Elektronic
#    logisim
#    kicad

    # Software
 #   beancount
		#calibre
		colordiff
   # darktable
		#emacs
		i3lock
		less
#		libreoffice
    lxappearance
#    (lxappearance.overrideAttrs(old:
#    rec {
#      name = "lxappearance-0.6.2";
#      src = fetchurl {
#        url = "mirror://sourceforge/project/lxde/LXAppearance/${name}.tar.xz";
#        sha256 = "07r0xbi6504zjnbpan7zrn7gi4j0kbsqqfpj8v2x94gr05p16qj4";
#      };
#    }))
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
    gnuplot
    #texlive.combined.scheme-basic
#		texlive.combined.scheme-full

    # Thinkpad
    linuxPackages.acpi_call
	]) ++ (with rolling; [
	discord
  libwacom
  #lxappearance
  dino
  emacs
	libreoffice
 # opentx
 # steam
  speedcrunch
 # notmuch
  firefox
 # firefox-bin
  bspwm
  #wineUnstable
  #tibia
  qutebrowser
  #torbrowser
  # qt5.qtwebengine
  samsung-unified-linux-driver

# touch
onboard
	]);
}


