############################################################################## NIX OS 
##############################################################################
{ config, pkgs, ... }:
{
 imports = [./hardware-configuration.nix];
 ####################################################################################################################################################################
 # Packages
 nixpkgs.config.allowUnfree = true;
 environment.systemPackages = with pkgs; [
# Apperence
gnome3.adwaita-icon-theme
gnome3.gnome_themes_standard
gtk_engines
gtk-engine-murrine
haskellPackages.xmobar
haskellPackages.xmonad
tango-icon-theme
rofi
trayer
xorg.xrandr
xscreensaver

# Software
firefox
i3lock
jabref
jdk
gparted
htop
libreoffice
lxappearance
mirage
neovim
openvpn
pcmanfm
roxterm
screenfetch
seafile-client
shutter
speedcrunch
texmaker
thunderbird
vlc
xarchiver
zathura  # PDF Viewer

# System
arandr
bashmount
busybox
curl
davfs2
exfat
ghc     
git
jdk
jre
shared_mime_info
lxmenu-data
networkmanagerapplet
ntfs3g
pasystray
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
xorg.libXt
xorg.libXtst

# Writing
aspell
aspellDicts.de
aspellDicts.en
texlive.combined.scheme-full
];

      
 ####################################################################################################################################################################
 # Services

 services.xserver = {   
  
	 # keyboard
         enable = true;
         layout = "de";
	 xkbOptions = "eurosign:e";
  	 
         # Notebook
	 synaptics.enable = true;
	 synaptics.twoFingerScroll = true;

         # UI         
	 windowManager.xmonad.enable = true;
         windowManager.default = "xmonad";
         windowManager.xmonad.enableContribAndExtras = true;
         desktopManager.xterm.enable = false;
         displayManager.auto = {
	 	enable = true;
         	user = "sky";
	};

	xrandrHeads = [ "HDMI1" "eDP1"];
 };
 
 ####################################################################################################################################################################
 # Gerneral config
  
 	 # Time
  	 time.timeZone = "Europe/Berlin";
  	 
         # System Language
         i18n = {
                defaultLocale = "de_DE.UTF-8";
         };
 
	 # Redshift
	 services.redshift = {
	     enable = true;
	     latitude = "50";
	     longitude = "10";
	 };

	 # Network
   	 networking.networkmanager.enable = true;
   	 
	 # Hardware
	 hardware = {
	     pulseaudio = {
   	 	enable = true;
		support32Bit = true;
	     };

	     opengl.driSupport32Bit = true; 
	 };
	 
	 # Printer
  	 services.printing = {
   	     enable = true;
	     drivers = [ pkgs.hplip ];
  	 };
         
         # ZSH
         programs.zsh.enable = true;
         users.defaultUserShell = "/run/current-system/sw/bin/zsh";

	 # light
	 programs.light.enable = true;

         # SSH
         services.openssh.enable = true;

	 # SWAP
	 #swapDevices = [{
	 #device = "/var/cache/swap/swap0";
	 #}];
	 #swapDevices.*.size = "1024";

	 # Clean up
	 nix.gc.automatic = true;
	 nix.gc.dates = "16:14";
	 nix.gc.options = "--delete-older-than 30d";
	 nix.extraOptions = 
	 ''
	 gc-keep-output = true
	 gc-keep-derivations = true
	 auto-optimise-stor = true
	 '';

 ####################################################################################################################################################################
 # Users
 
 # user
 users.extraUsers.sky = {
         isNormalUser = true;
         home = "/home/sky";
         extraGroups = ["davfs2""wheel" "networkmanager" "libvirt" "libvirtd" "vboxusers"];
 };
 
 ####################################################################################################################################################################
 # Config Version
 system.stateVersion = "16.09";
 
 ####################################################################################################################################################################
 # Bootloader

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices = [
    { 
    name = "root"; device = "/dev/sda2"; preLVM = true;
    }
  ];

 
####################################################################################################################################################################
 # Virtualisation
 #virtualisation.libvirtd.enable = true;
 #virtualisation.libvirtd.enableKVM = true;
 #boot.kernelModules = [ "kvm-intel" "tun" "virtio" ];
# virtualisation.virtualbox.host.enable = true;
 }
