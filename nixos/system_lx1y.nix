{ config, lib, pkgs, ...}: with lib;
{
	# Hostname
	networking.hostName = "nixos-lx1y";

  services.thermald.enable = true; # otherwise the cpu frequency/clock is always at maximum

#  powerManagement = {
#    enable = true;
#    cpuFreqGovernor = "ondemand";
#  };
 # services.tlp = {
 #   enable = true;
 #   extraConfig = ''
 #   DEVICES_TO_DISABLE_ON_STARTUP="bluetooth"
 #   DEVICES_TO_DISABLE_ON_STARTUP="wwan"
 #   '';
 # };
	services.xserver = {

		# Graphic
		videoDrivers = ["intel"];

		# Notebook
		synaptics.enable = true;
		synaptics.twoFingerScroll = true;
		multitouch.enable = true;
	};

	# Hardware
	hardware = {
		bluetooth.enable = true;
    bluetooth.powerOnBoot = false;
#	    bumblebee = {
#		      #connectDisplay = true;
#			  enable = true;
#	    };
	};

  # Bootloader
	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;
}
