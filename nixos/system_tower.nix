{ config, lib, pkgs, ...}: with lib;
{
	# Hostname
	networking.hostName = "nixos-tower";

	services.xserver = {

		# Graphic
		videoDrivers = [nvidia];
	}

	# Bootloader
	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;
	boot.initrd.luks.devices = [{
		device = "/dev/sda2";
		preLVM = true;
	}];
}
