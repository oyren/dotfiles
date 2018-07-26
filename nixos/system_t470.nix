{ config, lib, pkgs, ...}: with lib;
{
	# Hostname
	networking.hostName = "nixos-t470";

  services.thermald.enable = true; # otherwise the cpu frequency/clock is always at maximum

	services.xserver = {
    enable = true;
		layout = "de";
		xkbOptions = "eurosign:e";

    #desktopManager.plasma5.enable = true;
    #desktopManager.default = "plasma5";
		windowManager.bspwm.enable = true;
    #windowManager.bspwm.package = "pkgs.bspwm-unstable";
		windowManager.default = "bspwm";
		windowManager.bspwm.configFile = "/home/sky/dotfiles/common/bspwm/bspwmrc";
		windowManager.bspwm.sxhkd.configFile= "/home/sky/dotfiles/common/bspwm/sxhkdrc";
		desktopManager.xterm.enable = false;

		displayManager.auto = {
			enable = true;
			user = "sky";
		};


		# Graphic
		videoDrivers = ["intel"];

		# Notebook
		synaptics.enable = true;
		synaptics.twoFingerScroll = true;
		multitouch.enable = true;
	};

	#System Language
	i18n = {
		defaultLocale = "de_DE.UTF-8";
	};

	#Time
	time.timeZone = "Europe/Berlin";

	# ZSH
	programs.zsh = {
		enable = true;
		interactiveShellInit = ''
			export EDITOR=nvim
			# Java
			export _JAVA_AWT_WM_NONREPARENTING=1
		'';

		shellAliases = {
			ls="ls --color=auto";
			l="ls -alh";
      r="ranger";
		};
	};


	# Users
	# sky
	users.extraUsers.sky = {
		isNormalUser = true;
		home = "/home/sky";
		extraGroups = ["davfs2""wheel" "networkmanager" "vboxusers" "dialout" "adbusers" "lp" "scanner" "plugdev" "docker"];
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
  # Kernel
  # boot.kernelPackages = pkgs.linuxPackages_latest; #_4_9; #_latest;

  # Bootloader
	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;
}
