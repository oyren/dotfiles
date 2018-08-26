{ config, lib, pkgs, ...}: with lib;
{
	# Hostname
	networking.hostName = "nixos-mate";

  services.thermald.enable = true; # otherwise the cpu frequency/clock is always at maximum

	services.xserver = {
    enable = true;
		layout = "de";
		xkbOptions = "eurosign:e";

    desktopManager.mate.enable = true;
    desktopManager.default = "mate";

		displayManager.auto = {
			enable = true;
			user = "user";
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
	# user
	users.extraUsers.user = {
		isNormalUser = true;
		home = "/home/user";
		extraGroups = ["davfs2""wheel" "networkmanager" "vboxusers" "dialout" "adbusers" "lp" "scanner" "plugdev" "docker"];
	};

  # Bootloader
	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;
}
