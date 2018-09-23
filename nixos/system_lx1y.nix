{ config, lib, pkgs, ...}: with lib;
{
	# Hostname
	networking.hostName = "nixos-lx1y";

  services.thermald.enable = true; # otherwise the cpu frequency/clock is always at maximum

#  powerManagement = {
#    enable = false;
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
    enable = true;
		layout = "us";
		xkbOptions = "eurosign:e";

    #desktopManager.plasma5.enable = true;
    #desktopManager.default = "plasma5";
		windowManager.bspwm.enable = true;
    #windowManager.bspwm.package = "pkgs.bspwm-unstable";
		windowManager.default = "bspwm";
		windowManager.bspwm.configFile = "/home/user/dotfiles/common/bspwm/bspwmrc";
		windowManager.bspwm.sxhkd.configFile= "/home/user/dotfiles/common/bspwm/sxhkdrc";
		desktopManager.xterm.enable = false;

		displayManager.auto = {
			enable = true;
			user = "user";
		};


		# Graphic
		videoDrivers = ["intel"];

    libinput = {
      enable = true;
      clickMethod = "buttonareas";
      disableWhileTyping = true;
    };
		# Notebook
		#synaptics = {
    #          enable = true;
		#          twoFingerScroll = true;
    #          vertTwoFingerScroll = true;
    #          additionalOptions = ''
    #              MatchDevicePath "/dev/input/event*"
    #              Option "TapButton1" "1"
    #              Option "TapButton2" "2"
    #              Option "TapButton3" "3"
    #              Option "ClickPad" "true"
    #              Option "EmulateMidButtonTime" "0"
    #              Option "SoftButtonAreas" "50% 0 82% 0 0 0 0 0"
    #          '';
    #          maxSpeed = "0.7";
    #          minSpeed = "0.5";
    #};
    #libinput.clickMethod = "buttonareas";
		multitouch.enable = true;
	};

	#System Language
	i18n = {
		defaultLocale = "en_US.UTF-8";
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
      newc="/dea/org/dev/template/new_project.sh";
		};
	};

  #Andorid Debug Bridge (adb)
  programs.adb.enable = true;

	# Users
	# user
	users.extraUsers.user = {
		isNormalUser = true;
		home = "/home/user";
		extraGroups = ["davfs2""wheel" "networkmanager" "vboxusers" "dialout" "adbusers" "lp" "scanner" "plugdev" "docker"];
	};

	virtualisation.virtualbox.host.enable = true;
  virtualisation.docker.enable = true;

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
  boot.kernelPackages = pkgs.linuxPackages_latest; #_4_9; #_latest;

  # Bootloader
	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;
}
