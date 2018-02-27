{ config, pkgs, lib, ...}:

let
	nixcfg = {allowUnfree = true;};
	stable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-17.09.tar.gz) { config = nixcfg; };
	rolling = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) {  config = nixcfg; };

	pkgs = stable;
in
{
	imports = [
		/etc/nixos/hardware-configuration.nix
		./system.nix
		(import ./package.nix {inherit lib config pkgs stable rolling; })
	];
	services = {

    xserver = {

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


    # Wacom
    wacom.enable = true;
	  #	xrandrHeads = ["DP2-2" "DP2-1" "HDMI-0"];
	  };
    tlp.enable = true;
  };
	# Systemd
#	systemd = {
#		user.services.checkmail = {
#			description = "check mail";
#			serviceConfig = {
#				Type = "oneshot";
#				ExecStart = /home/user/dotfiles/common/scripts/checkmail.sh;
#			};
#		};
#		user.timers.checkmail = {
#			description = "Run the Checkmail Service every 10 Seconds";
#			timerConfig = {
#				Persistent= false;
#        #OnCalendar= "*:*:0/10";
#        OnBootSec = "1min";
#				OnUnitActiveSec = "10s";
#				Unit= "checkmail.service";
#			};
#			wantedBy = [ "timers.target" ];
#		};
#	};
	# Kernel
#  boot.kernelPackages = pkgs.linuxPackages_latest; #_4_9; #_latest;

	#System Language
	i18n = {
		defaultLocale = "en_US.UTF-8";
	};

	#Time
	time.timeZone = "Europe/Berlin";

	#security.sudo.extraConfig =
	#''
	#	user ALL=(ALL) NOPASSWD: /home/user/.dotfiles/script/webdav.sh
	#'';

	# Fonts
	fonts = {
		fonts = (with pkgs; [
			siji
			source-code-pro
		]);
	};

	#GTK3 Theme
	environment.etc."xdg/gtk-3.0/settings.ini" = {
		text = ''
			gtk-icon-theme-name=arc
			gtk-theme-name=arc-dark
      breeze-icons
		'';
		mode = "444";
	};

  environment.sessionVariables = {
      #XCURSOR_PATH = [
      #  "${config.system.path}/share/icons"
      #  "$HOME/.icons"
      #  "$HOME/.nix-profile/share/icons/"
      #];
      GTK_DATA_PREFIX = [
        "${config.system.path}"
      ];
  };

networking.firewall.allowedTCPPorts = [ 27036 ];
networking.firewall.allowedUDPPorts = [ 27036 ];
	#Redshift
#	services.redshift = {
#		enable = true;
#		latitude = "50";
#		longitude = "10";
#    temperature = {
#      night = 3200;
#      day = 4800;
#    };
#	};

	#Network
	networking.networkmanager.enable = true;


  #Scan

	#Hardware
	hardware = {
    sane = {
      enable = true;
#      netConf = "10.10.1.20";
    };
		pulseaudio = {
			enable = true;
			support32Bit = true;
		};

		opengl.driSupport32Bit = true;
	};

	# Printer
	services.printing = {
		enable = true;
		#drivers = [ pkgs.samsungUnifiedLinuxDriver ];
	};

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
	users.defaultUserShell = "/run/current-system/sw/bin/zsh";

	#light
	programs.light.enable = true;

	# SSH
	services.openssh.enable = true;
	services.openssh.forwardX11 = true;

	# SWAP
	#swapDevices = [{
	#	 device = "/var/cache/swap/swap0";
	#}];
	#swapDevices.*.size = "1024";

	# Clean up
	nix.gc.automatic = true;
	nix.gc.dates = "weekly";
	nix.gc.options = "--delete-older-than 30d";
	nix.extraOptions = ''
		gc-keep-output = true
		gc-keep-derivations = true
		auto-optimise-stor = true
	'';
	# boot.cleanTmpDir = true;

  # udev
  #services.udev.extraRules = ''
  #ACTION=="change", SUBSYSTEM=="drm", ENV{HOTPLUG}=="1", RUN+="${pkgs.stdenv.shell} -c /home/user/dotfiles/common/scripts/monitor-hotplug.sh"
  #'';

  #ADB
  programs.adb.enable = true;

	# Search
	services.locate.enable = true;
	services.locate.interval = "10 * * * *";

	# Users
	# user
	users.extraUsers.user = {
		isNormalUser = true;
		home = "/home/user";
		extraGroups = ["davfs2""wheel" "networkmanager" "vboxusers" "dialout" "adbusers" "lp" "scanner" "plugdev" "docker"];
	};

	virtualisation.virtualbox.host.enable = true;
  virtualisation.docker.enable = true;
	system.stateVersion = "17.03";
}
