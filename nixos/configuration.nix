{ config, pkgs, lib, ...}:

let
	nixcfg = {allowUnfree = true;};
	stable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-17.03.tar.gz) { config = nixcfg; };
	rolling = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) {  config = nixcfg; };

	pkgs = stable;
in
{
	imports = [
		./hardware-configuration.nix
		./system.nix
		(import ./package.nix {inherit lib config pkgs stable rolling; })
	];
	services.xserver = {

		enable = true;
		layout = "us";
		xkbOptions = "eurosign:e";

		windowManager.bspwm.enable = true;
		windowManager.default = "bspwm";
		windowManager.bspwm.configFile = "/home/user/dotfiles/common/bspwm/bspwmrc";
		windowManager.bspwm.sxhkd.configFile= "/home/user/dotfiles/common/bspwm/sxhkdrc";
		desktopManager.xterm.enable = false;

		displayManager.auto = {
			enable = true;
			user = "user";
		};

		xrandrHeads = ["DP-1" "HDMI-0"];
	};

	# Systemd
	systemd = {
		services.checkmail = {
			description = "check mail";
			serviceConfig = {
				Type = "oneshot";
				ExecStart = /home/user/dotfiles/common/scripts/checkmail.sh;
			};
		};
		timers.checkmail = {
			description = "Check Mail ever fifteen minutes";
			timerConfig = {
				Persistent= false;
				OnBootSec = "5min";
				OnUnitActiveSec = "15min";
				Unit= "checkmail.service";
			};
			wantedBy = [ "default.target" ];
		};
	};
	# Kernel
	#boot.kernelPackages = pkgs.linuxPackages_latest; #_4_9; #_latest;

	#System Language
	i18n = {
		defaultLocale = "en_US.UTF-8";
	};

	#Time
	time.timeZone = "Europe/Berlin";

	security.sudo.extraConfig =
	''
		user ALL=(ALL) NOPASSWD: /home/user/.dotfiles/script/webdav.sh
	'';

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
		'';
		mode = "444";
	};

	#Redshift
	services.redshift = {
		enable = true;
		latitude = "50";
		longitude = "10";
	};

	#Network
	networking.networkmanager.enable = true;

	#Hardware
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
	nix.gc.dates = "16:14";
	nix.gc.options = "--delete-older-than 30d";
	nix.extraOptions = ''
		gc-keep-output = true
		gc-keep-derivations = true
		auto-optimise-stor = true
	'';
	# boot.cleanTmpDir = true;

	# Search
	services.locate.enable = true;
	services.locate.interval = "10 * * * *";

	# Users
	# user
	users.extraUsers.user = {
		isNormalUser = true;
		home = "/home/user";
		extraGroups = ["davfs2""wheel" "networkmanager" "vboxusers"];
	};

	virtualisation.virtualbox.host.enable = true;

	system.stateVersion = "17.03";
}
