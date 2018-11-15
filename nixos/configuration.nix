{ config, pkgs, lib, ...}:

let
	nixcfg = {allowUnfree = true;};
	stable = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-18.09.tar.gz) { config = nixcfg; };
	rolling = import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/nixos-unstable.tar.gz) {  config = nixcfg; };

	pkgs = stable;
in
{
	imports = [
		/etc/nixos/hardware-configuration.nix
		./system.nix
		./xfce_bspwm.nix
		(import ./package.nix {inherit lib config pkgs stable rolling; })
	];

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

	#security.sudo.extraConfig =
	#''
	#	user ALL=(ALL) NOPASSWD: /home/user/.dotfiles/script/webdav.sh
	#'';

	## Fonts
	#fonts = {
	#	fonts = (with pkgs; [
	#		siji
	#		source-code-pro
  #    font-awesome_5
	#	]);
	#};

	##GTK3 Theme
	#environment.etc."xdg/gtk-3.0/settings.ini" = {
	#	text = ''
	#		gtk-icon-theme-name=arc
	#		gtk-theme-name=arc-dark
  #    breeze-icons
	#	'';
	#	mode = "444";
	#};

  #environment.sessionVariables = {
  #    #XCURSOR_PATH = [
  #    #  "${config.system.path}/share/icons"
  #    #  "$HOME/.icons"
  #    #  "$HOME/.nix-profile/share/icons/"
  #    #];
  #    GTK_DATA_PREFIX = [
  #      "${config.system.path}"
  #    ];
  #};

#networking.firewall.allowedTCPPorts = [ 27036 ];
#networking.firewall.allowedUDPPorts = [ 27036 ];
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
	#nix.gc.automatic = true;
	#nix.gc.dates = "17:00";
	#nix.gc.options = "--delete-older-than 30d";
	# boot.cleanTmpDir = true;

  # udev
  #services.udev.extraRules = ''
  #ACTION=="change", SUBSYSTEM=="drm", ENV{HOTPLUG}=="1", RUN+="${pkgs.stdenv.shell} -c /home/user/dotfiles/common/scripts/monitor-hotplug.sh"
  #'';


	# Search
	services.locate.enable = true;
	services.locate.interval = "10 * * * *";

	system.stateVersion = "18.03";
}
