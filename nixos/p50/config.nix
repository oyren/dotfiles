{
system.autoUpgrade.channel = "https://nixos.org/channel/nixos-unstable";
allowUnfree = true;
    packageOverrides = pkgs_: with pkgs_; {
                 all = with pkgs; buildEnv {
                 name = "all";
                 paths = [
                       chromium
	               cmake
	               gajim
	               hakuneko
                       mirage
	               mpv
		       mumble
                       perlPackages.ImageExifTool
                       # python27Packages.nbxmpp
                       # python27Packages.python-axolotl
                       # python27Packages.cryptography
                       # python27Packages.protobuf2_6
                       sc-im
                       speedcrunch	
                       qutebrowser
                       qemu
                       steam
                       virtmanager
                       vlc
                       xarchiver
                       mylatex
                       myemacs
                 ];
        };
    
        myemacs = with pkgs; buildEnv {
              name = "myemacs";
              paths = [
                    auctex
                    emacs
                    gv
                    pandoc
                    source-code-pro
                    xclip
              ];
        };

        mylatex = with pkgs; buildEnv {
             name = "mylatex";
             paths = [
                biber
                # jabref
                # jdk
                texlive.combined.scheme-full
                texmaker
              ];
        };
    };
}
