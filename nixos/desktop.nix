
{ config, pkgs, expr, buildVM, ... }:

let
  iconTheme = pkgs.breeze-icons.out;
  themeEnv = ''
    # QT: remove local user overrides (for determinism, causes hard to find bugs)
    rm -f ~/.config/Trolltech.conf
    # GTK3: remove local user overrides (for determinisim, causes hard to find bugs)
    rm -f ~/.config/gtk-3.0/settings.ini
    # GTK3: add breeze theme to search path for themes
    # (currently, we need to use gnome-breeze because the GTK3 version of kde5.breeze is broken)
    export XDG_DATA_DIRS="${pkgs.gnome-breeze}/share:$XDG_DATA_DIRS"
    # GTK3: add /etc/xdg/gtk-3.0 to search path for settings.ini
    # We use /etc/xdg/gtk-3.0/settings.ini to set the icon and theme name for GTK 3
    export XDG_CONFIG_DIRS="/etc/xdg:$XDG_CONFIG_DIRS"
    # GTK2 theme + icon theme
    export GTK2_RC_FILES=${pkgs.writeText "iconrc" ''gtk-icon-theme-name="breeze"''}:${pkgs.breeze-gtk}/share/themes/Breeze/gtk-2.0/gtkrc:$GTK2_RC_FILES
    # SVG loader for pixbuf (needed for GTK svg icon themes)
    export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)
    # LS colors
    # QT5: convince it to use our preferred style
    export QT_STYLE_OVERRIDE=breeze
  '';

in {

imports = [];
environment.extraInit = ''
  ${themeEnv}
  # these are the defaults, but some applications are buggy so we set them
  # here anyway
  export XDG_CONFIG_HOME=$HOME/.config
  export XDG_DATA_HOME=$HOME/.local/share
  export XDG_CACHE_HOME=$HOME/.cache
'';

# QT4/5 global theme
environment.etc."xdg/Trolltech.conf" = {
  text = ''
    [Qt]
    style=Breeze-dark
  '';
  mode = "444";
};

# GTK3 global theme (widget and icon theme)
environment.etc."xdg/gtk-3.0/settings.ini" = {
  text = ''
    [Settings]
    gtk-icon-theme-name=breeze-dark
    gtk-theme-name=Breeze-dark-gtk
  '';
  mode = "444";
};

# Fonts
fonts = {
	fonts = (with pkgs; [
		siji
		source-code-pro
    font-awesome_5
	]);
};

environment.systemPackages = with pkgs; [
  # Qt theme
  breeze-qt5

  # Icons (Main)
  iconTheme

  # Icons (Fallback)
  gnome3.adwaita-icon-theme
  hicolor_icon_theme

  # These packages are used in autostart, they need to in systemPackages
  # or icons won't work correctly
];

# Make applications find files in <prefix>/share
environment.pathsToLink = [ "/share" ];
}
