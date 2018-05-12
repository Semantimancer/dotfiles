# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  # Boot
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  boot.loader.systemd-boot.enable = true;
  networking.hostName = "Irmo";
  services.nixosManual.showManual = true;

  # Internationalization
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };
  time.timeZone = "America/New_York";

  # Packages
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    # Basics
    lxterminal termite
    ranger wget feh vim

    # Writing
    focuswriter libreoffice
    corefonts

    # Coding
    git
    gcc ghc

    # Graphics 
    x11
    gimp

    # Web
    chromium vivaldi

    # Chat
    mumble_git skypeforlinux
    irssi

    # Games
    steam

    # Music
    google-play-music-desktop-player
  ];

  # Graphics
  services.xserver.enable = true;
  services.xserver.autorun = true;

  services.xserver.displayManager.slim.enable = false;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.autoLogin.enable = true;
  services.xserver.displayManager.lightdm.autoLogin.user = "ben";

  services.xserver.desktopManager.default = "none";
  services.xserver.desktopManager.xterm.enable = false;

  services.xserver.windowManager.default = "xmonad";
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };

  services.xserver.xkbOptions = "caps:swapescape";

  # services.xserver.videoDrivers = [ "nvidia" ]; Can't seem to get this to work.
  hardware.opengl.driSupport32Bit = true;

  # Sound
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # Program Specifics

  programs.bash.enableCompletion = true;

  # Users
  users.extraUsers.ben = {
    createHome = true;
    home = "/home/ben";
    description = "Ben Kugler";

    extraGroups = [ "wheel" ];

    isNormalUser = true;
    uid = 1000;
  };

  security.sudo.enable = true;
  security.sudo.configFile = "%wheel ALL=(ALL) ALL";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

}
