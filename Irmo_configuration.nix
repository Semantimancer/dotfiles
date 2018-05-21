# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  # Boot
  imports =
    [ # Include the results of the hardware scan. 
      # I'm choosing to ignore the hardware scan because I've seen it reset and lose my /boot directory twice now.
      # Instead, I will manually migrate everything in hardware-configuration.nix into this file and mark it as such.
      # ./hardware-configuration.nix
      <nixpkgs/nixos/modules/installer/scan/not-detected.nix> # From hardware-configuration.nix
    ];

  # hardware-configuration.nix

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "sd_mod" "sr_mod" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/d844d67e-8350-4824-8a77-308495384de6";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/FBA8-92AA";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/0746fc53-b5ec-4d2c-a7d6-a18357a6c53e"; }
    ];

  nix.maxJobs = lib.mkDefault 12;
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
    ranger wget feh mupdf vim
    qalculate-gtk libqalculate

    # Writing
    focuswriter libreoffice
    corefonts

    # Coding
    git
    gcc ghc

    # Graphics 
    x11
    gimp scrot

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

  # services.xserver.videoDrivers = [ "nvidia" ]; Can't seem to get this to work.
  hardware.opengl.driSupport32Bit = true;

  # Sound
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # Other

  services.xserver.xkbOptions = "caps:swapescape";

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
