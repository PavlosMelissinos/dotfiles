# NixOS System Configuration for pavlos@localhost
# Generated for Fedora to NixOS migration
# Compatible with existing home-manager setup

{ config, pkgs, ... }:

{
  imports = [
    # Include the results of the hardware scan
    ./hardware-configuration.nix
  ];

  # Boot loader configuration (GRUB for EFI)
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    device = "nodev";  # Don't install to a device, use EFI
    # Configure for dual-boot if needed
    # extraEntries = ''
    #   menuentry "Fedora" {
    #     set root=(hd0,1)
    #     chainloader +1
    #   }
    # '';
  };
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot";

  # Kernel parameters for Intel Skylake
  boot.kernelParams = [
    "intel_pstate=active"          # Intel CPU power management
    "i915.enable_fbc=1"            # Intel graphics framebuffer compression
    "i915.enable_psr=1"            # Panel self-refresh for power saving
  ];

  # Hardware support
  hardware = {
    # CPU microcode updates
    cpu.intel.updateMicrocode = true;

    # Graphics support (Intel HD 520)
    graphics = {
      enable = true;
      enable32Bit = true; # For 32-bit applications like Steam
      extraPackages = with pkgs; [
        intel-media-driver  # VAAPI driver for newer Intel GPUs
        intel-vaapi-driver  # VAAPI driver for older Intel GPUs (HD 520)
        libvdpau-va-gl
      ];
    };

    # Bluetooth support
    bluetooth = {
      enable = true;
      powerOnBoot = true;
      settings = {
        General = {
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };
  };

  # Networking configuration
  networking = {
    hostName = "localhost-nixos"; # Define your hostname
    networkmanager = {
      enable = true;
      wifi.powersave = false; # Disable WiFi power saving for better performance
    };

    # Disable the global useDHCP flag as it's deprecated
    useDHCP = false;

    # Configure network interfaces
    interfaces = {
      enp0s31f6.useDHCP = true;  # Ethernet via ThinkPad dock
      wlp4s0.useDHCP = true;     # WiFi interface
    };

    # Firewall configuration
    firewall = {
      enable = true;
      # allowedTCPPorts = [ 22 ];  # SSH
      # allowedUDPPorts = [ ];
    };

    # Name resolution
    nameservers = [ "8.8.8.8" "8.8.4.4" ]; # Fallback DNS
  };

  # Time zone and locale
  time.timeZone = "Europe/Athens";
  i18n.defaultLocale = "en_US.UTF-8";

  # Console configuration
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Audio system (PipeWire)
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true; # For professional audio if needed
  };

  # Display manager and desktop environment
  services.xserver = {
    enable = true;
    displayManager.gdm.enable = false; # Disable GDM - user wants to remove it
    # Alternative: services.greetd for Wayland greeter
  };

  # Wayland-first environment
  services.greetd = {
    enable = true;
    settings = rec {
      initial_session = {
        command = "${pkgs.sway}/bin/sway";
        user = "pavlos";
      };
      default_session = initial_session;
    };
  };

  # XDG Desktop Portal for Wayland
  xdg.portal = {
    enable = true;
    wlr.enable = true; # For wlroots-based compositors like Sway
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # System services
  services = {
    # Time synchronization
    chrony.enable = true;

    # mDNS/service discovery
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };

    # Printing support
    printing = {
      enable = true;
      drivers = [ pkgs.hplip pkgs.gutenprint ]; # Common printer drivers
    };

    # USB automounting
    udisks2.enable = true;

    # Location services for gammastep
    geoclue2 = {
      enable = true;
      appConfig.gammastep = {
        isAllowed = true;
        isSystem = false;
      };
    };

    # Power management
    upower.enable = true;
    thermald.enable = true; # Intel thermal management

    # Firmware updates
    fwupd.enable = true;
  };

  # Security and authentication
  security = {
    # U2F hardware key support
    pam.services = {
      login.u2fAuth = true;
      sudo.u2fAuth = true;
      swaylock.u2fAuth = true;
    };

    pam.u2f = {
      enable = true;
      settings.cue = true; # Show "touch your security key" message
      # U2F mappings will need to be copied from /etc/u2f_mappings
    };

    # Sudo configuration
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };

    # PolKit for user privilege elevation
    polkit.enable = true;
  };

  # User configuration
  users.users.pavlos = {
    isNormalUser = true;
    description = "Pavlos Melissinos";
    extraGroups = [
      "wheel"          # sudo access
      "networkmanager" # network management
      "audio"          # audio devices
      "video"          # video devices
      "input"          # input devices
      "storage"        # storage management
      "power"          # power management
    ];
    shell = pkgs.zsh; # Set Zsh as default shell
  };

  # System packages (minimal - user packages managed by home-manager)
  environment.systemPackages = with pkgs; [
    # Essential system tools
    wget
    curl
    git
    vim
    htop
    pciutils  # lspci
    usbutils  # lsusb
    file

    # Network tools
    networkmanagerapplet

    # Hardware utilities
    lm_sensors
    smartmontools

    # U2F tools
    pam_u2f

    # Boot and system maintenance
    nixos-option
    
    # Home Manager for standalone usage
    home-manager
  ];

  # Shell configuration
  programs = {
    zsh.enable = true; # Enable Zsh system-wide
    sway = {
      enable = true;
      wrapperFeatures.gtk = true; # GTK applications support
    };

    # Enable home-manager as a NixOS module
    home-manager.enable = true;

    # GPG agent
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  # Environment variables
  environment.variables = {
    # Wayland-first environment
    NIXOS_OZONE_WL = "1"; # Force Electron apps to use Wayland
    MOZ_ENABLE_WAYLAND = "1"; # Force Firefox to use Wayland

    # XDG directories (should match home-manager config)
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };

  # Font configuration
  fonts = {
    enableDefaultPackages = true;
    packages = with pkgs; [
      # Essential fonts (more managed by home-manager)
      dejavu_fonts
      font-awesome
      powerline-fonts
    ];

    fontconfig = {
      defaultFonts = {
        monospace = [ "DejaVu Sans Mono" ];
        sansSerif = [ "DejaVu Sans" ];
        serif = [ "DejaVu Serif" ];
      };
    };
  };

  # Nix configuration
  nix = {
    settings = {
      # Enable flakes
      experimental-features = [ "nix-command" "flakes" ];

      # Optimize store
      auto-optimise-store = true;
    };

    # Garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };
  # Allow unfree packages (for proprietary software)
  nixpkgs.config.allowUnfree = true;

  # System version (using 25.05 for newer nixpkgs)
  system.stateVersion = "25.05";

  # Additional system configuration for compatibility

  # Enable D-Bus
  services.dbus.enable = true;

  # Enable systemd user services
  systemd.user.services = { };

  # Locale settings
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Firmware and microcode
  hardware.enableRedistributableFirmware = true;
}
