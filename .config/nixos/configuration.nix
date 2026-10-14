# NixOS System Configuration for pavlos@localhost
# Generated for Fedora to NixOS migration
# Compatible with existing home-manager setup

{ config, pkgs, lib, ... }:

let
  greeterWallpaper =
    "${pkgs.sway}/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png";

  # Blurred/dimmed copy of the wallpaper, baked in at build time so both
  # the ReGreet screen and the idle laptop panel show the same faded
  # image without any runtime filtering.
  greeterWallpaperFaded = pkgs.runCommand "greeter-wallpaper"
    { nativeBuildInputs = [ pkgs.imagemagick ]; } ''
      mkdir -p $out
      magick ${greeterWallpaper} -blur 0x12 -modulate 55 \
        $out/wallpaper.png
    '';

  # Minimal, greeter-only sway session: puts the Dell monitor on the
  # left (matching the normal session layout in
  # configs/sway/outputs-work.conf) and shows the faded wallpaper on
  # whichever output ReGreet isn't using. Focusing the Dell before
  # launching ReGreet puts its window there; if the Dell isn't
  # connected the focus command just fails and ReGreet opens on the
  # laptop panel instead.
  greeterSwayConfig = pkgs.writeText "greeter-sway.conf" ''
    output * bg ${greeterWallpaperFaded}/wallpaper.png fill
    output "Dell Inc. DELL U2518D 3C4YP88P229L" position 0 0 scale 1.25
    output eDP-1 position 2048 241

    default_border none
    hide_edge_borders both

    exec "swaymsg focus output 'Dell Inc. DELL U2518D 3C4YP88P229L'; \
          ${pkgs.dbus}/bin/dbus-run-session \
          ${lib.getExe config.programs.regreet.package}; \
          swaymsg exit"
  '';
in
{
  imports = [
    # Multiple fallback paths for maximum compatibility
    (let
      candidates = [
        ./hardware-configuration.nix           # Local to flake (installation)
        /etc/nixos/hardware-configuration.nix  # System location (post-install)
        /mnt/etc/nixos/hardware-configuration.nix  # Just in case
      ];
      existing = builtins.filter builtins.pathExists candidates;
    in
      if existing != []
      then builtins.head existing
      else throw "No hardware-configuration.nix found in any expected location")
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

  # acpi_call: lets us pulse an ACPI power resource (e.g. the CNVi
  # Bluetooth core's _OFF/_ON methods) from userspace when the Intel
  # Bluetooth controller hangs and falls off the USB bus.
  boot.extraModulePackages = [ config.boot.kernelPackages.acpi_call ];
  boot.kernelModules = [ "acpi_call" ];

  # Intel CNVi Bluetooth has been observed dropping off the USB bus
  # after a "reset full-speed device" error. USB autosuspend is the
  # most common trigger for this on Intel btusb controllers.
  boot.extraModprobeConfig = ''
    options btusb enable_autosuspend=0
  '';

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
          ControllerMode = "dual"; # classic BR/EDR + BLE
        };
      };
    };
  };

  # Networking configuration
  networking = {
    # This file is shared, unmodified, across both the work and home
    # machines (see flake.nix's hostPrimaryUser table, which wires the
    # right home-manager profile per host). "legion" is the default
    # here since that's this (work) machine; the home machine
    # overrides it with a plain assignment (higher priority than
    # mkDefault) in its own host-specific hardware-configuration.nix.
    hostName = lib.mkDefault "legion";
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
    wireplumber = {
      enable = true;
      # Prefer high-quality A2DP (SBC-XQ/AAC) for playback and mSBC for
      # mic/duplex. Disabling hardware volume avoids a known class of
      # A2DP<->HFP switch hangs (e.g. after Teams calls).
      extraConfig."51-bluetooth" = {
        "monitor.bluez.properties" = {
          "bluez5.enable-sbc-xq" = true;
          "bluez5.enable-msbc" = true;
          "bluez5.enable-hw-volume" = false;
          "bluez5.codecs" = [ "sbc_xq" "aac" "sbc" ];
          "bluez5.roles" = [
            "a2dp_sink"
            "a2dp_source"
            "hfp_hf"
            "hfp_ag"
            "hsp_hs"
            "hsp_ag"
          ];
        };
      };
    };
  };

  # Display manager and desktop environment
  services.displayManager.gdm.enable = false; # Disable GDM - user wants to remove it
  # Alternative: services.greetd for Wayland greeter

  # Wayland-first environment. The greetd module normally runs ReGreet
  # directly under cage (one output, extended across both monitors with
  # no control over placement or background), so we override the
  # session command to launch a minimal sway session instead -- see
  # greeterSwayConfig above for the output layout and wallpaper.
  services.greetd = {
    enable = true;
    settings.default_session = {
      user = "greeter";
      command = "${pkgs.dbus}/bin/dbus-run-session "
        + "${pkgs.sway}/bin/sway --config ${greeterSwayConfig}";
    };
  };

  # ReGreet greeter; the programs.regreet module manages the
  # config/CSS, greeter user, and accounts-daemon dependency, but is
  # launched from greeterSwayConfig above (under sway) instead of its
  # default cage session.
  programs.regreet = {
    enable = true;
    settings = {
      background = {
        path = "${greeterWallpaperFaded}/wallpaper.png";
        fit = "Cover";
      };
      commands = {
        reboot = [ "loginctl" "reboot" ];
        poweroff = [ "loginctl" "poweroff" ];
      };
    };
  };

  services.openssh = {
    enable = true;
    openFirewall = true;                 # firewall is enabled; this opens TCP 22
    settings.PasswordAuthentication = true;
  };

  environment.etc."sudo.conf".text = "Set group_source dynamic";

  # XDG Desktop Portal for Wayland
  xdg.portal = {
    enable = true;
    wlr.enable = true; # For wlroots-based compositors like Sway
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # System services
  services = {
    # Persist journals across reboots to inspect a freeze after a call.
    journald.extraConfig = "Storage=persistent";

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
    # fwupd.enable = true;
  };

  # Docker daemon (the CLI is provided by home-manager's docker-client)
  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
  };

  # Security and authentication
  security = {
    # U2F hardware key support
    pam.services = {
      # login.u2fAuth = true;
      # sudo.u2fAuth = true;
      # swaylock.u2fAuth = true;
    };

    # Real-time scheduling for PipeWire/JACK (needs the audio group).
    pam.loginLimits = [
      { domain = "@audio"; item = "memlock"; value = "unlimited"; }
      { domain = "@audio"; item = "rtprio"; value = "95"; }
    ];

    # pam.u2f = {
    #   enable = true;
    #   settings.cue = true; # Show "touch your security key" message
    #   # U2F mappings will need to be copied from /etc/u2f_mappings
    # };

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
      "docker"         # docker daemon access
    ];
    shell = pkgs.zsh; # Set Zsh as default shell
  };

  users.users.pmelissinos = {
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
      "docker"         # docker daemon access
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
    polkit_gnome

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

    # userspace but common
    foot
    wl-clipboard
    openvpn
  ];

  # Shell configuration
  programs = {
    zsh.enable = true; # Enable Zsh system-wide
    sway = {
      enable = true;
      wrapperFeatures.gtk = true; # GTK applications support
    };

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

  # home-manager.useGlobalPkgs = true (flake.nix) means home.nix's own
  # nixpkgs.config.permittedInsecurePackages (for ventoy) is ignored --
  # it only applies when home-manager builds its own pkgs. Mirror it
  # here so the shared pkgs instance actually allows it.
  nixpkgs.config.permittedInsecurePackages = [ "ventoy-1.1.12" ];

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
