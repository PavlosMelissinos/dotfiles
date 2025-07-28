{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "pavlos";
  home.homeDirectory = "/home/pavlos";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.11"; # Please read the comment before changing.

  # Allow unfree packages
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (pkgs.lib.getName pkg) [
    "claude-code"
    "spotify"
    "steam"
    "steam-unwrapped"
    "steam-run"
  ];

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.shellAliases.pip = "noglob pip";
  home.packages = with pkgs; [
    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
    awscli2
    babashka
    claude-code
    clojure
    curl
    flameshot
    firefox
    git
    gum
    handbrake
    htop
    jdk21
    libreoffice
    logrotate
    nodejs_22
    powerline-fonts
    pyright
    (python312.withPackages(ps: with ps; [yapf requests]))
    qbittorrent
    ripgrep
    signal-desktop
    # slack
    spotify
    #swaylock
    #swaylock-effects
    # stremio  # Could not initialize GLX
    tmux
    uv
    vscodium
    wofi
    xdg-utils
    zsh

    # Development tools
    rustc
    cargo
    cmake
    meson
    nix
    openssl
    guile
    rlwrap

    # Wayland/Sway desktop environment components
    waybar
    mako
    swayidle
    kanshi
    brightnessctl
    wob
    playerctl
    gammastep
    wlogout
    swappy
    wf-recorder

    # Desktop applications
    vlc
    mpv
    strawberry
    evince
    imagemagick
    imv
    nomacs
    font-manager
    nyxt
    thunderbird
    pidgin
    godot
    steam
    yt-dlp
    element-desktop
    dbeaver-bin
    appimage-run
    mesa
    libGL
    libglvnd

    # Remaining packages
    xfce.thunar
    qt6.qtbase
    geoclue2
    docker-compose
    gnome-control-center
    motion
    pavucontrol
    bsdgames
    mc
    xorg.xeyes
    iftop
    pv
    lilypond

    # System libraries and fonts
    fontconfig
    cacert  # replaces nss-certs
    glibcLocales
    dejavu_fonts  # replaces font-dejavu
    liberation_ttf  # replaces font-liberation
    font-awesome  # replaces font-awesome
    freefont_ttf  # replaces font-gnu-freefont
    # pth - not needed in modern systems
    # glibc - system-provided
    # font-ghostscript - not essential
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  # home.file = {
  #   # Swaylock config in native format
  #   ".config/swaylock/config".source = ./configs/swaylock/config;
  # };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/pavlos/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    BROWSER = "firefox";
    EDITOR = "emacs";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
    TMUX_HOME = "$HOME/.config/tmux"; # used by oh-my-tmux
    NIXOS_OZONE_WL = "1";
    NIXPKGS_ALLOW_UNFREE = 1;
    LANG="en_US.UTF-8";
    LC_ALL="en_US.UTF-8";

    # PyEnv configuration
    PYENV_ROOT = "$XDG_DATA_HOME/pyenv";
  };

  # Clean PATH management through home-manager
  home.sessionPath = [
    "$HOME/.nix-profile/bin"           # Nix profile binaries (home-manager packages) - HIGHEST PRIORITY
    "$HOME/.local/bin"                 # Local binaries
    "$PYENV_ROOT/bin"                  # PyEnv
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages = epkgs: [
      epkgs.nix-mode
      epkgs.magit
    ];
  };

  programs.firefox = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userEmail = "pavlos@baresquare.com";
    aliases.lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";

    extraConfig = {
      core = {
        autocrlf = false;
	      attributesfile = ".gitattributes";
      };
      diff.clojure.xfuncname = "(^\\(.*|\\s*\\(defn.*)";
      include.path = "git/.config";
      init.defaultBranch = "main";
      merge.conflictStyle = "diff3";
      pull.rebase = false;
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Enable swaylock but manage config file manually via home.file
  # programs.swaylock.enable = true;

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    history.path = "${config.xdg.stateHome}/zsh/history";
    dotDir = ".config/zsh";
    plugins = [
      {
        name = "theme-and-appearance";
        file = ".config/zsh/scripts/theme-and-appearance.zsh";
        src = builtins.fetchGit {
          url = "https://github.com/PavlosMelissinos/dotfiles";
          rev = "396ffbc99966dfebba044a60fa87fddb8b3008f6";
        };
      }
      {
        name = "theme-agnosterp";
        file = ".config/zsh/theme-agnosterp.zsh";
        src = builtins.fetchGit {
          url = "https://github.com/PavlosMelissinos/dotfiles";
          rev = "460b5f98b7ad33a0267c41f3fd66ebfe22190188";
        };
      }
      {
        name = "zsh-syntax-highlighting";
        file = "zsh-syntax-highlighting.zsh";
        src = builtins.fetchGit {
          url = "https://github.com/zsh-users/zsh-syntax-highlighting";
          rev = "e0165eaa730dd0fa321a6a6de74f092fe87630b0";
        };
      }
      {
        name = "zsh-autosuggestions";
        file = "zsh-autosuggestions.zsh";
        src = builtins.fetchGit {
          url = "https://github.com/zsh-users/zsh-autosuggestions";
          rev = "c3d4e576c9c86eac62884bd47c01f6faed043fc5";
        };
      }
    ];
    initContent = (builtins.readFile ./.zshrc);
  };
  #users.users.pavlos.shell = pkgs.zsh;

  # services.gpg-agent = {
  #   enable = true;
  #   defaultCacheTtl = 1800;
  #   enableSshSupport = true;
  #   # pinentryPackage = pkgs.pinentry-qt;
  #   # pinentryPackage = pkgs.pinentry-gnome3;
  #   # pinentryPackage = pkgs.pinentry-curses;
  #   pinentryPackage = pkgs.pinentry;
  #   # pinentryFlavor = "curses";
  # };

  # Declarative logrotate configuration managed as home files
  home.file.".config/logrotate/logrotate.conf" = {
    text = ''
      # Personal logrotate configuration

      # Slack logs
      ${config.home.homeDirectory}/.config/Slack/logs/*.log {
          size 5M
          rotate 3
          compress
          delaycompress
          missingok
          notifempty
          create 644 ${config.home.username} ${config.home.username}
      }

      # Antigen debug log
      ${config.home.homeDirectory}/.cache/antigen/debug.log {
          size 1M
          rotate 3
          compress
          delaycompress
          missingok
          notifempty
          create 644 ${config.home.username} ${config.home.username}
          postrotate
              # Truncate to prevent excessive growth
              if [ -f ${config.home.homeDirectory}/.cache/antigen/debug.log ]; then
                  tail -n 5000 ${config.home.homeDirectory}/.cache/antigen/debug.log > ${config.home.homeDirectory}/.cache/antigen/debug.log.tmp
                  mv ${config.home.homeDirectory}/.cache/antigen/debug.log.tmp ${config.home.homeDirectory}/.cache/antigen/debug.log
              fi
          endscript
      }

      # Emacs logs (if they exist, they should be in .cache according to XDG)
      ${config.home.homeDirectory}/.cache/emacs/*.log {
          size 2M
          rotate 2
          compress
          delaycompress
          missingok
          notifempty
          create 644 ${config.home.username} ${config.home.username}
      }
    '';
  };

  # Systemd user services for automated maintenance
  systemd.user.services.log-cleanup = {
    Unit = {
      Description = "Daily log rotation and system cleanup";
      After = [ "graphical-session.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.writeShellScript "log-cleanup" ''
        set -euo pipefail

        # Cache size limits (in MB)
        CACHE_LIMIT_TOTAL=2048      # 2GB total cache size limit
        CACHE_LIMIT_DIR=512         # 512MB per directory limit
        CACHE_WARNING_THRESHOLD=1536 # Warn at 1.5GB usage

        echo "[$(date)] Starting automated log cleanup with cache monitoring"

        # Monitor cache size before cleanup
        cache_size_mb=$(du -sm ${config.home.homeDirectory}/.cache 2>/dev/null | cut -f1 || echo "0")
        echo "Cache directory size: ''${cache_size_mb}MB"

        if [ "$cache_size_mb" -gt "$CACHE_WARNING_THRESHOLD" ]; then
          echo "WARNING: Cache size ($cache_size_mb MB) exceeds warning threshold ($CACHE_WARNING_THRESHOLD MB)"
        fi

        # Run logrotate first
        ${pkgs.logrotate}/bin/logrotate -s ${config.home.homeDirectory}/.local/state/logrotate.state ${config.home.homeDirectory}/.config/logrotate/logrotate.conf 2>/dev/null || true

        # Slack log management - truncate files that are still too large
        find ${config.home.homeDirectory}/.config/Slack/logs -name "*.log" -size +5M -exec sh -c 'tail -c 1048576 "$1" > "$1.tmp" && mv "$1.tmp" "$1"' _ {} \; 2>/dev/null || true

        # Enhanced cache cleanup with size monitoring
        echo "Starting cache cleanup phase..."

        # Clean temporary files first
        find ${config.home.homeDirectory}/.cache -name "*.tmp" -mtime +7 -delete 2>/dev/null || true
        find ${config.home.homeDirectory}/.cache -name "*.log" -size +10M -mtime +30 -delete 2>/dev/null || true

        # Monitor individual cache directories that exceed size limits
        for cache_dir in ${config.home.homeDirectory}/.cache/*/; do
          if [ -d "$cache_dir" ]; then
            dir_size_mb=$(du -sm "$cache_dir" 2>/dev/null | cut -f1 || echo "0")
            dir_name=$(basename "$cache_dir")

            if [ "$dir_size_mb" -gt "$CACHE_LIMIT_DIR" ]; then
              echo "Directory $dir_name exceeds limit: ''${dir_size_mb}MB > ''${CACHE_LIMIT_DIR}MB"

              # Clean old files more aggressively for oversized directories
              find "$cache_dir" -type f -mtime +14 -delete 2>/dev/null || true
              find "$cache_dir" -type f -size +50M -mtime +7 -delete 2>/dev/null || true

              # Re-check size after cleanup
              new_size_mb=$(du -sm "$cache_dir" 2>/dev/null | cut -f1 || echo "0")
              echo "Directory $dir_name after cleanup: ''${new_size_mb}MB"
            fi
          fi
        done

        # If total cache still exceeds limit, perform aggressive cleanup
        cache_size_after_mb=$(du -sm ${config.home.homeDirectory}/.cache 2>/dev/null | cut -f1 || echo "0")
        if [ "$cache_size_after_mb" -gt "$CACHE_LIMIT_TOTAL" ]; then
          echo "Cache still exceeds total limit ($cache_size_after_mb MB > $CACHE_LIMIT_TOTAL MB), performing aggressive cleanup"

          # Remove files older than 3 days from oversized cache
          find ${config.home.homeDirectory}/.cache -type f -mtime +3 -delete 2>/dev/null || true
          # Remove large files older than 1 day
          find ${config.home.homeDirectory}/.cache -type f -size +20M -mtime +1 -delete 2>/dev/null || true

          final_cache_size_mb=$(du -sm ${config.home.homeDirectory}/.cache 2>/dev/null | cut -f1 || echo "0")
          echo "Final cache size after aggressive cleanup: ''${final_cache_size_mb}MB"
        fi

        # Clean trash (keep files less than 30 days old)
        find ${config.home.homeDirectory}/.local/share/Trash/files -mtime +30 -delete 2>/dev/null || true
        find ${config.home.homeDirectory}/.local/share/Trash/info -mtime +30 -delete 2>/dev/null || true

        # Vacuum SQLite databases to reclaim space
        for db in $(find ${config.home.homeDirectory}/.config -name "*.db" -o -name "*.sqlite" 2>/dev/null); do
          ${pkgs.sqlite}/bin/sqlite3 "$db" "VACUUM;" 2>/dev/null || true
        done

        # Final cache size report
        final_total_mb=$(du -sm ${config.home.homeDirectory}/.cache 2>/dev/null | cut -f1 || echo "0")
        space_freed=$((cache_size_mb - final_total_mb))
        echo "Cache cleanup summary: freed ''${space_freed}MB (''${cache_size_mb}MB -> ''${final_total_mb}MB)"

        echo "[$(date)] Automated log cleanup completed with cache monitoring"
      ''}";
      StandardOutput = "journal";
      StandardError = "journal";
    };
  };

  systemd.user.timers.log-cleanup = {
    Unit = {
      Description = "Run log cleanup daily at random time";
      Requires = [ "log-cleanup.service" ];
    };
    Timer = {
      OnCalendar = "daily";
      RandomizedDelaySec = "30m";
      Persistent = true;
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };

  # Mako notification daemon service
  systemd.user.services.mako = {
    Unit = {
      Description = "Mako notification daemon";
      Documentation = "man:mako(1)";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };
    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.Notifications";
      ExecStart = "${pkgs.mako}/bin/mako";
      ExecReload = "${pkgs.mako}/bin/makoctl reload";
      Restart = "on-failure";
      RestartSec = "5s";
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };

  # Health monitoring service for critical user services
  systemd.user.services.service-health-check = {
    Unit = {
      Description = "Monitor health of critical user services";
      After = [ "graphical-session.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.writeShellScript "service-health-check" ''
        set -eo pipefail

        echo "[$(date)] Starting service health check"

        # List of critical services to monitor
        services="mako.service log-cleanup.timer"

        # Health check results
        healthy=0
        unhealthy=0
        total=0

        for service in $services; do
          echo "Checking service: $service"
          total=$((total + 1))

          # Get service status
          if status=$(systemctl --user is-active "$service" 2>/dev/null); then
            case "$status" in
              "active")
                echo "✓ $service is active"
                healthy=$((healthy + 1))
                ;;
              "inactive")
                echo "- $service is inactive (normal for timers)"
                healthy=$((healthy + 1))
                ;;
              "failed")
                echo "✗ $service has failed"
                unhealthy=$((unhealthy + 1))

                # Try to restart failed services (not timers)
                if echo "$service" | grep -q "\.service$"; then
                  echo "Attempting to restart $service"
                  if systemctl --user restart "$service" 2>/dev/null; then
                    if new_status=$(systemctl --user is-active "$service" 2>/dev/null); then
                      echo "Restart succeeded: $service is now $new_status"
                      if [ "$new_status" = "active" ]; then
                        healthy=$((healthy + 1))
                        unhealthy=$((unhealthy - 1))
                      fi
                    fi
                  else
                    echo "Restart failed for $service"
                  fi
                fi
                ;;
              *)
                echo "? $service status: $status"
                unhealthy=$((unhealthy + 1))
                ;;
            esac
          else
            echo "! $service: not found or inaccessible"
            unhealthy=$((unhealthy + 1))
          fi

          echo ""
        done

        # Summary report
        echo "=== Health Check Summary ==="
        echo "Total services: $total"
        echo "Healthy: $healthy"
        echo "Unhealthy: $unhealthy"

        if [ "$unhealthy" -eq 0 ]; then
          echo "✓ All services are healthy"
        else
          echo "⚠ $unhealthy service(s) need attention"
        fi

        echo "[$(date)] Service health check completed"
      ''}";
      StandardOutput = "journal";
      StandardError = "journal";
    };
  };

  # Timer for regular health checks
  systemd.user.timers.service-health-check = {
    Unit = {
      Description = "Run service health check every 30 minutes";
      Requires = [ "service-health-check.service" ];
    };
    Timer = {
      OnCalendar = "*:0/30";  # Every 30 minutes
      Persistent = true;
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      # Web browsing - using Firefox Nightly as your default
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
      "x-scheme-handler/ftp" = "firefox.desktop";
      "x-scheme-handler/chrome" = "firefox.desktop";
      "text/html" = "firefox.desktop";
      "application/x-extension-htm" = "firefox.desktop";
      "application/x-extension-html" = "firefox.desktop";
      "application/x-extension-shtml" = "firefox.desktop";
      "application/xhtml+xml" = "firefox.desktop";
      "application/x-extension-xhtml" = "firefox.desktop";
      "application/x-extension-xht" = "firefox.desktop";

      # Video files - using Celluloid as default
      "video/mp4" = "io.github.celluloid_player.Celluloid.desktop";
      "video/mpeg" = "io.github.celluloid_player.Celluloid.desktop";

      # Email
      "x-scheme-handler/mailto" = "userapp-Daily-HMC2R1.desktop";
      "message/rfc822" = "userapp-Daily-HMC2R1.desktop";
      "x-scheme-handler/mid" = "userapp-Daily-HMC2R1.desktop";

      # News/RSS
      "x-scheme-handler/news" = "userapp-Daily-SYH9Q1.desktop";
      "x-scheme-handler/snews" = "userapp-Daily-SYH9Q1.desktop";
      "x-scheme-handler/nntp" = "userapp-Daily-SYH9Q1.desktop";
      "x-scheme-handler/feed" = "userapp-Daily-GT38Q1.desktop";
      "application/rss+xml" = "userapp-Daily-GT38Q1.desktop";
      "application/x-extension-rss" = "userapp-Daily-GT38Q1.desktop";

      # Calendar
      "x-scheme-handler/webcal" = "userapp-Daily-8YM9Q1.desktop";
      "text/calendar" = "userapp-Daily-8YM9Q1.desktop";
      "application/x-extension-ics" = "userapp-Daily-8YM9Q1.desktop";
      "x-scheme-handler/webcals" = "userapp-Daily-8YM9Q1.desktop";

      # Social/Communication
      "x-scheme-handler/tootle" = "com.github.bleakgrey.tootle.desktop";
      # "x-scheme-handler/viber" = "viber.desktop";  # Disabled - using AppImage

      # Archives
      "application/zip" = "libreoffice-startcenter.desktop";
    };

    associations.added = {
      # Web browsers alternatives
      "x-scheme-handler/http" = [
        "firefox.desktop"
        "firefox-wayland.desktop"
      ];
      "x-scheme-handler/https" = [
        "firefox.desktop"
        "firefox-wayland.desktop"
      ];
      "text/html" = [
        "firefox.desktop"
        "firefox-wayland.desktop"
      ];

      # Video players alternatives
      "video/mp4" = [
        "io.github.celluloid_player.Celluloid.desktop"
        "mpv.desktop"
        "vlc.desktop"
      ];
      "video/x-matroska" = [
        "mpv.desktop"
        "io.github.celluloid_player.Celluloid.desktop"
      ];

      # Image viewers
      "image/jpeg" = [
        "imv-dir.desktop"
        "swappy.desktop"
        "imv.desktop"
      ];
    };
  };
}
