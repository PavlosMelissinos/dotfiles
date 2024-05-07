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

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.shellAliases.pip = "noglob pip";
  home.packages = [
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
    pkgs.babashka
    pkgs.fortune
    pkgs.git
    pkgs.htop
    pkgs.nodePackages.pyright
    pkgs.powerline-fonts
    pkgs.ripgrep
    pkgs.tmux
    pkgs.zsh
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

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
    EDITOR = "emacs";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
    TMUX_HOME="$HOME/.config/tmux"; # used by oh-my-tmux
  };

  # programs.emacs = {
  #   #enable = true;
  #   # extraPackages = epkgs: [
  #   #   epkgs.nix-mode
  #   #   epkgs.magit
  #   # ];
  # };

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
    initExtra = (builtins.readFile ./.zshrc);
  };
  #users.users.pavlos.shell = pkgs.zsh;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

}
