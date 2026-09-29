# Claude Code config: writable live dir, manually-approved capture.
# See docs/architecture/adr/0013-claude-code-xdg-config.md.
#
# configs/claude/ in this repo is deployed to a WRITABLE ~/.config/claude
# (not a read-only Nix-store symlink), because Claude Code writes its own
# settings.json at runtime (/model, /config, plugins). home.activation
# below only ever copies repo -> live, and only when the live file has no
# uncaptured edits. Pulling live edits back into the repo is a separate,
# explicit step: claude-config-capture.
{ config, lib, pkgs, ... }:

let
  claudeDotfilesRepo = "${config.home.homeDirectory}/workspace/dotfiles";
  claudeConfigDir = "${config.xdg.configHome}/claude";
  claudeConfigRepoDir =
    "${claudeDotfilesRepo}/.config/home-manager/configs/claude";
  claudeConfigStateDir = "${config.xdg.stateHome}/claude-config/deployed";

  # Top-level names under configs/claude/ this mechanism tracks. Runtime
  # state (credentials, history, transcripts, caches, plugin checkouts,
  # skills/synced) is deliberately left out and never touched here.
  claudeConfigTopLevel = [
    "settings.json"
    "keybindings.json"
    "CLAUDE.md"
    "agents"
    "commands"
    "output-styles"
    "hooks"
    "skills"
  ];

  # Shared by claude-config-capture and claude-config-check: walks the
  # tracked top-level names, diffing the live copy under $live against the
  # repo copy under $repo (skills/synced is always excluded). $mode is
  # "check" (report only) or "capture" (interactively pull changes into the
  # repo working tree; never commits).
  claudeConfigSync = pkgs.writeShellScript "claude-config-sync" ''
    set -uo pipefail
    mode="$1"
    live="${claudeConfigDir}"
    repo="${claudeConfigRepoDir}"
    state="${claudeConfigStateDir}"
    mkdir -p "$repo" "$state"

    list_paths() {
      local top="$1"
      {
        if [ -d "$live/$top" ]; then
          (cd "$live/$top" && find . -type f ! -path './synced/*' ! -path './synced')
        elif [ -e "$live/$top" ]; then
          echo "."
        fi
        if [ -d "$repo/$top" ]; then
          (cd "$repo/$top" && find . -type f)
        elif [ -e "$repo/$top" ]; then
          echo "."
        fi
      } | sort -u
    }

    any=0
    for top in ${lib.escapeShellArgs claudeConfigTopLevel}; do
      # rel is read from fd 3, not fd 0: the interactive `read -p` below
      # needs fd 0 free for the terminal, or it silently reads from this
      # loop's own input instead and every prompt looks like an empty
      # answer.
      while IFS= read -r -u 3 rel; do
        [ -z "$rel" ] && continue
        if [ "$rel" = "." ]; then
          liveFile="$live/$top"; repoFile="$repo/$top"; label="$top"
        else
          rel="''${rel#./}"
          liveFile="$live/$top/$rel"; repoFile="$repo/$top/$rel"; label="$top/$rel"
        fi

        if [ -e "$liveFile" ] && [ -e "$repoFile" ] && cmp -s "$liveFile" "$repoFile"; then
          continue
        fi
        any=1

        if [ "$mode" = "check" ]; then
          echo "claude-config: $label differs from the repo"
          continue
        fi

        echo "=== $label ==="
        if [ -e "$liveFile" ] && [ -e "$repoFile" ]; then
          diff -u "$repoFile" "$liveFile" || true
        elif [ -e "$liveFile" ]; then
          echo "(new in ~/.config/claude, not yet in the repo)"
        else
          echo "(removed from ~/.config/claude, still in the repo)"
        fi

        read -r -p "Capture into repo? [y/N/q] " ans
        case "$ans" in
          y | Y)
            # Must match the activation script's stateFile naming
            # ($claudeState/$relPath.sha256, relPath relative to
            # configs/claude), or a captured file looks "uncaptured"
            # forever on the next switch.
            stateFile="$state/$label.sha256"
            if [ -e "$liveFile" ]; then
              mkdir -p "$(dirname "$repoFile")" "$(dirname "$stateFile")"
              cp "$liveFile" "$repoFile"
              sha256sum "$liveFile" | cut -d' ' -f1 > "$stateFile"
              echo "Captured $label. Review with: git -C ${claudeDotfilesRepo} diff -- .config/home-manager/configs/claude/$top"
            else
              rm -f "$repoFile" "$stateFile"
              echo "Removed $label from the repo."
            fi
            ;;
          q | Q) exit 0 ;;
          *) echo "Skipped $label." ;;
        esac
      done 3< <(list_paths "$top")
    done

    if [ "$any" -eq 0 ]; then
      echo "claude-config: nothing to capture."
    elif [ "$mode" = "check" ]; then
      ${pkgs.libnotify}/bin/notify-send "Claude Code config" \
        "Local changes are not yet captured into the dotfiles repo. Run claude-config-capture."
    fi
  '';
in
{
  programs.claude-code = {
    enable = true;

    # Enforce XDG compliance. Settings are deliberately NOT declared via
    # this module's `settings` option: that deploys a read-only Nix-store
    # symlink, which breaks Claude Code writing its own settings.json at
    # runtime. See the comment at the top of this file.
    #
    # Since this differs from the upstream ~/.claude default, home-manager
    # exports $CLAUDE_CONFIG_DIR=~/.config/claude in hm-session-vars.sh
    # (takes effect on next login, not just a new shell).
    configDir = claudeConfigDir;
  };

  home.packages = [
    pkgs.libnotify
    (pkgs.writeShellScriptBin "claude-config-capture" ''
      exec ${claudeConfigSync} capture
    '')
    (pkgs.writeShellScriptBin "claude-config-check" ''
      exec ${claudeConfigSync} check
    '')
  ];

  # Deploys configs/claude/ into the writable ~/.config/claude, per the
  # rules in ADR-0013: never overwrite a live file that has uncaptured
  # edits (its hash no longer matches what was last deployed/captured).
  home.activation.claudeConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    claudeSrc="${./../configs/claude}"
    claudeLive="${claudeConfigDir}"
    claudeState="${claudeConfigStateDir}"
    run mkdir -p "$claudeLive" "$claudeState"

    while IFS= read -r -d "" srcFile; do
      relPath="''${srcFile#"$claudeSrc"/}"
      liveFile="$claudeLive/$relPath"
      stateFile="$claudeState/$relPath.sha256"
      run mkdir -p "$(dirname "$liveFile")" "$(dirname "$stateFile")"
      srcHash=$(sha256sum "$srcFile" | cut -d' ' -f1)

      # Bootstrap: an earlier generation deployed this path as a
      # read-only symlink into the Nix store (programs.claude-code's own
      # `settings` option, before this module existed). That's Nix's own
      # artifact, not a live edit, so replace it unconditionally.
      if [[ -L "$liveFile" ]] && [[ "$(readlink "$liveFile")" == /nix/store/* ]]; then
        run rm -f "$liveFile"
      fi

      if [[ ! -e "$liveFile" ]]; then
        run install -m644 "$srcFile" "$liveFile"
        run bash -c "printf '%s' \"$srcHash\" > \"$stateFile\""
        continue
      fi

      liveHash=$(sha256sum "$liveFile" | cut -d' ' -f1)
      recordedHash=$(cat "$stateFile" 2>/dev/null || echo "")

      if [[ "$liveHash" != "$recordedHash" ]]; then
        warnEcho "claude-code: ~/.config/claude/$relPath has uncaptured local changes; run claude-config-capture before this takes effect."
        continue
      fi

      if [[ "$srcHash" != "$recordedHash" ]]; then
        run install -m644 "$srcFile" "$liveFile"
        run bash -c "printf '%s' \"$srcHash\" > \"$stateFile\""
      fi
    done < <(find "$claudeSrc" -type f -print0)
  '';

  systemd.user.services.claude-config-check = {
    Unit = {
      Description = "Warn about uncaptured Claude Code config changes";
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${claudeConfigSync} check";
    };
  };

  systemd.user.timers.claude-config-check = {
    Unit = {
      Description = "Daily check for uncaptured Claude Code config changes";
      Requires = [ "claude-config-check.service" ];
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
}
