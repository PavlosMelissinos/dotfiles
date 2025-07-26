# PATH management now handled by home-manager sessionPath
# This file only handles specialized initialization

typeset -U path  # Ensure uniqueness in zsh

# PyEnv initialization (PATH handled by home-manager)
if [[ -d "$HOME/.local/share/pyenv" ]] && command -v pyenv >/dev/null 2>&1 && [[ -z "$PYENV_INITIALIZED" ]]; then
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
    export PYENV_INITIALIZED=1
fi

# Local binaries already handled by home-manager

# Source Guix environment if available
if [[ -n "$GUIX_PROFILE" && -f "$GUIX_PROFILE/etc/profile" ]]; then
    . "$GUIX_PROFILE/etc/profile"
fi

# Debug: uncomment to see final PATH
# echo "Final PATH entries: $(echo $PATH | tr ':' '\n' | wc -l)"