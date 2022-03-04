export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# Application homes
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export PYENV_ROOT="$XDG_DATA_HOME/pyenv"
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java
export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME/jupyter"
export HISTFILE="$XDG_STATE_HOME"/zsh/history
#export KODI_HOME="$XDG_CONFIG_HOME"
zstyle ':completion:*' cache-path $XDG_CACHE_HOME/zsh/zcompcache
export AWS_SHARED_CREDENTIALS_FILE="$XDG_CONFIG_HOME"/aws/credentials
export AWS_CONFIG_FILE="$XDG_CONFIG_HOME"/aws/config
export TMUX_PLUGIN_MANAGER_PATH="$XDG_DATA_HOME/tmux/plugins"

export PATH="$PATH:$HOME/.local/bin"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
export PATH="$PATH:$HOME/.poetry/bin"
export GUIX_PROFILE="$XDG_CONFIG_HOME/guix/current"
export PATH="$PATH:$CARGO_HOME/bin"
