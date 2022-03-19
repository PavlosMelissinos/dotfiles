export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# Application homes
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java
export AWS_SHARED_CREDENTIALS_FILE="$XDG_CONFIG_HOME"/aws/credentials
export AWS_CONFIG_FILE="$XDG_CONFIG_HOME"/aws/config
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export DOCKER_CONFIG="$XDG_CONFIG_HOME"/docker 
export GUIX_PROFILE="$XDG_CONFIG_HOME/guix/current"
export HISTFILE="$XDG_STATE_HOME"/zsh/history
export JUPYTER_CONFIG_DIR="$XDG_CONFIG_HOME/jupyter"
#export KODI_HOME="$XDG_CONFIG_HOME"
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc
export PGPASSFILE="$XDG_CONFIG_HOME/pg/pgpass"
export PYENV_ROOT="$XDG_DATA_HOME/pyenv"
export TMUX_PLUGIN_MANAGER_PATH="$XDG_DATA_HOME/tmux/plugins"
zstyle ':completion:*' cache-path $XDG_CACHE_HOME/zsh/zcompcache

export PATH="$PATH:$HOME/.local/bin"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
export PATH="$PATH:$HOME/.poetry/bin"
export PATH="$PATH:$CARGO_HOME/bin"

#now
#screenfetch
