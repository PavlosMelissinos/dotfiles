eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

. "$GUIX_PROFILE/etc/profile"

#unsetopt menu_complete   # do not autoselect the first completion entry
# unsetopt flowcontrol
# setopt auto_menu         # show completion menu on successive tab press
# setopt complete_in_word
# setopt always_to_end

# tab completion
autoload -Uz compinit
compinit

zmodload -i zsh/complist
zstyle ':completion:*' menu select

source $XDG_CONFIG_HOME/zsh/scripts/key-bindings.zsh # for home/end/delete to work
source $XDG_CONFIG_HOME/zsh/scripts/theme-and-appearance.zsh # for coloured output
#source $XDG_CONFIG_HOME/zsh/scripts/command_not_found.zsh
source $XDG_CONFIG_HOME/zsh/theme-agnosterp.zsh # stripped down version of agnosterj
source $HOME/.guix-profile/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOME/.guix-profile/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
