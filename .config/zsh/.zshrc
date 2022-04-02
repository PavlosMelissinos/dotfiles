#export ADOTDIR="$XDG_CACHE_HOME/antigen"
#source $ZDOTDIR/antigen.zsh

# Load the oh-my-zsh's library.
#antigen use oh-my-zsh

# # Bundles from the default repo (robbyrussell's oh-my-zsh).
# antigen bundle git  # aliases
# antigen bundle git-extras  # auto-completion
# #antigen bundle git-prompt  # shows extra info about current repo
# antigen bundle pip
# antigen bundle command-not-found
# antigen bundle web-search
# antigen bundle python
# antigen bundle docker
# #antigen bundle history-substring-search
# #antigen bundle emacs
# #antigen bundle zsh-interactive-cd

# Syntax highlighting bundle.
#antigen bundle zsh-users/zsh-syntax-highlighting
#antigen bundle zsh-users/zsh-autosuggestions

# antigen bundle darvid/zsh-poetry

# Load the theme.
#antigen theme agnoster

# Tell Antigen that you're done.
#antigen apply

#source $XDG_CONFIG_HOME/zsh/theme-agnosterj.zsh

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
