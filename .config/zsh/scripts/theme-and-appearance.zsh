# ls colors
autoload -U colors && colors

# Enable ls colors
export LSCOLORS="Gxfxcxdxbxegedabagacad"

if [[ -z "$LS_COLORS" ]]; then
  (( $+commands[dircolors] )) && eval "$(dircolors -b)"
fi

ls --color -d . &>/dev/null && alias ls='ls --color=tty' || { ls -G . &>/dev/null && alias ls='ls -G' }

# Take advantage of $LS_COLORS for completion as well.
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# enable diff color if possible.
if command diff --color /dev/null /dev/null &>/dev/null; then
  alias diff='diff --color'
fi

setopt auto_cd
setopt multios
setopt prompt_subst

[[ -n "$WINDOW" ]] && SCREEN_NO="%B$WINDOW%b " || SCREEN_NO=""

# git theming default: Variables for theming the git info prompt
# ZSH_THEME_GIT_PROMPT_PREFIX="git:("         # Prefix at the very beginning of the prompt, before the branch name
# ZSH_THEME_GIT_PROMPT_SUFFIX=")"             # At the very end of the prompt
# ZSH_THEME_GIT_PROMPT_DIRTY="*"              # Text to display if the branch is dirty
# ZSH_THEME_GIT_PROMPT_CLEAN=""               # Text to display if the branch is clean
# ZSH_THEME_RUBY_PROMPT_PREFIX="("
# ZSH_THEME_RUBY_PROMPT_SUFFIX=")"
