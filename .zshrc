#!/bin/zsh
export VISUAL=nvim
export EDITOR=$VISUAL
VBoxClient-all stop ; VBoxClient-all start
alias ls='ls --color'
alias logout="pkill -KILL -u $UID"

autoload -Uz compinit
compinit
export PS1="%B[%F{red}%n %F{white}: %F{magenta}%~%f]%b $ "
zstyle ':completion:*' menu select
setopt COMPLETE_ALIASES
setopt HISTIGNOREDUPS
setopt CORRECT
SPROMPT="Correct spelling: %B%F{red}%R%f -> %F{green}%r%f%b? [nyae] "
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt extendedglob
bindkey -v

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
