#!/bin/zsh
export VISUAL=nvim
export EDITOR=$VISUAL
VBoxClient-all stop ; VBoxClient-all start
alias ls='ls --color'
alias logout="pkill -KILL -u $UID"
alias stackrun='stack build && stack exec $(basename $(pwd))'
alias nvim='nvim -u ~/.vimrc'
alias xclip='xclip -sel c'

shorten_string() {
   let "max_len=$1"
   string="$2"

   let len="${#string}"
   if [ "$len" -gt "$max_len" ]; then
      let "start_point=$len-$max_len"
      echo "...${string:$start_point}"
   else
      echo $string
   fi
}
shorten_prompt() {
   read arg
   echo "$(shorten_string 25 $arg)"
}

autoload -Uz compinit
compinit
export PS1='%B[%F{red}%n %F{green}: %F{magenta}$(dirs | shorten_prompt)%f]%b $ '
zstyle ':completion:*' menu select
setopt PROMPT_SUBST
setopt COMPLETE_ALIASES
setopt HISTIGNOREDUPS
setopt CORRECT
SPROMPT="Correct spelling: %B%F{red}%R%f -> %F{green}%r%f%b? [nyae] "
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt extendedglob
bindkey "^K" up-line-or-search
bindkey "^J" down-line-or-search
bindkey "^H" backward-word
bindkey "^L" forward-word
bindkey -e

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
fzf-recursive() {
   find . | fzf
   zle accept-and-hold
}
zle -N fzf-recursive
# bindkey '^T' fzf-recursive
