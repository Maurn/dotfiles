HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt appendhistory autocd extendedglob prompt_subst HIST_IGNORE_DUPS
unsetopt beep
bindkey -e

zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*' menu select
zstyle :compinstall filename '/home/maurn/.config/zsh/.zshrc'

autoload -Uz compinit
compinit

source $ZDOTDIR/git-prompt.zsh

function precmd(){
    PROMPT='%F{blue}%n%f@%F{blue}%m%f %~ %F{blue}>%f '
    RPROMPT=$(rprompt-git)
}

export EDITOR=vim

source $ZDOTDIR/aliases.zsh
source $ZDOTDIR/keybindings.zsh

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)" > /dev/null
fi

# Plugins
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh