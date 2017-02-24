HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt appendhistory autocd extendedglob prompt_subst HIST_IGNORE_DUPS
unsetopt beep
bindkey -e

zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*' menu select
zstyle :compinstall filename '/home/maurn/.config/zsh/.zshrc'
zstyle ':completion:*:*:mpv:*:*files' ignored-patterns '*.srt'

autoload -Uz compinit
compinit

source $ZDOTDIR/git-prompt.zsh

function precmd(){
    PROMPT='%F{cyan}%n%f%F{green}@%F{cyan}%m%f %~ %F{cyan}>%f '
    RPROMPT=$(git-prompt)
}

export EDITOR=nvim

source $ZDOTDIR/aliases.zsh

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)" > /dev/null
fi

# Plugins
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autopair/autopair.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

source $ZDOTDIR/color-man-pages.zsh
source $ZDOTDIR/keybindings.zsh
