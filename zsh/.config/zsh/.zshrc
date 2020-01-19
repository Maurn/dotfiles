HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000

setopt share_history autocd extendedglob prompt_subst hist_ignore_dups
unsetopt beep
bindkey -e

zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}'
zstyle ':completion:*' menu select
zstyle :compinstall filename '/home/maurn/.config/zsh/.zshrc'

autoload -Uz compinit
compinit

zmodload zsh/zpty

source $ZDOTDIR/git-prompt.zsh

function precmd(){
    PROMPT='%F{cyan}%n%f%F{green}@%F{cyan}%m%f %~ %F{cyan}>%f '
    RPROMPT=$(git-prompt)
}

export EDITOR=nvim
export TERM=xterm-termite

source $ZDOTDIR/vi-mode.zsh
# Colored man pages
export LESS_TERMCAP_mb=$'\E[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\E[1;36m'     # begin blink
export LESS_TERMCAP_me=$'\E[0m'        # reset bold/blink
export LESS_TERMCAP_so=$'\E[01;44;33m' # begin reverse video
export LESS_TERMCAP_se=$'\E[0m'        # reset reverse video
export LESS_TERMCAP_us=$'\E[1;32m'     # begin underline
export LESS_TERMCAP_ue=$'\E[0m'        # reset underline

source $ZDOTDIR/aliases.zsh

typeset -U path
path+=(~/.npm-global/bin/)

eval $(keychain --eval --quiet --noask id_rsa)

# Plugins
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autopair/autopair.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/doc/pkgfile/command-not-found.zsh

source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

source $ZDOTDIR/keybindings.zsh

export ANDROID_HOME=/home/maurn/android/sdk
