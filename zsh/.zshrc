# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/maurn/.zshrc'
zstyle ':completion:*' menu select

autoload -Uz compinit
compinit
# End of lines added by compinstall

PROMPT='%n@%m: %~ $ '

export EDITOR=vim

#Aliases
alias ls='ls --color=auto'
alias updatemirrors='sudo reflector --verbose --country 'Denmark' --age 12 --sort rate --save /etc/pacman.d/mirrorlist'
alias i3conf='vim ~/.config/i3/config'

# Plugins
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
