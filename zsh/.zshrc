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

source .aliases

# Plugins
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
