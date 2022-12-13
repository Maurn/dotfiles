export VOLTA_HOME=$HOME/.volta
typeset -U path PATH
path=($VOLTA_HOME/bin $path)
export PATH

export ZDOTDIR=$HOME/.config/zsh

export SSH_AUTH_SOCK=/run/user/1000/ssh-agent.socket
