export VOLTA_HOME=$HOME/.volta
typeset -U path PATH
path=($VOLTA_HOME/bin $path)
export PATH

export ZDOTDIR=$HOME/.config/zsh

export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
