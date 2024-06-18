export ZDOTDIR=$HOME/.config/zsh

export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket

export QT_STYLE_OVERRIDE=kvantum

export PROTO_HOME=$HOME/.proto

typeset -U path PATH
path=("$PROTO_HOME/bin" $path)
path=("$PROTO_HOME/shims" $path)

export PATH
