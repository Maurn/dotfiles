alias ls='ls --color=auto'
alias tree='tree -C'
alias pacin='pacaur -S'
alias pacun='sudo pacman -Rs'
alias updatemirrors='sudo reflector --verbose --country 'Denmark' --age 12 --sort rate --save /etc/pacman.d/mirrorlist'
alias systemupdate='updatemirrors && pacaur -Syu && sudo updatedb'
alias removeorphans='sudo pacman -Rns $(pacman -Qtdq)'
alias v='nvim'
alias vim='nvim'
e (){ emacsclient -cn }
alias irc='irssi'
alias feh='feh -.B black'
alias sudo='sudo ' # need this for sudo aliases to work
alias music='ncmpcpp'
alias subdl='subliminal download -l en'
alias stream='peerflix --mpv'
mkcd () { mkdir "$1"; cd "$1" }
