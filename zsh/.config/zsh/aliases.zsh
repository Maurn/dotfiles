alias ls='ls --color=auto'
alias tree='tree -C'
alias pacin='yay -S'
alias pacun='sudo pacman -Rs'
alias updatemirrors='sudo reflector --verbose --country 'Denmark' --age 12 --sort rate --save /etc/pacman.d/mirrorlist'
alias systemupdate='updatemirrors && yay -Syu'
alias removeorphans='sudo pacman -Rns $(pacman -Qtdq)'
alias v='nvim'
alias vim='nvim'
alias e='emacsclient -cn'
alias z='zathura --fork'
alias irc='irssi'
alias feh='feh -.B black'
alias sudo='sudo ' # need this for sudo aliases to work
alias music='ncmpcpp'
alias subdl='subliminal download -l en'
alias stream='peerflix --mpv'
alias nvm='unalias nvm && source "$HOME"/.nvm/nvm.sh && nvm'
alias pip='pip --user'
mkcd () { mkdir "$1"; cd "$1" }

earth-background () {
    local metadata=$(curl --silent https://epic.gsfc.nasa.gov/api/natural | jq '.[0]')
    local img=$(echo $metadata | jq -r '.image')
    local date=$(date --date="$(echo $metadata | jq -r '.date')" +%Y/%m/%d)

    curl "https://epic.gsfc.nasa.gov/archive/natural/$date/png/$img.png" | feh -.B black --bg-max -
}
