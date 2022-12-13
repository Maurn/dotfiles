alias ls='ls --color=auto'
alias tree='tree -C'
alias pacin='paru -S'
alias pacun='paru -Rs'
alias pacclean='paru -Sc'
alias sysupdate='paru -Syu'
alias removeorphans='sudo pacman -Rns $(pacman -Qtdq)'
alias v='nvim'
alias vim='nvim'
alias e='TERM=alacritty-direct emacsclient -c -nw'
alias E="TERM=alacritty-direct SUDO_EDITOR=\"emacsclient -t -a emacs\" sudoedit"
alias z='zathura --fork'
alias n='n -A'
alias cat='bat'
alias irc='irssi'
alias feh='feh -.B black'
alias sudo='sudo ' # need this for sudo aliases to work
alias ssh='TERM=xterm ssh'
alias music='ncmpcpp'
alias subdl='subliminal download -l en'
alias stream='peerflix --mpv'
alias nvm='unalias nvm && source /usr/share/nvm/init-nvm.sh && nvm'
alias pip='pip --user'
alias xclip='xclip -selection c'
alias ysi='tmuxinator start ysi'
alias hololink='tmuxinator start hololink'
alias ysiapi='cd ~/entropyfox/YSI/services/api/ && npm start'
alias ysifront='cd ~/entropyfox/YSI/services/frontend/ && npm start'
alias ysiplen='cd ~/entropyfox/YSI/services/plenary/ && npm start'
alias ysizoom='cd ~/entropyfox/YSI/services/zoomweb/ && npm start'
alias ysiops='cd ~/entropyfox/YSI/ops'
alias pryglapi='cd ~/code/prygl.nu/services/api/ && npm start'
alias pryglfront='cd ~/code/prygl.nu/services/frontend/ && npm start'
alias pryglops='cd ~/code/prygl.nu/ops'
mkcd () { mkdir "$1"; cd "$1" }

earth-background () {
    local metadata=$(curl --silent https://epic.gsfc.nasa.gov/api/natural | jq '.[0]')
    local img=$(echo $metadata | jq -r '.image')
    local date=$(date --date="$(echo $metadata | jq -r '.date')" +%Y/%m/%d)

    curl "https://epic.gsfc.nasa.gov/archive/natural/$date/png/$img.png" | feh -.B black --bg-max -
}

wav2flac () {
  for file in *.wav; do ffmpeg -i $file ${file%.*}.flac; done
}
