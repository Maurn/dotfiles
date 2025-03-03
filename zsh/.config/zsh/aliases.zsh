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
alias xclip='xclip -selection c'
alias ysi='tmuxinator start ysi'
alias hololink='tmuxinator start hololink'
alias ysiapi='cd ~/entropylabs/YSI/services/api/ && npm start'
alias ysifront='cd ~/entropylabs/YSI/services/frontend/ && npm start'
alias ysiops='cd ~/entropylabs/YSI/ops'
alias pryglapi='cd ~/code/prygl.nu/services/api/ && npm start'
alias pryglfront='cd ~/code/prygl.nu/services/frontend/ && npm start'
alias pryglops='cd ~/code/prygl.nu/ops'
alias wacky='cd ~/code/wackywiz && alacritty --working-directory ~/code/wackywiz &;  npm run dev'

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

flac2wav () {
  for file in *.flac; do ffmpeg -i $file ${file%.*}.wav; done
}

measure_loudness () {
  ffmpeg -i "$1" -y -vn -af ebur128=target=-16:peak='true' /tmp/loudnessmeasure.wav
}

aac_encode () {
  ffmpeg -i "$1" -c:a libfdk_aac -vbr 3 -movflags +faststart -vn "$2"
}

create_timelapse () {
  ffmpeg -framerate "$1" -pattern_type glob -i '*.png' -c:v libx264 -pix_fmt yuv420p "$2"
}

deploy () {
  if [[ $# -eq 0 ]] ; then
    echo 'no hosts supplied'
    return 1
  fi

  pushd ~mono/infrastructure
  ansible-playbook deploy-sveltekit.yml -l "$1"
  popd
}
