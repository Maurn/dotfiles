function git-prompt(){
    local git_info=($(git rev-parse --is-inside-git-dir --abbrev-ref=loose HEAD 2> /dev/null))

    if [ -z $git_info[1] ] || [ $git_info[1] = 'true' ]; then
        return
    fi

    local branch=$git_info[2]

    local color=''
    local git_status=$(git status --porcelain)
    if [[ -z $git_status ]]; then
        color='%F{green}'
    elif [[ -n $(echo $git_status | grep '^MM') ]]; then
        color='%F{red}'
    elif [[ -n $(echo $git_status | grep '^[ \?MDA]') ]]; then
        color='%F{yellow}'
    fi

    local sync=''
    local git_sync=$(git rev-list --left-right --count origin/$branch...HEAD 2> /dev/null)
    if [[ -n $git_sync ]]; then
        behind=$git_sync[1]
        ahead=$git_sync[3]

        sync_color=''
        if [ $behind -eq 0 ] && [ $ahead -eq 0 ]; then
            sync_color='%F{green}'
        elif [ $behind -ne 0 ] && [ $ahead -ne 0 ]; then
            sync_color='%F{red}'
        elif [ $behind -ne 0 ] || [ $ahead -ne 0 ]; then
            sync_color='%F{yellow}'
        fi

        sync="$sync_color$behind↓ $ahead↑"
    fi

    echo "$color $branch%f $sync"
}