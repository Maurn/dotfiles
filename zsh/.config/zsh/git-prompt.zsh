function rprompt-git(){
    gitinfo=($(git rev-parse --is-inside-git-dir --abbrev-ref=loose HEAD 2> /dev/null))

    if [ -z $gitinfo[1] ]; then
        return
    fi

    if [ $gitinfo[1] = 'true' ]; then
        return
    fi

    branch=$gitinfo[2]

    color=''
    gitstatus=$(git status --porcelain)
    if [[ -z $gitstatus ]]; then
        color='%F{green}'
    elif [[ -n $(echo $gitstatus | grep '^MM') ]]; then
        color='%F{red}'
    elif [[ -n $(echo $gitstatus | grep '^[ \?MDA]') ]]; then
        color='%F{yellow}'
    fi

    sync=''
    gitsync=$(git rev-list --left-right --count origin/$branch...HEAD 2> /dev/null)
    if [[ -n $gitsync ]]; then
        behind=$gitsync[1]
        ahead=$gitsync[3]

        scolor=''
        if [ $behind -eq 0 ] && [ $ahead -eq 0 ]; then
            scolor='%F{green}'
        elif [ $behind -ne 0 ] && [ $ahead -ne 0 ]; then
            scolor='%F{red}'
        elif [ $behind -ne 0 ] || [ $ahead -ne 0 ]; then
            scolor='%F{yellow}'
        fi

        sync=" $scolor$behind↓ $ahead↑"
    fi

    echo "$color\ue0a0 $branch%f$sync"
}