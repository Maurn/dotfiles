bm () {
    hash -d $1=$2
}

bm mono /home/maurn/code/monorepo
bm infra /home/maurn/code/infrastructure

unfunction bm
