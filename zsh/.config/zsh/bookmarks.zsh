bm () {
    hash -d $1=$2
}

bm ysi /home/maurn/entropyfox/YSI
bm nginx /etc/nginx
bm sites-available /etc/nginx/sites-available
bm sites-enabled /etc/nginx/sites-enabled

unfunction bm
