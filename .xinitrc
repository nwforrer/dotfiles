#!/bin/bash

xsetroot -cursor_name left_ptr &
xrdb -load ~/.Xresources
wal -i "$(< "$HOME/.cache/wal/wal")"

function wm() {

    xsetroot -name $1
    setxkbmap -layout us -variant dvp & 

    dunst -follow keyboard -geometry x5 &

    $HOME/bin/check-monitors.sh &
    slack &
    nm-applet &

    numlockx &
    sxhkd &

    redshift -l 35.830204:-78.641439 -t 4500K:3500K &

    SpiderOakBlue &

    urxvt -e vim -O ~/usr/doc/todo.txt ~/usr/doc/notes.txt &
    urxvt -e weechat &

    exec $1
}

wm i3

