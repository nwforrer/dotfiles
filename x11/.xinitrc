#!/bin/bash

xrdb -load ~/.Xresources

function wm() {
	xsetroot -cursor_name left_ptr &
	#wal -i "$(< "$HOME/.cache/wal/wal")"

    xsetroot -name $1
    setxkbmap -layout us -variant dvp & 

    #$HOME/bin/barmk.sh |lemonbar -B\#ff1d1d1d -g 720x30+2243+20 &

    #dunst -follow keyboard -geometry x5 &

    #$HOME/bin/check-monitors.sh &
    #slack &
    #nm-applet &

    #numlockx &
    #sxhkd &

    #redshift -l 35.830204:-78.641439 -t 4500K:3500K &

    #SpiderOakBlue &

    #urxvt -e vim -O ~/usr/doc/todo.txt ~/usr/doc/notes.txt &
    #urxvt -e weechat &

    exec $1
}

wm i3
#exec gnome-session

