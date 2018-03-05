#!/bin/bash

function status_bar_loop() {
    while true
    do
	VOL=$(amixer get Master | tail -1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')
	LOCALTIME=$(date +%Z\=%Y-%m-%dT%H:%M)
	OTHERTIME=$(TZ=Europe/London date +%Z\=%H:%M)
	IP=$(for i in `ip r`; do echo $i; done | grep -A 1 src | tail -n1) # can get confused if you use vmware
	TEMP="$(($(cat /sys/class/thermal/thermal_zone0/temp) / 1000))C"

	if acpi -a | grep off-line > /dev/null
	then
	    BAT="Bat. $(acpi -b | awk '{ print $4 " " $5 }' | tr -d ',')"
	    xsetroot -name "$IP $BAT $VOL $TEMP $LOCALTIME $OTHERTIME"
	else
	    xsetroot -name "$IP $VOL $TEMP $LOCALTIME $OTHERTIME"
	fi
	sleep 20s
    done &
}

function launch_exwm() {
    xrdb -load ~/.Xresources

    # disable access control
    xhost +SI:localuser:$USER

    # themes, etc.
    gnome-settings-daemon &

    # fallback cursor
    xsetroot -cursor_name left_ptr

    # keyboard repeat rate
    xset r rate 200 60

    exec dbus-launch --exit-with-session emacs
}

function launch_gnome() {
    setxkbmap -layout us -option ctrl:swapcaps &
    
    exec gnome-session
}

function launch_i3() {
    setxkbmap -layout us -option ctrl:swapcaps

    numlockx

    xrdb -load ~/.Xresources

    ~/.fehbg

    dunst &

    sleep 1

    slack &
    rocketchat &
    #hexchat &
    urxvt -e weechat &
    nm-applet &
    redshift-gtk &
    SpiderOakGroups &

    sleep 1

    ~/.config/polybar/launch.sh
    
    exec i3
}

function launch_dwm() {
    setxkbmap -layout us -option ctrl:swapcaps &

    status_bar_loop

    slack &
    hexchat &
    nm-applet &
    
    exec dwm
}

#launch_exwm
#launch_gnome
#launch_dwm
launch_i3
#exec openbox-session
