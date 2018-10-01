#!/bin/sh

#suspend notification display
pkill -u "$USER" -USR1 dunst

i3lock -n -i ~/Pictures/wallpaper -tk --indicator --datestr="" --timestr="%H:%M" --ringcolor=282828ff --datecolor=ebdbb2ff --timecolor=ebdbb2ff --keyhlcolor=ebdbb2ff

#resume notification display
pkill -u "$USER" -USR2 dunst
