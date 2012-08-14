#!/usr/bin/env bash

# xgamma -gamma 0.8
xmodmap -e "remove Lock = Caps_Lock"
xmodmap -e "keysym Caps_Lock = Control_L"
xmodmap -e "add Control = Control_L"

# trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x000000 --height 12 &

nohup xscreensaver -no-splash &

xrdb -merge ~/.Xresources
