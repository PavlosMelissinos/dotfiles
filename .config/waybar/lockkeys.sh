#!/usr/bin/env bash

capslock_active=`cat /sys/class/leds/input24::capslock/brightness`
numlock_active=`cat /sys/class/leds/input24::numlock/brightness`

capslock_text=""
if [[ "$capslock_active" != "0" ]]; then
    capslock_text="⬆"
fi

numlock_text=""
if [[ "$numlock_active" != "0" ]]; then
    numlock_text="1"
fi

if [[ "$capslock_text" != "" || "$numlock_text" != "" ]]; then
    echo '{"text": "'$capslock_text$numlock_text'","tooltip":"$tooltip","class":"$class"}'
fi
