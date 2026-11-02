#!/usr/bin/env bash

vpn=$(nmcli -t -f TYPE,NAME connection show --active | awk -F: '$1=="vpn"{print $2; exit}')

if [[ -n "$vpn" ]]; then
    echo '{"text":"VPN","tooltip":"Connected: '"$vpn"'","class":"connected"}'
else
    echo '{"text":"VPN","tooltip":"VPN disconnected","class":"disconnected"}'
fi
