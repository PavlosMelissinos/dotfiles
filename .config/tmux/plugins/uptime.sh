#!/usr/bin/env bash

set -e

# Place holder for status left/right
place_holder="\#{uptime}"

tmux_option() {
    local -r value=$(tmux show-option -gqv "$1")
    local -r default="$2"

    if [ -n "$value" ]; then
        echo "$value"
    else
        echo "$default"
    fi
}

format_style() {
    echo "#[${1}]" | sed -e 's/,/]#[/g'
}

main() {
    # credit to mindc for the awk cmd: https://unix.stackexchange.com/a/34033
    local -r uptime="$(awk '{printf("↑%dd %02dh %02dm\n",($1/60/60/24),($1/60/60%24),($1/60%60))}' /proc/uptime)"

    local -r status_left_value="$(tmux_option "status-left")"
    tmux set-option -gq "status-left" "${status_left_value/$place_holder/$uptime}"

    local -r status_right_value="$(tmux_option "status-right")"
    tmux set-option -gq "status-right" "${status_right_value/$place_holder/$uptime}"
}

main
