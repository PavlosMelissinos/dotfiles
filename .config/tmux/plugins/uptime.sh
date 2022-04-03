#!/usr/bin/env bash

set -e

# Place holder for status left/right
place_holder="\#{uptime}"

# Possible configurations
fg_color_config='@uptime_fg'
bg_color_config='@uptime_bg'
output_prefix='@uptime_output_prefix'
output_suffix='@uptime_output_suffix'
show_copy_config='@uptime_show_copy_mode'
show_sync_config='@uptime_show_sync_mode'
copy_attr_config='@uptime_copy_mode_attr'
sync_attr_config='@uptime_sync_mode_attr'
prefix_prompt='@uptime_prefix_prompt'
copy_prompt='@uptime_copy_prompt'
sync_prompt='@uptime_sync_prompt'
empty_prompt='@uptime_empty_prompt'
empty_attr_config='@uptime_empty_attr'
empty_has_affixes='@uptime_empty_has_affixes'

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

# Defaults
default_fg='colour231'
default_bg='colour04'
default_copy_attr='fg=default,bg=yellow'
default_sync_attr='fg=default,bg=yellow'
default_empty_attr='fg=default,bg=default'
default_prefix_prompt=$(tmux_option prefix | tr "[:lower:]" "[:upper:]" | sed 's/C-/\^/')
default_copy_prompt='Copy'
default_sync_prompt='Sync'
default_empty_prompt=''

main() {
    local -r \
        fg_color=$(tmux_option "$fg_color_config" "$default_fg") \
        bg_color=$(tmux_option "$bg_color_config" "$default_bg") \
        show_copy_mode=$(tmux_option "$show_copy_config" "off") \
        show_sync_mode=$(tmux_option "$show_sync_config" "off") \
        output_prefix=$(tmux_option "$output_prefix" " ") \
        output_suffix=$(tmux_option "$output_suffix" " ") \
        copy_attr=$(tmux_option "$copy_attr_config" "$default_copy_attr") \
        sync_attr=$(tmux_option "$sync_attr_config" "$default_sync_attr") \
        prefix_prompt=$(tmux_option "$prefix_prompt" "$default_prefix_prompt") \
        copy_prompt=$(tmux_option "$copy_prompt" "$default_copy_prompt") \
        sync_prompt=$(tmux_option "$sync_prompt" "$default_sync_prompt") \
        empty_prompt=$(tmux_option "$empty_prompt" "$default_empty_prompt") \
        empty_attr=$(tmux_option "$empty_attr_config" "$default_empty_attr") \
        empty_has_affixes=$(tmux_option "$empty_has_affixes" "off")

    # credit to mindc for the awk cmd: https://unix.stackexchange.com/a/34033
    local -r uptime="$(awk '{printf("↑%dd %02dh %02dm\n",($1/60/60/24),($1/60/60%24),($1/60%60))}' /proc/uptime)"

    local -r status_left_value="$(tmux_option "status-left")"
    tmux set-option -gq "status-left" "${status_left_value/$place_holder/$uptime}"

    local -r status_right_value="$(tmux_option "status-right")"
    tmux set-option -gq "status-right" "${status_right_value/$place_holder/$uptime}"
}

main
