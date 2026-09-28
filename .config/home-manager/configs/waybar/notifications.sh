#!/usr/bin/env bash
# mako notification indicator/toggle for waybar.
# No argument prints a waybar JSON status; "toggle" flips do-not-disturb,
# dismisses visible notifications, and refreshes waybar via SIGRTMIN+8.

if [ "${1:-}" = toggle ]; then
    makoctl mode -t do-not-disturb
    makoctl dismiss -a
    pkill -RTMIN+8 waybar || true
    exit 0
fi

if makoctl mode 2>/dev/null | grep -qx 'do-not-disturb'; then
    icon=$(printf '\uf1f6')
    class=dnd
    state="Notifications: muted"
else
    icon=$(printf '\uf0f3')
    class=enabled
    state="Notifications: on"
fi

printf '{"text":"%s","class":"%s","tooltip":"%s"}\n' "$icon" "$class" "$state"
