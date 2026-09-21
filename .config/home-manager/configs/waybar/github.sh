#!/usr/bin/env bash

token=`cat ${HOME}/.config/github/notifications.token`
notifications=`curl -u PavlosMelissinos:${token} https://api.github.com/notifications`
count=`echo $notifications | jq '. | length'`
tooltip=`echo $notifications | jq '. | map({"title": .subject | .title, "repo": .repository | .full_name} | "(" + .repo + ") " + .title) | join("\n\n")'`
class=0

if [[ "$count" != "0" ]]; then
    echo '{"text":'$count',"tooltip":'$tooltip',"class":'$class'}' | jq --unbuffered --compact-output
fi
