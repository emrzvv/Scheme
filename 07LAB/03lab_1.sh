#!/bin/bash

function start() {
    while :; do
        echo "started $1"
        python3 $1 && sleep $2
        while jobs | grep "Запущен" | grep 'python3 \$1' > '/dev/null'; do
            sleep $2
        done
    done
}

start "$1" "$2"
