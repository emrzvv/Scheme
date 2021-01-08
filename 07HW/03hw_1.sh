#!/bin/bash

function my_tree() {
    local dir=${1:-.} prefix=$2
    local file
    for file in "$dir"/*; do
        [ -e "$file" ] || [ -L "$file" ] || continue
        if [ -d "$file" ]; then
            echo " " "$prefix" "${file##*/}"
            my_tree "$file" "       ${prefix}———"
        else
            echo " " "$prefix" "${file##*/}"
        fi
    done
}

function my_tree_only_dirs() {
    local dir=${1:-.} prefix=$2
    local file
    for file in "$dir"/*; do
        [ -e "$file" ] || [ -L "$file" ] || continue
        if [ -d "$file" ]; then
            echo " " "$prefix" "${file##*/}"
            my_tree_only_dirs "$file" "       ${prefix}———"
        fi
    done
}

function main() {
    if [ "$1" == "-d" ]; then
        if [ "$2" == "-o" ]; then
            echo "$3"
            my_tree_only_dirs "$3" \|
        else 
            echo .
            my_tree_only_dirs . \|
        fi
    elif [ "$1" == "-o" ]; then
        if [ "$3" == "-d" ]; then
            echo "$2"
            my_tree_only_dirs "$2" \|
        else 
            echo "$2"
            my_tree "$2" \|
        fi
    else
        echo .
        my_tree . \|
    fi
}

main "$1" "$2" "$3"