#!/bin/bash

function count_strings() {
    #touch temp_result.txt
    local all_lines=$(< $1 wc -l)
    local space_lines=$(grep -c '^$' $1)
    return $(($all_lines - $space_lines + 1));
}

function rec() {
    if [ -d "$1" ]; then
        ls "$1" | while read name; do
            rec "$1/$name"
        done
    else
        if [[ -f "$1" ]]; then
            #echo $1
            if [[ $1 == *.c ]]; then
                count_strings $1
                local temp_amount=$?
                #echo $temp_amount
                amount=$(($amount + $temp_amount))
                echo $amount >> amount.txt
            fi
            
            if [[ $1 == *.h ]]; then
                count_strings $1
                local temp_amount=$?
                #echo $temp_amount
                amount=$(($amount + $temp_amount))
                echo $amount >> amount.txt
            fi

            if [[ $1 == *.cxx ]]; then
                count_strings $1
                local temp_amount=$?
                #echo $temp_amount
                amount=$(($amount + $temp_amount))
                echo $amount >> amount.txt
            fi
        fi
        
    fi
    
}

touch amount.txt
amount=0
amount=$(rec $1)
$amount >> amount.txt
tail -1 amount.txt
rm amount.txt

# 03lab_2.sh ~/c_projects/algos/01_module