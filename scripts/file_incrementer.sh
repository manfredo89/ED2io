#!/bin/bash

for file in $(ls *.h5); do

    myyear=$(echo "$file" | cut -c11-14)
    mynewyear=$((myyear-500))
    newfile="${file/$myyear/$mynewyear}"
    mv -i -- "$file" "$newfile"

done
    



