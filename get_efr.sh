#!/bin/bash


function url-year-page() {
    printf "http://www.bankofcanada.ca/%4d/page/%d/?content_type=research" $y $p
}

y=${1:-2017}
dest=data/y${y}
mkdir -p $dest

p=1
while :
do
    Ofile="${dest}/${y}-p${p}.html"
    echo Downloading ${y}-p${p}.html
    wget $(url-year-page $y $p) -O ${Ofile} > /dev/null 2>&1
    st=$?
    if (( st == 8 )) 
    then
        echo "Not found - done"
        rm ${Ofile}
        break
    elif (( st == 0 ))
    then
        (( p++ ))
        continue
    else
        echo "wget exited with error $st"
        exit $st
    fi
done 

