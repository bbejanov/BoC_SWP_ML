#!/bin/bash

data=${1:-data}

for kind in swp san sdp 
do 
    cut -d \" -f 16 $data/${kind}.csv | tail -n +2 | 
    while read URL
    do
        Odir=$data/pdf/$kind
        mkdir -p ${Odir}
        Ofile=$(cut -d '/' -f 8 <<< $URL)
        echo "$URL" '->' "$Odir/$Ofile"
        wget $URL -O ${Odir}/${Ofile} > /dev/null 2>&1
    done
done 
