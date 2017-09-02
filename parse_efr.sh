#!/bin/bash

find $1 -type f -name "*.html" -execdir grep -o -E 'http\S*working-paper-\S*/' \{\} \; | {
while read URL
do
    Odir=$1/swp
    mkdir -p ${Odir}
    Ofile=$(cut -d '/' -f 6 <<< $URL).html
    echo $Ofile
    wget $URL -O ${Odir}/${Ofile} > /dev/null 2>&1
done
}

find $1 -type f -name "*.html" -execdir grep -o -E 'http\S*analytical-note-\S*/' \{\} \; | {
while read URL
do
    Odir=$1/san
    mkdir -p ${Odir}
    Ofile=$(cut -d '/' -f 6 <<< $URL).html
    echo $Ofile
    wget $URL -O ${Odir}/${Ofile} > /dev/null 2>&1
done
}

find $1 -type f -name "*.html" -execdir grep -o -E 'http\S*discussion-paper-\S*/' \{\} \; | {
while read URL
do
    Odir=$1/sdp
    mkdir -p ${Odir}
    Ofile=$(cut -d '/' -f 6 <<< $URL).html
    echo $Ofile
    wget $URL -O ${Odir}/${Ofile} > /dev/null 2>&1
done
}


