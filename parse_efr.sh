#!/bin/bash

data=${1:-data}

find $data -type f -name "*.html" -execdir grep -o -E 'http\S*working-paper-[[:digit:]]+\S*/' \{\} \; | {
while read URL
do
    Odir=$data/swp
    mkdir -p ${Odir}
    Ofile=$(cut -d '/' -f 6 <<< $URL).html
    echo $Ofile
    wget $URL -O ${Odir}/${Ofile} > /dev/null 2>&1
done
}

find $data -type f -name "*.html" -execdir grep -o -E 'http\S*analytical-note-[[:digit:]]+\S*/' \{\} \; | {
while read URL
do
    Odir=$data/san
    mkdir -p ${Odir}
    Ofile=$(cut -d '/' -f 6 <<< $URL).html
    echo $Ofile
    wget $URL -O ${Odir}/${Ofile} > /dev/null 2>&1
done
}

find $data -type f -name "*.html" -execdir grep -o -E 'http\S*discussion-paper-[[:digit:]]+\S*/' \{\} \; | {
while read URL
do
    Odir=$data/sdp
    mkdir -p ${Odir}
    Ofile=$(cut -d '/' -f 6 <<< $URL).html
    echo $Ofile
    wget $URL -O ${Odir}/${Ofile} > /dev/null 2>&1
done
}


