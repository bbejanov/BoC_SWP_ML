#!/bin/bash

data=${1:-data}

find $data/pdf -type f -name '*.pdf' |
while read PDF
do
    TXT=${PDF//pdf/txt}
    mkdir -p $(dirname $TXT)
    pdftotext -raw -f 2 -l 2 $PDF $TXT

done

