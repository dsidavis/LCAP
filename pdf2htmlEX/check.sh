#!/bin/bash

for pdfname in data/*.pdf; do
  htmlname=${pdfname/%pdf/html}
  htmlname=${htmlname/#data/html}
  if [[ ! -e $htmlname ]]
  then
    echo $pdfname
  fi
done
