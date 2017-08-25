#!/bin/bash

mkdir -p html

for pdfname in data/*.pdf; do
  htmlname=${pdfname/%pdf/html}
  htmlname=${htmlname/#data/html}
  pdf2htmlEX $pdfname $htmlname
done

