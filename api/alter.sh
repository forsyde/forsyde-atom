#!/bin/bash

sed -i 's/<link href="ocean.css"[^>]*>//g' *.html

cat  \
   | perl -pe 's/fox.*?([0-9]{4})//g'

# for f in *.html; do
#     echo -e "---
# layout: haddock
# ---
# $(cat $f)" > $f
# done
