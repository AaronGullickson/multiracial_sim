#!/bin/bash

# run the full project
quarto render

# move the products over to the public_html folder after removing the old ones
if [ -d ~/public_html/research/multiracial_sim ]; then
  rm -r ~/public_html/research/multiracial_sim
fi

if [ -d _products ]; then
  cp -r _products ~/public_html/research/multiracial_sim
fi