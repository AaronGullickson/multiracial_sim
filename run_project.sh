#!/bin/bash

# run the full project
quarto render

# move the products over to the public_html folder after removing the old ones
rm -r ~/public_html/research/multiracial_sim
cp -r _products ~/public_html/research/multiracial_sim