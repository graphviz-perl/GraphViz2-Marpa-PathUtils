#!/bin/bash

perl -Ilib scripts/generate.demo.pl
perl -Ilib scripts/code.attributes2html.pl

# $DR is my web server's doc root.

PM=Perl-modules/html/graphviz2.pathutils

cp html/*.html html/*.svg $DR/$PM

echo Copied files to $DR/$PM
echo Warning: Check the version number in the demo index
