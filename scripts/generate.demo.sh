#!/bin/bash

perl -Ilib scripts/generate.demo.pl

# $DR is doc root and $PM is Perl-modules/html/graphviz2.pathutils.

cp html/*.html html/*.svg $DR/$PM

echo Check the version number in the demo index
