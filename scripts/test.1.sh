#!/bin/bash
#
# Parameters:
# 1: The name of input and files.
#	16 means the input is data/16.gv, and the output is
#	$DR/Perl-modules/html/graphviz2.marpa/16.svg.
# $DR is my web server's doc root (in Debian's RAM disk).

GV=path.set.$1.gv
SVG=path.set.$1.svg

dot -Tsvg data/$GV > html/$SVG

cp html/$SVG $DR/Perl-modules/html/graphviz2.marpa.pathutils

ls -aFl html/$SVG $DR/Perl-modules/html/graphviz2.marpa.pathutils/$SVG
