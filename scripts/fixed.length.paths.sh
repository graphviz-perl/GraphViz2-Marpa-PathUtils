#!/bin/bash

# FILE=01.fixed.paths
# NODE=Act_1
#
# FILE=02.fixed.paths
# NODE=5
#
# FILE=03.fixed.paths
# NODE=A
# ...

FILE=01.fixed.paths
NODE=Act_1

perl -Ilib scripts/fixed.length.paths.pl -input data/$FILE.in.gv \
	-report_paths 1 -allow_cycles 0 -path_length 3 -start_node $NODE \
	-parsed_file data/$FILE.in.csv \
	-tree_dot_file data/$FILE.out.gv -tree_image html/$FILE.out.svg

perl scripts/generate.demo.pl

cp html/index.html html/*.svg $DR/Perl-modules/html/graphviz2.pathutils/
