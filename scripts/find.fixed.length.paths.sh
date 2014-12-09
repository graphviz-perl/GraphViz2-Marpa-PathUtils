#!/bin/bash

# FILE=01
# NODE=Act_1
#
# FILE=02
# NODE=5
#
# FILE=03
# NODE=A

FILE=$1
NODE=$2

perl -Ilib scripts/find.fixed.length.paths.pl -input data/fixed.paths.in.$FILE.gv \
	-allow_cycles 0 -path_length 1 -start_node $NODE -max info  \
	-report_paths 1 -output_dot_file out/fixed.paths.out.$FILE.gv

dot -Tsvg data/fixed.paths.in.$FILE.gv > html/fixed.paths.in.$FILE.svg
dot -Tsvg out/fixed.paths.out.$FILE.gv > html/fixed.paths.out.$FILE.svg

cp html/fixed.paths.*.$FILE.svg $DR/Perl-modules/html/graphviz2.marpa.pathutils
