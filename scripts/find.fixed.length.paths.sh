#!/bin/bash

# FILE=01
# NODE=Act_1
#
# FILE=02
# NODE=5
#
# FILE=03
# NODE=A

LENGTH=$1
FILE=$2
NODE=$3

perl -Ilib scripts/find.fixed.length.paths.pl \
	-allow_cycles 0 \
	-input data/fixed.paths.in.$FILE.gv \
	-max info \
	-output_dot_file out/fixed.paths.out.$FILE.gv \
	-path_length $LENGTH \
	-report_paths 1 \
	-start_node $NODE

dot -Tsvg data/fixed.paths.in.$FILE.gv > html/fixed.paths.in.$FILE.svg
dot -Tsvg out/fixed.paths.out.$FILE.gv > html/fixed.paths.out.$FILE.svg

cp html/fixed.paths.*.$FILE.svg $DR/Perl-modules/html/graphviz2.marpa.pathutils
