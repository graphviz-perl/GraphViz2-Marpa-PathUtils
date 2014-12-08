#!/bin/bash

# FILE=fixed.paths.in.01.gv
# NODE=Act_1
#
# FILE=fixed.paths.in.02.gv
# NODE=5
#
# FILE=fixed.paths.in.03.gv
# NODE=A

FILE=fixed.paths.in.03.gv
NODE=A

perl -Ilib scripts/find.fixed.length.paths.pl -input data/$FILE.in.gv \
	-allow_cycles 0 -path_length 3 -start_node $NODE \
	-output_dot_file_prefix out/$FILE.out.gv
