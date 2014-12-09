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

perl -Ilib scripts/find.fixed.length.paths.pl -input data/$FILE \
	-allow_cycles 0 -path_length 1 -start_node $NODE -max info  \
	-report_paths 1 -output_dot_file_prefix out/$FILE.out.gv
