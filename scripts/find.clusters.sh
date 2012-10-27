#!/bin/bash

# FILE=01.clusters
# FILE=02.clusters
# ...

FILE=01.clusters.complex

dot -Tsvg data/$FILE.in.gv > html/$FILE.in.svg

perl -Ilib scripts/find.clusters.pl -input data/$FILE.in.gv \
	-parsed_file data/$FILE.in.csv -tree_dot_file data/$FILE.out.gv \
	-report_clusters 1 -tree_image html/$FILE.out.svg

perl -Ilib scripts/generate.demo.pl

cp html/index.html html/*.svg $DR/Perl-modules/html/graphviz2.pathutils/
