#!/bin/bash

# FILE=01.clusters
# FILE=02.clusters
# ...

FILE=12.clusters

dot -Tsvg data/$FILE.in.gv > html/$FILE.in.svg

perl -Ilib scripts/find.clusters.pl -input data/$FILE.in.gv \
	-parsed_file data/$FILE.in.csv -tree_dot_file data/$FILE.out.gv \
	-report_clusters 1 -report_forest 1 -tree_image html/$FILE.out.svg
