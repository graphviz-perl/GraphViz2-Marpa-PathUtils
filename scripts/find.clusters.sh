#!/bin/bash

dot -Tsvg data/$1.clusters.in.gv > html/$1.clusters.in.svg

perl -Ilib scripts/find.clusters.pl -input data/$1.clusters.in.gv $2 $3 \
	-parsed_file data/$1.clusters.in.csv -tree_dot_file data/$1.clusters.out.gv \
	-report_clusters 1 -report_forest 1 -tree_image html/$1.clusters.out.svg
