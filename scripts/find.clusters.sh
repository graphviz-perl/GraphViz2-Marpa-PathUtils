#!/bin/bash

GV=path.set.$1.gv
SVG=path.set.$1.svg

dot -Tsvg data/$GV > html/$SVG

cp html/$SVG $DR/Perl-modules/html/graphviz2.marpa.pathutils

ls -aFl html/$SVG $DR/Perl-modules/html/graphviz2.marpa.pathutils/$SVG

perl -Ilib scripts/find.clusters.pl -input data/$GV -max info

#	-parsed_file data/$1.clusters.in.csv -tree_dot_file data/$1.clusters.out.gv \
#	-report_clusters 1 -report_forest 1 -tree_image html/$1.clusters.out.svg
