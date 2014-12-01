#!/bin/bash

GV=path.set.$1.in.gv
GV2=path.set.$1.out.gv
SVG=path.set.$1.in.svg
SVG2=path.set.$1.out.svg

dot -Tsvg data/$GV > html/$SVG

cp html/$SVG $DR/Perl-modules/html/graphviz2.marpa.pathutils

perl -Ilib scripts/find.clusters.pl -input data/$GV -max $2 -output_dot data/$GV2 -output_image html/$SVG2 -report_clusters 1

cp html/$SVG2 $DR/Perl-modules/html/graphviz2.marpa.pathutils

ls -aFl html/$SVG html/$SVG2 $DR/Perl-modules/html/graphviz2.marpa.pathutils/$SVG $DR/Perl-modules/html/graphviz2.marpa.pathutils/$SVG2

#	-parsed_file data/$1.clusters.in.csv -tree_dot_file data/$1.clusters.out.gv \
#	-report_clusters 1 -report_forest 1 -tree_image html/$1.clusters.out.svg
