#!/bin/bash

GV=path.set.$1.in.gv
GV2=path.set.$1.out
SVG=path.set.$1.in.svg

MAX=$2

if [ -z "$MAX" ]
then
	MAX=info
fi

dot -Tsvg data/$GV > html/$SVG

cp html/$SVG $DR/Perl-modules/html/graphviz2.marpa.pathutils

perl -Ilib scripts/find.clusters.pl -input data/$GV -max $MAX -output_dot_file_prefix data/$GV2 -report_clusters 1

for i in data/$GV2* ;
do
	IN=`basename $i .gv`
	OUT="$IN.svg"
	IN="$IN.gv";

	dot -Tsvg data/$IN > html/$OUT

	cp html/$OUT $DR/Perl-modules/html/graphviz2.marpa.pathutils/
done

#ls -aFl data/$GV2* -aFl html/$GV2* $DR/Perl-modules/html/graphviz2.marpa.pathutils/$GV2*
