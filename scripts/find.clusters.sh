#!/bin/bash

GV=clusters.in.$1.gv
GV2=clusters.out.$1
SVG=clusters.in.$1.svg

MAX=$2

if [ -z "$MAX" ]
then
	MAX=info
fi

dot -Tsvg data/$GV > html/$SVG

cp html/$SVG $DR/Perl-modules/html/graphviz2.marpa.pathutils

perl -Ilib scripts/find.clusters.pl -input data/$GV -max $MAX -output out/$GV2 -report_clusters 1

for i in out/$GV2* ;
do
	IN=`basename $i .gv`
	OUT="$IN.svg"
	IN="$IN.gv";

	dot -Tsvg out/$IN > html/$OUT

	cp html/$OUT $DR/Perl-modules/html/graphviz2.marpa.pathutils/
done

#ls -aFl data/$GV2* -aFl html/$GV2* $DR/Perl-modules/html/graphviz2.marpa.pathutils/$GV2*
