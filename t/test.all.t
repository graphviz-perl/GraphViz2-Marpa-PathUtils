#!/usr/bin/env perl

use strict;
use warnings;

use Capture::Tiny 'capture';

use GraphViz2::Marpa::PathUtils;

use Test::More;

# -------------

sub check_clusters
{
	GraphViz2::Marpa::PathUtils -> new
	(
		input_file      => 'data/01.clusters.in.gv',
		report_clusters => 1,
	) -> find_clusters;
}

# -------------

sub check_fixed
{
	GraphViz2::Marpa::PathUtils -> new
	(
		input_file   => 'data/01.fixed.paths.in.gv',
		report_paths => 1,
		start_node   => 'Act_1',
		path_length  => 3,
	) -> find_fixed_length_paths;
}

# -------------

my($test_count)      = 2;
my($stdout, $stderr) = capture \&check_fixed;
my($expected)        = <<'EOS';
Starting node: Act_1. Path length: 3. Allow cycles: 0. Solutions: 9:
Act_1 -> Act_23 -> Act_25 -> Act_3
Act_1 -> Act_23 -> Act_25 -> Act_24
Act_1 -> Act_23 -> Act_24 -> Ext_3
Act_1 -> Act_23 -> Act_24 -> Act_25
Act_1 -> Act_23 -> Act_24 -> Act_22
Act_1 -> Act_23 -> Act_22 -> Act_24
Act_1 -> Act_23 -> Act_22 -> Act_21
Act_1 -> Act_21 -> Act_22 -> Act_24
Act_1 -> Act_21 -> Act_22 -> Act_23
EOS

ok($stdout eq $expected);

($stdout, $stderr) = capture \&check_clusters;
$expected          = <<'EOS';
Clusters:
1: J
2: A, B, C, D, E, K, X, Y
3: I, M
4: F, G, H, L
EOS

ok($stdout eq $expected);

done_testing($test_count);
