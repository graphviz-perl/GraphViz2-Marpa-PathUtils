#!/usr/bin/env perl

use feature qw/say unicode_strings/;
use open qw(:std :utf8);
use strict;
use warnings;
use warnings qw(FATAL utf8);

use Date::Format; # For time2str().

use File::Slurp; # For read_dir().

use GraphViz2::Marpa::PathUtils;
use GraphViz2::Marpa::PathUtils::Config;

use Text::Xslate 'mark_raw';

# -----------------------------------------------

sub build_table
{
	my($templater, $file_name) = @_;

	my($table) = $templater -> render
	(
		'basic.table.tx',
		{
			border => 0,
			row    => [ [map{ {td => mark_raw $_} } @$file_name] ],
		}
	);

	say $table;
	say '-' x 50;

	return $table;

} # End of build_table.

# -----------------------------------------------

my($config)    = GraphViz2::Marpa::PathUtils::Config -> new -> config;
my($templater) = Text::Xslate -> new
(
  input_layer => '',
  path        => $$config{template_path},
);
my($count)        = 0;
my(@demo_file)    = sort grep{! /index/} read_dir('html');
my(@cluster_in)   = grep{/\.clusters.*\.in\./}    @demo_file;
my(@cluster_out)  = grep{/\.clusters.*\.out\./}   @demo_file;
my(@fixed_in)     = grep{/\.fixed\.paths\.in\./}  @demo_file;
my(@fixed_out)    = grep{/\.fixed\.paths\.out\./} @demo_file;

say map{"Demo file: $_\n"} @demo_file;

my(@cluster);

for my $i (0 .. $#cluster_in)
{
	push @cluster,
	[
		{td => mark_raw qq|<object data="$cluster_in[$i]">|},
		{td => $cluster_in[$i]},
	],
	[
		{td => mark_raw qq|<object data="$cluster_out[$i]">|},
		{td => $cluster_out[$i]},
	];
}

my(@fixed_path);

for my $i (0 .. $#fixed_in)
{
	push @fixed_path,
	[
		{td => mark_raw qq|<object data="$fixed_in[$i]">|},
		{td => $fixed_in[$i]},
	],
	[
		{td => mark_raw qq|<object data="$fixed_out[$i]">|},
		{td => $fixed_out[$i]},
	];
}

my($index) = $templater -> render
(
	'pathutils.report.tx',
	{
		border       => 1,
		cluster_data => [@cluster],
		date_stamp   => time2str('%Y-%m-%d %T', time),
		fixed_data   => [@fixed_path],
		version      => $GraphViz2::Marpa::PathUtils::VERSION,
	}
);
my($file_name) = File::Spec -> catfile('html', 'index.html');

open(OUT, '>', $file_name);
print OUT $index;
close OUT;

print "Wrote: $file_name. \n";
