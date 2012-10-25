#!/usr/bin/env perl

use feature qw/say unicode_strings/;
use open qw(:std :utf8);
use strict;
use warnings;
use warnings qw(FATAL utf8);

use Getopt::Long;

use GraphViz2::Marpa::PathUtils;

use Pod::Usage;

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
	\%option,
	'description=s',
	'format=s',
	'help',
	'input_file=s',
	'lexed_file=s',
	'maxlevel=s',
	'minlevel=s',
	'output_file=s',
	'parsed_file=s',
	'report_clusters=i',
	'report_forest=i',
	'report_items=i',
	'tree_dot_file=s',
	'tree_image_file=s',
) )
{
	pod2usage(1) if ($option{'help'});

	exit GraphViz2::Marpa::PathUtils -> new(%option) -> find_clusters;
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

find.clusters.pl - Run GraphViz2::Marpa::PathUtils.

=head1 SYNOPSIS

find.clusters.pl [options]

	Options:
	-description graphDescription
	-format imageFormatType
	-help
	-input_file aDotInputFileName
	-lexed_file aLexedOutputFileName
	-maxlevel logOption1
	-minlevel logOption2
	-output_file aDOTInputFileName
	-parsed_file aParsedOutputFileName
	-report_clusters $Boolean
	-report_forest $Boolean
	-report_items $Boolean
	-tree_dot_file aDOTInputFileName
	-tree_image_file aDOTOutputFileName

Exit value: 0 for success, 1 for failure. Die upon error.

=head1 OPTIONS

=over 4

=item o -description graphDescription

Read the DOT-style graph definition from the command line.

You are strongly encouraged to surround this string with '...' to protect it from your shell.

See also the -input_file option to read the description from a file.

The -description option takes precedence over the -input_file option.

Default: ''.

=item o -format anImageFormatType

Specify the type of image output by DOT to the tree_image_file, if any.

Default: 'svg'.

=item o -help

Print help and exit.

=item o -input_file aDotInputFileName

Read the DOT-style graph definition from a file.

See also the -description option to read the graph definition from the command line.

The -description option takes precedence over the -input_file option.

Default: ''.

See the distro for data/*.gv.

=item o -lexed_file aLexedOutputFileName

Specify the name of a CSV file of parsed tokens to write.

See the distro for data/*.lex.

Default: ''.

The default means the file is not written.

=item o -maxlevel logOption1

This option affects Log::Handler.

See the Log::handler docs.

Default: 'notice'.

=item o -minlevel logOption2

This option affects Log::Handler.

See the Log::handler docs.

Default: 'error'.

No lower levels are used.

=item o -parsed_file aParsedOutputFileName

Specify the name of a CSV file of parsed tokens to write.

See the distro for data/*.parse.

Default: ''.

The default means the file is not written.

=item o -report_clusters $Boolean

Log the clusters detected.

Default: 0.

=item o -report_forest $Boolean

Log the forest parsed from the input file.

Default: 0.

=item o -report_items $Boolean

Log the items recognized by the parser.

Default: 0.

=item o -tree_dot_file aDOTInputFileName

Specify the name of a DOT file to write for the trees found.

Default: ''.

The default means the file is not written.

=item o -tree_image_file aDOTOutputFileName

Specify the name of an SVG file to write for the trees found.

If this file is created, the value of the format option (which defaults to 'svg') is passed to dot.

Default: ''.

The default means the file is not written.

=back

=cut
