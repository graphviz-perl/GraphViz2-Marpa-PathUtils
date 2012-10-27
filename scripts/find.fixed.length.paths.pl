#!/usr/bin/env perl

use strict;
use warnings;

use Getopt::Long;

use GraphViz2::Marpa::PathUtils;

use Pod::Usage;

# -----------------------------------------------

my($option_parser) = Getopt::Long::Parser -> new();

my(%option);

if ($option_parser -> getoptions
(
	\%option,
	'allow_cycles=i',
	'description=s',
	'format=s',
	'help',
	'input_file=s',
	'lexed_file=s',
	'maxlevel=s',
	'minlevel=s',
	'output_file=s',
	'parsed_file=s',
	'path_length=i',
	'report_forest=i',
	'report_items=i',
	'report_paths=i',
	'start_node=s',
	'tree_dot_file=s',
	'tree_image_file=s',
) )
{
	pod2usage(1) if ($option{'help'});

	exit GraphViz2::Marpa::PathUtils -> new(%option) -> find_fixed_length_paths;
}
else
{
	pod2usage(2);
}

__END__

=pod

=head1 NAME

find.fixed.length.paths.pl - Run GraphViz2::Marpa::PathUtils.

=head1 SYNOPSIS

find.fixed.length.paths.pl [options]

	Options:
	-allow_cycles $integer
	-description graphDescription
	-format imageFormatType
	-help
	-input_file aDotInputFileName
	-lexed_file aLexedOutputFileName
	-maxlevel logOption1
	-minlevel logOption2
	-output_file aDOTInputFileName
	-parsed_file aParsedOutputFileName
	-path_length $integer
	-report_forest $Boolean
	-report_items $Boolean
	-report_paths $Boolean
	-start_node aNodeName
	-tree_dot_file aDOTInputFileName
	-tree_image_file aDOTOutputFileName

Exit value: 0 for success, 1 for failure. Die upon error.

=head1 OPTIONS

=over 4

=item o -allow_cycles $integer

Specify whether or not cycles are allowed in the paths found.

Values for $integer:

=over 4

=item o 0 - Do not allow any cycles

This is the default.

=item o 1 - Allow any node to be included once or twice.

=back

Default: 0.

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

=item o -path_length $integer

The path length which all detected trees must have.

Defailt: 0.

=item o -report_forest $Boolean

Log the forest parsed from the input file.

Default: 0.

=item o -report_items $Boolean

Log the items recognized by the parser.

Default: 0.

=item o -report_paths $Boolean

Log the paths detected.

Default: 0.

=item o -start_node aNodeName

The name of the node which all trees must start from.

Default: ''.

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
