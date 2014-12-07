package GraphViz2::Marpa::PathUtils::Utils;

use strict;
use warnings;
use warnings qw(FATAL utf8); # Fatalize encoding glitches.
use open     qw(:std :utf8); # Undeclared streams in UTF-8.

use Config;

use Date::Format; # For time2str().
use Date::Simple;

use File::Spec;

use GraphViz2::Marpa::PathUtils;
use GraphViz2::Marpa::PathUtils::Config;

use Moo;

our $VERSION = '2.00';

# -----------------------------------------------

sub find_clusters
{
	my($self, $data_dir, $html_dir, $file_name) = @_;
	my($graph) = GraphViz2::Marpa::PathUtils -> new;

	my($in_file_name);
	my($out_file_name);
	my($parsed_file_name);
	my($result);
	my($tree_dot_name, $tree_image_name);

	for my $name (sort @$file_name)
	{
		$out_file_name    = $name =~ s/\.in\.gv/\.in\.svg/r;
		$parsed_file_name = $name =~ s/gv$/csv/r;
		$tree_dot_name    = $name =~ s/\.in\./\.out\./r;
		$tree_image_name  = $name =~ s/in.gv/out.svg/r;
		$in_file_name     = File::Spec -> catfile($data_dir, $name);

		$graph -> input_file($in_file_name);
		$graph -> parsed_file(File::Spec -> catfile($data_dir, $parsed_file_name) );
		$graph -> tree_dot_file(File::Spec -> catfile($data_dir, $tree_dot_name) );
		$graph -> tree_image_file(File::Spec -> catfile($html_dir, $tree_image_name) );

		$result        = $graph -> find_clusters;
		$out_file_name = File::Spec -> catfile($html_dir, $out_file_name);

		`dot -Tsvg $in_file_name > $out_file_name`;
	}

} # End of find_clusters.

# -----------------------------------------------

sub find_fixed_length_paths
{
	my($self, $data_dir, $html_dir, $file_name) = @_;
	my($graph)      = GraphViz2::Marpa::PathUtils -> new;
	my(%start_node) =
	(
		'01.fixed.paths.in.gv' => 'Act_1',
		'02.fixed.paths.in.gv' => '5',
		'03.fixed.paths.in.gv' => 'A',
	);

	my($in_file_name);
	my($out_file_name);
	my($parsed_file_name);
	my($result);
	my($tree_dot_name, $tree_image_name);

	for my $name (sort @$file_name)
	{
		$out_file_name    = $name =~ s/\.in\.gv/\.in\.svg/r;
		$parsed_file_name = $name =~ s/gv$/csv/r;
		$tree_dot_name    = $name =~ s/\.in\./\.out\./r;
		$tree_image_name  = $name =~ s/in.gv/out.svg/r;
		$in_file_name     = File::Spec -> catfile($data_dir, $name);

		$graph -> allow_cycles(0);
		$graph -> input_file($in_file_name);
		$graph -> parsed_file(File::Spec -> catfile($data_dir, $parsed_file_name) );
		$graph -> tree_dot_file(File::Spec -> catfile($data_dir, $tree_dot_name) );
		$graph -> path_length(3);
		$graph -> start_node($start_node{$name});
		$graph -> tree_image_file(File::Spec -> catfile($html_dir, $tree_image_name) );

		$result        = $graph -> find_fixed_length_paths;
		$out_file_name = File::Spec -> catfile($html_dir, $out_file_name);

		`dot -Tsvg $in_file_name > $out_file_name`;
	}

} # End of find_fixed_length_paths.

# -----------------------------------------------

1;

=pod

=head1 NAME

C<GraphViz2::Marpa::PathUtils::Demo> - Provide various analyses of Graphviz dot files

=head1 SYNOPSIS

	shell> perl scripts/generate.demo.pl

=head1 DESCRIPTION

GraphViz2::Marpa::PathUtils::Demo generates html/index.html using html/*.svg files.

See scripts/generate.demo.pl.

=head1 Distributions

This module is available as a Unix-style distro (*.tgz).

See L<http://savage.net.au/Perl-modules/html/installing-a-module.html>
for help on unpacking and installing distros.

=head1 Installation

See L<GraphViz2::Marpa::PathUtils/Installation>.

=head1 Constructor and Initialization

=head2 Calling new()

C<new()> is called as C<< my($obj) = GraphViz2::Marpa::PathUtils::Demo -> new >>.

It returns a new object of type C<GraphViz2::Marpa::PathUtils::Demo>.

=head1 Methods

=head1 Version Numbers

Version numbers < 1.00 represent development versions. From 1.00 up, they are production versions.

=head1 Machine-Readable Change Log

The file CHANGES was converted into Changelog.ini by L<Module::Metadata::Changes>.

=head1 Support

Email the author, or log a bug on RT:

L<https://rt.cpan.org/Public/Dist/Display.html?Name=GraphViz2::Marpa::PathUtils>.

=head1 Author

L<GraphViz2::Marpa::PathUtils> was written by Ron Savage I<E<lt>ron@savage.net.auE<gt>> in 2012.

Home page: L<http://savage.net.au/index.html>.

=head1 Copyright

Australian copyright (c) 2012, Ron Savage.

	All Programs of mine are 'OSI Certified Open Source Software';
	you can redistribute them and/or modify them under the terms of
	The Artistic License, a copy of which is available at:
	http://www.opensource.org/licenses/index.html

=cut
