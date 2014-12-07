package GraphViz2::Marpa::PathUtils::Demo;

use strict;
use warnings;
use warnings qw(FATAL utf8); # Fatalize encoding glitches.
use open     qw(:std :utf8); # Undeclared streams in UTF-8.

use Config;

use Date::Format; # For time2str().
use Date::Simple;

use File::Spec; # splitpath().

use GraphViz2::Marpa::PathUtils;
use GraphViz2::Marpa::PathUtils::Config;

use Moo;

use Path::Tiny; # For path().

use Text::Xslate 'mark_raw';

use Types::Standard qw/Str/;

has input_dot_file_prefix =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has output_html_file_name =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

our $VERSION = '2.00';

# -----------------------------------------------

sub generate_demo
{
	my($self)       = @_;
	my($data_dir)   = 'data';
	my($html_dir)   = 'html';
	my(@demo_file)  = path($data_dir);
	my(@cluster_in) = grep{/clusters.in.gv/} grep{!/skip/} @demo_file;

	$self -> find_clusters($data_dir, $html_dir, \@cluster_in);

	my(@fixed_in) = grep{/fixed.paths.in.gv/}  @demo_file;

	$self -> find_fixed_length_paths($data_dir, $html_dir, \@fixed_in);

	@demo_file       = read_dir($html_dir);
	@cluster_in      = sort grep{/clusters.in.svg/}     @demo_file;
	my(@cluster_out) = sort grep{/clusters.out.svg/}    @demo_file;
	@fixed_in        = sort grep{/fixed.paths.in.svg/}  @demo_file;
	my(@fixed_out)   = sort grep{/fixed.paths.out.svg/} @demo_file;

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

	my($config)    = $self -> config;
	my($templater) = Text::Xslate -> new
	(
	  input_layer => '',
	  path        => $$config{template_path},
	);
	my($index) = $templater -> render
	(
		'pathutils.report.tx',
		{
			border          => 1,
			cluster_data    => [@cluster],
			date_stamp      => time2str('%Y-%m-%d %T', time),
			default_css     => "$$config{css_url}/default.css",
			environment     => $self -> generate_demo_environment,
			fancy_table_css => "$$config{css_url}/fancy.table.css",
			fixed_data      => [@fixed_path],
			version         => $VERSION,
		}
	);
	my($file_name) = File::Spec -> catfile('html', 'index.html');

	open(my $fh, '>', $file_name);
	print $fh $index;
	close $fh;

	print "Wrote: $file_name\n";

} # End of generate_demo.

# ------------------------------------------------

sub generate_demo_environment
{
	my($self) = @_;

	my(@environment);

	# mark_raw() is needed because of the HTML tag <a>.

	push @environment,
	{left => 'Author', right => mark_raw(qq|<a href="http://savage.net.au/">Ron Savage</a>|)},
	{left => 'Date',   right => Date::Simple -> today},
	{left => 'OS',     right => 'Debian V 7'},
	{left => 'Perl',   right => $Config{version} };

	return \@environment;
}
 # End of generate_demo_environment.

# -----------------------------------------------

sub generate_html4cluster
{
	my($self)     = @_;
	my($prefix)   = $self -> input_dot_file_prefix;
	my(@file)     = File::Spec -> splitpath($prefix);
	my($dir_name) = File::Spec -> catpath($file[0], $file[1]);
	my(@path)     = path($dir_name) -> children(qr/$file[2].+/);
	my($in)       = grep{/$file[2]\.in\.gv/} @path;
	my(@out)      = grep{/$file[2]\.out\.\d+\.gv/} sort @path;

	print "Input: $in. \n";

	for my $path (@out)
	{
		print "Output $path. \n";
	}

} # End of generate_html4cluster.

# -----------------------------------------------

1;

=pod

=head1 NAME

C<GraphViz2::Marpa::PathUtils::Demo> - Provide various analyses of Graphviz dot files

=head1 SYNOPSIS

	shell> perl scripts/generate.demo.pl

=head1 DESCRIPTION

This module is only for the use of the author.

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

=head2 generate_demo()

Generates html/index.html using html/*.svg files.

See scripts/generate.demo.pl.

=head2 generate_demo_environment()

Called by generate_demo(). Just adds a footer to the output file html/index.html.

=head2 generate_html4cluster()

Given an input_dot_file_prefix, provided to C<new()> or by calling C<generate_html4cluster($s)>,
find the input DOT file, and all the output DOT files, and combine their corresponding SVGs into
a web page.

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