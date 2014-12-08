package GraphViz2::Marpa::PathUtils::Demo;

use strict;
use warnings;
use warnings qw(FATAL utf8); # Fatalize encoding glitches.
use open     qw(:std :utf8); # Undeclared streams in UTF-8.

use Capture::Tiny 'capture';

use Config;

use Date::Simple;

use File::Spec;

use GraphViz2::Marpa::PathUtils;
use GraphViz2::Marpa::PathUtils::Config;

use Moo;

use Path::Tiny; # For path().

use Text::Xslate 'mark_raw';

use Types::Standard qw/HashRef/;

has config =>
(
	default => sub {return GraphViz2::Marpa::PathUtils::Config -> new -> config},
	is      => 'ro',
	isa     => HashRef,
);

our $VERSION = '2.00';

# -----------------------------------------------

sub find_clusters
{
	my($self, $data_dir, $out_dir) = @_;

	my($out_prefix);
	my($result);

	for my $in_file (sort {"$a" cmp "$b"} path($data_dir) -> children(qr/^clusters/) )
	{
		$out_prefix = $in_file =~ s/\.in\./\.out\./r;
		$out_prefix =~ s/\.gv$//;
		$out_prefix =~ s/^$data_dir/$out_dir/;

		print "$in_file => $out_prefix\n";

		$result = GraphViz2::Marpa::PathUtils -> new
					(
						input_file             => "$in_file",
						output_dot_file_prefix => "$out_prefix",
					) -> find_clusters;
	}

} # End of find_clusters.

# -----------------------------------------------

sub generate_demo
{
	my($self)     = @_;
	my($data_dir) = 'data/';
	my($html_dir) = 'html/';
	my($out_dir)  = 'out/';

	$self -> find_clusters($data_dir, $out_dir);

	my(@html4clusters) = map
	{
		s/^$html_dir//;

		my($s) = $_;
		$s     =~ s/^/$data_dir/;
		$s     =~ s/html$/gv/;

		"<a href = '$_'>$s</a>"
	} @{$self -> generate_html4clusters};

=pod

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

=cut

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
			cluster_data    => [map{[{td => mark_raw($_)}]} @html4clusters],
			default_css     => "$$config{css_url}/default.css",
			environment     => $self -> generate_demo_environment,
			fancy_table_css => "$$config{css_url}/fancy.table.css",
#			fixed_data      => [@fixed_path],
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

sub generate_html4clusters
{
	my($self)      = @_;
	my($data_dir)  = 'data/';
	my($html_dir)  = 'html/';
	my($out_dir)   = 'out/';
	my($config)    = $self -> config;
	my($templater) = Text::Xslate -> new
	(
	  input_layer => '',
	  path        => $$config{template_path},
	);

	my($iter);
	my($html_prefix, $html_file, @html_file);
	my($out_prefix, $out_file);
	my($stdout, $stderr, $svg_in_prefix, $svg_in_file, $svg_out_prefix, $svg_out_file, @svg_out_file);

	for my $in_file (sort {"$a" cmp "$b"} path($data_dir) -> children(qr/^clusters/) )
	{
		($stdout, $stderr) = capture{system('dot', '-T', 'svg', $in_file)};
		$svg_in_prefix     = $in_file =~ s/^$data_dir/$html_dir/r;
		$svg_in_prefix     =~ s/\.gv$//;
		$svg_in_file       = path("$svg_in_prefix.svg");
		$html_file         = path("$svg_in_prefix.html");

		$svg_in_file -> spew_utf8($stdout);

		# We must remove the html/ prefix before $svg_in_file goes into the template.

		$svg_in_file  =~ s/^$html_dir//;
		$out_prefix   = $in_file =~ s/\.in\./\.out\./r;
		$out_prefix   =~ s/\.gv$//;
		$out_prefix   =~ s/^$data_dir/$out_dir/;
		@svg_out_file = ();
		$iter         = path($out_dir) -> iterator;

		while ($out_file = $iter -> () )
		{
			next if ($out_file !~ /^\Q$out_prefix\E/);

			($stdout, $stderr) = capture{system('dot', '-T', 'svg', $out_file)};
			$svg_out_prefix    = $out_file =~ s/^$out_dir/$html_dir/r;
			$svg_out_prefix    =~ s/\.gv$//;
			$svg_out_file      = path("$svg_out_prefix.svg");

			$svg_out_file -> spew_utf8($stdout);

			$svg_out_file =~ s/^$html_dir//;

			push @svg_out_file, [$out_file, $svg_out_file];
		}

		my($index) = $templater -> render
		(
			'cluster.report.tx',
			{
				border       => 1,
				default_css  => "$$config{css_url}/default.css",
				environment  => $self -> generate_demo_environment,
				input_data   =>
				[
					[
						{td => $in_file},
						{td => mark_raw("<object data = '$svg_in_file'></object>")},
					],
				],
				input_file   => $in_file,
				output_data  =>
				[
					map
					{
						[
							{td => $$_[0]},
							{td => mark_raw("<object data = '$$_[1]'></object>")},
						]
					} sort{$$a[0] cmp $$b[0]} @svg_out_file,
				],
				version      => $VERSION,
			}
		);

		$html_file -> spew_utf8($index);

		push @html_file, $html_file;

		print "Wrote: $html_file\n";
	}

	return \@html_file;

} # End of generate_html4clusters.

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
