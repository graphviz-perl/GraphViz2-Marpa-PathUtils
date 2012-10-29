use Module::Build;

if (open my $fh, '|dot', )
{
	close $fh;
}
else
{
	die "Please install Graphviz from http://www.graphviz.org/.\n";
}

my $class = Module::Build->subclass(code => <<'EOF');

sub ACTION_build
{
	my $self = shift;
	$self->SUPER::ACTION_build(@_);
	$self->run_perl_script('scripts/copy.config.pl');
}
EOF

$class -> new
(
	module_name    => 'GraphViz2::Marpa::PathUtils',
	license        => 'artistic',
	dist_abstract  => 'Provide various analyses of Graphviz dot files',
	dist_author    => 'Ron Savage <ron@savage.net.au>',
	build_requires =>
	{
		Test::More => 0.98,
		Test::Pod  => 1.44,
	},
	configure_requires =>
	{
		Module::Build => 0.3800,
	},
	requires =>
	{
		Capture::Tiny    => 0.10,
		Config           => undef,
		Config::Tiny     => 2.14,
		Date::Format     => 2.24,
		Date::Simple     => 3.03,
		feature          => 1.20,
		File::Copy       => 2.21,
		File::HomeDir    => 0.99,
		File::Slurp      => 9999.19,
		File::Spec       => 3.33,
		File::Which      => 1.09,
		Getopt::Long     => 2.38,
		GraphViz2        => 2.05,
		GraphViz2::Marpa => 1.06,
		Hash::FieldHash  => 0.10,
		IPC::Run3        => 0.045,
		open             => 1.08,
		parent           => 0.225,
		Path::Class      => 0.23,
		Pod::Usage       => 1.36,
		Set::Tiny        => 0.01,
		strict           => 1.03,
		Tree             => 1.01,
		Text::Xslate     => 1.4001,
		warnings         => 1.03,
	},
) -> create_build_script();
