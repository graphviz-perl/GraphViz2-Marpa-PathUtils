package GraphViz2::Marpa::PathUtils;

use feature qw/say unicode_strings/;
use open qw(:std :utf8);
use parent 'GraphViz2::Marpa';
use strict;
use warnings;
use warnings qw(FATAL utf8);

use Config;

use Data::Dumper::Concise;

use Date::Format; # For time2str().
use Date::Simple;

use File::Slurp; # For read_dir().
use File::Which; # For which().

use GraphViz2;
use GraphViz2::Marpa::PathUtils::Config;

use Hash::FieldHash ':all';

use IPC::Run3; # For run3().

use Set::Tiny;

use Text::Xslate 'mark_raw';

use Tree::DAG_Node;

fieldhash my %allow_cycles     => 'allow_cycles';
fieldhash my %attributes       => 'attributes';
fieldhash my %cluster_edge_set => 'cluster_edge_set';
fieldhash my %cluster_set      => 'cluster_set';
fieldhash my %config           => 'config';
fieldhash my %dot_input        => 'dot_input';
fieldhash my %dot_output       => 'dot_output';
fieldhash my %driver           => 'driver';
fieldhash my %fixed_path_set   => 'fixed_path_set';
fieldhash my %format           => 'format';
fieldhash my %path_length      => 'path_length';
fieldhash my %report_clusters  => 'report_clusters';
fieldhash my %report_paths     => 'report_paths';
fieldhash my %start_node       => 'start_node';
fieldhash my %tree_dot_file    => 'tree_dot_file';
fieldhash my %tree_image_file  => 'tree_image_file';

our $VERSION = '1.01';

# -----------------------------------------------
# For each node, find all the children of the root
# which lead to all copies of that node.
# Warning: A node with no edges has no ancestors.

sub _find_ancestors
{
	my($self) = @_;

	my(%ancestor);
	my($node_value);
	my($root_value);

	for my $root ($self -> parser -> paths -> daughters)
	{
		$root_value = $root -> name;

		$root -> walk_down
		({
			callback =>
			sub
			{
				my($node)                           = @_;
				$node_value                         = $node -> name;
				$ancestor{$node_value}              = {} if (! $ancestor{$node_value});
				$ancestor{$node_value}{$root_value} = 1;

				return 1;
			},
			_depth => 0,
		});
	}

	$self -> log(info => 'Ancestors:');

	for $node_value (sort keys %ancestor)
	{
		$self -> log(info => "Node $node_value. Ancestors: " . join(', ', sort keys %{$ancestor{$node_value} }) );
	}

	return \%ancestor;

} # End of _find_ancestors.

# -----------------------------------------------

sub _find_cluster_kin
{
	my($self)      = @_;
	my($ancestors) = $self -> _find_ancestors;

	# Phase 1: Put nodes with edges into sets.

	my(%cluster);
	my($value);

	$self -> parser -> paths -> walk_down
	({
		callback =>
		sub
		{
			my($node) = @_;

			# Skip the root.

			return 1 if (! defined $node -> mother);

			$value = $node -> name;

			for my $ancestor_value (keys %{$$ancestors{$value} })
			{
				$cluster{$ancestor_value} = Set::Tiny -> new if (! defined $cluster{$ancestor_value});

				$cluster{$ancestor_value} -> insert($value);
			}

			return 1;
		},
		_depth => 0,
	});

	# Phase 2: Put nodes without edges into sets.

	my($found);

	for $value (keys %{$self -> parser -> nodes})
	{
		$found = 0;

		for my $key (keys %cluster)
		{
			if ($cluster{$key} -> member($value) )
			{
				$found = 1;

				last;
			}
		}

		$cluster{$value} = Set::Tiny -> new($value) if (! $found);
	}

	return \%cluster;

} # End of _find_cluster_kin.

# -----------------------------------------------
# For each cluster, we have to re-scan the original *.gv file
# to find out how the clusters' members are linked together.
# This means finding the edges between them, and the edges' attributes.

sub _find_cluster_paths
{
	my($self)  = @_;
	my($count) = 0;
	my($set)   = $self -> cluster_set;

	my(@cluster_set);
	my($edge, @edge_set);
	my(@member, %member);
	my($name_1, $name_2);

	for my $cluster (@$set)
	{
		$count++;

		@edge_set        = ();
		@member          = $cluster -> members;
		%member          = ();
		@member{@member} = (1) x @member;

		if ($#member == 0)
		{
			# Use undef since node names can be ''.

			push @edge_set, [$member[0], undef];
		}
		else
		{
			$self -> parser -> edges -> walk_down
			({
				callback => sub
				{
					my($node) = @_;
					$name_1   = $node -> name;

					if (defined $member{$name_1})
					{
						$edge = ${$node -> attributes}{edge};

						for my $kid ($node -> daughters)
						{
							$name_2 = $kid -> name;

							if (defined $member{$name_2} && ($name_1 ne $name_2) )
							{
								push @edge_set, [$name_1, $name_2];
							}
						}
					}

					return 1;
				},
				_depth => 0,
			});
		}

		@edge_set = sort{(defined($a) ? $$a[0] : '') cmp (defined($b) ? $$b[0] : '')} @edge_set;

		push @cluster_set, [@edge_set];
	}

	# The extra () stop warning msgs of the form: Argument "X" isn't numeric

	@cluster_set = sort{(defined($a) ? $$a[0][0] : '') cmp (defined($b) ? $$b[0][0] : '')} @cluster_set;

	$self -> cluster_edge_set(\@cluster_set);

} # End of _find_cluster_paths.

# -----------------------------------------------
# Called by the user.

sub find_clusters
{
	my($self) = @_;

	# Run code common to all algorithms.

	$self -> _set_up_forest;

	# Process the tree.

	$self -> _winnow_cluster_sets($self -> _find_cluster_kin);
	$self -> report_cluster_members if ($self -> report_clusters);
	$self -> _find_cluster_paths;
	$self -> output_cluster_image;

	print join("\n", @{$self -> parser -> paths -> draw_ascii_tree}), "\n"; # TODO.

	# Return 0 for success and 1 for failure.

	return 0;

} # End of find_clusters.

# -----------------------------------------------

sub _find_edge_attributes
{
	my($self, $from, $to) = @_;
	my($found) = 0;

	my(%attributes);
	my($from_value);
	my($to_value);

	$self -> parser -> edges -> walk_down
	({
		attributes => \%attributes,
		callback   =>
		sub
		{
			my($node, $options) = @_;

			# Skip the root.

			return 1 if (! defined $node -> mother);

			$from_value = $node -> name;

			return 1 if ($from ne $from_value);

			for my $child ($node -> daughters)
			{
				$to_value = $child -> name;

				last if ($to ne $to_value);

				$$options{attributes} = $node -> attributes;

				return 0;
			}

			return 1;
		},
		_depth => 0,
	});

	return {%attributes};

} # End of _find_edge_attributes.

# -----------------------------------------------
# Find N candidates for the next node along the path.

sub _find_fixed_length_candidates
{
	my($self, $solution, $stack) = @_;
	my($current_node) = $$solution[$#$solution];

	# Add the node's parent, if it's not the root.
	# Then add the node's children.

	my(@neighbours);

	$self -> parser -> edges -> walk_down
	({
		callback =>
		sub
		{
			my($node) = @_;

			# We only want neighbours of the current node.
			# So, skip this node if:
			# o It is the root node.
			# o It is not the current node.

			return 1 if ($node -> name ne $current_node -> name);

			# Now find its neighbours.

			for my $n ($node -> mother, $node -> daughters)
			{
				push @neighbours, $n if (defined $n -> mother);
			}

			return 1;
		},
		_depth => 0,
	});

	# Elements:
	# 0 .. N - 1: The neighbours.
	# N:          The count of neighbours.

	push @$stack, @neighbours, $#neighbours + 1;

} # End of _find_fixed_length_candidates.

# -----------------------------------------------
# Find all paths starting from any copy of the target start_node.

sub _find_fixed_length_path_set
{
	my($self, $start) = @_;
	my($one_solution) = [];
	my($stack)        = [];

	my(@all_solutions);
	my($count, $candidate);

	# Push the first copy of the start node, and its count (1), onto the stack.

	push @$stack, $$start[0], 1;

	# Process these N candidates 1-by-1.
	# The top-of-stack is a candidate count.

	while ($#$stack >= 0)
	{
		while ($$stack[$#$stack] > 0)
		{
			($count, $candidate) = (pop @$stack, pop @$stack);

			push @$stack, $count - 1;
			push @$one_solution, $candidate;

			# Does this candidate suit the solution so far?

			if ($#$one_solution == $self -> path_length)
			{
				# Yes. Save this solution.

				push @all_solutions, [@$one_solution];

				# Discard this candidate, and try another.

				pop @$one_solution;
			}
			else
			{
				# No. The solution is still too short.
				# Push N more candidates onto the stack.

				$self -> _find_fixed_length_candidates($one_solution, $stack);
			}
		}

		# Pop the candidate count (0) off the stack.

		pop @$stack;

		# Remaining candidates, if any, must be contending for the 2nd last slot.
		# So, pop off the node in the last slot, since we've finished
		# processing all candidates for that slot.
		# Then, backtrack to test the next set of candidates for what,
		# after this pop, will be the new last slot.

		pop @$one_solution;
	}

	$self -> fixed_path_set([@all_solutions]);

} # End of _find_fixed_length_path_set.

# -----------------------------------------------
# Find all paths starting from any copy of the target start_node.

sub _find_fixed_length_paths
{
	my($self) = @_;

	# Phase 1: Find all copies of the start node.

	my(@start);

	$self -> parser -> edges -> walk_down
	({
		callback =>
		sub
		{
			my($node) = @_;

			# Skip the root.

			return 1 if (! defined $node -> mother);

			push @start, $node if ($node -> name eq $self -> start_node);

			return 1;
		},
		_depth => 0,
	});

	# Give up if the given node was not found.

	die 'Error: Start node (', $self -> start_node, ") not found\n" if ($#start < 0);

	# Phase 2: Process each copy of the start node.

	$self -> _find_fixed_length_path_set(\@start);

} # End of _find_fixed_length_paths.

# -----------------------------------------------
# Called by the user.

sub find_fixed_length_paths
{
	my($self) = @_;

	die "Error: No start node specified\n"  if (! defined $self -> start_node);
	die "Error: Path length must be >= 0\n" if ($self -> path_length < 0);

	# Run code common to all algorithms.

	$self -> _set_up_forest;

	# Process the tree.

	$self -> _find_fixed_length_paths;
	$self -> _winnow_fixed_length_paths;

	my($title) = 'Starting node: ' . $self -> start_node . "\\n" .
		'Path length: ' . $self -> path_length . "\\n" .
		'Allow cycles: ' . $self -> allow_cycles . "\\n" .
		'Solutions: ' . scalar @{$self -> fixed_path_set};

	$self -> _prepare_fixed_length_output($title);
	$self -> report_fixed_length_paths($title) if ($self -> report_paths);
	$self -> output_dot_file                   if ($self -> tree_dot_file);
	$self -> output_fixed_length_image         if ($self -> tree_image_file);

	# Return 0 for success and 1 for failure.

	return 0;

} # End of find_fixed_length_paths.

# -----------------------------------------------

sub _init
{
	my($self, $arg)         = @_;
	$$arg{allow_cycles}     ||= 0;     # Caller can set.
	$$arg{attributes}       = {};
	$$arg{cluster_edge_set} = {};
	$$arg{cluster_set}      = {};
	$$arg{config}           = GraphViz2::Marpa::PathUtils::Config -> new -> config;
	$$arg{dot_input}        = '';
	$$arg{dot_output}       = '';
	$$arg{driver}           ||= which('dot'); # Caller can set.
	$$arg{fixed_path_set}   = [];
	$$arg{format}           ||= 'svg'; # Caller can set.
	$$arg{path_length}      ||= 0;     # Caller can set.
	$$arg{report_clusters}  ||= 0;     # Caller can set.
	$$arg{report_paths}     ||= 0;     # Caller can set.
	$$arg{start_node}       = defined($$arg{start_node}) ? $$arg{start_node} : undef; # Caller can set (to 0).
	$$arg{tree_dot_file}    ||= ''; # Caller can set.
	$$arg{tree_image_file}  ||= ''; # Caller can set.
	$self                   = $self -> SUPER::_init($arg);

	return $self;

} # End of _init.

# --------------------------------------------------

sub new
{
	my($class, %arg) = @_;
	my($self)        = bless {}, $class;
	$self            = $self -> _init(\%arg);

	return $self;

}	# End of new.

# -----------------------------------------------

sub output_cluster_image
{
	my($self) = @_;

	# Short-circuit if no output wanted.

	return if (! $self -> tree_dot_file && ! $self -> tree_image_file);

	my(%nodes) = %{$self -> parser -> nodes};
	my(%style) = %{$self -> parser -> style};
	my(%type)  = %{$self -> parser -> type};
	my($graph) = GraphViz2 -> new
		(
			edge   => '', # TODO.
			global => {directed => $type{digraph}, name => $type{graph_id}, strict => $type{strict} },
			graph  => {label => $style{label} || 'Cluster set', rankdir => $style{rankdir} || 'LR'},
			logger => $self -> logger,
			node   => '', # TODO.
		);

	# Note: $graph -> run() must be called even if $self -> tree_image_file is '',
	# so as to generate $graph -> dot_input, which is used below.

	my($cluster_name);
	my($from);
	my(%seen);
	my($to);

	for my $cluster (@{$self -> cluster_edge_set})
	{
		$cluster_name = "cluster $$cluster[0][0]";

		$graph -> push_subgraph(name => $cluster_name, graph => {label => ucfirst $cluster_name});

		for my $node (@$cluster)
		{
			$from = $$node[0];
			$to   = $$node[1];

			$graph -> add_node(name => $from, %{$nodes{$from}{attributes} });

			# Allow for the case of 1-node (isolated node) clusters.

			if (defined $to)
			{
				$seen{$from}      = {} if (! $seen{$from});
				$seen{$from}{$to} = $self -> _find_edge_attributes($from, $to) if (! $seen{$from}{$to});

				$graph -> add_node(name => $to, %{$nodes{$to}{attributes} });
				$graph -> add_edge(from => $from, to => $to, %{$seen{$from}{$to} });
			}
		}

		$graph -> pop_subgraph;
	}

	$graph -> run
	(
		format      => $self -> format,
		output_file => $self -> tree_image_file ? $self -> tree_image_file : '',
	);

	$self -> log(notice => 'Wrote ' . $self -> tree_image_file . '. Size: ' . (-s $self -> tree_image_file) . ' bytes') if ($self -> tree_image_file);

	if ($self -> tree_dot_file)
	{
		$self -> dot_input($graph -> dot_input);
		$self -> output_dot_file;
	}

} # End of output_cluster_image.

# -----------------------------------------------

sub output_dot_file
{
	my($self) = @_;

	open(OUT, '>', $self -> tree_dot_file) || die "Error: Can't open(> ", $self -> tree_dot_file, "): $!\n";
	print OUT $self -> dot_input;
	close OUT;

	$self -> log(debug => 'Wrote ' . $self -> tree_dot_file . '. Size: ' . length($self -> dot_input) . ' bytes');

} # End of output_dot_file.

# -----------------------------------------------

sub output_fixed_length_image
{
	my($self) = @_;

	if ($self -> input_file eq $self -> tree_image_file)
	{
		die "Error: Input file and tree image file have the same name. Refusing to overwrite the latter\n";
	}

	my($driver)     = $self -> driver;
	my($format)     = $self -> format;
	my($image_file) = $self -> tree_image_file;

	# This line has been copied from GraphViz2's run() method.
	# Except, that is, for the timeout, which is not used in GraphViz2 anyway.

	$self -> log(debug => "Driver: $driver. Output file: $image_file. Format: $format");

	my($stdout, $stderr);

	run3([$driver, "-T$format"], \$self -> dot_input, \$stdout, \$stderr);

	die "Error: $stderr" if ($stderr);

	$self -> dot_output($stdout);

	if ($image_file)
	{
		open(OUT, '>', $image_file) || die "Error: Can't open(> $image_file): $!";
		binmode OUT;
		print OUT $stdout;
		close OUT;

		$self -> log(notice => "Wrote $image_file. Size: " . length($stdout) . ' bytes');
	}

} # End of output_fixed_length_image.

# -----------------------------------------------
# Prepare the dot input, renumbering the nodes so dot does not coalesce the path set.

sub _prepare_fixed_length_output
{
	my($self, $title) = @_;

	# We have to rename all the nodes so they can all be included
	# in a single DOT file without dot linking them based on their names.

	my($nodes)  = $self -> parser -> nodes;
	my($new_id) = 0;

	my($name);
	my(@set);

	for my $set (@{$self -> fixed_path_set})
	{
		my(%node_set, @node_set);

		for my $node (@$set)
		{
			$name = $node -> name;

			# Allow for paths with loops, so we don't declare the same node twice.
			# Actually, I doubt Graphviz would care, since each declaration would be identical.
			# Also, later, we sort by name (i.e. $new_id) to get the order of nodes in the path.

			if (! defined($node_set{$name}) )
			{
				$node_set{$name} = {label => $name, name => ++$new_id, %{$$nodes{$name}{attributes} } };
			}

			push @node_set, $node_set{$name};
		}

		push @set, [@node_set];
	}

	# Now output the paths, using the nodes' original names as labels.

	my(%style)       = %{$self -> parser -> style};
	my($orientation) = $style{rankdir} ? $style{rankdir} : 'LR';
	my(%type)        = %{$self -> parser -> type};
	my($graph)       = qq|\tgraph [label = \"$title\" rankdir = $orientation]|;

	my(@dot_text);

	push @dot_text, ($type{strict} ? 'strict ' : '') . ($type{digraph} ? 'digraph ' : 'graph ') . $type{graph_id}, '{', $graph, '';

	# Firstly, declare all nodes.

	my($s);

	for my $set (@set)
	{
		for my $node (@$set)
		{
			push @dot_text, qq|\t\"$$node{name}\" [| . join(' ', map{qq|$_ = \"$$node{$_}\"|} sort keys %$node) . ']';
		}
	}

	# Secondly, declare all edges.

	my($edge) = $type{digraph} ? ' -> ' : ' -- ';

	for my $set (@set)
	{
			push @dot_text, "\t" . join($edge, map{'"' . $$_{name} . '"'} @$set) . ";";
	}

	push @dot_text, '}', '';
	$self -> dot_input(join("\n", @dot_text) );

} # End of _prepare_fixed_length_output.

# -----------------------------------------------

sub report_cluster_members
{
	my($self)  = @_;
	my($count) = 0;
	my($set)   = $self -> cluster_set;

	$self -> log(notice => 'Clusters:');

	for my $cluster (@$set)
	{
		$count++;

		$self -> log(notice => "$count: " . join(', ', sort $cluster -> members) );
	}

} # End of report_cluster_members.

# -----------------------------------------------

sub report_fixed_length_paths
{
	my($self, $title) = @_;
	$title            =~ s/\\n/. /g;

	$self -> log(notice => "$title:");

	for my $candidate (@{$self -> fixed_path_set})
	{
		$self -> log(notice => join(' -> ', map{$_ -> name} @$candidate) );
	}

} # End of report_fixed_length_paths.

# -----------------------------------------------
# Parse the input dot file and build a forest of all paths.
# Also, find the ancestor of each node in each path.

sub _set_up_forest
{
	my($self) = @_;

	# Generate the RAM-based version of the graph.

	my($result) = $self -> run;

	$self -> log(info => "Result of calling lexer and parser: $result (0 is success)");

} # End of _set_up_forest.

# -----------------------------------------------
# Eliminate solutions which share members.

sub _winnow_cluster_sets
{
	my($self, $cluster) = @_;

	my($overlap);
	my(%merge);
	my(%seen);

	for my $set_1 (keys %$cluster)
	{
		next if ($merge{$set_1});

		$overlap      = 1;
		$seen{$set_1} = 1;

		while ($overlap)
		{
			$overlap = 0;

			for my $set_2 (keys %$cluster)
			{
				next if ($merge{$set_2} || $seen{$set_2});

				# If the sets have any members in common, (size > 0), so coelesce.

				if ($$cluster{$set_1} -> intersection($$cluster{$set_2}) -> size)
				{
					$overlap       = 1;
					$merge{$set_2} = 1;

					$$cluster{$set_1} -> insert($$cluster{$set_2} -> members);
				}
			}
		}
	}

	delete $$cluster{$_} for keys %merge;

	$self -> cluster_set([values %$cluster]);

} # End of _winnow_cluster_sets.

# -----------------------------------------------
# Eliminate solutions which have (unwanted) cycles.

sub _winnow_fixed_length_paths
{
	my($self)   = @_;
	my($cycles) = $self -> allow_cycles;

	my(@solutions);

	for my $candidate (@{$self -> fixed_path_set})
	{
		# Count the number of times each node appears in this candidate path.

		my(%seen);

		$seen{$_}++ for map{$_ -> name} @$candidate;

		# Exclude nodes depending on the allow_cycles option:
		# o 0 - Do not allow any cycles.
		# o 1 - Allow any node to be included once or twice.

		if ($cycles == 0)
		{
			@$candidate = grep{$seen{$_ -> name} == 1} @$candidate;
		}
		elsif ($cycles == 1)
		{
			@$candidate = grep{$seen{$_ -> name} <= 2} @$candidate;
		}

		push @solutions, [@$candidate] if ($#$candidate == $self -> path_length);
	}

	$self -> fixed_path_set([@solutions]);

} # End of _winnow_fixed_length_paths.

# -----------------------------------------------

1;

=pod

=head1 NAME

L<GraphViz2::Marpa::PathUtils> - Provide various analyses of Graphviz dot files

=head1 SYNOPSIS

All scripts and input and output files listed here are shipped with the distro.

=head2 Finding clusters

Either pass parameters in to new():

	GraphViz2::Marpa::PathUtils -> new
	(
	    input_file      => 'data/jointed.edges.gv',
	    report_clusters => 1,
	    tree_dot_file   => 'data/clusters.gv',
	    tree_image_file => 'html/clusters.svg',
	);

Or call methods to set parameters;

	my($parser) = GraphViz2::Marpa::PathUtils -> new;

	$parser -> input_file('data/jointed.edges.gv');
	$parser -> report_clusters(1);
	$parser -> tree_dot_file('data/fixed.length.paths.gv');
	$parser -> tree_image_file('html/fixed.length.paths.sgv');

And then:

	$parser -> find_clusters;

Command line usage:

	shell> perl scripts/find.clusters.pl -h

Or see scripts/find.clusters.sh, which hard-codes my test data values.

=head2 Finding fixed length paths

Either pass parameters in to new():

	GraphViz2::Marpa::PathUtils -> new
	(
	    allow_cycles    => 1,
	    input_file      => 'data/90.KW91.gv',
	    path_length     => 4,
	    report_paths    => 1,
	    start_node      => 'Act_1',
	    tree_dot_file   => 'data/fixed.length.paths.gv',
	    tree_image_file => 'html/fixed.length.paths.svg',
	) -> find_fixed_length_paths;

Or call methods to set parameters;

	my($parser) = GraphViz2::Marpa::PathUtils -> new;

	$parser -> allow_cycles(1);
	$parser -> input_file('data/90.KW91.gv');
	$parser -> path_length(4);
	$parser -> report_paths(1);
	$parser -> start_node('Act_1');
	$parser -> tree_dot_file('data/fixed.length.paths.gv');
	$parser -> tree_image_file('html/fixed.length.paths.sgv');

And then:

	$parser -> find_fixed_length_paths;

Command line usage:

	shell> perl scripts/find.fixed.length.paths.pl -h

Or see scripts/fixed.length.paths.sh, which hard-codes my test data values.

=head1 DESCRIPTION

GraphViz2::Marpa::PathUtils parses L<Graphviz|http://www.graphviz.org/> dot files and processes the output in various ways.

This class is a descendent of L<GraphViz2::Marpa>, and hence inherits all its keys to new(), and all its methods.

Currently, the only feature available is to find all paths of a given length starting from a given node.

Sample output: L<http://savage.net.au/Perl-modules/html/graphviz2.pathutils/index.html>.

=head1 Scripts shipped with this distro

All scripts are in the scripts/ directory. This means they do I<not> get installed along with the package.

Data files are in data/, while html and svg files are in html/.

=over 4

=item o copy.config.pl

During installation, this copies config/.htgraphviz2.marpa.pathutils.conf to a dir as discussed under
L</The Configuration File>.

=item o find.clusters.pl

This runs the L</find_clusters()> method in GraphViz2::Marpa::PathUtils.

=item o find.clusters.sh

This runs find.clusters.pl with hard-coded parameters, and is what I use for testing new code.

Then it runs generate.demo.pl.

Lastly it copies the output to my web server's dir, $DR/Perl-modules/html/graphviz2.pathutils/.

=item o find.fixed.length.paths.pl

This runs the L</find_fixed_length_paths()> method in GraphViz2::Marpa::PathUtils.

Try shell> perl find.fixed.length.paths.pl -h

=item o find.fixed.length.paths.sh

This runs find.fixed.length.paths.pl with hard-coded parameters, and is what I use for testing new code.

Then it runs generate.demo.pl.

Lastly it copies the output to my web server's doc dir, $DR/Perl-modules/html/graphviz2.pathutils/.

=item o generate.demo.pl

This uses the L<Text::Xslate> template file htdocs/assets/templates/graphviz2/marpa/pathutils/pathutils.tx
to generate html/index.html.

=item o generate.demo.sh

Runs generate.demo.pl and then copies html/*.html and html/*.svg to my web server's dir, $DR/Perl-modules/html/graphviz2.pathutils/.

=item o pod2html.sh

Converts all *.pm files to *.html, and copies them in my web server's dir structure.

=item o test.set.tiny.pl

Check that L<Set::Tiny>'s is_subset() and is_proper_subset() behave as expected.

=back

See also t/test.all.t.

=head1 Distributions

This module is available as a Unix-style distro (*.tgz).

See L<http://savage.net.au/Perl-modules/html/installing-a-module.html>
for help on unpacking and installing distros.

=head1 Installation

=head2 The Module Itself

Install L<GraphViz2::Marpa::PathUtils> as you would for any C<Perl> module:

Run:

	cpanm GraphViz2::Marpa::PathUtils

or run:

	sudo cpan GraphViz2::Marpa::PathUtils

or unpack the distro, and then either:

	perl Build.PL
	./Build
	./Build test
	sudo ./Build install

or:

	perl Makefile.PL
	make (or dmake or nmake)
	make test
	make install

=head2 The Configuration File

All that remains is to tell L<GraphViz2::Marpa::PathUtils> your values for some options.

For that, see config/.htgraphviz2.marpa.pathutils.conf.

If you are using Build.PL, running Build (without parameters) will run scripts/copy.config.pl,
as explained next.

If you are using Makefile.PL, running make (without parameters) will also run scripts/copy.config.pl.

Either way, before editing the config file, ensure you run scripts/copy.config.pl. It will copy
the config file using L<File::HomeDir>, to a directory where the run-time code in
L<GraphViz2::Marpa::PathUtils> will look for it.

	shell>cd GraphViz2-Marpa-PathUtils-1.00
	shell>perl scripts/copy.config.pl

Under Debian, this directory will be $HOME/.perl/GraphViz2-Marpa-PathUtils/. When you
run copy.config.pl, it will report where it has copied the config file to.

Check the docs for L<File::HomeDir> to see what your operating system returns for a
call to my_dist_config().

The point of this is that after the module is installed, the config file will be
easily accessible and editable without needing permission to write to the directory
structure in which modules are stored.

That's why L<File::HomeDir> and L<Path::Class> are pre-requisites for this module.

Although this is a good mechanism for modules which ship with their own config files, be advised that some
CPAN tester machines run tests as users who don't have home directories, resulting in test failures.

=head1 Constructor and Initialization

=head2 Calling new()

C<new()> is called as C<< my($obj) = GraphViz2::Marpa::PathUtils -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<GraphViz2::Marpa::PathUtils>.

This class is a descendent of L<GraphViz2::Marpa>, and hence inherits all its keys to new(), and all its methods.

Further, these key-value pairs are accepted in the parameter list (see corresponding methods for details
[e.g. L</path_length($integer)>]):

=over 4

=item o allow_cycles => $integer

Specify whether or not cycles are allowed in the paths found.

Values for $integer:

=over 4

=item o 0 - Do not allow any cycles

This is the default.

=item o 1 - Allow any node to be included once or twice.

=back

Default: 0.

This option is only used when calling L</find_fixed_length_paths()>.

=item o driver => thePathToDot

Specify the OS's path to the I<dot> program, to override the default.

Default: Use which('dot'), via the module L<File::Which>, to find the I<dot> executable.

=item o format => $aDOTOutputImageFormat

Specify the image type to pass to I<dot>, as the value of dot's -T option.

Default: 'svg'.

=item o path_length => $integer

Specify the length of all paths to be included in the output.

Here, length means the number of edges between nodes.

Default: 0.

This parameter is mandatory, and must be > 0.

This option is only used when calling L</find_fixed_length_paths()>.

=item o report_clusters => $Boolean

Specify whether or not to print a report of the clusters found.

Default: 0 (do not print).

This option is only used when calling L</find_clusters()>.

=item o report_paths => $Boolean

Specify whether or not to print a report of the paths found.

Default: 0 (do not print).

This option is only used when calling L</find_fixed_length_paths()>.

=item o start_node => $theNameOfANode

Specify the name of the node where all paths must start from.

Default: ''.

This parameter is mandatory.

The name can be the empty string, but must not be undef.

This option is only used when calling L</find_fixed_length_paths()>.

=item o tree_dot_file => aDOTInputFileName

Specify the name of a file to write which will contain the DOT description of the image of all solutions.

Default: ''.

This file is not written if the value is ''.

=item o tree_image_file => aDOTOutputFileName

Specify the name of a file to write which will contain the output of running I<dot>.

The value of the I<format> option determines what sort of image is created.

Default: ''.

This file is not written if the value is ''.

=back

=head1 Methods

This class is a descendent of L<GraphViz2::Marpa>, and hence inherits all its methods.

Further, these methods are implemented.

=head2 allow_cycles([$integer])

Here the [] indicate an optional parameter.

Get or set the value determining whether or not cycles are allowed in the paths found.

'allow_cycles' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 cluster_edge_set()

Returns an arrayref of clusters, where each element is an arrayref.

Within the inner arrayrefs, each element is a 2-element arrayref. If the 2nd element is defined,
the 2 elements are the ends of an edge. If the 2nd element is not defined, the 1st element is the
name of a node which is the only node in the set.

See the source code for L</output_cluster_image()> for sample code.

=head2 cluster_set()

Returns an arrayref of clusters, where each element is an object of type L<Set::Tiny>.
The members of each set are the stringified I<names> of the members of the clusters.

See the source code of L</report_cluster_members()> for sample usage.

=head2 dot_input()

Returns the string which will be input to the I<dot> program.

=head2 dot_output()

Returns the string which has been output by the I<dot> program.

=head2 driver([$pathToDot])

Here the [] indicate an optional parameter.

Get or set the OS's path to the I<dot> program.

=head2 find_clusters()

This is one of the methods which does all the work, and hence must be called.
The other is L</find_fixed_length_paths()>.

See the L</Synopsis> and scripts/find.clusters.pl.

Returns 0 for success and 1 for failure.

=head2 find_fixed_length_paths()

This is one of the methods which does all the work, and hence must be called.
The other is L</find_clusters()>.

See the L</Synopsis> and scripts/find.fixed.length.paths.pl.

Returns 0 for success and 1 for failure.

=head2 fixed_path_set()

Returns the arrayref of paths found. Each element is 1 path, and paths are stored as an arrayref of
objects of type L<Tree>.

See the source code of sub L</report_fixed_length_paths()> for sample usage.

=head2 format([$string])

Here the [] indicate an optional parameter.

Get or set the type of image to be output when running I<dot>.

'format' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head2 output_cluster_image()

This writes the clusters found, as a DOT output file, as long as new(tree_image_file => $name) was specified.

=head2 output_fixed_length_image($title)

This writes the paths found, as a DOT output file, as long as new(tree_image_file => $name) was specified,
or if tree_image_file($name) was called before L</find_fixed_length_paths()> was called.

=head2 output_dot_file($title)

This writes the DOT file generated, as long as new(tree_dot_file => $name) was specified.

=head2 path_length([$integer])

Here the [] indicate an optional parameter.

Get or set the length of the paths to be searched for.

'path_length' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 report_cluster_members()

This prints the clusters found, if new() was called as new(report_clusters => 1).

=head2 report_clusters([$Boolean])

Here the [] indicate an optional parameter.

Get or set the option which determines whether or not the clusters found are printed.

'report_clusters' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 report_fixed_length_paths()

This prints the fixed length paths found, if new() was called as new(report_paths => 1).

=head2 report_paths([$Boolean])

Here the [] indicate an optional parameter.

Get or set the option which determines whether or not the fixed length paths found are printed.

'report_paths' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 start_node([$string])

Here the [] indicate an optional parameter.

Get or set the name of the node from where all paths must start.

'start_node' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 tree_dot_file([$name])

Here the [] indicate an optional parameter.

Get or set the name of the I<dot> input file to write.

'tree_dot_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head2 tree_image_file([$name])

Here the [] indicate an optional parameter.

Get or set the name of the I<dot> output file to write.

The type of image comes from the I<format> parameter to new().

'tree_image_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

=head1 FAQ

=head2 How are clusters named?

The names of the nodes in each cluster are sorted, and the first is arbitrarily chosen as the name of the cluster.

=head2 Sometimes the cluster has the wrong shape for a node

Correct. The code does not handle a file such as:

	digraph X
	{
	    node [shape = Mdiamond]
	    node_1
	    node [shape = Msquare]
	    node_2
	}

The output uses the last node shape found by the parser, Msquare, for all nodes which don't have a shape
specified explicitly.

See data/03.clusters.in.gv for an instance. See also L</TODO>.

=head2 In clusters, edge attributes in the input file are ignored

Correct. The code does not implement the complexity required to handle this yet. See also L</TODO>.

=head2 How are cycles in fixed path length analysis handled?

This is controlled by the I<allow_cycles> option to new(), or the corresponding method L</allow_cycles($integer)>.

The code keeps track of the number of times each node is entered. If new(allow_cycles => 0) was called,
nodes are only considered if they are entered once. If new(allow_cycles => 1) was called, nodes are also
considered if they are entered a second time.

Sample code: Using the input file data/90.KW91.lex (see scripts/fixed.length.paths.sh) we can specify
various combinations of parameters like this:

	allow_cycles  path_length  start node  solutions
	0             3            Act_1       9
	1             3            Act_1       22

	0             4            Act_1       12
	1             4            Act_1       53

=head2 Are all (fixed length) paths found unique?

Yes, as long as they are unique in the input. Something like this produces 8 identical solutions
(starting from A, of path length 3) because each node B, C, D, can be entered in 2 different ways,
and 2**3 = 8.

	digraph G
	{
	    A -> B -> C -> D;
	    A -> B -> C -> D;
	}

See data/01.non.unique.gv and html/01.non.unique.svg.

=head2 The number of options is confusing!

Agreed. Remember that this code calls L<GraphViz2::Marpa>'s run() method, which expects a large number of
options because it calls both the lexer and the parser.

=head2 Isn't your code at risk from the 'combinatorial explosion' problem?

Yes. The code does limit the number of possibilies as quickly as possible, but of course there will always be
graphs which can't be processed by this module.

Such graphs are deemed to be pathological.

=head2 Why do I get error messages like the following?

	Error: <stdin>:1: syntax error near line 1
	context: digraph >>>  Graph <<<  {

Graphviz reserves some words as keywords, meaning they can't be used as an ID, e.g. for the name of the graph.

The keywords are: I<digraph>, I<edge>, I<graph>, I<node>, I<strict> and I<subgraph>.
Compass points are not keywords.

See L<keywords|http://www.graphviz.org/content/dot-language> in the discussion of the syntax of DOT
for details.

So, don't do this:

	strict graph graph{...}
	strict graph Graph{...}
	strict graph strict{...}
	etc...

Likewise for non-strict graphs, and digraphs. You can however add double-quotes around such reserved words:

	strict graph "graph"{...}

Even better, use a more meaningful name for your graph...

=head2 How does the code handle ports attached to nodes?

So far, the code has not been tested on graphs which use ports.

=head1 Reference

Combinatorial Algorithms for Computers and Calculators, A Nijenhuis and H Wilf, p 240.

This books very clearly explains the backtracking parser I used to process the combinations of nodes found
at each point along each path. Source code in the book is in Fortran.

The book is now downloadable as a PDF from L<http://www.math.upenn.edu/~wilf/website/CombAlgDownld.html>.

=head1 TODO

=over 4

=item o High priority - Handle ports

=item o High priority - Handle edge attributes

=item o Low priority - Handle multiple class definitions which change nodes' shapes

See the L<FAQ/Sometimes the cluster has the wrong shape for a node> above for sample code.

=item o Low priority - Perhaps implement logic to find paths which end on a given node

=back

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
