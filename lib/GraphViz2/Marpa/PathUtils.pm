package GraphViz2::Marpa::PathUtils;

use parent 'GraphViz2::Marpa';
use strict;
use warnings;
use warnings qw(FATAL utf8);    # Fatalize encoding glitches.
use open     qw(:std :utf8);    # Undeclared streams in UTF-8.

use File::Basename; # For basename().

use GraphViz2::Marpa::Renderer::Graphviz;

use Moo;

use Set::Tiny;

use Types::Standard qw/ArrayRef Bool HashRef Int Str/;

has allow_cycles =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Bool,
	required => 0,
);

has cluster_sets =>
(
	default  => sub{return {} },
	is       => 'rw',
	isa      => HashRef,
	required => 0,
);

has dot_input =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has fixed_path_set =>
(
	default  => sub{return []},
	is       => 'rw',
	isa      => ArrayRef,
	required => 0,
);

has cluster_trees =>
(
	default  => sub{return {} },
	is       => 'rw',
	isa      => HashRef,
	required => 0,
);

has output_dot_file =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

has path_length =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Int,
	required => 0,
);

has report_clusters =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Bool,
	required => 0,
);

has report_paths =>
(
	default  => sub{return 0},
	is       => 'rw',
	isa      => Bool,
	required => 0,
);

has start_node =>
(
	default  => sub{return ''},
	is       => 'rw',
	isa      => Str,
	required => 0,
);

our $VERSION = '2.00';

# -----------------------------------------------

sub _coelesce_cluster_membership
{
	my($self, $count, $node, $clusters, $subgraph) = @_;

	# Some nodes may be heads but never tails, so they won't exist in this hash.

	return if (! defined $$clusters{$node});

	for my $member ($$clusters{$node} -> members)
	{
		next if ($$subgraph{$count} -> member($member) );

		$$subgraph{$count} -> insert($member);
		$self -> _coelesce_cluster_membership($count, $member, $clusters, $subgraph);
	}

} # End of _coelesce_cluster_membership.

# -----------------------------------------------

sub _coelesce_cluster_sets
{
	my($self, $clusters) = @_;

	# We take a copy of the keys so that we can delete hash keys
	# in arbitrary order while processing the sets which are the values.

	my(@nodes) = keys %$clusters;
	my($count) = 0;

	my(%subgraphs);

	for my $node (@nodes)
	{
		$count++;

		$subgraphs{$count} = Set::Tiny -> new;

		$subgraphs{$count} -> insert($node);
		$self -> _coelesce_cluster_membership($count, $node, $clusters, \%subgraphs);
	}

	return \%subgraphs;

} # End of _coelesce_cluster_sets.

# -----------------------------------------------

sub _find_cluster_containing_start_node
{
	my($self) = @_;
	my($sets) = $self -> cluster_sets;

	my(%new_trees);

	for my $id (keys %$sets)
	{
		next if (! $$sets{$id} -> member($self -> start_node) );

		$new_trees{$id} = $self -> _find_clusters_trees([$$sets{$id} -> members]);
	}

	# There should be just 1 tree containing the start node.

	my(@ids) = keys %new_trees;

	die "Error: @{[$#ids + 1]} trees containg the start node. There should be exactly 1\n" if ($#ids != 0);

	return $new_trees{$ids[0]};

} # End of _find_cluster_containing_start_node.

# -----------------------------------------------

sub _find_cluster_mothers_with_edges
{
	my($self) = @_;

	my($attributes);
	my($name);
	my(%seen);
	my($uid);

	$self -> tree -> walk_down
	({
		callback => sub
		{
			my($node) = @_;
			$name     = $node -> name;

			# Ignore non-edges.

			return 1 if ($name ne 'edge_id'); # Keep walking.

			$attributes = $node -> mother -> attributes;
			$uid        = $$attributes{uid};

			# Ignore if this mother has been seen.

			return 1 if ($seen{$uid}); # Keep walking.

			# Save mother's details.

			$seen{$uid} = $node -> mother;

			return 1; # Keep walking.
		},
		_depth => 0,
	});

	return \%seen;

} # End of _find_cluster_mothers_with_edges.

# -----------------------------------------------

sub _find_cluster_reachable_nodes
{
	my($self, $edgy) = @_;

	my(%clusters);
	my(@daughters);
	my(%node, $name);
	my($value);

	# This loop is outwardly the same as the one in _find_clusters_trees().

	for my $mother_uid (sort keys %$edgy)
	{
		@daughters = $$edgy{$mother_uid} -> daughters;

		for my $index (0 .. $#daughters)
		{
			$name = $daughters[$index] -> name;

			# Ignore non-edges, since we are checking for connectivity.

			next if ($name ne 'edge_id');

			# These offsets are only valid if the DOT file is valid.

			$node{tail} =
			{
				name  => $daughters[$index - 1] -> name,
				value => ${$daughters[$index - 1] -> attributes}{value},
			};
			$node{head} =
			{
				name  => $daughters[$index + 1] -> name,
				value => ${$daughters[$index + 1] -> attributes}{value},
			};

			# Cases to handle (see data/path.set.01.in.gv):
			# o These edge combinations:
			# o a -> b.
			# o c -> { d e }.
			# o { f g } -> h.
			# o { i j } -> { k l }.

			if ($node{tail}{name} eq 'node_id')
			{
				if ($node{head}{name} eq 'node_id')
				{
					# Case: a -> b.
					# So now we add 'b' to the set of all nodes reachable from 'a'.
					# The point is that, if 'a' appears anywhere else in the graph, then all
					# nodes connected (via edges) to that other copy of 'a' are also connected to 'b'.
					# Likewise for 'a'.
					# The reason for doing both 'a' and 'b' is that either one may appear elsewhere
					# in the graph, but we don't know which one, if either, does.

					for my $n (qw/tail head/)
					{
						$value            = $node{$n}{value};
						$clusters{$value} ||= Set::Tiny -> new;

						$clusters{$value} -> insert($n eq 'tail' ? $node{head}{value} : $node{tail}{value});
					}
				}
				else
				{
					# Case: c -> { d e }.
					# From 'c', every node in the subgraph can be reached.
					# Start at $index + 1, which is the '{'.

					$self -> _find_cluster_reachable_subgraph_1(\%clusters, \%node, $daughters[$index + 1]);
				}
			}
			else
			{
				if ($node{head}{name} eq 'node_id')
				{
					# Case: { f g } -> h.
					# Every node in the subgraph can reach 'h'.
					# We use $index - 2 ('{') since we know $index - 1 is '}'.

					$self -> _find_cluster_reachable_subgraph_2(\%clusters, \%node, $daughters[$index - 2]);
				}
				else
				{
					# Case: { i j } -> { k l }.
					# Every node in the 1st subgraph can reach every node in the 2nd.
					# Start at $index + 1 in order to skip the '{'.
					# We use $index - 2 ('{') since we know $index - 1 is '}'.

					$self -> _find_cluster_reachable_subgraph_3(\%clusters, \%node, $daughters[$index - 2], $daughters[$index + 1]);
				}
			}
		}
	}

	return \%clusters;

} # End of _find_cluster_reachable_nodes.

# -----------------------------------------------

sub _find_cluster_reachable_subgraph_1
{
	my($self, $clusters, $node, $head_subgraph) = @_;
	my($real_tail) = $$node{tail}{value};
	my(@daughters) = $head_subgraph -> daughters;

	my($attributes);
	my($node_name);
	my($real_head);

	for my $i (0 .. $#daughters)
	{
		$node_name  = $daughters[$i] -> name;
		$attributes = $daughters[$i] -> attributes;

		# Ignore non-nodes within the head subgraph.

		next if ($node_name ne 'node_id');

		# Stockpile all nodes within the head subgraph.

		$real_head             = $$attributes{value};
		$$clusters{$real_tail} ||= Set::Tiny -> new;

		$$clusters{$real_tail} -> insert($real_head);

		$$clusters{$real_head} ||= Set::Tiny -> new;

		$$clusters{$real_head} -> insert($real_tail);
	}

} # End of _find_cluster_reachable_subgraph_1.

# -----------------------------------------------

sub _find_cluster_reachable_subgraph_2
{
	my($self, $clusters, $node, $tail_subgraph) = @_;
	my($real_head) = $$node{head}{value};
	my(@daughters) = $tail_subgraph -> daughters;

	my($attributes);
	my($node_name);
	my($real_tail);

	for my $i (0 .. $#daughters)
	{
		$node_name  = $daughters[$i] -> name;
		$attributes = $daughters[$i] -> attributes;

		# Ignore non-nodes within tail subgraph.

		next if ($node_name ne 'node_id');

		# Stockpile all nodes within the tail subgraph.

		$real_tail             = $$attributes{value};
		$$clusters{$real_head} ||= Set::Tiny -> new;

		$$clusters{$real_head} -> insert($real_tail);

		$$clusters{$real_tail} ||= Set::Tiny -> new;

		$$clusters{$real_tail} -> insert($real_head);
	}

} # End of _find_cluster_reachable_subgraph_2.

# -----------------------------------------------

sub _find_cluster_reachable_subgraph_3
{
	my($self, $clusters, $node, $tail_subgraph, $head_subgraph) = @_;
	my(@tail_daughters) = $tail_subgraph -> daughters;
	my(@head_daughters) = $head_subgraph -> daughters;

	my($attributes);
	my($node_name);
	my($real_tail, $real_head);

	for my $i (0 .. $#tail_daughters)
	{
		$node_name  = $tail_daughters[$i] -> name;
		$attributes = $tail_daughters[$i] -> attributes;

		# Ignore non-nodes within tail subgraph.

		next if ($node_name ne 'node_id');

		# Stockpile all nodes within the tail subgraph.

		$real_tail = $$attributes{value};

		for my $j (0 .. $#head_daughters)
		{
			$node_name  = $head_daughters[$j] -> name;
			$attributes = $head_daughters[$j] -> attributes;

			# Ignore non-nodes within head subgraph.

			next if ($node_name ne 'node_id');

			$real_head             = $$attributes{value};
			$$clusters{$real_head} ||= Set::Tiny -> new;

			$$clusters{$real_head} -> insert($real_tail);

			$$clusters{$real_tail} ||= Set::Tiny -> new;

			$$clusters{$real_tail} -> insert($real_head);
		}
	}

} # End of _find_cluster_reachable_subgraph_3.

# -----------------------------------------------

sub _find_cluster_standalone_nodes
{
	my($self, $subgraphs) = @_;

	# We need to find the # of subgraphs, since stand-alone clusters
	# will be numbered starting 1 after the highest set # so far.

	my(@keys)  = sort keys %$subgraphs;
	my($count) = $#keys < 0 ? 0 : $keys[$#keys];

	# Get the names of all members of all subgraphs.
	# Any nodes found by walk_down(), which are not in this collection,
	# are standalone nodes.

	my(@members);
	my(%seen);

	for my $id (keys %$subgraphs)
	{
		@members        = $$subgraphs{$id} -> members;
		@seen{@members} = (1) x @members;
	}

	my($attributes);
	my($name);
	my($value);

	$self -> tree -> walk_down
	({
		callback => sub
		{
			my($node)   = @_;
			$name       = $node -> name;
			$attributes = $node -> attributes;
			$value      = $$attributes{value};

			# Ignore non-nodes and nodes which have been seen.

			return 1 if ( ($name ne 'node_id') || defined $seen{$value}); # Keep walking.

			$count++;

			$$subgraphs{$count} = Set::Tiny -> new;

			$$subgraphs{$count} -> insert($value);

			return 1; # Keep walking.
		},
		_depth => 0,
	});

	$self -> cluster_sets($subgraphs);

} # End of _find_cluster_standalone_nodes.

# -----------------------------------------------
# Called by the user.

sub find_clusters
{
	my($self)          = @_;
	my($subgraph_sets) = $self -> _preprocess;

	# Find nodes which do not participate in paths,
	# i.e. there are no edges leading to them or from them.
	# Such nodes are stand-alone clusters.

	$self -> _find_cluster_standalone_nodes($subgraph_sets);
	$self -> report_cluster_members if ($self -> report_clusters);
	$self -> _generate_tree_per_cluster;
	$self -> output_clusters if ($self -> output_dot_file);

	# Return 0 for success and 1 for failure.

	return 0;

} # End of find_clusters.

# -----------------------------------------------

sub _find_clusters_trees
{
	my($self, $members) = @_;

	# We use a copy of the tree so the user of this module can still get access to the whole tree
	# after each cluster cuts out the unwanted nodes (i.e. those nodes in all other clusters).
	# Further, this means the copy has all the node and edge attributes for the wanted nodes.
	#
	# We will excise all nodes which are not members, and also excise any subgraphs consisting
	# entirely of unwanted nodes. Also, we'll excise all edges involving the unwanted nodes.
	# Note: The Tree::DAG_Node docs warn against editing the tree during a walk_down(), so we
	# stockpile mothers whose daughters need to be processed after walk_down() returns.

	my($cluster) = $self -> tree -> copy_tree;

	my(%wanted);

	@wanted{@$members} = (1) x @$members;

	my($attributes);
	my($name);
	my(%seen);
	my($uid);
	my($value);

	$cluster -> walk_down
	({
		callback => sub
		{
			my($node)   = @_;
			$name       = $node -> name;
			$attributes = $node -> attributes;
			$uid        = $$attributes{uid};
			$value      = $$attributes{value};

			# Check for unwanted nodes.

			if ( ($name eq 'node_id') && ! $wanted{$value})
			{
				$seen{$uid} = $node -> mother;
			}

			return 1; # Keep walking.
		},
		_depth => 0,
	});

	# This loop is outwardly the same as the one in _find_reachable_nodes().

	my($candidate_uid);
	my(@daughters);
	my($limit);
	my($mother);

	for my $unwanted_uid (keys %seen)
	{
		$mother    = $seen{$unwanted_uid};
		@daughters = $mother -> daughters;
		$limit     = $#daughters;

		# Cases to handle (see data/path.set.01.in.gv):
		# o Delete the node, and edges in these combinations:
		# o a -> b.
		# o c -> { d e }.
		# o { f g } -> h.
		# o { i j } -> { k l } (Impossible, given the tree node is a 'node_id').

		for my $i (0 .. $limit)
		{
			$candidate_uid = ${$daughters[$i] -> attributes}{uid};

			next if ($unwanted_uid != $candidate_uid);

			$daughters[$i] -> unlink_from_mother;

			for my $offset ($i - 1, $i + 1)
			{
				next if ( ($offset < 0) || ($offset > $limit) );

				$name = $daughters[$offset] -> name;

				$daughters[$offset] -> unlink_from_mother if ($name eq 'edge_id');
			}
		}
	}

	return $cluster;

} # End of _find_clusters_trees.

# -----------------------------------------------
# Find N candidates for the next node along the path.

sub _find_fixed_length_candidates
{
	my($self, $tree, $solution, $stack) = @_;
	my($current_node) = $$solution[$#$solution];

	# Add the node's parent, if it's not the root.
	# Then add the node's children.

	my(@daughters);
	my($i);
	my($name, @neighbours);

	$tree -> walk_down
	({
		callback =>
		sub
		{
			my($node) = @_;
			$name     = $node -> name;

			# We only want neighbours of the current node.

			return 1 if (${$node -> attributes}{value} ne ${$current_node -> attributes}{value}); # Keep walking.

			# Now find its neighbours. These are sisters separated by an edge.

			@daughters = $node -> mother -> daughters;
			$i         = $node -> my_daughter_index;

			# TODO [$i + 2] could be a subgraph.

			if ( ( ($i + 2) <= $#daughters) && ($daughters[$i + 1] -> name eq 'edge_id') )
			{
				push @neighbours, $daughters[$i + 2];
			}

			return 1; # Keep walking.
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
	my($self, $tree, $start) = @_;

	my(@all_solutions);
	my($count, $candidate);
	my(@one_solution);
	my(@stack);

	# Push the first copy of the start node, and its count (1), onto the stack.

	push @stack, $$start[0], 1;

	# Process these N candidates 1-by-1.
	# The top-of-stack is a candidate count.

	while ($#stack >= 0)
	{
		while ($stack[$#stack] > 0)
		{
			($count, $candidate) = (pop @stack, pop @stack);

			push @stack, $count - 1;
			push @one_solution, $candidate;

			# Does this candidate suit the solution so far?

			if ($#one_solution == $self -> path_length)
			{
				# Yes. Save this solution.

				push @all_solutions, [@one_solution];

				# Discard this candidate, and try another.

				pop @one_solution;
			}
			else
			{
				# No. The solution is still too short.
				# Push N more candidates onto the stack.

				$self -> _find_fixed_length_candidates($tree, \@one_solution, \@stack);
			}
		}

		# Pop the candidate count (0) off the stack.

		pop @stack;

		# Remaining candidates, if any, must be contending for the 2nd last slot.
		# So, pop off the node in the last slot, since we've finished
		# processing all candidates for that slot.
		# Then, backtrack to test the next set of candidates for what,
		# after this pop, will be the new last slot.

		pop @one_solution;
	}

	$self -> fixed_path_set([@all_solutions]);

} # End of _find_fixed_length_path_set.

# -----------------------------------------------
# Find all paths starting from any copy of the target start_node.

sub _find_fixed_length_paths
{
	my($self, $tree) = @_;

	# Phase 1: Find all copies of the start node.

	my($attributes);
	my(@daughters);
	my($index);
	my($name);
	my(@start);
	my($uid);
	my($value);

	$tree -> walk_down
	({
		callback =>
		sub
		{
			my($node) = @_;
			$name     = $node -> name;

			# Skip the special tree nodes.

			return 1 if ($name =~ /(?:graph|prolog|root)/); # Keep walking.

			# Skip the tree nodes with names other than the start node.

			$attributes = $node -> attributes;
			$value      = $$attributes{value};

			return 1 if ($value ne $self -> start_node); # Keep walking.

			# Skip the tree nodes which are not on a path.

			$index     = $node -> my_daughter_index;
			@daughters = $node -> mother -> daughters;

			return 1 if ( ($index == $#daughters) || ($daughters[$index + 1] -> name ne 'edge_id') ); # Keep walking.

			$uid = $$attributes{uid};

			push @start, $node;

			return 1; # Keep walking.
		},
		_depth => 0,
	});

	# Give up if the given node was not found.

	die 'Error: Start node (', $self -> start_node, ") not found\n" if ($#start < 0);

	# Phase 2: Process each copy of the start node.

	$self -> _find_fixed_length_path_set($tree, \@start);

} # End of _find_fixed_length_paths.

# -----------------------------------------------
# Called by the user.

sub find_fixed_length_paths
{
	my($self) = @_;

	die "Error: No start node specified\n" if (length($self -> start_node) == 0);
	die "Error: Path length must be > 0\n" if ($self -> path_length <= 0);

	$self -> cluster_sets($self -> _preprocess);

	my($tree) = $self -> _find_cluster_containing_start_node;

	$self -> _find_fixed_length_paths($tree);
	$self -> _winnow_fixed_length_paths;

	$self -> log(notice => 'Raw name:  ' . $self -> input_file);
	$self -> log(notice => 'Base name: ' . basename($self -> input_file) );

	my($title) = 'Input file: ' . basename($self -> input_file) . "\\n" .
		'Starting node: ' . $self -> start_node . "\\n" .
		'Path length: ' . $self -> path_length . "\\n" .
		'Allow cycles: ' . $self -> allow_cycles . "\\n" .
		'Paths: ' . scalar @{$self -> fixed_path_set};

	$self -> report_fixed_length_paths($title)      if ($self -> report_paths);
	$self -> _output_fixed_length_gv($tree, $title) if ($self -> output_dot_file);

	# Return 0 for success and 1 for failure.

	return 0;

} # End of find_fixed_length_paths.

# -----------------------------------------------

sub _generate_tree_per_cluster
{
	my($self) = @_;
	my($sets) = $self -> cluster_sets;

	my(%new_clusters);

	for my $id (sort keys %$sets)
	{
		$self -> log(debug => "Tree for cluster $id:");

		$new_clusters{$id} = $self -> _find_clusters_trees([$$sets{$id} -> members]);

		$self -> log(debug => join("\n", @{$new_clusters{$id} -> tree2string}) );

		# TODO: This is commented out because I'm not ready to handle nested subgraphs.

		#$self -> _winnow_cluster_tree($new_clusters{$id});
		#$self -> log(debug => join("\n", @{$new_clusters{$id} -> tree2string}) );
	}

	$self -> cluster_trees(\%new_clusters);

} # End of _generate_tree_per_cluster.

# -----------------------------------------------

sub output_clusters
{
	my($self)   = @_;
	my($sets)   = $self -> cluster_trees;
	my($prefix) = $self -> output_dot_file;

	my($file_name);
	my($renderer);

	for my $id (sort keys %$sets)
	{
		$file_name = sprintf('%s.%03i.gv', $prefix, $id);

		GraphViz2::Marpa::Renderer::Graphviz -> new
		(
			logger      => $self -> logger,
			output_file => $file_name,
			tree        => $$sets{$id},
		) -> run;

		$self -> log(info => "Wrote cluster $id to $file_name");
	}

} # End of output_clusters.

# -----------------------------------------------
# Prepare the dot input, renumbering the nodes so dot does not coalesce the path set.

sub _output_fixed_length_gv
{
	my($self, $tree, $title) = @_;

	# We have to rename all the nodes so they can all be included
	# in a single DOT file without dot linking them based on their names.

	my($new_id) = 0;

	my($attributes);
	my(@set);
	my($value);

	for my $set (@{$self -> fixed_path_set})
	{
		my(%node_set, @node_set);

		for my $node (@$set)
		{
			$attributes = $node -> attributes;
			$value      = $$attributes{value};

			# Allow for paths with loops, so we don't declare the same node twice.
			# Actually, I doubt Graphviz would care, since each declaration would be identical.
			# Also, later, we sort by name (i.e. $new_id) to get the order of nodes in the path.

			if (! defined($node_set{$value}) )
			{
				$node_set{$value} = {label => $value, name => ++$new_id};
			}

			push @node_set, $node_set{$value};
		}

		push @set, [@node_set];
	}

	# Was the original graph strict or not, and a digraph or not?
	# So we examine the daughters of the prolog tree node.

	my($strict)  = '';
	my($digraph) = 'graph';

	for my $node ( ($tree -> daughters)[0] -> daughters)
	{
		$value   = ${$node -> attributes}{value};
		$strict  = 'strict ' if ($value eq 'strict');
		$digraph = 'digraph' if ($value eq 'digraph');
	}

	# Now output the paths, using the nodes' original names as labels.

	my(@dot_text);

	push @dot_text, "$strict$digraph fixed_length_paths", '{', qq|\tlabel = "$title" rankdir = LR|, '';

	# Firstly, declare all nodes.

	my($s);

	for my $set (@set)
	{
		for my $node (@$set)
		{
			push @dot_text, qq|\t"$$node{name}" [label = "$$node{label}"]|; # We don't know the attributes of the node.
		}
	}

	push @dot_text, '';

	# Secondly, declare all edges.

	my($edge) = ($digraph eq 'digraph') ? ' -> ' : ' -- ';

	for my $set (@set)
	{
		push @dot_text, "\t" . join(" $edge ", map{qq|"$$_{name}"|} @$set);
	}

	push @dot_text, '}', '';

	my($output_file) = $self -> output_dot_file;

	open(my $fh, '> :encoding(utf-8)', $output_file) || die "Can't open(> $output_file): $!";
	print $fh join("\n", @dot_text);
	close $fh;

} # End of _output_fixed_length_gv.

# -----------------------------------------------

sub _preprocess
{
	my($self) = @_;

	# Parse the input and create $self -> tree.

	$self -> run;

	# Find mothers who have edges amongst their daughters.
	# Then process the daughters of those mothers.

	my($edgy)      = $self -> _find_cluster_mothers_with_edges;
	my($clusters)  = $self -> _find_cluster_reachable_nodes($edgy);
	my($subgraphs) = $self -> _coelesce_cluster_sets($clusters);

	# Renumber subgraphs by discarding duplicate sets.

	my($count) = 0;

	my(@members, $members);
	my(%subgraph_sets, %seen);

	for my $id (keys %$subgraphs)
	{
		@members = sort $$subgraphs{$id} -> members;
		$members = join(' ', @members);

		next if ($seen{$members});

		$seen{$members} = 1;

		$count++;

		$subgraph_sets{$count} = Set::Tiny -> new;

		$subgraph_sets{$count} -> insert(@members);
	}

	return \%subgraph_sets;

} # End of _preprocess.

# -----------------------------------------------

sub report_cluster_members
{
	my($self) = @_;
	my($sets) = $self -> cluster_sets;

	$self -> log(notice => 'Input file: ' . $self -> input_file . '. Cluster membership:');

	for my $id (sort{0+$a <=> 0+$b} keys %$sets)
	{
		$self -> log(notice => "Cluster: $id. " . $$sets{$id} -> as_string);
	}

} # End of report_cluster_members.

# -----------------------------------------------

sub report_fixed_length_paths
{
	my($self, $title) = @_;
	$title            =~ s/\\n/. /g;
	my($count)        = 0;

	$self -> log(notice => "$title:");

	for my $candidate (@{$self -> fixed_path_set})
	{
		$count++;

		$self -> log(notice => "Path: $count. " . join(' -> ', map{${$_ -> attributes}{value} } @$candidate) );
	}

} # End of report_fixed_length_paths.

# -----------------------------------------------

sub _winnow_cluster_tree
{
	my($self, $tree) = @_;

	# Each tree will probably have had nodes deleted. This may have left
	# empty subgraphs, such as {} or subgraph {} or subgraph sub_name {}.
	# We wish to delete these from the tree. They are all characterized by
	# having no nodes in the {}, even if they have edges and attributes there.
	# Of course, there must be no nodes in nested subgraphs too.

	my(@daughters);
	my(%seen);
	my($uid);

	$tree -> walk_down
	({
		callback => sub
		{
			my($node)  = @_;
			@daughters = $node -> daughters;

			if ($self -> _winnow_subgraph(\@daughters) )
			{
				$uid        = ${$node -> attributes}{uid};
				$seen{$uid} = 1;
			}

			return 1; # Keep walking.
		},
		_depth => 0,
	});

	$self -> log(info => "Target for winnow: $_") for sort keys %seen;

} # End of _winnow_cluster_tree.

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

		$seen{$_}++ for map{${$_ -> attributes}{value} } @$candidate;

		# Exclude nodes depending on the allow_cycles option:
		# o 0 - Do not allow any cycles.
		# o 1 - Allow any node to be included once or twice.

		if ($cycles == 0)
		{
			@$candidate = grep{$seen{${$_ -> attributes}{value} } == 1} @$candidate;
		}
		elsif ($cycles == 1)
		{
			@$candidate = grep{$seen{${$_ -> attributes}{value}} <= 2} @$candidate;
		}

		push @solutions, [@$candidate] if ($#$candidate == $self -> path_length);
	}

	$self -> fixed_path_set([@solutions]);

} # End of _winnow_fixed_length_paths.

# -----------------------------------------------

sub _winnow_subgraph
{
	my($self, $daughters) = @_;

	# A subgraph is empty if 2 daughters, { and }, have no nodes between them.

	my($brace_found) = 0;

	my($attributes);
	my($node_count);

	for my $i (0 .. $#$daughters)
	{
		$attributes = $$daughters[$i] -> attributes;

		# Find an open brace, {.

		if ( ($$daughters[$i] -> name eq 'literal') && ($$attributes{value} eq '{') )
		{
			$brace_found = 1;
			$node_count  = 0;

			for (my $j = $i + 1; $j <= $#$daughters; $j++)
			{
				$attributes = $$daughters[$j] -> attributes;

				# Find the first closing brace, }.

				last if ( ($$daughters[$j] -> name eq 'literal') && ($$attributes{value} eq '}') );

				# Count the nodes between the { and }.

				$node_count++ if ($$daughters[$j] -> name eq 'node_id');
			}

			# Exit when at least 1 set of 'empty' braces was found.

			last if ($node_count == 0);
		}
	}

	# Return 0 if there is no need to stockpile the uid of the mother,
	# and return 1 if a nodeless set of braces was found, since in the
	# latter case there is something in the tree worth winnowing.

	return ($brace_found && ($node_count == 0) ) ? 1 : 0;

} # End of _winnow_subgraph.

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
	    input_file        => 'data/jointed.edges.gv',
	    output_dot_file   => 'data/clusters.gv',
	    output_image_file => 'html/clusters.svg',
	    report_clusters   => 1,
	);

Or call methods to set parameters;

	my($parser) = GraphViz2::Marpa::PathUtils -> new;

	$parser -> input_file('data/jointed.edges.gv');
	$parser -> output_dot_file('data/fixed.length.paths.gv');
	$parser -> output_image_file('html/fixed.length.paths.sgv');
	$parser -> report_clusters(1);

And then:

	$parser -> find_clusters;

Command line usage:

	shell> perl scripts/find.clusters.pl -h

Or see scripts/find.clusters.sh, which hard-codes my test data values.

=head2 Finding fixed length paths

Either pass parameters in to new():

	GraphViz2::Marpa::PathUtils -> new
	(
	    allow_cycles      => 1,
	    input_file        => 'data/90.KW91.gv',
	    output_dot_file   => 'data/fixed.length.paths.gv',
	    output_image_file => 'html/fixed.length.paths.svg',
	    path_length       => 4,
	    report_paths      => 1,
	    start_node        => 'Act_1',
	) -> find_fixed_length_paths;

Or call methods to set parameters;

	my($parser) = GraphViz2::Marpa::PathUtils -> new;

	$parser -> allow_cycles(1);
	$parser -> input_file('data/90.KW91.gv');
	$parser -> output_dot_file('data/fixed.length.paths.gv');
	$parser -> output_image_file('html/fixed.length.paths.sgv');
	$parser -> path_length(4);
	$parser -> report_paths(1);
	$parser -> start_node('Act_1');

And then:

	$parser -> find_fixed_length_paths;

Command line usage:

	shell> perl scripts/find.fixed.length.paths.pl -h

Or see scripts/fixed.length.paths.sh, which hard-codes my test data values.

=head1 DESCRIPTION

GraphViz2::Marpa::PathUtils parses L<Graphviz|http://www.graphviz.org/> dot files and processes
the output in various ways.

This class is a descendent of L<GraphViz2::Marpa>, and hence inherits all its keys to new(), and
all its methods.

Currently, the only feature available is to find all paths of a given length starting from a given
node.

Sample output: L<http://savage.net.au/Perl-modules/html/graphviz2.marpa.pathutils/index.html>.

=head1 Scripts shipped with this distro

All scripts are in the scripts/ directory. This means they do I<not> get installed along with the
package.

Data files are in data/, while html and svg files are in html/.

=over 4

=item o copy.config.pl

During installation, this copies config/.htgraphviz2.marpa.pathutils.conf to a dir as discussed
under L</The Configuration File>.

=item o find.clusters.pl

This runs the L</find_clusters()> method in GraphViz2::Marpa::PathUtils.

=item o find.clusters.sh

This runs find.clusters.pl with hard-coded parameters, and is what I use for testing new code.

=item o find.fixed.length.paths.pl

This runs the L</find_fixed_length_paths()> method in GraphViz2::Marpa::PathUtils.

Try shell> perl find.fixed.length.paths.pl -h

=item o find.fixed.length.paths.sh

This runs find.fixed.length.paths.pl with hard-coded parameters, and is what I use for testing new
code.

=item o generate.demo.pl

Uses the L<Text::Xslate> template htdocs/assets/templates/graphviz2/marpa/pathutils/pathutils.tx
to generate html/index.html.

=item o generate.demo.sh

Runs generate.demo.pl.

Then copies html/*.html and html/*.svg to my web server's dir,
$DR/Perl-modules/html/graphviz2.marpa.pathutils/.

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

=head1 Constructor and Initialization

=head2 Calling new()

C<new()> is called as C<< my($obj) = GraphViz2::Marpa::PathUtils -> new(k1 => v1, k2 => v2, ...) >>.

It returns a new object of type C<GraphViz2::Marpa::PathUtils>.

This class is a descendent of L<GraphViz2::Marpa>, and hence inherits all its keys to new(), and all
its methods.

Specifically, see L<GraphViz2::Marpa/Constructor and Initialization> for more options to new(),
including I<maxlevel>.

Further, these key-value pairs are accepted in the parameter list (see corresponding methods for
details [e.g. L</path_length($integer)>]):

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

=item o output_dot_file => aDOTInputFileName

Specify the name of a file to write which will contain the DOT description of the image of all
solutions.

Default: ''.

This file is not written if the value is ''.

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

=head2 new()

See L</Constructor and Initialization> for details on the parameters accepted by L</new()>.

=head2 output_cluster_image()

This writes the clusters found, as a DOT output file, as long as new(output_image_file => $name) was
specified.

=head2 output_dot_file([$name])

Here the [] indicate an optional parameter.

Get or set the name of the I<dot> input file to write.

'output_dot_file' is a parameter to L</new()>. See L</Constructor and Initialization> for details.

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

=head1 FAQ

=head2 I used a node label of "\N" and now your module doesn't work!

The most likely explanation is that you're calling I<find_fixed_path_lengths()> and you've specified
all nodes, or at least some, to have a label like "\N".

This escape sequence triggers special processing in AT&T's Graphviz to generate labels for nodes,
overriding the code I use to re-name nodes in the output of I<find_fixed_path_lengths()>.

See I<_prepare_fixed_length_output()> for the gory details.

The purpose of my re-numbering code is to allow a node to appear in the output multiple times but to
stop Graphviz automatically regarding all such references to be the same node. Giving a node
different names (which are un-seen) but the same label (which is seen) makes Graphviz think they are
really different nodes.

The 3 samples in part 2 of
L<the demo page|http://savage.net.au/Perl-modules/html/graphviz2.marpa.pathutils/index.html>
should make this issue clear.

=head2 What is the homepage of Marpa?

L<http://jeffreykegler.github.com/Marpa-web-site/>.

See also Jeffrey's annotated
L<blog|http://jeffreykegler.github.io/Ocean-of-Awareness-blog/metapages/annotated.html>.

=head2 How are clusters named?

They are simply counted in the order discovered in the input file.

But see the next questions.

=head2 If I run your code multiple times, I get different clusters!

No, the results will be the same, but the output files will be generated in a different order.

This just means means the arbitrary numbers assigned to the generated clusters are being assigned in
a  different order from run to run.

It's not clear to me why the clusters are not found in the same order each time the program is run.

=head2 How are cycles in fixed path length analysis handled?

This is controlled by the I<allow_cycles> option to new(), or the corresponding method
L</allow_cycles($integer)>.

The code keeps track of the number of times each node is entered. If new(allow_cycles => 0) was
called, nodes are only considered if they are entered once. If new(allow_cycles => 1) was called,
nodes are also considered if they are entered a second time.

Sample code: Using the input file data/90.KW91.lex (see scripts/fixed.length.paths.sh) we can
specify various combinations of parameters like this:

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

Agreed. Remember that this code calls L<GraphViz2::Marpa>'s run() method, which expects a large
number of options because it calls both the lexer and the parser.

=head2 Why do I get error messages like the following?

	Error: <stdin>:1: syntax error near line 1
	context: digraph >>>  Graph <<<  {

Graphviz reserves some words as keywords, meaning they can't be used as an ID, e.g. for the name of
the graph.

The keywords are: I<digraph>, I<edge>, I<graph>, I<node>, I<strict> and I<subgraph>.
Compass points are not keywords.

See L<keywords|http://www.graphviz.org/content/dot-language> in the discussion of the syntax of DOT
for details.

So, don't do this:

	strict graph graph{...}
	strict graph Graph{...}
	strict graph strict{...}
	etc...

Likewise for non-strict graphs, and digraphs. You can however add double-quotes around such reserved
words:

	strict graph "graph"{...}

Even better, use a more meaningful name for your graph...

=head1 Reference

V 1 of this module used code from this book. Since V 2 now properly parses DOT files, the code
referred to here is no longer needed.

Combinatorial Algorithms for Computers and Calculators, A Nijenhuis and H Wilf, p 240.

This books very clearly explains the backtracking parser I used to process the combinations of nodes
found at each point along each path. Source code in the book is in Fortran.

It's available as a PDF from L<http://www.math.upenn.edu/~wilf/website/CombAlgDownld.html>.

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
