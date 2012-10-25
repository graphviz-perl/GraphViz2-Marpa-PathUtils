#!/usr/bin/env perl

use feature qw/say unicode_strings/;
use open qw(:std :utf8);
use strict;
use warnings;
use warnings qw(FATAL utf8);

use Set::Tiny;

# ------------

my($set_a) = Set::Tiny -> new(qw/A B C D E K/);
my($set_b) = Set::Tiny -> new(qw/A D K/);
my($set_c) = Set::Tiny -> new(qw/A D K/);

say "a is a subset of b"        if ($set_a -> is_subset($set_b) );
say "b is a subset of a"        if ($set_b -> is_subset($set_a) );
say "b is a proper subset of a" if ($set_b -> is_proper_subset($set_a) );
say "b is a proper subset of c" if ($set_b -> is_proper_subset($set_c) );
