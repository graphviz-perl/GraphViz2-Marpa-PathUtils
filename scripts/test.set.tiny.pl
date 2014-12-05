#!/usr/bin/env perl

use strict;
use warnings;
use warnings qw(FATAL utf8); # Fatalize encoding glitches.
use open     qw(:std :utf8); # Undeclared streams in UTF-8.

use Set::Tiny;

# ------------

my($s) = Set::Tiny -> new(qw/A B C/);

$s -> insert('A');

print join(', ', sort $s -> members), "\n";
