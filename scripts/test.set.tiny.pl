#!/usr/bin/env perl

use feature qw/say unicode_strings/;
use open qw(:std :utf8);
use strict;
use warnings;
use warnings qw(FATAL utf8);

use Set::Tiny;

# ------------

my($s) = Set::Tiny -> new(qw/A B C/);

$s -> insert('A');

print join(', ', sort $s -> members), "\n";
