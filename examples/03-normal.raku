#!/usr/bin/env raku

# See "GNU Scientific Library" manual Chapter 20 Random Number Distributions, Paragraph 20.41 Examples
# Third example

use lib 'lib';
use Math::Libgsl::RandomDistribution;

my Num $x = 2e0;
my $P = ugaussian-P($x);
say "Prob(x < $x) = $P";
my $Q = ugaussian-Q($x);
say "prob(x > $x) = $Q";
say "Pinv($P) = { ugaussian-Pinv($P) }";
say "Qinv($Q) = { ugaussian-Qinv($Q) }";
