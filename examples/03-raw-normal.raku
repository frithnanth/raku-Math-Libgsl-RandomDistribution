#!/usr/bin/env raku

# See "GNU Scientific Library" manual Chapter 20 Random Number Distributions, Paragraph 20.41 Examples
# Third example

use lib 'lib';
use Math::Libgsl::Constants;
use Math::Libgsl::Raw::RandomDistribution :gauss;

my num64 $x = 2e0;
my $P = gsl_cdf_ugaussian_P($x);
say "Prob(x < $x) = $P";
my $Q = gsl_cdf_ugaussian_Q($x);
say "prob(x > $x) = $Q";
say "Pinv($P) = { gsl_cdf_ugaussian_Pinv($P) }";
say "Qinv($Q) = { gsl_cdf_ugaussian_Qinv($Q) }";
