#!/usr/bin/env raku

# See "GNU Scientific Library" manual Chapter 20 Random Number Distributions, Paragraph 20.41 Examples
# Second example

use lib 'lib';
use Math::Libgsl::Constants;
use Math::Libgsl::Raw::Random :ALL;
use Math::Libgsl::Raw::RandomDistribution :dir;

my gsl_rng $r = mgsl_rng_setup(DEFAULT);
my num64 ($x, $y) = (0e0, 0e0);
my num64 ($dx, $dy);
for ^40 {
  gsl_ran_dir_2d($r, $dx, $dy);
  $x += $dx;
  $y += $dy;
  say "$x $y";
}
