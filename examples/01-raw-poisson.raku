#!/usr/bin/env raku

# See "GNU Scientific Library" manual Chapter 20 Random Number Distributions, Paragraph 20.41 Examples
# First example

use lib 'lib';
use Math::Libgsl::Constants;
use Math::Libgsl::Raw::Random :ALL;
use Math::Libgsl::Raw::RandomDistribution :poisson;

my gsl_rng $r = mgsl_rng_setup(DEFAULT);
say gsl_ran_poisson($r, 3e0) for ^10;
