#!/usr/bin/env raku

# See "GNU Scientific Library" manual Chapter 20 Random Number Distributions, Paragraph 20.41 Examples
# First example

use lib 'lib';
use Math::Libgsl::Random;
use Math::Libgsl::RandomDistribution;

my Math::Libgsl::Random $r .= new;
say poisson($r, 3) for ^10;
