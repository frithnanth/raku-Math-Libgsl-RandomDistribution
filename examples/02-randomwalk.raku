#!/usr/bin/env raku

# See "GNU Scientific Library" manual Chapter 20 Random Number Distributions, Paragraph 20.41 Examples
# Second example

use lib 'lib';
use Math::Libgsl::Random;
use Math::Libgsl::RandomDistribution;

my Math::Libgsl::Random $r .= new;
my Num ($x, $y) = (0e0, 0e0);
for ^40 {
  my ($dx, $dy) = d2dir($r);
  $x += $dx;
  $y += $dy;
  say "$x $y";
}
