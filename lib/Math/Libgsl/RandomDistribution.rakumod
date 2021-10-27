use v6.c;

unit module Math::Libgsl::RandomDistribution:ver<0.0.3>:auth<cpan:FRITH>;

use NativeCall;
use NativeHelpers::Blob;
use Math::Libgsl::Vector;
use Math::Libgsl::Matrix;
use Math::Libgsl::Random;
use Math::Libgsl::Exception;
use Math::Libgsl::Constants;
use Math::Libgsl::Raw::RandomDistribution :ALL;

# Gaussian Distribution
sub gaussian(Math::Libgsl::Random $r, Num() $sigma --> Num) is export
{
  gsl_ran_gaussian($r.r, $sigma)
}
sub gaussian-pdf(Num() $x, Num() $sigma --> Num) is export
{
  gsl_ran_gaussian_pdf($x, $sigma)
}
sub gaussian-ziggurat(Math::Libgsl::Random $r, Num() $sigma --> Num) is export
{
  gsl_ran_gaussian_ziggurat($r.r, $sigma)
}
sub gaussian-ratio-method(Math::Libgsl::Random $r, Num() $sigma --> Num) is export
{
  gsl_ran_gaussian_ratio_method($r.r, $sigma)
}
sub ugaussian(Math::Libgsl::Random $r --> Num) is export
{
  gsl_ran_ugaussian($r.r)
}
sub ugaussian-pdf(Num() $x --> Num) is export
{
  gsl_ran_ugaussian_pdf($x)
}
sub ugaussian-ratio-method(Math::Libgsl::Random $r --> Num) is export
{
  gsl_ran_ugaussian_ratio_method($r.r)
}
sub gaussian-P(Num() $x, Num() $sigma --> Num) is export
{
  gsl_cdf_gaussian_P($x, $sigma)
}
sub gaussian-Q(Num() $x, Num() $sigma --> Num) is export
{
  gsl_cdf_gaussian_Q($x, $sigma)
}
sub gaussian-Pinv(Num() $P, Num() $sigma --> Num) is export
{
  gsl_cdf_gaussian_Pinv($P, $sigma)
}
sub gaussian-Qinv(Num() $Q, Num() $sigma --> Num) is export
{
  gsl_cdf_gaussian_Qinv($Q, $sigma)
}
sub ugaussian-P(Num() $x --> Num) is export
{
  gsl_cdf_ugaussian_P($x)
}
sub ugaussian-Q(Num() $x --> Num) is export
{
  gsl_cdf_ugaussian_Q($x)
}
sub ugaussian-Pinv(Num() $P --> Num) is export
{
  gsl_cdf_ugaussian_Pinv($P)
}
sub ugaussian-Qinv(Num() $Q --> Num) is export
{
  gsl_cdf_ugaussian_Qinv($Q)
}
# Gaussian Tail Distribution
sub gaussian-tail(Math::Libgsl::Random $r, Num() $a where * > 0, Num() $sigma --> Num) is export
{
  gsl_ran_gaussian_tail($r.r, $a, $sigma)
}
sub gaussian-tail-pdf(Num() $x, Num() $a where * > 0, Num() $sigma --> Num) is export
{
  gsl_ran_gaussian_tail_pdf($x, $a, $sigma)
}
sub ugaussian-tail(Math::Libgsl::Random $r, Num() $a where * > 0 --> Num) is export
{
  gsl_ran_ugaussian_tail($r.r, $a)
}
sub ugaussian-tail-pdf(Num() $x, Num() $a where * > 0 --> Num) is export
{
  gsl_ran_ugaussian_tail_pdf($x, $a)
}
# Bivariate Gaussian Distribution
sub bivariate-gaussian(Math::Libgsl::Random $r, Num() $sigma-x, Num() $sigma-y, Num() $rho where -1 < * < 1
                       --> List) is export
{
  my num64 ($x, $y);
  gsl_ran_bivariate_gaussian($r.r, $sigma-x, $sigma-y, $rho, $x, $y);
  return $x, $y;
}
sub bivariate-gaussian-pdf(Num() $x, Num() $y, Num() $sigma-x, Num() $sigma-y, Num() $rho where -1 < * < 1
                           --> Num) is export
{
  gsl_ran_bivariate_gaussian_pdf($x, $y, $sigma-x, $sigma-y, $rho);
}
# Multivariate Gaussian Distribution
sub multivariate-gaussian(Math::Libgsl::Random $r, Math::Libgsl::Vector $mu,
                          Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 &&
                                                          $L.matrix.size1 == $mu.vector.size }
                          --> Math::Libgsl::Vector) is export
{
  my Math::Libgsl::Vector $result .= new: $mu.vector.size;
  my $ret = gsl_ran_multivariate_gaussian($r.r, $mu.vector, $L.matrix, $result.vector);
  fail X::Libgsl.new: errno => $ret, error => "Error in multivariate-gaussian" if $ret ≠ GSL_SUCCESS;
  return $result;
}
sub multivariate-gaussian-pdf(Math::Libgsl::Vector $x,
                              Math::Libgsl::Vector $mu where *.vector.size == $x.vector.size,
                              Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 &&
                                                              $L.matrix.size1 == $mu.vector.size } --> Num) is export
{
  my num64 $result;
  my Math::Libgsl::Vector $work .= new: $mu.vector.size;
  my $ret = gsl_ran_multivariate_gaussian_pdf($x.vector, $mu.vector, $L.matrix, $result, $work.vector);
  fail X::Libgsl.new: errno => $ret, error => "Error in multivariate-gaussian-pdf" if $ret ≠ GSL_SUCCESS;
  return $result;
}
sub multivariate-gaussian-log-pdf(Math::Libgsl::Vector $x,
                                  Math::Libgsl::Vector $mu where *.vector.size == $x.vector.size,
                                  Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 &&
                                                                  $L.matrix.size1 == $mu.vector.size } --> Num) is export
{
  my num64 $result;
  my Math::Libgsl::Vector $work .= new: $mu.vector.size;
  my $ret = gsl_ran_multivariate_gaussian_log_pdf($x.vector, $mu.vector, $L.matrix, $result, $work.vector);
  fail X::Libgsl.new: errno => $ret, error => "Error in multivariate-gaussian-log-pdf" if $ret ≠ GSL_SUCCESS;
  return $result;
}
sub multivariate-gaussian-mean(Math::Libgsl::Matrix $X --> Math::Libgsl::Vector) is export
{
  my Math::Libgsl::Vector $mu-hat .= new: $X.matrix.size2;
  my $ret = gsl_ran_multivariate_gaussian_mean($X.matrix, $mu-hat.vector);
  fail X::Libgsl.new: errno => $ret, error => "Error in multivariate-gaussian-mean" if $ret ≠ GSL_SUCCESS;
  return $mu-hat;
}
sub multivariate-gaussian-vcov(Math::Libgsl::Matrix $X --> Math::Libgsl::Matrix) is export
{
  my Math::Libgsl::Matrix $mu-hat .= new: $X.matrix.size2, $X.matrix.size2;
  my $ret = gsl_ran_multivariate_gaussian_vcov($X.matrix, $mu-hat.matrix);
  fail X::Libgsl.new: errno => $ret, error => "Error in multivariate-gaussian-vcov" if $ret ≠ GSL_SUCCESS;
  return $mu-hat;
}
# Exponential Distribution
sub exponential(Math::Libgsl::Random $r, Num() $mu --> Num) is export
{
  gsl_ran_exponential($r.r, $mu);
}
sub exponential-pdf(Num() $x, Num() $mu --> Num) is export
{
  gsl_ran_exponential_pdf($x, $mu);
}
sub exponential-P(Num() $x, Num() $mu --> Num) is export
{
  gsl_cdf_exponential_P($x, $mu);
}
sub exponential-Q(Num() $x, Num() $mu --> Num) is export
{
  gsl_cdf_exponential_Q($x, $mu);
}
sub exponential-Pinv(Num() $P, Num() $mu --> Num) is export
{
  gsl_cdf_exponential_Pinv($P, $mu);
}
sub exponential-Qinv(Num() $Q, Num() $mu --> Num) is export
{
  gsl_cdf_exponential_Qinv($Q, $mu);
}
# Laplace Distribution
sub laplace(Math::Libgsl::Random $r, Num() $a --> Num) is export
{
  gsl_ran_laplace($r.r, $a);
}
sub laplace-pdf(Num() $x, Num() $a --> Num) is export
{
  gsl_ran_laplace_pdf($x, $a);
}
sub laplace-P(Num() $x, Num() $a --> Num) is export
{
  gsl_cdf_laplace_P($x, $a);
}
sub laplace-Q(Num() $x, Num() $a --> Num) is export
{
  gsl_cdf_laplace_Q($x, $a);
}
sub laplace-Pinv(Num() $P, Num() $a --> Num) is export
{
  gsl_cdf_laplace_Pinv($P, $a);
}
sub laplace-Qinv(Num() $Q, Num() $a --> Num) is export
{
  gsl_cdf_laplace_Qinv($Q, $a);
}
# Exponential Power Distribution
sub exppow(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_exppow($r.r, $a, $b);
}
sub exppow-pdf(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_exppow_pdf($x, $a, $b);
}
sub exppow-P(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_exppow_P($x, $a, $b);
}
sub exppow-Q(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_exppow_Q($x, $a, $b);
}
# Cauchy Distribution
sub cauchy(Math::Libgsl::Random $r, Num() $a --> Num) is export
{
  gsl_ran_cauchy($r.r, $a);
}
sub cauchy-pdf(Num() $x, Num() $a --> Num) is export
{
  gsl_ran_cauchy_pdf($x, $a);
}
sub cauchy-P(Num() $x, Num() $a --> Num) is export
{
  gsl_cdf_cauchy_P($x, $a);
}
sub cauchy-Q(Num() $x, Num() $a --> Num) is export
{
  gsl_cdf_cauchy_Q($x, $a);
}
sub cauchy-Pinv(Num() $P, Num() $a --> Num) is export
{
  gsl_cdf_cauchy_Pinv($P, $a);
}
sub cauchy-Qinv(Num() $Q, Num() $a --> Num) is export
{
  gsl_cdf_cauchy_Qinv($Q, $a);
}
# Rayleigh Distribution
sub rayleigh(Math::Libgsl::Random $r, Num() $sigma --> Num) is export
{
  gsl_ran_rayleigh($r.r, $sigma);
}
sub rayleigh-pdf(Num() $x, Num() $sigma --> Num) is export
{
  gsl_ran_rayleigh_pdf($x, $sigma);
}
sub rayleigh-P(Num() $x, Num() $sigma --> Num) is export
{
  gsl_cdf_rayleigh_P($x, $sigma);
}
sub rayleigh-Q(Num() $x, Num() $sigma --> Num) is export
{
  gsl_cdf_rayleigh_Q($x, $sigma);
}
sub rayleigh-Pinv(Num() $P, Num() $sigma --> Num) is export
{
  gsl_cdf_rayleigh_Pinv($P, $sigma);
}
sub rayleigh-Qinv(Num() $Q, Num() $sigma --> Num) is export
{
  gsl_cdf_rayleigh_Qinv($Q, $sigma);
}
# Rayleigh Tail Distribution
sub rayleigh-tail(Math::Libgsl::Random $r, Num() $a, Num() $sigma --> Num) is export
{
  gsl_ran_rayleigh_tail($r.r, $a, $sigma);
}
sub rayleigh-tail-pdf(Num() $x, Num() $a, Num() $sigma --> Num) is export
{
  gsl_ran_rayleigh_tail_pdf($x, $a, $sigma);
}
# Landau Distribution
sub landau(Math::Libgsl::Random $r --> Num) is export
{
  gsl_ran_landau($r.r);
}
sub landau-pdf(Num() $x --> Num) is export
{
  gsl_ran_landau_pdf($x);
}
# Levy alpha-Stable Distribution
sub levy(Math::Libgsl::Random $r, Num() $c, Num() $alpha where 0 < * ≤ 2 --> Num) is export
{
  gsl_ran_levy($r.r, $c, $alpha);
}
# Levy skew alpha-Stable Distribution
sub levy-skew(Math::Libgsl::Random $r, Num() $c, Num() $alpha where 0 < * ≤ 2, Num() $beta --> Num) is export
{
  gsl_ran_levy_skew($r.r, $c, $alpha, $beta);
}
# Gamma Distribution
sub gamma(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_gamma($r.r, $a, $b);
}
sub gamma-knuth(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_gamma_knuth($r.r, $a, $b);
}
sub gamma-pdf(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_gamma_pdf($x, $a, $b);
}
sub gamma-P(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gamma_P($x, $a, $b);
}
sub gamma-Q(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gamma_Q($x, $a, $b);
}
sub gamma-Pinv(Num() $P, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gamma_Pinv($P, $a, $b);
}
sub gamma-Qinv(Num() $Q, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gamma_Qinv($Q, $a, $b);
}
# Flat (Uniform) Distribution
sub flat(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_flat($r.r, $a, $b);
}
sub flat-pdf(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_flat_pdf($x, $a, $b);
}
sub flat-P(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_flat_P($x, $a, $b);
}
sub flat-Q(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_flat_Q($x, $a, $b);
}
sub flat-Pinv(Num() $P, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_flat_Pinv($P, $a, $b);
}
sub flat-Qinv(Num() $Q, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_flat_Qinv($Q, $a, $b);
}
# Lognormal Distribution
sub lognormal(Math::Libgsl::Random $r, Num() $zeta, Num() $sigma --> Num) is export
{
  gsl_ran_lognormal($r.r, $zeta, $sigma);
}
sub lognormal-pdf(Num() $x, Num() $zeta, Num() $sigma --> Num) is export
{
  gsl_ran_lognormal_pdf($x, $zeta, $sigma);
}
sub lognormal-P(Num() $x, Num() $zeta, Num() $sigma --> Num) is export
{
  gsl_cdf_lognormal_P($x, $zeta, $sigma);
}
sub lognormal-Q(Num() $x, Num() $zeta, Num() $sigma --> Num) is export
{
  gsl_cdf_lognormal_Q($x, $zeta, $sigma);
}
sub lognormal-Pinv(Num() $P, Num() $zeta, Num() $sigma --> Num) is export
{
  gsl_cdf_lognormal_Pinv($P, $zeta, $sigma);
}
sub lognormal-Qinv(Num() $Q, Num() $zeta, Num() $sigma --> Num) is export
{
  gsl_cdf_lognormal_Qinv($Q, $zeta, $sigma);
}
# Chi-squared Distribution
sub chisq(Math::Libgsl::Random $r, Num() $nu --> Num) is export
{
  gsl_ran_chisq($r.r, $nu);
}
sub chisq-pdf(Num() $x where * ≥ 0, Num() $nu --> Num) is export
{
  gsl_ran_chisq_pdf($x, $nu);
}
sub chisq-P(Num() $x where * ≥ 0, Num() $nu --> Num) is export
{
  gsl_cdf_chisq_P($x, $nu);
}
sub chisq-Q(Num() $x where * ≥ 0, Num() $nu --> Num) is export
{
  gsl_cdf_chisq_Q($x, $nu);
}
sub chisq-Pinv(Num() $P, Num() $nu --> Num) is export
{
  gsl_cdf_chisq_Pinv($P, $nu);
}
sub chisq-Qinv(Num() $Q, Num() $nu --> Num) is export
{
  gsl_cdf_chisq_Qinv($Q, $nu);
}
# F-distribution
sub fdist(Math::Libgsl::Random $r, Num() $nu1, Num() $nu2 --> Num) is export
{
  gsl_ran_fdist($r.r, $nu1, $nu2);
}
sub fdist-pdf(Num() $x where * ≥ 0, Num() $nu1, Num() $nu2 --> Num) is export
{
  gsl_ran_fdist_pdf($x, $nu1, $nu2);
}
sub fdist-P(Num() $x where * ≥ 0, Num() $nu1, Num() $nu2 --> Num) is export
{
  gsl_cdf_fdist_P($x, $nu1, $nu2);
}
sub fdist-Q(Num() $x where * ≥ 0, Num() $nu1, Num() $nu2 --> Num) is export
{
  gsl_cdf_fdist_Q($x, $nu1, $nu2);
}
sub fdist-Pinv(Num() $P, Num() $nu1, Num() $nu2 --> Num) is export
{
  gsl_cdf_fdist_Pinv($P, $nu1, $nu2);
}
sub fdist-Qinv(Num() $Q, Num() $nu1, Num() $nu2 --> Num) is export
{
  gsl_cdf_fdist_Qinv($Q, $nu1, $nu2);
}
# t-distribution
sub tdist(Math::Libgsl::Random $r, Num() $nu --> Num) is export
{
  gsl_ran_tdist($r.r, $nu);
}
sub tdist-pdf(Num() $x, Num() $nu --> Num) is export
{
  gsl_ran_tdist_pdf($x, $nu);
}
sub tdist-P(Num() $x, Num() $nu --> Num) is export
{
  gsl_cdf_tdist_P($x, $nu);
}
sub tdist-Q(Num() $x, Num() $nu --> Num) is export
{
  gsl_cdf_tdist_Q($x, $nu);
}
sub tdist-Pinv(Num() $P, Num() $nu --> Num) is export
{
  gsl_cdf_tdist_Pinv($P, $nu);
}
sub tdist-Qinv(Num() $Q, Num() $nu --> Num) is export
{
  gsl_cdf_tdist_Qinv($Q, $nu);
}
# Beta Distribution
sub beta(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_beta($r.r, $a, $b);
}
sub beta-pdf(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_beta_pdf($x, $a, $b);
}
sub beta-P(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_beta_P($x, $a, $b);
}
sub beta-Q(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_beta_Q($x, $a, $b);
}
sub beta-Pinv(Num() $P, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_beta_Pinv($P, $a, $b);
}
sub beta-Qinv(Num() $Q, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_beta_Qinv($Q, $a, $b);
}
# Logistic Distribution
sub logistic(Math::Libgsl::Random $r, Num() $a --> Num) is export
{
  gsl_ran_logistic($r.r, $a);
}
sub logistic-pdf(Num() $x, Num() $a --> Num) is export
{
  gsl_ran_logistic_pdf($x, $a);
}
sub logistic-P(Num() $x, Num() $a --> Num) is export
{
  gsl_cdf_logistic_P($x, $a);
}
sub logistic-Q(Num() $x, Num() $a --> Num) is export
{
  gsl_cdf_logistic_Q($x, $a);
}
sub logistic-Pinv(Num() $P, Num() $a --> Num) is export
{
  gsl_cdf_logistic_Pinv($P, $a);
}
sub logistic-Qinv(Num() $Q, Num() $a --> Num) is export
{
  gsl_cdf_logistic_Qinv($Q, $a);
}
# Pareto Distribution
sub pareto(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_pareto($r.r, $a, $b);
}
sub pareto-pdf(Num() $x, Num() $a, Num() $b where { $x ≥ $b } --> Num) is export
{
  gsl_ran_pareto_pdf($x, $a, $b);
}
sub pareto-P(Num() $x, Num() $a, Num() $b where { $x ≥ $b } --> Num) is export
{
  gsl_cdf_pareto_P($x, $a, $b);
}
sub pareto-Q(Num() $x, Num() $a, Num() $b where { $x ≥ $b } --> Num) is export
{
  gsl_cdf_pareto_Q($x, $a, $b);
}
sub pareto-Pinv(Num() $P, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_pareto_Pinv($P, $a, $b);
}
sub pareto-Qinv(Num() $Q, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_pareto_Qinv($Q, $a, $b);
}
# Spherical Vector Distributions
sub d2dir(Math::Libgsl::Random $r --> List) is export
{
  my num64 $x = 0e0;
  my num64 $y = 0e0;
  gsl_ran_dir_2d($r.r, $x, $y);
  return $x, $y;
}
sub d2dir-trig-method(Math::Libgsl::Random $r --> List) is export
{
  my num64 $x = 0e0;
  my num64 $y = 0e0;
  gsl_ran_dir_2d_trig_method($r.r, $x, $y);
  return $x, $y;
}
sub d3dir(Math::Libgsl::Random $r --> List) is export
{
  my num64 $x = 0e0;
  my num64 $y = 0e0;
  my num64 $z = 0e0;
  gsl_ran_dir_3d($r.r, $x, $y, $z);
  return $x, $y, $z;
}
sub dndir(Math::Libgsl::Random $r, Int $n --> List) is export
{
  my $xarr = CArray[num64].new: 0e0 xx $n;
  gsl_ran_dir_nd($r.r, $n, $xarr);
  return $xarr.list;
}
# Weibull Distribution
sub weibull(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_weibull($r.r, $a, $b);
}
sub weibull-pdf(Num() $x where $x ≥ 0, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_weibull_pdf($x, $a, $b);
}
sub weibull-P(Num() $x where $x ≥ 0, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_weibull_P($x, $a, $b);
}
sub weibull-Q(Num() $x where $x ≥ 0, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_weibull_Q($x, $a, $b);
}
sub weibull-Pinv(Num() $P, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_weibull_Pinv($P, $a, $b);
}
sub weibull-Qinv(Num() $Q, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_weibull_Qinv($Q, $a, $b);
}
# Type-1 Gumbel Distribution
sub gumbel1(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_gumbel1($r.r, $a, $b);
}
sub gumbel1-pdf(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_gumbel1_pdf($x, $a, $b);
}
sub gumbel1-P(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gumbel1_P($x, $a, $b);
}
sub gumbel1-Q(Num() $x, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gumbel1_Q($x, $a, $b);
}
sub gumbel1-Pinv(Num() $P, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gumbel1_Pinv($P, $a, $b);
}
sub gumbel1-Qinv(Num() $Q, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gumbel1_Qinv($Q, $a, $b);
}
# Type-2 Gumbel Distribution
sub gumbel2(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_gumbel2($r.r, $a, $b);
}
sub gumbel2-pdf(Num() $x where * > 0, Num() $a, Num() $b --> Num) is export
{
  gsl_ran_gumbel2_pdf($x, $a, $b);
}
sub gumbel2-P(Num() $x where * > 0, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gumbel2_P($x, $a, $b);
}
sub gumbel2-Q(Num() $x where * > 0, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gumbel2_Q($x, $a, $b);
}
sub gumbel2-Pinv(Num() $P, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gumbel2_Pinv($P, $a, $b);
}
sub gumbel2-Qinv(Num() $Q, Num() $a, Num() $b --> Num) is export
{
  gsl_cdf_gumbel2_Qinv($Q, $a, $b);
}
# Dirichlet Distribution
sub dirichlet(Math::Libgsl::Random $r, Int $K, @alpha where *.all > 0 --> List) is export
{
  my $alpha = CArray[num64].new: @alpha».Num;
  my $theta = CArray[num64].new: 0e0 xx @alpha.elems;
  gsl_ran_dirichlet($r.r, $K, $alpha, $theta);
  return $theta.list;
}
sub dirichlet-pdf(Int $K, @alpha where *.all > 0, @theta where *.all ≥ 0 --> Num) is export
{
  my $alpha = CArray[num64].new: @alpha».Num;
  my $theta = CArray[num64].new: @theta».Num;
  gsl_ran_dirichlet_pdf($K, $alpha, $theta);
}
sub dirichlet-lnpdf(Int $K, @alpha where *.all > 0, @theta where *.all ≥ 0 --> Num) is export
{
  my $alpha = CArray[num64].new: @alpha».Num;
  my $theta = CArray[num64].new: @theta».Num;
  gsl_ran_dirichlet_lnpdf($K, $alpha, $theta);
}
# General Discrete Distributions
class Discrete
{
  has gsl_ran_discrete_t $!discrete;
  has Int                $.size;
  has                    @.probability;

  submethod BUILD(Int() :$!size, :@!probability where *.all > 0) {
    my $probability = CArray[num64].new: @!probability».Num;
    $!discrete = gsl_ran_discrete_preproc($!size, $probability);
  }
  submethod DESTROY() {
    gsl_ran_discrete_free($!discrete);
  }
  method discrete(Math::Libgsl::Random $r --> Int) {
    gsl_ran_discrete($r.r, $!discrete);
  }
  method discrete-pdf(Int $k --> Num) {
    gsl_ran_discrete_pdf($k, $!discrete);
  }
}
# Poisson Distribution
sub poisson(Math::Libgsl::Random $r, Num() $mu --> UInt) is export
{
  gsl_ran_poisson($r.r, $mu);
}
sub poisson-pdf(UInt $k, Num() $mu --> Num) is export
{
  gsl_ran_poisson_pdf($k, $mu);
}
sub poisson-P(UInt $k, Num() $mu --> Num) is export
{
  gsl_cdf_poisson_P($k, $mu);
}
sub poisson-Q(UInt $k, Num() $mu --> Num) is export
{
  gsl_cdf_poisson_Q($k, $mu);
}
# Bernoulli Distribution
sub bernoulli(Math::Libgsl::Random $r, Num() $p --> Int) is export
{
  gsl_ran_bernoulli($r.r, $p);
}
sub bernoulli-pdf(UInt $k, Num() $p --> Num) is export
{
  gsl_ran_bernoulli_pdf($k, $p);
}
# Binomial Distribution
sub binomial(Math::Libgsl::Random $r, Num() $p, UInt $n --> Int) is export
{
  gsl_ran_binomial($r.r, $p, $n);
}
sub binomial-pdf(UInt $k, Num() $p, UInt $n where * ≥ $k --> Num) is export
{
  gsl_ran_binomial_pdf($k, $p, $n);
}
sub binomial-P(UInt $k, Num() $p, UInt $n where * ≥ $k --> Num) is export
{
  gsl_cdf_binomial_P($k, $p, $n);
}
sub binomial-Q(UInt $k, Num() $p, UInt $n where * ≥ $k --> Num) is export
{
  gsl_cdf_binomial_Q($k, $p, $n);
}
# Multinomial Distribution
sub multinomial(Math::Libgsl::Random $r, Int $K, UInt $N, *@p where { @p.all ~~ Numeric } --> List) is export
{
  my $p = CArray[num64].new: @p».Num;
  my $n = CArray[uint32].new: 0 xx $K;
  gsl_ran_multinomial($r.r, $K, $N, $p, $n);
  return $n.list;
}
sub multinomial-pdf(Int $K, @p where { @p.all ~~ Numeric }, *@n where { @n.all ~~ UInt } --> Num) is export
{
  my $p = CArray[num64].new: @p».Num;
  my $n = CArray[uint32].new: @n;
  gsl_ran_multinomial_pdf($K, $p, $n);
}
sub multinomial-lnpdf(Int $K, @p where { @p.all ~~ Numeric }, *@n where { @n.all ~~ UInt } --> Num) is export
{
  my $p = CArray[num64].new: @p».Num;
  my $n = CArray[uint32].new: @n;
  gsl_ran_multinomial_lnpdf($K, $p, $n);
}
# Negative Binomial Distribution
sub negative-binomial(Math::Libgsl::Random $r, Num() $p, Num() $n --> UInt) is export
{
  gsl_ran_negative_binomial($r.r, $p, $n);
}
sub negative-binomial-pdf(UInt $k, Num() $p, Num() $n --> Num) is export
{
  gsl_ran_negative_binomial_pdf($k, $p, $n);
}
sub negative-binomial-P(UInt $k, Num() $p, Num() $n --> Num) is export
{
  gsl_cdf_negative_binomial_P($k, $p, $n);
}
sub negative-binomial-Q(UInt $k, Num() $p, Num() $n --> Num) is export
{
  gsl_cdf_negative_binomial_Q($k, $p, $n);
}
# Pascal Distribution
sub pascal(Math::Libgsl::Random $r, Num() $p, UInt $n --> UInt) is export
{
  gsl_ran_pascal($r.r, $p, $n);
}
sub pascal-pdf(UInt $k, Num() $p, UInt $n --> Num) is export
{
  gsl_ran_pascal_pdf($k, $p, $n);
}
sub pascal-P(UInt $k, Num() $p, UInt $n --> Num) is export
{
  gsl_cdf_pascal_P($k, $p, $n);
}
sub pascal-Q(UInt $k, Num() $p, UInt $n --> Num) is export
{
  gsl_cdf_pascal_Q($k, $p, $n);
}
# Geometric Distribution
sub geometric(Math::Libgsl::Random $r, Num() $p --> UInt) is export
{
  gsl_ran_geometric($r.r, $p);
}
sub geometric-pdf(UInt $k where * ≥ 1, Num() $p --> Num) is export
{
  gsl_ran_geometric_pdf($k, $p);
}
sub geometric-P(UInt $k where * ≥ 1, Num() $p --> Num) is export
{
  gsl_cdf_geometric_P($k, $p);
}
sub geometric-Q(UInt $k where * ≥ 1, Num() $p --> Num) is export
{
  gsl_cdf_geometric_Q($k, $p);
}
# Hypergeometric Distribution
sub hypergeometric(Math::Libgsl::Random $r, UInt $n1, UInt $n2, UInt $t --> UInt) is export
{
  gsl_ran_hypergeometric($r.r, $n1, $n2, $t);
}
sub hypergeometric-pdf(UInt $k, UInt $n1, UInt $n2, UInt $t --> Num) is export
{
  gsl_ran_hypergeometric_pdf($k, $n1, $n2, $t);
}
sub hypergeometric-P(UInt $k, UInt $n1, UInt $n2, UInt $t --> Num) is export
{
  gsl_cdf_hypergeometric_P($k, $n1, $n2, $t);
}
sub hypergeometric-Q(UInt $k, UInt $n1, UInt $n2, UInt $t --> Num) is export
{
  gsl_cdf_hypergeometric_Q($k, $n1, $n2, $t);
}
# Logarithmic Distribution
sub logarithmic(Math::Libgsl::Random $r, Num() $p --> UInt) is export
{
  gsl_ran_logarithmic($r.r, $p);
}
sub logarithmic-pdf(UInt $k where * ≥ 1, Num() $p --> Num) is export
{
  gsl_ran_logarithmic_pdf($k, $p);
}
# Wishart Distribution
sub wishart(Math::Libgsl::Random $r,
            Num() $n,
            Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $n ≤ $L.matrix.size1 - 1 }
            --> Math::Libgsl::Matrix) is export
{
  my $N = $L.matrix.size1;
  my Math::Libgsl::Matrix $result .= new: $N, $N;
  my Math::Libgsl::Matrix $work   .= new: $N, $N;
  my $ret = gsl_ran_wishart($r.r, $n, $L.matrix, $result.matrix, $work.matrix);
  fail X::Libgsl.new: errno => $ret, error => "Error in wishart" if $ret ≠ GSL_SUCCESS;
  return $result;
}
sub wishart-pdf(Math::Libgsl::Matrix $X where { $X.matrix.size1 == $X.matrix.size2 },
                Math::Libgsl::Matrix $L-X where { $L-X.matrix.size1 == $L-X.matrix.size2 },
                Num() $n where * > $X.matrix.size1 - 1,
                Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 &&
                                                $X.matrix.size1 == $L.matrix.size1 &&
                                                $L-X.matrix.size1 == $L.matrix.size1
                                              }
                --> Num) is export
{
  my $N = $L.matrix.size1;
  my Math::Libgsl::Matrix $work .= new: $N, $N;
  my num64 $result;
  my $ret = gsl_ran_wishart_pdf($X.matrix, $L-X.matrix, $n, $L.matrix, $result, $work.matrix);
  fail X::Libgsl.new: errno => $ret, error => "Error in wishart-pdf" if $ret ≠ GSL_SUCCESS;
  return $result;
}
sub wishart-log-pdf(Math::Libgsl::Matrix $X where { $X.matrix.size1 == $X.matrix.size2 },
                Math::Libgsl::Matrix $L-X where { $L-X.matrix.size1 == $L-X.matrix.size2 },
                Num() $n where * > $X.matrix.size1 - 1,
                Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 &&
                                                $X.matrix.size1 == $L.matrix.size1 &&
                                                $L-X.matrix.size1 == $L.matrix.size1
                                              }
                --> Num) is export
{
  my $N = $L.matrix.size1;
  my Math::Libgsl::Matrix $work .= new: $N, $N;
  my num64 $result;
  my $ret = gsl_ran_wishart_log_pdf($X.matrix, $L-X.matrix, $n, $L.matrix, $result, $work.matrix);
  fail X::Libgsl.new: errno => $ret, error => "Error in wishart-log-pdf" if $ret ≠ GSL_SUCCESS;
  return $result;
}
# Shuffling and Sampling
sub shuffle(Math::Libgsl::Random $r, @base --> List) is export
{
  my $type = @base[0].WHAT;

  given $type {
    when Int|UInt {
      my CArray[uint64] $base .= new: @base;
      gsl_ran_shuffle($r.r, pointer-to($base), @base.elems, nativesizeof(uint64));
      return $base.list;
    }
    when Num|Rat {
      my CArray[num64] $base .= new: @base».Num;
      gsl_ran_shuffle($r.r, pointer-to($base), @base.elems, nativesizeof(num64));
      return $base.list;
    }
  }
}
sub choose(Math::Libgsl::Random $r, Int $k, @src where *.elems ≥ $k --> List) is export
{
  my $type = @src[0].WHAT;

  given $type {
    when Int|UInt {
      my CArray[uint64] $src .= new: @src;
      my $out = CArray[uint64].allocate: $k;
      gsl_ran_choose($r.r, pointer-to($out), $k, pointer-to($src), @src.elems, nativesizeof(uint64));
      return $out.list;
    }
    when Num|Rat {
      my CArray[num64] $src .= new: @src».Num;
      my $out = CArray[num64].allocate: $k;
      gsl_ran_choose($r.r, pointer-to($out), $k, pointer-to($src), @src.elems, nativesizeof(num64));
      return $out.list;
    }
  }
}
sub sample(Math::Libgsl::Random $r, Int $k, @src --> List) is export
{
  my $type = @src[0].WHAT;

  given $type {
    when Int|UInt {
      my CArray[uint64] $src .= new: @src;
      my $out = CArray[uint64].allocate: $k;
      gsl_ran_sample($r.r, pointer-to($out), $k, pointer-to($src), @src.elems, nativesizeof(uint64));
      return $out.list;
    }
    when Num|Rat {
      my CArray[num64] $src .= new: @src».Num;
      my $out = CArray[num64].allocate: $k;
      gsl_ran_sample($r.r, pointer-to($out), $k, pointer-to($src), @src.elems, nativesizeof(num64));
      return $out.list;
    }
  }
}

=begin pod

=head1 NAME

Math::Libgsl::RandomDistribution - An interface to libgsl, the Gnu Scientific Library - Random Number Distributions

=head1 SYNOPSIS

=begin code :lang<perl6>

use Math::Libgsl::Random;
use Math::Libgsl::RandomDistribution;

my Math::Libgsl::Random $r .= new;
say poisson($r, 3) for ^10;

=end code

=head1 DESCRIPTION

Math::Libgsl::RandomDistribution is an interface to the Random Number Distributions section of libgsl, the Gnu Scientific Library.

=head3 gaussian(Math::Libgsl::Random $r, Num() $sigma --> Num)

This function returns a Gaussian random variate, with mean zero and standard deviation $sigma.

=head3 gaussian-pdf(Num() $x, Num() $sigma --> Num)

This function computes the probability density at $x for a Gaussian distribution with standard deviation $sigma.

=head3 gaussian-ziggurat(Math::Libgsl::Random $r, Num() $sigma --> Num)
=head3 gaussian-ratio-method(Math::Libgsl::Random $r, Num() $sigma --> Num)

This function computes a Gaussian random variate using the alternative Marsaglia-Tsang ziggurat and Kinderman-Monahan-Leva ratio methods.

=head3 ugaussian(Math::Libgsl::Random $r --> Num)
=head3 ugaussian-pdf(Num() $x --> Num)
=head3 ugaussian-ratio-method(Math::Libgsl::Random $r --> Num)

These functions compute results for the unit Gaussian distribution. They are equivalent to the functions above with $sigma = 1.

=head3 gaussian-P(Num() $x, Num() $sigma --> Num)
=head3 gaussian-Q(Num() $x, Num() $sigma --> Num)
=head3 gaussian-Pinv(Num() $P, Num() $sigma --> Num)
=head3 gaussian-Qinv(Num() $Q, Num() $sigma --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses.

=head3 ugaussian-P(Num() $x --> Num)
=head3 ugaussian-Q(Num() $x --> Num)
=head3 ugaussian-Pinv(Num() $P --> Num)
=head3 ugaussian-Qinv(Num() $Q --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the unit Gaussian distribution.

=head3 gaussian-tail(Math::Libgsl::Random $r, Num() $a where * > 0, Num() $sigma --> Num)

This function provides random variates from the upper tail of a Gaussian distribution with standard deviation $sigma.

=head3 gaussian-tail-pdf(Num() $x, Num() $a where * > 0, Num() $sigma --> Num)

This function computes the probability density p(x) at $x for a Gaussian tail distribution with standard deviation $sigma and lower limit $a.

=head3 ugaussian-tail(Math::Libgsl::Random $r, Num() $a where * > 0 --> Num)
=head3 ugaussian-tail-pdf(Num() $x, Num() $a where * > 0 --> Num)

These functions compute results for the tail of a unit Gaussian distribution.

=head3 bivariate-gaussian(Math::Libgsl::Random $r, Num() $sigma-x, Num() $sigma-y, Num() $rho where -1 < * < 1 --> List)

This function generates a pair of correlated Gaussian variates, with mean zero, correlation coefficient $rho and
standard deviations $sigma_x and $sigma_y.
The function returns two Num values: the Gauusian correlates along the x and y directions.

=head3 bivariate-gaussian-pdf(Num() $x, Num() $y, Num() $sigma-x, Num() $sigma-y, Num() $rho where -1 < * < 1 --> Num)

This function computes the probability density p(x, y) at ($x, $y) for a bivariate Gaussian distribution with standard deviations $sigma_x, $sigma_y and correlation coefficient $rho.

=head3 multivariate-gaussian(Math::Libgsl::Random $r, Math::Libgsl::Vector $mu, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $L.matrix.size1 == $mu.vector.size } --> Math::Libgsl::Vector)

This function generates a random vector satisfying the k-dimensional multivariate Gaussian distribution with mean μ and variance-covariance matrix Σ. On input, the k-vector μ is given in $mu, and the Cholesky factor of the k-by-k matrix Σ = LT(L) is given in the lower triangle of $L, as output from Math::Libgsl::LinearAlgebra::cholesky-decomp().
The function returns the random vector as a Math::Libgsl::Vector object.

=head3 multivariate-gaussian-pdf(Math::Libgsl::Vector $x, Math::Libgsl::Vector $mu where *.vector.size == $x.vector.size, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $L.matrix.size1 == $mu.vector.size } --> Num)
=head3 multivariate-gaussian-log-pdf(Math::Libgsl::Vector $x, Math::Libgsl::Vector $mu where *.vector.size == $x.vector.size, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $L.matrix.size1 == $mu.vector.size } --> Num)

These functions compute p(x) or log p(x) at the point $x, using mean vector $mu and variance-covariance matrix specified by its Cholesky factor $L.

=head3 multivariate-gaussian-mean(Math::Libgsl::Matrix $X --> Math::Libgsl::Vector)

Given a set of n samples Xⱼ from a k-dimensional multivariate Gaussian distribution, this function computes the maximum likelihood estimate of the mean of the distribution.
The function returns the maximum likelihood estimate as a Math::Libgsl::Vector object.

=head3 multivariate-gaussian-vcov(Math::Libgsl::Matrix $X --> Math::Libgsl::Matrix)

Given a set of n samples Xⱼ from a k-dimensional multivariate Gaussian distribution, this function computes the maximum likelihood estimate of the variance-covariance matrix of the distribution.
The function returns the maximum likelihood estimate of the variance-covariance matrix as a Math::Libgsl::Matrix object.

=head3 exponential(Math::Libgsl::Random $r, Num() $mu --> Num)

This function returns a random variate from the exponential distribution with mean $mu.

=head3 exponential-pdf(Num() $x, Num() $mu --> Num)

This function computes the probability density p(x) at $x for an exponential distribution with mean $mu.

=head3 exponential-P(Num() $x, Num() $mu --> Num)
=head3 exponential-Q(Num() $x, Num() $mu --> Num)
=head3 exponential-Pinv(Num() $P, Num() $mu --> Num)
=head3 exponential-Qinv(Num() $Q, Num() $mu --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the exponential distribution with mean $mu.

=head3 laplace(Math::Libgsl::Random $r, Num() $a --> Num)

This function returns a random variate from the Laplace distribution with width $a.

=head3 laplace-pdf(Num() $x, Num() $a --> Num)

This function computes the probability density p(x) at $x for a Laplace distribution with width $a.

=head3 laplace-P(Num() $x, Num() $a --> Num)
=head3 laplace-Q(Num() $x, Num() $a --> Num)
=head3 laplace-Pinv(Num() $P, Num() $a --> Num)
=head3 laplace-Qinv(Num() $Q, Num() $a --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Laplace distribution with width $a.

=head3 exppow(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the exponential power distribution with scale parameter $a and exponent $b.

=head3 exppow-pdf(Num() $x, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for an exponential power distribution with scale parameter $a and exponent $b.

=head3 exppow-P(Num() $x, Num() $a, Num() $b --> Num)
=head3 exppow-Q(Num() $x, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) for the exponential power distribution with parameters $a and $b.

=head3 cauchy(Math::Libgsl::Random $r, Num() $a --> Num)

This function returns a random variate from the Cauchy distribution with scale parameter $a.

=head3 cauchy-pdf(Num() $x, Num() $a --> Num)

This function computes the probability density p(x) at $x for a Cauchy distribution with scale parameter $a.

=head3 cauchy-P(Num() $x, Num() $a --> Num)
=head3 cauchy-Q(Num() $x, Num() $a --> Num)
=head3 cauchy-Pinv(Num() $P, Num() $a --> Num)
=head3 cauchy-Qinv(Num() $Q, Num() $a --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Cauchy distribution with scale parameter $a.

=head3 rayleigh(Math::Libgsl::Random $r, Num() $sigma --> Num)

This function returns a random variate from the Rayleigh distribution with scale parameter $sigma.

=head3 rayleigh-pdf(Num() $x, Num() $sigma --> Num)

This function computes the probability density p(x) at $x for a Rayleigh distribution with scale parameter $sigma.

=head3 rayleigh-P(Num() $x, Num() $sigma --> Num)
=head3 rayleigh-Q(Num() $x, Num() $sigma --> Num)
=head3 rayleigh-Pinv(Num() $P, Num() $sigma --> Num)
=head3 rayleigh-Qinv(Num() $Q, Num() $sigma --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Rayleigh distribution with scale parameter $sigma.

=head3 rayleigh-tail(Math::Libgsl::Random $r, Num() $a, Num() $sigma --> Num)

This function returns a random variate from the tail of the Rayleigh distribution with scale parameter $sigma and a lower limit of $a.

=head3 rayleigh-tail-pdf(Num() $x, Num() $a, Num() $sigma --> Num)

This function computes the probability density p(x) at $x for a Rayleigh tail distribution with scale parameter $sigma and lower limit $a.

=head3 landau(Math::Libgsl::Random $r --> Num)

This function returns a random variate from the Landau distribution.

=head3 landau-pdf(Num() $x --> Num)

This function computes the probability density p(x) at $x for the Landau distribution.

=head3 levy(Math::Libgsl::Random $r, Num() $c, Num() $alpha where 0 < * ≤ 2 --> Num)

This function returns a random variate from the Levy symmetric stable distribution with scale $c and exponent $alpha.

=head3 levy-skew(Math::Libgsl::Random $r, Num() $c, Num() $alpha where 0 < * ≤ 2, Num() $beta --> Num)

This function returns a random variate from the Levy skew stable distribution with scale $c, exponent $alpha and skewness parameter $beta.

=head3 gamma(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the gamma distribution.

=head3 gamma-knuth(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a gamma variate using the algorithms from Knuth.

=head3 gamma-pdf(Num() $x, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a gamma distribution with parameters $a and $b.

=head3 gamma-P(Num() $x, Num() $a, Num() $b --> Num)
=head3 gamma-Q(Num() $x, Num() $a, Num() $b --> Num)
=head3 gamma-Pinv(Num() $P, Num() $a, Num() $b --> Num)
=head3 gamma-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the gamma distribution with parameters $a and $b.

=head3 flat(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the flat (uniform) distribution from $a to $b.

=head3 flat-pdf(Num() $x, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a uniform distribution from $a to $b.

=head3 flat-P(Num() $x, Num() $a, Num() $b --> Num)
=head3 flat-Q(Num() $x, Num() $a, Num() $b --> Num)
=head3 flat-Pinv(Num() $P, Num() $a, Num() $b --> Num)
=head3 flat-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for a uniform distribution from $a to $b.

=head3 lognormal(Math::Libgsl::Random $r, Num() $zeta, Num() $sigma --> Num)

This function returns a random variate from the lognormal distribution.

=head3 lognormal-pdf(Num() $x, Num() $zeta, Num() $sigma --> Num)

This function computes the probability density p(x) at $x for a lognormal distribution with parameters $zeta and $sigma.

=head3 lognormal-P(Num() $x, Num() $zeta, Num() $sigma --> Num)
=head3 lognormal-Q(Num() $x, Num() $zeta, Num() $sigma --> Num)
=head3 lognormal-Pinv(Num() $P, Num() $zeta, Num() $sigma --> Num)
=head3 lognormal-Qinv(Num() $Q, Num() $zeta, Num() $sigma --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the lognormal distribution with parameters $zeta and $sigma.

=head3 chisq(Math::Libgsl::Random $r, Num() $nu --> Num)

This function returns a random variate from the chi-squared distribution with $nu degrees of freedom.

=head3 chisq-pdf(Num() $x where * ≥ 0, Num() $nu --> Num)

This function computes the probability density p(x) at $x for a chi-squared distribution with $nu degrees of freedom.

=head3 chisq-P(Num() $x where * ≥ 0, Num() $nu --> Num)
=head3 chisq-Q(Num() $x where * ≥ 0, Num() $nu --> Num)
=head3 chisq-Pinv(Num() $P, Num() $nu --> Num)
=head3 chisq-Qinv(Num() $Q, Num() $nu --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the chi-squared distribution with $nu degrees of freedom.

=head3 fdist(Math::Libgsl::Random $r, Num() $nu1, Num() $nu2 --> Num)

This function returns a random variate from the F-distribution with degrees of freedom $nu1 and $nu2.

=head3 fdist-pdf(Num() $x where * ≥ 0, Num() $nu1, Num() $nu2 --> Num)

This function computes the probability density p(x) at $x for an F-distribution with $nu1 and $nu2 degrees of freedom.

=head3 fdist-P(Num() $x where * ≥ 0, Num() $nu1, Num() $nu2 --> Num)
=head3 fdist-Q(Num() $x where * ≥ 0, Num() $nu1, Num() $nu2 --> Num)
=head3 fdist-Pinv(Num() $P, Num() $nu1, Num() $nu2 --> Num)
=head3 fdist-Qinv(Num() $Q, Num() $nu1, Num() $nu2 --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the F-distribution with $nu1 and $nu2 degrees of freedom.

=head3 tdist(Math::Libgsl::Random $r, Num() $nu --> Num)

This function returns a random variate from the t-distribution.

=head3 tdist-pdf(Num() $x, Num() $nu --> Num)

This function computes the probability density p(x) at $x for a t-distribution with $nu degrees of freedom.

=head3 tdist-P(Num() $x, Num() $nu --> Num)
=head3 tdist-Q(Num() $x, Num() $nu --> Num)
=head3 tdist-Pinv(Num() $P, Num() $nu --> Num)
=head3 tdist-Qinv(Num() $Q, Num() $nu --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the t-distribution with $nu degrees of freedom.

=head3 beta(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the beta distribution.

=head3 beta-pdf(Num() $x, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a beta distribution with parameters $a and $b.

=head3 beta-P(Num() $x, Num() $a, Num() $b --> Num)
=head3 beta-Q(Num() $x, Num() $a, Num() $b --> Num)
=head3 beta-Pinv(Num() $P, Num() $a, Num() $b --> Num)
=head3 beta-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the beta distribution with parameters $a and $b.

=head3 logistic(Math::Libgsl::Random $r, Num() $a --> Num)

This function returns a random variate from the logistic distribution.

=head3 logistic-pdf(Num() $x, Num() $a --> Num)

This function computes the probability density p(x) at $x for a logistic distribution with scale parameter $a.

=head3 logistic-P(Num() $x, Num() $a --> Num)
=head3 logistic-Q(Num() $x, Num() $a --> Num)
=head3 logistic-Pinv(Num() $P, Num() $a --> Num)
=head3 logistic-Qinv(Num() $Q, Num() $a --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the logistic distribution with scale parameter $a.

=head3 pareto(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the Pareto distribution of order $a and scale $b.

=head3 pareto-pdf(Num() $x, Num() $a, Num() $b where { $x ≥ $b } --> Num)

This function computes the probability density p(x) at $x for a Pareto distribution with exponent $a and scale $b.

=head3 pareto-P(Num() $x, Num() $a, Num() $b where { $x ≥ $b } --> Num)
=head3 pareto-Q(Num() $x, Num() $a, Num() $b where { $x ≥ $b } --> Num)
=head3 pareto-Pinv(Num() $P, Num() $a, Num() $b --> Num)
=head3 pareto-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Pareto distribution with exponent $a and scale $b.

=head3 d2dir(Math::Libgsl::Random $r --> List)
=head3 d2dir-trig-method(Math::Libgsl::Random $r --> List)

This function returns a random direction vector v = (x, y) in two dimensions.
The return value is a List of two Num(s): the x and y components of the 2D vector.

=head3 d3dir(Math::Libgsl::Random $r --> List)

This function returns a random direction vector v = (x, y, z) in three dimensions.
The return value is a List of three Num(s): the x, y, and z components of the 3D vector.

=head3 dndir(Math::Libgsl::Random $r, Int $n --> List)

This function returns a random direction vector v = (x 1 , x 2 , . . . , x n ) in $n dimensions.
The return value is a List of $n Num(s): the components of the n-D vector.

=head3 weibull(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the Weibull distribution.

=head3 weibull-pdf(Num() $x where $x ≥ 0, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a Weibull distribution with scale $a and exponent $b.

=head3 weibull-P(Num() $x where $x ≥ 0, Num() $a, Num() $b --> Num)
=head3 weibull-Q(Num() $x where $x ≥ 0, Num() $a, Num() $b --> Num)
=head3 weibull-Pinv(Num() $P, Num() $a, Num() $b --> Num)
=head3 weibull-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Weibull distribution with scale $a and exponent $b.

=head3 gumbel1(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the Type-1 Gumbel distribution.

=head3 gumbel1-pdf(Num() $x, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a Type-1 Gumbel distribution with parameters $a and $b.

=head3 gumbel1-P(Num() $x, Num() $a, Num() $b --> Num)
=head3 gumbel1-Q(Num() $x, Num() $a, Num() $b --> Num)
=head3 gumbel1-Pinv(Num() $P, Num() $a, Num() $b --> Num)
=head3 gumbel1-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Type-1 Gumbel distribution with parameters $a and $b.

=head3 gumbel2(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the Type-2 Gumbel distribution.

=head3 gumbel2-pdf(Num() $x where * > 0, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a Type-2 Gumbel distribution with parameters $a and $b.

=head3 gumbel2-P(Num() $x where * > 0, Num() $a, Num() $b --> Num)
=head3 gumbel2-Q(Num() $x where * > 0, Num() $a, Num() $b --> Num)
=head3 gumbel2-Pinv(Num() $P, Num() $a, Num() $b --> Num)
=head3 gumbel2-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Type-2 Gumbel distribution with parameters $a and $b.

=head3 dirichlet(Math::Libgsl::Random $r, Int $K, @alpha where *.all > 0 --> List)

This function returns an array of K random variates as a List of Num(s) from a Dirichlet distribution of order K-1.

=head3 dirichlet-pdf(Int $K, @alpha where *.all > 0, @theta where *.all ≥ 0 --> Num)

This function computes the probability density p(θ₁, …, θₖ) at theta[K] for a Dirichlet distribution with parameters alpha[K].

=head3 dirichlet-lnpdf(Int $K, @alpha where *.all > 0, @theta where *.all ≥ 0 --> Num)

This function computes the logarithm of the probability density p(θ₁, …, θₖ) at theta[K] for a Dirichlet distribution with parameters alpha[K].

=head3 poisson(Math::Libgsl::Random $r, Num() $mu --> UInt)

This function returns a random integer from the Poisson distribution with mean $mu.

=head3 poisson-pdf(UInt $k, Num() $mu --> Num)

This function computes the probability p(k) of obtaining $k from a Poisson distribution with mean $mu.

=head3 poisson-P(UInt $k, Num() $mu --> Num)
=head3 poisson-Q(UInt $k, Num() $mu --> Num)

These functions compute the cumulative distribution functions P(k), Q(k) for the Poisson distribution with parameter $mu.

=head3 bernoulli(Math::Libgsl::Random $r, Num() $p --> Int)

This function returns either 0 or 1, the result of a Bernoulli trial with probability $p.

=head3 bernoulli-pdf(UInt $k, Num() $p --> Num)

This function computes the probability p(k) of obtaining $k from a Bernoulli distribution with probability parameter $p.

=head3 binomial(Math::Libgsl::Random $r, Num() $p, UInt $n --> Int)

This function returns a random integer from the binomial distribution, the number of successes in $n independent trials with probability $p.

=head3 binomial-pdf(UInt $k, Num() $p, UInt $n where * ≥ $k --> Num)

This function computes the probability p(k) of obtaining $k from a binomial distribution with parameters $p and $n.

=head3 binomial-P(UInt $k, Num() $p, UInt $n where * ≥ $k --> Num)
=head3 binomial-Q(UInt $k, Num() $p, UInt $n where * ≥ $k --> Num)

These functions compute the cumulative distribution functions P (k), Q(k) for the binomial distribution with parameters $p and $n.

=head3 multinomial(Math::Libgsl::Random $r, Int $K, UInt $N, *@p where { @p.all ~~ Numeric } --> List)

This function returns a random sample from the multinomial distribution formed by $N trials from an underlying distribution p[K] as a List of UInt(s).

=head3 multinomial-pdf(Int $K, @p where { @p.all ~~ Numeric }, *@n where { @n.all ~~ UInt } --> Num)

This function computes the probability P(n₁, n₂, …, nₖ) of sampling n[K] from a multinomial distribution with parameters p[K].

=head3 multinomial-lnpdf(Int $K, @p where { @p.all ~~ Numeric }, *@n where { @n.all ~~ UInt } --> Num)

This function computes the logarithm of the probability P(n₁, n₂, …, nₖ) for the multinomial distribution with parameters p[K].

=head3 negative-binomial(Math::Libgsl::Random $r, Num() $p, Num() $n --> UInt)

This function returns a random integer from the negative binomial distribution.

=head3 negative-binomial-pdf(UInt $k, Num() $p, Num() $n --> Num)

This function computes the probability p(k) of obtaining $k from a negative binomial distribution with parameters $p and $n.

=head3 negative-binomial-P(UInt $k, Num() $p, Num() $n --> Num)
=head3 negative-binomial-Q(UInt $k, Num() $p, Num() $n --> Num)

These functions compute the cumulative distribution functions P(k), Q(k) for the negative binomial distribution with parameters $p and $n.

=head3 pascal(Math::Libgsl::Random $r, Num() $p, UInt $n --> UInt)

This function returns a random integer from the Pascal distribution.

=head3 pascal-pdf(UInt $k, Num() $p, UInt $n --> Num)

This function computes the probability p(k) of obtaining $k from a Pascal distribution with parameters $p and $n.

=head3 pascal-P(UInt $k, Num() $p, UInt $n --> Num)
=head3 pascal-Q(UInt $k, Num() $p, UInt $n --> Num)

These functions compute the cumulative distribution functions P(k), Q(k) for the Pascal distribution with parameters $p and $n.

=head3 geometric(Math::Libgsl::Random $r, Num() $p --> UInt)

This function returns a random integer from the geometric distribution, the number of independent trials with probability $p until the first success.

=head3 geometric-pdf(UInt $k where * ≥ 1, Num() $p --> Num)

This function computes the probability p(k) of obtaining $k from a geometric distribution with probability parameter $p.

=head3 geometric-P(UInt $k where * ≥ 1, Num() $p --> Num)
=head3 geometric-Q(UInt $k where * ≥ 1, Num() $p --> Num)

These functions compute the cumulative distribution functions P(k), Q(k) for the geometric distribution with parameter $p.

=head3 hypergeometric(Math::Libgsl::Random $r, UInt $n1, UInt $n2, UInt $t --> UInt)

This function returns a random integer from the hypergeometric distribution.

=head3 hypergeometric-pdf(UInt $k, UInt $n1, UInt $n2, UInt $t --> Num)

This function computes the probability p(k) of obtaining $k from a hypergeometric distribution with parameters $n1, $n2, $t.

=head3 hypergeometric-P(UInt $k, UInt $n1, UInt $n2, UInt $t --> Num)
=head3 hypergeometric-Q(UInt $k, UInt $n1, UInt $n2, UInt $t --> Num)

These functions compute the cumulative distribution functions P(k), Q(k) for the hypergeometric distribution with parameters $n1, $n2 and $t.

=head3 logarithmic(Math::Libgsl::Random $r, Num() $p --> UInt)

This function returns a random integer from the logarithmic distribution.

=head3 logarithmic-pdf(UInt $k where * ≥ 1, Num() $p --> Num)

This function computes the probability p(k) of obtaining $k from a logarithmic distribution with probability parameter $p.

=head3 wishart(Math::Libgsl::Random $r, Num() $n, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $n ≤ $L.matrix.size1 - 1 } --> Math::Libgsl::Matrix)

This function returns a random symmetric p-by-p matrix from the Wishart distribution as a Math::Libgsl::Matrix object.

=head3 wishart-pdf(Math::Libgsl::Matrix $X where { $X.matrix.size1 == $X.matrix.size2 }, Math::Libgsl::Matrix $L-X where { $L-X.matrix.size1 == $L-X.matrix.size2 }, Num() $n where * > $X.matrix.size1 - 1, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $X.matrix.size1 == $L.matrix.size1 && $L-X.matrix.size1 == $L.matrix.size1 } --> Num)
=head3 wishart-log-pdf(Math::Libgsl::Matrix $X where { $X.matrix.size1 == $X.matrix.size2 }, Math::Libgsl::Matrix $L-X where { $L-X.matrix.size1 == $L-X.matrix.size2 }, Num() $n where * > $X.matrix.size1 - 1, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $X.matrix.size1 == $L.matrix.size1 && $L-X.matrix.size1 == $L.matrix.size1 } --> Num)

These functions return p(X) or log p(X) for the p-by-p matrix $X, whose Cholesky factor is specified in $L-X.
The degrees of freedom is given by $n, the Cholesky factor of the scale matrix V is specified in $L.

=head3 shuffle(Math::Libgsl::Random $r, @base --> List)

This function randomly shuffles the order of the elements of the array @base and returns them as a List.

=head3 choose(Math::Libgsl::Random $r, Int $k, @src where *.elems ≥ $k --> List)

This function returns the list of $k objects taken randomly from the elements of the array @src.

=head3 sample(Math::Libgsl::Random $r, Int $k, @src --> List)

This function is like choose() but samples $k items from the original array of n items @src with replacement, so the same object can appear more than once in the output.

=head2 Class Math::Libgsl::RandomDistribution::Discrete

This probability distribution needs a lookup table for the discrete random number generator, so it's implemented as a class, which hides the implementation details.

=begin code :lang<perl6>

use Math::Libgsl::RandomDistribution;
use Math::Libgsl::Random;

my Int $size = 3;
my @probability = .59, .4, .01;
my Math::Libgsl::Random $r .= new;
my Math::Libgsl::RandomDistribution::Discrete $d .= new: :$size, :@probability;

say "Using probability values: { $d.probability }";
say $d.discrete($r) for ^10;

say $d.discrete-pdf(2);

=end code

=head3 new(Int() :$size, :@probability where *.all > 0)

Creates the lookup table for the discrete random number generator. The array @probability contains the probabilities of the discrete events.

=head3 discrete(Math::Libgsl::Random $r --> Int)

This method returns one discrete random number.

=head3 discrete-pdf(Int $k --> Num)

This method returns the probability P[k] of observing the variable $k.

=head1 C Library Documentation

For more details on libgsl see L<https://www.gnu.org/software/gsl/>.
The excellent C Library manual is available here L<https://www.gnu.org/software/gsl/doc/html/index.html>, or here L<https://www.gnu.org/software/gsl/doc/latex/gsl-ref.pdf> in PDF format.

=head1 Prerequisites

This module requires the libgsl library to be installed. Please follow the instructions below based on your platform:

=head2 Debian Linux and Ubuntu 20.04

=begin code
sudo apt install libgsl23 libgsl-dev libgslcblas0
=end code

That command will install libgslcblas0 as well, since it's used by the GSL.

=head2 Ubuntu 18.04

libgsl23 and libgslcblas0 have a missing symbol on Ubuntu 18.04.
I solved the issue installing the Debian Buster version of those three libraries:

=item L<http://http.us.debian.org/debian/pool/main/g/gsl/libgslcblas0_2.5+dfsg-6_amd64.deb>
=item L<http://http.us.debian.org/debian/pool/main/g/gsl/libgsl23_2.5+dfsg-6_amd64.deb>
=item L<http://http.us.debian.org/debian/pool/main/g/gsl/libgsl-dev_2.5+dfsg-6_amd64.deb>

=head1 Installation

To install it using zef (a module management tool):

=begin code
$ zef install Math::Libgsl::RandomDistribution
=end code

=head1 AUTHOR

Fernando Santagata <nando.santagata@gmail.com>

=head1 COPYRIGHT AND LICENSE

Copyright 2020 Fernando Santagata

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

=end pod
