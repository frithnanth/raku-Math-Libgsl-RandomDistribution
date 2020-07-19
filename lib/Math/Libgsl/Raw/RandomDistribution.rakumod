use v6;

unit module Math::Libgsl::Raw::RandomDistribution:ver<0.0.1>:auth<cpan:FRITH>;

use NativeCall;
use Math::Libgsl::Raw::Matrix :ALL;
use Math::Libgsl::Raw::Random :ALL;

sub LIB {
  run('/sbin/ldconfig', '-p', :chomp, :out)
    .out
    .slurp(:close)
    .split("\n")
    .grep(/^ \s+ libgsl\.so\. \d+ /)
    .sort
    .head
    .comb(/\S+/)
    .head;
}

class gsl_ran_discrete_t is repr('CStruct') is export {
  has size_t          $.K;
  has Pointer[size_t] $.A;
  has Pointer[num64]  $.F;
}

# Gaussian Distribution
sub gsl_ran_gaussian(gsl_rng $r, num64 $sigma --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_ran_gaussian_pdf(num64 $x, num64 $sigma --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_ran_gaussian_ziggurat(gsl_rng $r, num64 $sigma --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_ran_gaussian_ratio_method(gsl_rng $r, num64 $sigma --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_ran_ugaussian(gsl_rng $r --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_ran_ugaussian_pdf(num64 $x --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_ran_ugaussian_ratio_method(gsl_rng $r --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_cdf_gaussian_P(num64 $x, num64 $sigma --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_cdf_gaussian_Q(num64 $x, num64 $sigma --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_cdf_gaussian_Pinv(num64 $P, num64 $sigma --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_cdf_gaussian_Qinv(num64 $Q, num64 $sigma --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_cdf_ugaussian_P(num64 $x --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_cdf_ugaussian_Q(num64 $x --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_cdf_ugaussian_Pinv(num64 $P --> num64) is native(&LIB) is export(:gauss) { * }
sub gsl_cdf_ugaussian_Qinv(num64 $Q --> num64) is native(&LIB) is export(:gauss) { * }
# Gaussian Tail Distribution
sub gsl_ran_gaussian_tail(gsl_rng $r, num64 $a, num64 $sigma --> num64) is native(&LIB) is export(:gausstail) { * }
sub gsl_ran_gaussian_tail_pdf(num64 $x, num64 $a, num64 $sigma --> num64) is native(&LIB) is export(:gausstail) { * }
sub gsl_ran_ugaussian_tail(gsl_rng $r, num64 $a --> num64) is native(&LIB) is export(:gausstail) { * }
sub gsl_ran_ugaussian_tail_pdf(num64 $x, num64 $a --> num64) is native(&LIB) is export(:gausstail) { * }
# Bivariate Gaussian Distribution
sub gsl_ran_bivariate_gaussian(gsl_rng $r, num64 $sigma_x, num64 $sigma_y, num64 $rho, num64 $x is rw, num64 $y is rw) is native(&LIB) is export(:gaussbivar) { * }
sub gsl_ran_bivariate_gaussian_pdf(num64 $x, num64 $y, num64 $sigma_x, num64 $sigma_y, num64 $rho --> num64) is native(&LIB) is export(:gaussbivar) { * }
# Multivariate Gaussian Distribution
sub gsl_ran_multivariate_gaussian(gsl_rng $r, gsl_vector $mu, gsl_matrix $L, gsl_vector $result --> int32) is native(&LIB) is export(:gaussmulti) { * }
sub gsl_ran_multivariate_gaussian_pdf(gsl_vector $x, gsl_vector $mu, gsl_matrix $L, num64 $result is rw, gsl_vector $work --> int32) is native(&LIB) is export(:gaussmulti) { * }
sub gsl_ran_multivariate_gaussian_log_pdf(gsl_vector $x, gsl_vector $mu, gsl_matrix $L, num64 $result is rw, gsl_vector $work --> int32) is native(&LIB) is export(:gaussmulti) { * }
sub gsl_ran_multivariate_gaussian_mean(gsl_matrix $X, gsl_vector $mu_hat --> int32) is native(&LIB) is export(:gaussmulti) { * }
sub gsl_ran_multivariate_gaussian_vcov(gsl_matrix $X, gsl_matrix $sigma_hat --> int32) is native(&LIB) is export(:gaussmulti) { * }
# Exponential Distribution
sub gsl_ran_exponential(gsl_rng $r, num64 $mu --> num64) is native(&LIB) is export(:exp) { * }
sub gsl_ran_exponential_pdf(num64 $x, num64 $mu --> num64) is native(&LIB) is export(:exp) { * }
sub gsl_cdf_exponential_P(num64 $x, num64 $mu --> num64) is native(&LIB) is export(:exp) { * }
sub gsl_cdf_exponential_Q(num64 $x, num64 $mu --> num64) is native(&LIB) is export(:exp) { * }
sub gsl_cdf_exponential_Pinv(num64 $P, num64 $mu --> num64) is native(&LIB) is export(:exp) { * }
sub gsl_cdf_exponential_Qinv(num64 $Q, num64 $mu --> num64) is native(&LIB) is export(:exp) { * }
# Laplace Distribution
sub gsl_ran_laplace(gsl_rng $r, num64 $a --> num64) is native(&LIB) is export(:laplace) { * }
sub gsl_ran_laplace_pdf(num64 $x, num64 $a --> num64) is native(&LIB) is export(:laplace) { * }
sub gsl_cdf_laplace_P(num64 $x, num64 $a --> num64) is native(&LIB) is export(:laplace) { * }
sub gsl_cdf_laplace_Q(num64 $x, num64 $a --> num64) is native(&LIB) is export(:laplace) { * }
sub gsl_cdf_laplace_Pinv(num64 $P, num64 $a --> num64) is native(&LIB) is export(:laplace) { * }
sub gsl_cdf_laplace_Qinv(num64 $Q, num64 $a --> num64) is native(&LIB) is export(:laplace) { * }
# Exponential Power Distribution
sub gsl_ran_exppow(gsl_rng $r, num64 $a, num64 $b --> num64) is native(&LIB) is export(:exppow) { * }
sub gsl_ran_exppow_pdf(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:exppow) { * }
sub gsl_cdf_exppow_P(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:exppow) { * }
sub gsl_cdf_exppow_Q(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:exppow) { * }
# Cauchy Distribution
sub gsl_ran_cauchy(gsl_rng $r, num64 $a --> num64) is native(&LIB) is export(:cauchy) { * }
sub gsl_ran_cauchy_pdf(num64 $x, num64 $a --> num64) is native(&LIB) is export(:cauchy) { * }
sub gsl_cdf_cauchy_P(num64 $x, num64 $a --> num64) is native(&LIB) is export(:cauchy) { * }
sub gsl_cdf_cauchy_Q(num64 $x, num64 $a --> num64) is native(&LIB) is export(:cauchy) { * }
sub gsl_cdf_cauchy_Pinv(num64 $P, num64 $a --> num64) is native(&LIB) is export(:cauchy) { * }
sub gsl_cdf_cauchy_Qinv(num64 $Q, num64 $a --> num64) is native(&LIB) is export(:cauchy) { * }
# Rayleigh Distribution
sub gsl_ran_rayleigh(gsl_rng $r, num64 $sigma --> num64) is native(&LIB) is export(:rayleigh) { * }
sub gsl_ran_rayleigh_pdf(num64 $x, num64 $sigma --> num64) is native(&LIB) is export(:rayleigh) { * }
sub gsl_cdf_rayleigh_P(num64 $x, num64 $sigma --> num64) is native(&LIB) is export(:rayleigh) { * }
sub gsl_cdf_rayleigh_Q(num64 $x, num64 $sigma --> num64) is native(&LIB) is export(:rayleigh) { * }
sub gsl_cdf_rayleigh_Pinv(num64 $P, num64 $sigma --> num64) is native(&LIB) is export(:rayleigh) { * }
sub gsl_cdf_rayleigh_Qinv(num64 $Q, num64 $sigma --> num64) is native(&LIB) is export(:rayleigh) { * }
# Rayleigh Tail Distribution
sub gsl_ran_rayleigh_tail(gsl_rng $r, num64 $a, num64 $sigma --> num64) is native(&LIB) is export(:rayleigh) { * }
sub gsl_ran_rayleigh_tail_pdf(num64 $x, num64 $a, num64 $sigma --> num64) is native(&LIB) is export(:rayleigh) { * }
# Landau Distribution
sub gsl_ran_landau(gsl_rng $r --> num64) is native(&LIB) is export(:landau) { * }
sub gsl_ran_landau_pdf(num64 $x --> num64) is native(&LIB) is export(:landau) { * }
# Levy alpha-Stable Distribution
sub gsl_ran_levy(gsl_rng $r, num64 $c, num64 $alpha --> num64) is native(&LIB) is export(:levy) { * }
# Levy skew alpha-Stable Distribution
sub gsl_ran_levy_skew(gsl_rng $r, num64 $c, num64 $alpha, num64 $beta --> num64) is native(&LIB) is export(:levy) { * }
# Gamma Distribution
sub gsl_ran_gamma(gsl_rng $r, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gamma) { * }
sub gsl_ran_gamma_knuth(gsl_rng $r, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gamma) { * }
sub gsl_ran_gamma_pdf(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gamma) { * }
sub gsl_cdf_gamma_P(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gamma) { * }
sub gsl_cdf_gamma_Q(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gamma) { * }
sub gsl_cdf_gamma_Pinv(num64 $P, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gamma) { * }
sub gsl_cdf_gamma_Qinv(num64 $Q, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gamma) { * }
# Flat (Uniform) Distribution
sub gsl_ran_flat(gsl_rng $r, num64 $a, num64 $b --> num64) is native(&LIB) is export(:flat) { * }
sub gsl_ran_flat_pdf(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:flat) { * }
sub gsl_cdf_flat_P(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:flat) { * }
sub gsl_cdf_flat_Q(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:flat) { * }
sub gsl_cdf_flat_Pinv(num64 $P, num64 $a, num64 $b --> num64) is native(&LIB) is export(:flat) { * }
sub gsl_cdf_flat_Qinv(num64 $Q, num64 $a, num64 $b --> num64) is native(&LIB) is export(:flat) { * }
# Lognormal Distribution
sub gsl_ran_lognormal(gsl_rng $r, num64 $zeta, num64 $sigma --> num64) is native(&LIB) is export(:lognormal) { * }
sub gsl_ran_lognormal_pdf(num64 $x, num64 $zeta, num64 $sigma --> num64) is native(&LIB) is export(:lognormal) { * }
sub gsl_cdf_lognormal_P(num64 $x, num64 $zeta, num64 $sigma --> num64) is native(&LIB) is export(:lognormal) { * }
sub gsl_cdf_lognormal_Q(num64 $x, num64 $zeta, num64 $sigma --> num64) is native(&LIB) is export(:lognormal) { * }
sub gsl_cdf_lognormal_Pinv(num64 $P, num64 $zeta, num64 $sigma --> num64) is native(&LIB) is export(:lognormal) { * }
sub gsl_cdf_lognormal_Qinv(num64 $Q, num64 $zeta, num64 $sigma --> num64) is native(&LIB) is export(:lognormal) { * }
# Chi-squared Distribution
sub gsl_ran_chisq(gsl_rng $r, num64 $nu --> num64) is native(&LIB) is export(:chisq) { * }
sub gsl_ran_chisq_pdf(num64 $x, num64 $nu --> num64) is native(&LIB) is export(:chisq) { * }
sub gsl_cdf_chisq_P(num64 $x, num64 $nu --> num64) is native(&LIB) is export(:chisq) { * }
sub gsl_cdf_chisq_Q(num64 $x, num64 $nu --> num64) is native(&LIB) is export(:chisq) { * }
sub gsl_cdf_chisq_Pinv(num64 $P, num64 $nu --> num64) is native(&LIB) is export(:chisq) { * }
sub gsl_cdf_chisq_Qinv(num64 $Q, num64 $nu --> num64) is native(&LIB) is export(:chisq) { * }
# F-distribution
sub gsl_ran_fdist(gsl_rng $r, num64 $nu1, num64 $nu2 --> num64) is native(&LIB) is export(:fdist) { * }
sub gsl_ran_fdist_pdf(num64 $x, num64 $nu1, num64 $nu2 --> num64) is native(&LIB) is export(:fdist) { * }
sub gsl_cdf_fdist_P(num64 $x, num64 $nu1, num64 $nu2 --> num64) is native(&LIB) is export(:fdist) { * }
sub gsl_cdf_fdist_Q(num64 $x, num64 $nu1, num64 $nu2 --> num64) is native(&LIB) is export(:fdist) { * }
sub gsl_cdf_fdist_Pinv(num64 $P, num64 $nu1, num64 $nu2 --> num64) is native(&LIB) is export(:fdist) { * }
sub gsl_cdf_fdist_Qinv(num64 $Q, num64 $nu1, num64 $nu2 --> num64) is native(&LIB) is export(:fdist) { * }
# t-distribution
sub gsl_ran_tdist(gsl_rng $r, num64 $nu --> num64) is native(&LIB) is export(:tdist) { * }
sub gsl_ran_tdist_pdf(num64 $x, num64 $nu --> num64) is native(&LIB) is export(:tdist) { * }
sub gsl_cdf_tdist_P(num64 $x, num64 $nu --> num64) is native(&LIB) is export(:tdist) { * }
sub gsl_cdf_tdist_Q(num64 $x, num64 $nu --> num64) is native(&LIB) is export(:tdist) { * }
sub gsl_cdf_tdist_Pinv(num64 $P, num64 $nu --> num64) is native(&LIB) is export(:tdist) { * }
sub gsl_cdf_tdist_Qinv(num64 $Q, num64 $nu --> num64) is native(&LIB) is export(:tdist) { * }
# Beta Distribution
sub gsl_ran_beta(gsl_rng $r, num64 $a, num64 $b --> num64) is native(&LIB) is export(:beta) { * }
sub gsl_ran_beta_pdf(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:beta) { * }
sub gsl_cdf_beta_P(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:beta) { * }
sub gsl_cdf_beta_Q(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:beta) { * }
sub gsl_cdf_beta_Pinv(num64 $P, num64 $a, num64 $b --> num64) is native(&LIB) is export(:beta) { * }
sub gsl_cdf_beta_Qinv(num64 $Q, num64 $a, num64 $b --> num64) is native(&LIB) is export(:beta) { * }
# Logistic Distribution
sub gsl_ran_logistic(gsl_rng $r, num64 $a --> num64) is native(&LIB) is export(:logistic) { * }
sub gsl_ran_logistic_pdf(num64 $x, num64 $a --> num64) is native(&LIB) is export(:logistic) { * }
sub gsl_cdf_logistic_P(num64 $x, num64 $a --> num64) is native(&LIB) is export(:logistic) { * }
sub gsl_cdf_logistic_Q(num64 $x, num64 $a --> num64) is native(&LIB) is export(:logistic) { * }
sub gsl_cdf_logistic_Pinv(num64 $P, num64 $a --> num64) is native(&LIB) is export(:logistic) { * }
sub gsl_cdf_logistic_Qinv(num64 $Q, num64 $a --> num64) is native(&LIB) is export(:logistic) { * }
# Pareto Distribution
sub gsl_ran_pareto(gsl_rng $r, num64 $a, num64 $b --> num64) is native(&LIB) is export(:pareto) { * }
sub gsl_ran_pareto_pdf(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:pareto) { * }
sub gsl_cdf_pareto_P(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:pareto) { * }
sub gsl_cdf_pareto_Q(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:pareto) { * }
sub gsl_cdf_pareto_Pinv(num64 $P, num64 $a, num64 $b --> num64) is native(&LIB) is export(:pareto) { * }
sub gsl_cdf_pareto_Qinv(num64 $Q, num64 $a, num64 $b --> num64) is native(&LIB) is export(:pareto) { * }
# Spherical Vector Distributions
sub gsl_ran_dir_2d(gsl_rng $r, num64 $x is rw, num64 $y is rw) is native(&LIB) is export(:dir) { * }
sub gsl_ran_dir_2d_trig_method(gsl_rng $r, num64 $x is rw, num64 $y is rw) is native(&LIB) is export(:dir) { * }
sub gsl_ran_dir_3d(gsl_rng $r, num64 $x is rw, num64 $y is rw, num64 $z is rw) is native(&LIB) is export(:dir) { * }
sub gsl_ran_dir_nd(gsl_rng $r, size_t $n, CArray[num64] $x) is native(&LIB) is export(:dir) { * }
# Weibull Distribution
sub gsl_ran_weibull(gsl_rng $r, num64 $a, num64 $b --> num64) is native(&LIB) is export(:weibull) { * }
sub gsl_ran_weibull_pdf(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:weibull) { * }
sub gsl_cdf_weibull_P(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:weibull) { * }
sub gsl_cdf_weibull_Q(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:weibull) { * }
sub gsl_cdf_weibull_Pinv(num64 $P, num64 $a, num64 $b --> num64) is native(&LIB) is export(:weibull) { * }
sub gsl_cdf_weibull_Qinv(num64 $Q, num64 $a, num64 $b --> num64) is native(&LIB) is export(:weibull) { * }
# Type-1 Gumbel Distribution
sub gsl_ran_gumbel1(gsl_rng $r, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel1) { * }
sub gsl_ran_gumbel1_pdf(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel1) { * }
sub gsl_cdf_gumbel1_P(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel1) { * }
sub gsl_cdf_gumbel1_Q(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel1) { * }
sub gsl_cdf_gumbel1_Pinv(num64 $P, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel1) { * }
sub gsl_cdf_gumbel1_Qinv(num64 $Q, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel1) { * }
# Type-2 Gumbel Distribution
sub gsl_ran_gumbel2(gsl_rng $r, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel2) { * }
sub gsl_ran_gumbel2_pdf(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel2) { * }
sub gsl_cdf_gumbel2_P(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel2) { * }
sub gsl_cdf_gumbel2_Q(num64 $x, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel2) { * }
sub gsl_cdf_gumbel2_Pinv(num64 $P, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel2) { * }
sub gsl_cdf_gumbel2_Qinv(num64 $Q, num64 $a, num64 $b --> num64) is native(&LIB) is export(:gumbel2) { * }
# Dirichlet Distribution
sub gsl_ran_dirichlet(gsl_rng $r, size_t $K, CArray[num64] $alpha, CArray[num64] $theta) is native(&LIB) is export(:dirichlet) { * }
sub gsl_ran_dirichlet_pdf(size_t $K, CArray[num64] $alpha, CArray[num64] $theta --> num64) is native(&LIB) is export(:dirichlet) { * }
sub gsl_ran_dirichlet_lnpdf(size_t $K, CArray[num64] $alpha, CArray[num64] $theta --> num64) is native(&LIB) is export(:dirichlet) { * }
# General Discrete Distributions
sub gsl_ran_discrete_preproc(size_t $K, CArray[num64] $P --> gsl_ran_discrete_t) is native(&LIB) is export(:gendiscrete) { * }
sub gsl_ran_discrete(gsl_rng $r, gsl_ran_discrete_t $g --> size_t) is native(&LIB) is export(:gendiscrete) { * }
sub gsl_ran_discrete_pdf(size_t $k, gsl_ran_discrete_t $g --> num64) is native(&LIB) is export(:gendiscrete) { * }
sub gsl_ran_discrete_free(gsl_ran_discrete_t $g) is native(&LIB) is export(:gendiscrete) { * }
# Poisson Distribution
sub gsl_ran_poisson(gsl_rng $r, num64 $mu --> uint32) is native(&LIB) is export(:poisson) { * }
sub gsl_ran_poisson_pdf(uint32 $k, num64 $mu --> num64) is native(&LIB) is export(:poisson) { * }
sub gsl_cdf_poisson_P(uint32 $k, num64 $mu --> num64) is native(&LIB) is export(:poisson) { * }
sub gsl_cdf_poisson_Q(uint32 $k, num64 $mu --> num64) is native(&LIB) is export(:poisson) { * }
# Bernoulli Distribution
sub gsl_ran_bernoulli(gsl_rng $r, num64 $p --> uint32) is native(&LIB) is export(:bernoulli) { * }
sub gsl_ran_bernoulli_pdf(uint32 $k, num64 $p --> num64) is native(&LIB) is export(:bernoulli) { * }
# Binomial Distribution
sub gsl_ran_binomial(gsl_rng $r, num64 $p, uint32 $n --> uint32) is native(&LIB) is export(:binomial) { * }
sub gsl_ran_binomial_pdf(uint32 $k, num64 $p, uint32 $n --> num64) is native(&LIB) is export(:binomial) { * }
sub gsl_cdf_binomial_P(uint32 $k, num64 $p, uint32 $n --> num64) is native(&LIB) is export(:binomial) { * }
sub gsl_cdf_binomial_Q(uint32 $k, num64 $p, uint32 $n --> num64) is native(&LIB) is export(:binomial) { * }
# Multinomial Distribution
sub gsl_ran_multinomial(gsl_rng $r, size_t $K, uint32 $N, CArray[num64] $p, CArray[uint32] $n) is native(&LIB) is export(:multinomial) { * }
sub gsl_ran_multinomial_pdf(size_t $K, CArray[num64] $p, CArray[uint32] $n --> num64) is native(&LIB) is export(:multinomial) { * }
sub gsl_ran_multinomial_lnpdf(size_t $K, CArray[num64] $p, CArray[uint32] $n --> num64) is native(&LIB) is export(:multinomial) { * }
# Negative Binomial Distribution
sub gsl_ran_negative_binomial(gsl_rng $r, num64 $p, num64 $n --> uint32) is native(&LIB) is export(:nbinomial) { * }
sub gsl_ran_negative_binomial_pdf(uint32 $k, num64 $p, num64 $n --> num64) is native(&LIB) is export(:nbinomial) { * }
sub gsl_cdf_negative_binomial_P(uint32 $k, num64 $p, num64 $n --> num64) is native(&LIB) is export(:nbinomial) { * }
sub gsl_cdf_negative_binomial_Q(uint32 $k, num64 $p, num64 $n --> num64) is native(&LIB) is export(:nbinomial) { * }
# Pascal Distribution
sub gsl_ran_pascal(gsl_rng $r, num64 $p, uint32 $n --> uint32) is native(&LIB) is export(:pascal) { * }
sub gsl_ran_pascal_pdf(uint32 $k, num64 $p, uint32 $n --> num64) is native(&LIB) is export(:pascal) { * }
sub gsl_cdf_pascal_P(uint32 $k, num64 $p, uint32 $n --> num64) is native(&LIB) is export(:pascal) { * }
sub gsl_cdf_pascal_Q(uint32 $k, num64 $p, uint32 $n --> num64) is native(&LIB) is export(:pascal) { * }
# Geometric Distribution
sub gsl_ran_geometric(gsl_rng $r, num64 $p --> uint32) is native(&LIB) is export(:geometric) { * }
sub gsl_ran_geometric_pdf(uint32 $k, num64 $p --> num64) is native(&LIB) is export(:geometric) { * }
sub gsl_cdf_geometric_P(uint32 $k, num64 $p --> num64) is native(&LIB) is export(:geometric) { * }
sub gsl_cdf_geometric_Q(uint32 $k, num64 $p --> num64) is native(&LIB) is export(:geometric) { * }
# Hypergeometric Distribution
sub gsl_ran_hypergeometric(gsl_rng $r, uint32 $n1, uint32 $n2, uint32 $t --> uint32) is native(&LIB) is export(:hypergeometric) { * }
sub gsl_ran_hypergeometric_pdf(uint32 $k, uint32 $n1, uint32 $n2, uint32 $t --> num64) is native(&LIB) is export(:hypergeometric) { * }
sub gsl_cdf_hypergeometric_P(uint32 $k, uint32 $n1, uint32 $n2, uint32 $t --> num64) is native(&LIB) is export(:hypergeometric) { * }
sub gsl_cdf_hypergeometric_Q(uint32 $k, uint32 $n1, uint32 $n2, uint32 $t --> num64) is native(&LIB) is export(:hypergeometric) { * }
# Logarithmic Distribution
sub gsl_ran_logarithmic(gsl_rng $r, num64 $p --> uint32) is native(&LIB) is export(:logarithmic) { * }
sub gsl_ran_logarithmic_pdf(uint32 $k, num64 $p --> num64) is native(&LIB) is export(:logarithmic) { * }
# Wishart Distribution
sub gsl_ran_wishart(gsl_rng $r, num64 $n, gsl_matrix $L, gsl_matrix $result, gsl_matrix $work --> int32) is native(&LIB) is export(:wishart) { * }
sub gsl_ran_wishart_pdf(gsl_matrix $X, gsl_matrix $L_X, num64 $n, gsl_matrix $L, num64 $result is rw, gsl_matrix $work --> int32) is native(&LIB) is export(:wishart) { * }
sub gsl_ran_wishart_log_pdf(gsl_matrix $X, gsl_matrix $L_X, num64 $n, gsl_matrix $L, num64 $result is rw, gsl_matrix $work --> int32) is native(&LIB) is export(:wishart) { * }
# Shuffling and Sampling
sub gsl_ran_shuffle(gsl_rng $r, Pointer[void] $base, size_t $n, size_t $size) is native(&LIB) is export(:shuffle) { * }
sub gsl_ran_choose(gsl_rng $r, Pointer[void] $dest, size_t $k, Pointer[void] $src, size_t $n, size_t $size) is native(&LIB) is export(:shuffle) { * }
sub gsl_ran_sample(gsl_rng $r, Pointer[void] $dest, size_t $k, Pointer[void] $src, size_t $n, size_t $size) is native(&LIB) is export(:shuffle) { * }
