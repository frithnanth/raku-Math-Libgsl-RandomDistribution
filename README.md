[![Actions Status](https://github.com/frithnanth/raku-Math-Libgsl-RandomDistribution/workflows/test/badge.svg)](https://github.com/frithnanth/raku-Math-Libgsl-RandomDistribution/actions)

NAME
====

Math::Libgsl::RandomDistribution - An interface to libgsl, the Gnu Scientific Library - Random Number Distributions

SYNOPSIS
========

```raku
use Math::Libgsl::Random;
use Math::Libgsl::RandomDistribution;

my Math::Libgsl::Random $r .= new;
say poisson($r, 3) for ^10;
```

DESCRIPTION
===========

Math::Libgsl::RandomDistribution is an interface to the Random Number Distributions section of libgsl, the Gnu Scientific Library.

### gaussian(Math::Libgsl::Random $r, Num() $sigma --> Num)

This function returns a Gaussian random variate, with mean zero and standard deviation $sigma.

### gaussian-pdf(Num() $x, Num() $sigma --> Num)

This function computes the probability density at $x for a Gaussian distribution with standard deviation $sigma.

### gaussian-ziggurat(Math::Libgsl::Random $r, Num() $sigma --> Num)

### gaussian-ratio-method(Math::Libgsl::Random $r, Num() $sigma --> Num)

This function computes a Gaussian random variate using the alternative Marsaglia-Tsang ziggurat and Kinderman-Monahan-Leva ratio methods.

### ugaussian(Math::Libgsl::Random $r --> Num)

### ugaussian-pdf(Num() $x --> Num)

### ugaussian-ratio-method(Math::Libgsl::Random $r --> Num)

These functions compute results for the unit Gaussian distribution. They are equivalent to the functions above with $sigma = 1.

### gaussian-P(Num() $x, Num() $sigma --> Num)

### gaussian-Q(Num() $x, Num() $sigma --> Num)

### gaussian-Pinv(Num() $P, Num() $sigma --> Num)

### gaussian-Qinv(Num() $Q, Num() $sigma --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses.

### ugaussian-P(Num() $x --> Num)

### ugaussian-Q(Num() $x --> Num)

### ugaussian-Pinv(Num() $P --> Num)

### ugaussian-Qinv(Num() $Q --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the unit Gaussian distribution.

### gaussian-tail(Math::Libgsl::Random $r, Num() $a where * > 0, Num() $sigma --> Num)

This function provides random variates from the upper tail of a Gaussian distribution with standard deviation $sigma.

### gaussian-tail-pdf(Num() $x, Num() $a where * > 0, Num() $sigma --> Num)

This function computes the probability density p(x) at $x for a Gaussian tail distribution with standard deviation $sigma and lower limit $a.

### ugaussian-tail(Math::Libgsl::Random $r, Num() $a where * > 0 --> Num)

### ugaussian-tail-pdf(Num() $x, Num() $a where * > 0 --> Num)

These functions compute results for the tail of a unit Gaussian distribution.

### bivariate-gaussian(Math::Libgsl::Random $r, Num() $sigma-x, Num() $sigma-y, Num() $rho where -1 < * < 1 --> List)

This function generates a pair of correlated Gaussian variates, with mean zero, correlation coefficient $rho and standard deviations $sigma_x and $sigma_y. The function returns two Num values: the Gauusian correlates along the x and y directions.

### bivariate-gaussian-pdf(Num() $x, Num() $y, Num() $sigma-x, Num() $sigma-y, Num() $rho where -1 < * < 1 --> Num)

This function computes the probability density p(x, y) at ($x, $y) for a bivariate Gaussian distribution with standard deviations $sigma_x, $sigma_y and correlation coefficient $rho.

### multivariate-gaussian(Math::Libgsl::Random $r, Math::Libgsl::Vector $mu, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $L.matrix.size1 == $mu.vector.size } --> Math::Libgsl::Vector)

This function generates a random vector satisfying the k-dimensional multivariate Gaussian distribution with mean μ and variance-covariance matrix Σ. On input, the k-vector μ is given in $mu, and the Cholesky factor of the k-by-k matrix Σ = LT(L) is given in the lower triangle of $L, as output from Math::Libgsl::LinearAlgebra::cholesky-decomp(). The function returns the random vector as a Math::Libgsl::Vector object.

### multivariate-gaussian-pdf(Math::Libgsl::Vector $x, Math::Libgsl::Vector $mu where *.vector.size == $x.vector.size, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $L.matrix.size1 == $mu.vector.size } --> Num)

### multivariate-gaussian-log-pdf(Math::Libgsl::Vector $x, Math::Libgsl::Vector $mu where *.vector.size == $x.vector.size, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $L.matrix.size1 == $mu.vector.size } --> Num)

These functions compute p(x) or log p(x) at the point $x, using mean vector $mu and variance-covariance matrix specified by its Cholesky factor $L.

### multivariate-gaussian-mean(Math::Libgsl::Matrix $X --> Math::Libgsl::Vector)

Given a set of n samples Xⱼ from a k-dimensional multivariate Gaussian distribution, this function computes the maximum likelihood estimate of the mean of the distribution. The function returns the maximum likelihood estimate as a Math::Libgsl::Vector object.

### multivariate-gaussian-vcov(Math::Libgsl::Matrix $X --> Math::Libgsl::Matrix)

Given a set of n samples Xⱼ from a k-dimensional multivariate Gaussian distribution, this function computes the maximum likelihood estimate of the variance-covariance matrix of the distribution. The function returns the maximum likelihood estimate of the variance-covariance matrix as a Math::Libgsl::Matrix object.

### exponential(Math::Libgsl::Random $r, Num() $mu --> Num)

This function returns a random variate from the exponential distribution with mean $mu.

### exponential-pdf(Num() $x, Num() $mu --> Num)

This function computes the probability density p(x) at $x for an exponential distribution with mean $mu.

### exponential-P(Num() $x, Num() $mu --> Num)

### exponential-Q(Num() $x, Num() $mu --> Num)

### exponential-Pinv(Num() $P, Num() $mu --> Num)

### exponential-Qinv(Num() $Q, Num() $mu --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the exponential distribution with mean $mu.

### laplace(Math::Libgsl::Random $r, Num() $a --> Num)

This function returns a random variate from the Laplace distribution with width $a.

### laplace-pdf(Num() $x, Num() $a --> Num)

This function computes the probability density p(x) at $x for a Laplace distribution with width $a.

### laplace-P(Num() $x, Num() $a --> Num)

### laplace-Q(Num() $x, Num() $a --> Num)

### laplace-Pinv(Num() $P, Num() $a --> Num)

### laplace-Qinv(Num() $Q, Num() $a --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Laplace distribution with width $a.

### exppow(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the exponential power distribution with scale parameter $a and exponent $b.

### exppow-pdf(Num() $x, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for an exponential power distribution with scale parameter $a and exponent $b.

### exppow-P(Num() $x, Num() $a, Num() $b --> Num)

### exppow-Q(Num() $x, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) for the exponential power distribution with parameters $a and $b.

### cauchy(Math::Libgsl::Random $r, Num() $a --> Num)

This function returns a random variate from the Cauchy distribution with scale parameter $a.

### cauchy-pdf(Num() $x, Num() $a --> Num)

This function computes the probability density p(x) at $x for a Cauchy distribution with scale parameter $a.

### cauchy-P(Num() $x, Num() $a --> Num)

### cauchy-Q(Num() $x, Num() $a --> Num)

### cauchy-Pinv(Num() $P, Num() $a --> Num)

### cauchy-Qinv(Num() $Q, Num() $a --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Cauchy distribution with scale parameter $a.

### rayleigh(Math::Libgsl::Random $r, Num() $sigma --> Num)

This function returns a random variate from the Rayleigh distribution with scale parameter $sigma.

### rayleigh-pdf(Num() $x, Num() $sigma --> Num)

This function computes the probability density p(x) at $x for a Rayleigh distribution with scale parameter $sigma.

### rayleigh-P(Num() $x, Num() $sigma --> Num)

### rayleigh-Q(Num() $x, Num() $sigma --> Num)

### rayleigh-Pinv(Num() $P, Num() $sigma --> Num)

### rayleigh-Qinv(Num() $Q, Num() $sigma --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Rayleigh distribution with scale parameter $sigma.

### rayleigh-tail(Math::Libgsl::Random $r, Num() $a, Num() $sigma --> Num)

This function returns a random variate from the tail of the Rayleigh distribution with scale parameter $sigma and a lower limit of $a.

### rayleigh-tail-pdf(Num() $x, Num() $a, Num() $sigma --> Num)

This function computes the probability density p(x) at $x for a Rayleigh tail distribution with scale parameter $sigma and lower limit $a.

### landau(Math::Libgsl::Random $r --> Num)

This function returns a random variate from the Landau distribution.

### landau-pdf(Num() $x --> Num)

This function computes the probability density p(x) at $x for the Landau distribution.

### levy(Math::Libgsl::Random $r, Num() $c, Num() $alpha where 0 < * ≤ 2 --> Num)

This function returns a random variate from the Levy symmetric stable distribution with scale $c and exponent $alpha.

### levy-skew(Math::Libgsl::Random $r, Num() $c, Num() $alpha where 0 < * ≤ 2, Num() $beta --> Num)

This function returns a random variate from the Levy skew stable distribution with scale $c, exponent $alpha and skewness parameter $beta.

### gamma(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the gamma distribution.

### gamma-knuth(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a gamma variate using the algorithms from Knuth.

### gamma-pdf(Num() $x, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a gamma distribution with parameters $a and $b.

### gamma-P(Num() $x, Num() $a, Num() $b --> Num)

### gamma-Q(Num() $x, Num() $a, Num() $b --> Num)

### gamma-Pinv(Num() $P, Num() $a, Num() $b --> Num)

### gamma-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the gamma distribution with parameters $a and $b.

### flat(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the flat (uniform) distribution from $a to $b.

### flat-pdf(Num() $x, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a uniform distribution from $a to $b.

### flat-P(Num() $x, Num() $a, Num() $b --> Num)

### flat-Q(Num() $x, Num() $a, Num() $b --> Num)

### flat-Pinv(Num() $P, Num() $a, Num() $b --> Num)

### flat-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for a uniform distribution from $a to $b.

### lognormal(Math::Libgsl::Random $r, Num() $zeta, Num() $sigma --> Num)

This function returns a random variate from the lognormal distribution.

### lognormal-pdf(Num() $x, Num() $zeta, Num() $sigma --> Num)

This function computes the probability density p(x) at $x for a lognormal distribution with parameters $zeta and $sigma.

### lognormal-P(Num() $x, Num() $zeta, Num() $sigma --> Num)

### lognormal-Q(Num() $x, Num() $zeta, Num() $sigma --> Num)

### lognormal-Pinv(Num() $P, Num() $zeta, Num() $sigma --> Num)

### lognormal-Qinv(Num() $Q, Num() $zeta, Num() $sigma --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the lognormal distribution with parameters $zeta and $sigma.

### chisq(Math::Libgsl::Random $r, Num() $nu --> Num)

This function returns a random variate from the chi-squared distribution with $nu degrees of freedom.

### chisq-pdf(Num() $x where * ≥ 0, Num() $nu --> Num)

This function computes the probability density p(x) at $x for a chi-squared distribution with $nu degrees of freedom.

### chisq-P(Num() $x where * ≥ 0, Num() $nu --> Num)

### chisq-Q(Num() $x where * ≥ 0, Num() $nu --> Num)

### chisq-Pinv(Num() $P, Num() $nu --> Num)

### chisq-Qinv(Num() $Q, Num() $nu --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the chi-squared distribution with $nu degrees of freedom.

### fdist(Math::Libgsl::Random $r, Num() $nu1, Num() $nu2 --> Num)

This function returns a random variate from the F-distribution with degrees of freedom $nu1 and $nu2.

### fdist-pdf(Num() $x where * ≥ 0, Num() $nu1, Num() $nu2 --> Num)

This function computes the probability density p(x) at $x for an F-distribution with $nu1 and $nu2 degrees of freedom.

### fdist-P(Num() $x where * ≥ 0, Num() $nu1, Num() $nu2 --> Num)

### fdist-Q(Num() $x where * ≥ 0, Num() $nu1, Num() $nu2 --> Num)

### fdist-Pinv(Num() $P, Num() $nu1, Num() $nu2 --> Num)

### fdist-Qinv(Num() $Q, Num() $nu1, Num() $nu2 --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the F-distribution with $nu1 and $nu2 degrees of freedom.

### tdist(Math::Libgsl::Random $r, Num() $nu --> Num)

This function returns a random variate from the t-distribution.

### tdist-pdf(Num() $x, Num() $nu --> Num)

This function computes the probability density p(x) at $x for a t-distribution with $nu degrees of freedom.

### tdist-P(Num() $x, Num() $nu --> Num)

### tdist-Q(Num() $x, Num() $nu --> Num)

### tdist-Pinv(Num() $P, Num() $nu --> Num)

### tdist-Qinv(Num() $Q, Num() $nu --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the t-distribution with $nu degrees of freedom.

### beta(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the beta distribution.

### beta-pdf(Num() $x, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a beta distribution with parameters $a and $b.

### beta-P(Num() $x, Num() $a, Num() $b --> Num)

### beta-Q(Num() $x, Num() $a, Num() $b --> Num)

### beta-Pinv(Num() $P, Num() $a, Num() $b --> Num)

### beta-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the beta distribution with parameters $a and $b.

### logistic(Math::Libgsl::Random $r, Num() $a --> Num)

This function returns a random variate from the logistic distribution.

### logistic-pdf(Num() $x, Num() $a --> Num)

This function computes the probability density p(x) at $x for a logistic distribution with scale parameter $a.

### logistic-P(Num() $x, Num() $a --> Num)

### logistic-Q(Num() $x, Num() $a --> Num)

### logistic-Pinv(Num() $P, Num() $a --> Num)

### logistic-Qinv(Num() $Q, Num() $a --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the logistic distribution with scale parameter $a.

### pareto(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the Pareto distribution of order $a and scale $b.

### pareto-pdf(Num() $x, Num() $a, Num() $b where { $x ≥ $b } --> Num)

This function computes the probability density p(x) at $x for a Pareto distribution with exponent $a and scale $b.

### pareto-P(Num() $x, Num() $a, Num() $b where { $x ≥ $b } --> Num)

### pareto-Q(Num() $x, Num() $a, Num() $b where { $x ≥ $b } --> Num)

### pareto-Pinv(Num() $P, Num() $a, Num() $b --> Num)

### pareto-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Pareto distribution with exponent $a and scale $b.

### d2dir(Math::Libgsl::Random $r --> List)

### d2dir-trig-method(Math::Libgsl::Random $r --> List)

This function returns a random direction vector v = (x, y) in two dimensions. The return value is a List of two Num(s): the x and y components of the 2D vector.

### d3dir(Math::Libgsl::Random $r --> List)

This function returns a random direction vector v = (x, y, z) in three dimensions. The return value is a List of three Num(s): the x, y, and z components of the 3D vector.

### dndir(Math::Libgsl::Random $r, Int $n --> List)

This function returns a random direction vector v = (x 1 , x 2 , . . . , x n ) in $n dimensions. The return value is a List of $n Num(s): the components of the n-D vector.

### weibull(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the Weibull distribution.

### weibull-pdf(Num() $x where $x ≥ 0, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a Weibull distribution with scale $a and exponent $b.

### weibull-P(Num() $x where $x ≥ 0, Num() $a, Num() $b --> Num)

### weibull-Q(Num() $x where $x ≥ 0, Num() $a, Num() $b --> Num)

### weibull-Pinv(Num() $P, Num() $a, Num() $b --> Num)

### weibull-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Weibull distribution with scale $a and exponent $b.

### gumbel1(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the Type-1 Gumbel distribution.

### gumbel1-pdf(Num() $x, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a Type-1 Gumbel distribution with parameters $a and $b.

### gumbel1-P(Num() $x, Num() $a, Num() $b --> Num)

### gumbel1-Q(Num() $x, Num() $a, Num() $b --> Num)

### gumbel1-Pinv(Num() $P, Num() $a, Num() $b --> Num)

### gumbel1-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Type-1 Gumbel distribution with parameters $a and $b.

### gumbel2(Math::Libgsl::Random $r, Num() $a, Num() $b --> Num)

This function returns a random variate from the Type-2 Gumbel distribution.

### gumbel2-pdf(Num() $x where * > 0, Num() $a, Num() $b --> Num)

This function computes the probability density p(x) at $x for a Type-2 Gumbel distribution with parameters $a and $b.

### gumbel2-P(Num() $x where * > 0, Num() $a, Num() $b --> Num)

### gumbel2-Q(Num() $x where * > 0, Num() $a, Num() $b --> Num)

### gumbel2-Pinv(Num() $P, Num() $a, Num() $b --> Num)

### gumbel2-Qinv(Num() $Q, Num() $a, Num() $b --> Num)

These functions compute the cumulative distribution functions P(x), Q(x) and their inverses for the Type-2 Gumbel distribution with parameters $a and $b.

### dirichlet(Math::Libgsl::Random $r, Int $K, @alpha where *.all > 0 --> List)

This function returns an array of K random variates as a List of Num(s) from a Dirichlet distribution of order K-1.

### dirichlet-pdf(Int $K, @alpha where *.all > 0, @theta where *.all ≥ 0 --> Num)

This function computes the probability density p(θ₁, …, θₖ) at theta[K] for a Dirichlet distribution with parameters alpha[K].

### dirichlet-lnpdf(Int $K, @alpha where *.all > 0, @theta where *.all ≥ 0 --> Num)

This function computes the logarithm of the probability density p(θ₁, …, θₖ) at theta[K] for a Dirichlet distribution with parameters alpha[K].

### poisson(Math::Libgsl::Random $r, Num() $mu --> UInt)

This function returns a random integer from the Poisson distribution with mean $mu.

### poisson-pdf(UInt $k, Num() $mu --> Num)

This function computes the probability p(k) of obtaining $k from a Poisson distribution with mean $mu.

### poisson-P(UInt $k, Num() $mu --> Num)

### poisson-Q(UInt $k, Num() $mu --> Num)

These functions compute the cumulative distribution functions P(k), Q(k) for the Poisson distribution with parameter $mu.

### bernoulli(Math::Libgsl::Random $r, Num() $p --> Int)

This function returns either 0 or 1, the result of a Bernoulli trial with probability $p.

### bernoulli-pdf(UInt $k, Num() $p --> Num)

This function computes the probability p(k) of obtaining $k from a Bernoulli distribution with probability parameter $p.

### binomial(Math::Libgsl::Random $r, Num() $p, UInt $n --> Int)

This function returns a random integer from the binomial distribution, the number of successes in $n independent trials with probability $p.

### binomial-pdf(UInt $k, Num() $p, UInt $n where * ≥ $k --> Num)

This function computes the probability p(k) of obtaining $k from a binomial distribution with parameters $p and $n.

### binomial-P(UInt $k, Num() $p, UInt $n where * ≥ $k --> Num)

### binomial-Q(UInt $k, Num() $p, UInt $n where * ≥ $k --> Num)

These functions compute the cumulative distribution functions P (k), Q(k) for the binomial distribution with parameters $p and $n.

### multinomial(Math::Libgsl::Random $r, Int $K, UInt $N, *@p where { @p.all ~~ Numeric } --> List)

This function returns a random sample from the multinomial distribution formed by $N trials from an underlying distribution p[K] as a List of UInt(s).

### multinomial-pdf(Int $K, @p where { @p.all ~~ Numeric }, *@n where { @n.all ~~ UInt } --> Num)

This function computes the probability P(n₁, n₂, …, nₖ) of sampling n[K] from a multinomial distribution with parameters p[K].

### multinomial-lnpdf(Int $K, @p where { @p.all ~~ Numeric }, *@n where { @n.all ~~ UInt } --> Num)

This function computes the logarithm of the probability P(n₁, n₂, …, nₖ) for the multinomial distribution with parameters p[K].

### negative-binomial(Math::Libgsl::Random $r, Num() $p, Num() $n --> UInt)

This function returns a random integer from the negative binomial distribution.

### negative-binomial-pdf(UInt $k, Num() $p, Num() $n --> Num)

This function computes the probability p(k) of obtaining $k from a negative binomial distribution with parameters $p and $n.

### negative-binomial-P(UInt $k, Num() $p, Num() $n --> Num)

### negative-binomial-Q(UInt $k, Num() $p, Num() $n --> Num)

These functions compute the cumulative distribution functions P(k), Q(k) for the negative binomial distribution with parameters $p and $n.

### pascal(Math::Libgsl::Random $r, Num() $p, UInt $n --> UInt)

This function returns a random integer from the Pascal distribution.

### pascal-pdf(UInt $k, Num() $p, UInt $n --> Num)

This function computes the probability p(k) of obtaining $k from a Pascal distribution with parameters $p and $n.

### pascal-P(UInt $k, Num() $p, UInt $n --> Num)

### pascal-Q(UInt $k, Num() $p, UInt $n --> Num)

These functions compute the cumulative distribution functions P(k), Q(k) for the Pascal distribution with parameters $p and $n.

### geometric(Math::Libgsl::Random $r, Num() $p --> UInt)

This function returns a random integer from the geometric distribution, the number of independent trials with probability $p until the first success.

### geometric-pdf(UInt $k where * ≥ 1, Num() $p --> Num)

This function computes the probability p(k) of obtaining $k from a geometric distribution with probability parameter $p.

### geometric-P(UInt $k where * ≥ 1, Num() $p --> Num)

### geometric-Q(UInt $k where * ≥ 1, Num() $p --> Num)

These functions compute the cumulative distribution functions P(k), Q(k) for the geometric distribution with parameter $p.

### hypergeometric(Math::Libgsl::Random $r, UInt $n1, UInt $n2, UInt $t --> UInt)

This function returns a random integer from the hypergeometric distribution.

### hypergeometric-pdf(UInt $k, UInt $n1, UInt $n2, UInt $t --> Num)

This function computes the probability p(k) of obtaining $k from a hypergeometric distribution with parameters $n1, $n2, $t.

### hypergeometric-P(UInt $k, UInt $n1, UInt $n2, UInt $t --> Num)

### hypergeometric-Q(UInt $k, UInt $n1, UInt $n2, UInt $t --> Num)

These functions compute the cumulative distribution functions P(k), Q(k) for the hypergeometric distribution with parameters $n1, $n2 and $t.

### logarithmic(Math::Libgsl::Random $r, Num() $p --> UInt)

This function returns a random integer from the logarithmic distribution.

### logarithmic-pdf(UInt $k where * ≥ 1, Num() $p --> Num)

This function computes the probability p(k) of obtaining $k from a logarithmic distribution with probability parameter $p.

### wishart(Math::Libgsl::Random $r, Num() $n, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $n ≤ $L.matrix.size1 - 1 } --> Math::Libgsl::Matrix)

This function returns a random symmetric p-by-p matrix from the Wishart distribution as a Math::Libgsl::Matrix object.

### wishart-pdf(Math::Libgsl::Matrix $X where { $X.matrix.size1 == $X.matrix.size2 }, Math::Libgsl::Matrix $L-X where { $L-X.matrix.size1 == $L-X.matrix.size2 }, Num() $n where * > $X.matrix.size1 - 1, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $X.matrix.size1 == $L.matrix.size1 && $L-X.matrix.size1 == $L.matrix.size1 } --> Num)

### wishart-log-pdf(Math::Libgsl::Matrix $X where { $X.matrix.size1 == $X.matrix.size2 }, Math::Libgsl::Matrix $L-X where { $L-X.matrix.size1 == $L-X.matrix.size2 }, Num() $n where * > $X.matrix.size1 - 1, Math::Libgsl::Matrix $L where { $L.matrix.size1 == $L.matrix.size2 && $X.matrix.size1 == $L.matrix.size1 && $L-X.matrix.size1 == $L.matrix.size1 } --> Num)

These functions return p(X) or log p(X) for the p-by-p matrix $X, whose Cholesky factor is specified in $L-X. The degrees of freedom is given by $n, the Cholesky factor of the scale matrix V is specified in $L.

### shuffle(Math::Libgsl::Random $r, @base --> List)

This function randomly shuffles the order of the elements of the array @base and returns them as a List.

### choose(Math::Libgsl::Random $r, Int $k, @src where *.elems ≥ $k --> List)

This function returns the list of $k objects taken randomly from the elements of the array @src.

### sample(Math::Libgsl::Random $r, Int $k, @src --> List)

This function is like choose() but samples $k items from the original array of n items @src with replacement, so the same object can appear more than once in the output.

Class Math::Libgsl::RandomDistribution::Discrete
------------------------------------------------

This probability distribution needs a lookup table for the discrete random number generator, so it's implemented as a class, which hides the implementation details.

```raku
use Math::Libgsl::RandomDistribution;
use Math::Libgsl::Random;

my Int $size = 3;
my @probability = .59, .4, .01;
my Math::Libgsl::Random $r .= new;
my Math::Libgsl::RandomDistribution::Discrete $d .= new: :$size, :@probability;

say "Using probability values: { $d.probability }";
say $d.discrete($r) for ^10;

say $d.discrete-pdf(2);
```

### new(Int() :$size, :@probability where *.all > 0)

Creates the lookup table for the discrete random number generator. The array @probability contains the probabilities of the discrete events.

### discrete(Math::Libgsl::Random $r --> Int)

This method returns one discrete random number.

### discrete-pdf(Int $k --> Num)

This method returns the probability P[k] of observing the variable $k.

C Library Documentation
=======================

For more details on libgsl see [https://www.gnu.org/software/gsl/](https://www.gnu.org/software/gsl/). The excellent C Library manual is available here [https://www.gnu.org/software/gsl/doc/html/index.html](https://www.gnu.org/software/gsl/doc/html/index.html), or here [https://www.gnu.org/software/gsl/doc/latex/gsl-ref.pdf](https://www.gnu.org/software/gsl/doc/latex/gsl-ref.pdf) in PDF format.

Prerequisites
=============

This module requires the libgsl library to be installed. Please follow the instructions below based on your platform:

Debian Linux and Ubuntu 20.04+
------------------------------

    sudo apt install libgsl23 libgsl-dev libgslcblas0

That command will install libgslcblas0 as well, since it's used by the GSL.

Ubuntu 18.04
------------

libgsl23 and libgslcblas0 have a missing symbol on Ubuntu 18.04. I solved the issue installing the Debian Buster version of those three libraries:

  * [http://http.us.debian.org/debian/pool/main/g/gsl/libgslcblas0_2.5+dfsg-6_amd64.deb](http://http.us.debian.org/debian/pool/main/g/gsl/libgslcblas0_2.5+dfsg-6_amd64.deb)

  * [http://http.us.debian.org/debian/pool/main/g/gsl/libgsl23_2.5+dfsg-6_amd64.deb](http://http.us.debian.org/debian/pool/main/g/gsl/libgsl23_2.5+dfsg-6_amd64.deb)

  * [http://http.us.debian.org/debian/pool/main/g/gsl/libgsl-dev_2.5+dfsg-6_amd64.deb](http://http.us.debian.org/debian/pool/main/g/gsl/libgsl-dev_2.5+dfsg-6_amd64.deb)

Installation
============

To install it using zef (a module management tool):

    $ zef install Math::Libgsl::RandomDistribution

AUTHOR
======

Fernando Santagata <nando.santagata@gmail.com>

COPYRIGHT AND LICENSE
=====================

Copyright 2020 Fernando Santagata

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

