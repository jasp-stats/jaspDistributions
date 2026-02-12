# Syntax for specifying distributions

The set of distributions can be specified manually by using the distribution syntax that we describe below.

## Basic rules

- Each distribution is specified on a new line
- A distribution is specified by its name + parameter specification inside brackets. For example, `Normal(mu=0, sigma=1)` specifies a normal distribution with mean 0 and standard deviation 1.
- By default, specified parameters are considered free and will therefore be estimated using the data using maximum likelihood estimation (MLE). 
- For some distributions, MLE is found by numerical optimization. The module internally uses some heuristics to select intelligent starting values, but these may fail. In that case, a second attempt to fit the distribution will be made using the specified parameter values as the starting values. You can therefor set the parameter values to initialize the optimization in a reasonable point in the parameter space and therefore help converge to an optimum.
- By using `fixed()`, you can specify that a parameter is going to be treated as a constant (i.e., not estimated from data). For example, `Normal(mu=0, sigma2=fixed(12))` specifies a normal distribution with mean 1 and fixed variance of 12; only the mean parameter will be estimated from data.
- Some distributions can be specified using different parametrizations. See above for the example of `sigma` vs `sigma2` for a normal distribution with a std. deviation vs. variance parameter.

## List of available distributions and their parameters

### Unbounded distributions

Distributions with an unbounded support.

- `StandardNormal()`: Standard normal distribution.
- `Normal(mu, sigma)`, `Normal(mu, sigma2)`, `Normal(mu, tau)`: Normal distribution with parameters `mu`: mean, `sigma`: std. deviation, `sigma2`: variance, `tau`: precision.
- `StandardT(nu)`: Standard t distribution with parameter `nu`: degrees of freedom.
- `StudentT(nu, mu, sigma)`: Student t distribution with parameters `nu`: degrees of freedom, `mu`: location, `sigma`: scale.
- `NoncentralT(nu, kappa)`: Noncentral t distribution with parameters `nu`: degrees of freedom, `kappa`: noncentrality.
- `NoncentralStudentT(nu, kappa, mu, sigma)`: Noncentral Student t distribution with parameters `nu`: degrees of freedom, `kappa`: noncentrality, `mu`: location, `sigma`: scale.
- `Cauchy(mu, sigma)`: Cauchy distribution with parameters `mu`: location, `sigma`: scale.
- `Gumbel(mu, beta)`: Gumbel distribution with parameters `mu`: location, `beta`: scale.
- `Laplace(mu, beta)`: Laplace distribution with parameters `mu`: location, `beta`: scale.
- `Logistic(mu, sigma)`: Logistic distribution with parameters `mu`: location, `sigma`: scale.
- `SkewedGeneralizedT(mu, sigma, lambda, p, q)`: Skewed generalized t-distribution with parameters `mu`: location, `sigma`: scale, `lambda`: skew, `p` and `q`: kurtosis.
- `SymmetricGeneralizedNormal(mu, alpha, beta)`: Symmetric generalized normal with parameters `mu`: location, `alpha`: scale, `beta`: shape.
- `SkewNormal(xi, omega, alpha)`, Skew normal with parameters `xi`: location, `omega`: scale, `alpha`: slant (skew).
- `SkewCauchy(xi, omega, alpha)`, Skew Cauchy with parameters `xi`: location, `omega`: scale, `alpha`: slant (skew).
- `SkewT(xi, omega, alpha, nu)`, Skew t with parameters `xi`: location, `omega`: scale, `alpha`: slant (skew), `nu`: degrees of freedom.

### Bounded distributions

These distributions are in some way bounded. There are two types: Distributions with fixed support, and distributions whose support depends on its parameter values.

#### Fixed support

- `Beta(alpha, beta)`: Beta distribution is bounded between 0 and 1 and is specified with parameters `alpha`, `beta`: shape.

The rest of the distributions are supported on the interval from 0 to infinity.

- `BetaPrime(alpha, beta)`, Beta prime distribution with  parameters `alpha`, `beta`: shape.
- `CentralF(nu1, nu2)`: F distribution with parameters `nu1`, `nu2`: degrees of freedom.
- `NoncentralF(nu1, nu2, kappa)`: Noncentral F-distribution with parameters `nu1`, `nu2`: degrees of freedom and `kappa`: noncentrality.
- `ChiSquared(nu)`: Chi-squared distribution is defined from 0 to infinity and is specified with parameter `nu`: degrees of freedom.
- `NoncentralChiSquared(nu, kappa)`: Same as Chi-squared, with an additional `kappa`: noncentrality parameter.
- `Exponential(lambda)` `Exponential(beta)`: Exponential distribution with parameter `lambda`: rate, or `beta`: scale.
- `Gamma(alpha, theta)`, `Gamma(alpha, lambda)`, `Gamma(alpha, mu)`: Gamma distribution with parameter `alpha`: shape, and one of `theta`: scale, `lambda`: rate, or `mu`: mean.
- `InverseGamma(alpha, theta)`, `Gamma(alpha, lambda)`, `Gamma(alpha, mu)`: Inverse gamma distribution. See `Gamma`.
- `Gompertz(eta, beta)`: Gompertz distribution with parameters `eta`: shape, `beta`: scale.
- `LogLogistic(mu, sigma)`, `LogLogistic(alpha, beta)`: Log-logistic distribution with parameters `mu`: log location, `sigma`: log scale, or `alpha`: scale, `beta`: shape.
- `LogNormal(mu, sigma)`: Log-normal distribution with parameters `mu`: log mean and `sigma`: log standard deviation.
- `Wald(mu, lambda)`, `Wald(nu, alpha, sigma)`: Wald (inverse gaussian) with parameters `mu`: mean, `lambda`: shape, or `nu`: drift rate, `alpha`: threshold, `sigma`: noise. Note that for `Wald(nu, alpha, sigma)`, at least one of the three parameters must be fixed.
- `Weibull(shape, scale)`: Weibull distribution with parameters `shape`: shape, `scale`: scale.

#### Dynamic support

Support of these distributions depends on the parameter values.

- `Amoroso(a, theta, alpha, beta)`: Amoroso distribution with parameters `a`: location/bound, `theta`: scale, `alpha`: shape, `beta`: shape. The support is from `a` to infinity if `theta` is positive, and from -infinity to a if `theta` is negative. The parameter `a` is fixed by default. It is advisable to initialize `theta` at negative/positive value, depending on what boundedness you want to capture with the distribution.
- `StretchedBeta(alpha, beta, min, max)`: Beta distribution (see `Beta(alpa, beta)` above) stretched to the interval between parameters `min` and `max`. These two parameters are fixed by default.
- `Frechet(alpha, sigma, theta)`: Fréchet distribution with parameters `alpha`: shape, `sigma`: scale, `theta`: location. The distribution is defined from `theta` to infinity.
- `Pareto(alpha, beta)`: Pareto distribution with parameters `alpha`: shape, `beta`: scale.  The distribution is defined from `beta` to infinity.
- `Triangular(a, b, c)`: Triangular distribution with `a`: minimum, `b`: maximum, and `c`: mode. The distribution is defined from `a` to `b`.
- `Uniform(min, max)`: Uniform distribution with `min`: minimum, `max`: maximum. The distribution is defined from `min` to `max`.

Shifted distributions are another class of distributions with a dynamic support, and are made by adding a `shift` parameter to distributions that are bounded between 0 and infinity. These are `ShiftedExponential`, `ShiftedLogNormal`, `ShiftedGamma`, `ShiftedInverseGamma`, `ShiftedLogLogistic`, `ShiftedWald`, `ShiftedWeibull`. Note that estimating the `shift` parameter might be tricky for some of these distributions and for some data; it may be necessary to fix it.