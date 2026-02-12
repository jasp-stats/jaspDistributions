.pragma library

var unbounded = [
    "Normal(mu=0, sigma=1)",
    "Cauchy(mu=0, sigma=1)",
    "StudentT(nu=5, mu=0, sigma=1)",
    "Gumbel(mu=0, beta=1)",
    "Laplace(mu=0, beta=1)",
    "Logistic(mu=0, sigma=1)",
    "SkewNormal(xi=0, omega=1, alpha=0)",
    "SkewCauchy(xi=0, omega=1, alpha=0)",
    "SkewT(xi=0, omega=1, alpha=0, nu=5)",
    "SymmetricGeneralizedNormal(mu=0, alpha=1, beta=2)"
]

var shifted = [
    "ShiftedExponential(lambda=1)",
    "ShiftedLogNormal(mu=0, sigma=1)",
    "ShiftedGamma(alpha=2, theta=1)",
    "ShiftedInverseGamma(alpha=2, theta=1)",
    "ShiftedLogLogistic(mu=0, sigma=1)",
    "ShiftedWald(mu=1, lambda=1)",
    "ShiftedWeibull(shape=2, scale=1)"
]

var bounded = [
    "Exponential(lambda=1)",
    "LogNormal(mu=0, sigma=1)",
    "Gamma(alpha=2, theta=1)",
    "InverseGamma(alpha=2, theta=1)",
    "LogLogistic(mu=0, sigma=1)",
    "Wald(mu=1, lambda=1)",
    "Weibull(shape=2, scale=1)"
]

function fixedShifted(shiftValue) {
    var shifted = [
        "ShiftedExponential(lambda=1, shift=fixed(.shift))",
        "ShiftedLogNormal(mu=0, sigma=1, shift=fixed(.shift))",
        "ShiftedGamma(alpha=2, theta=1, shift=fixed(.shift))",
        "ShiftedInverseGamma(alpha=2, theta=1, shift=fixed(.shift))",
        "ShiftedLogLogistic(mu=0, sigma=1, shift=fixed(.shift))",
        "ShiftedWald(mu=1, lambda=1, shift=fixed(.shift))",
        "ShiftedWeibull(shape=2, scale=1, shift=fixed(.shift))"
    ]
    return shifted.map(function(dist) { return dist.replace(".shift", shiftValue) })
}