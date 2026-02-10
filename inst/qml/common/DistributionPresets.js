.pragma library

var unbounded = [
    "normal(mu=0, sigma=1)",
    "cauchy(mu=0, sigma=1)",
    "student_t(nu=5, mu=0, sigma=1)",
    "gumbel(mu=0, beta=1)",
    "laplace(mu=0, beta=1)",
    "logistic(mu=0, sigma=1)",
    "skew_normal(xi=0, omega=1, alpha=0)",
    "skew_cauchy(xi=0, omega=1, alpha=0)",
    "skew_t(xi=0, omega=1, alpha=0, nu=5)",
    "symmetric_generalized_normal(mu=0, alpha=1, beta=2)"
]

var shifted = [
    "shifted_exponential(lambda=1)",
    "shifted_log_normal(mu=0, sigma=1)",
    "shifted_gamma(alpha=2, theta=1)",
    "shifted_inverse_gamma(alpha=2, theta=1)",
    "shifted_log_logistic(mu=0, sigma=1)",
    "shifted_wald(mu=1, lambda=1)",
    "shifted_weibull(shape=2, scale=1)"
]

var bounded = [
    "exponential(lambda=1)",
    "log_normal(mu=0, sigma=1)",
    "gamma(alpha = 2, theta = 1)",
    "inverse_gamma(alpha = 2, theta = 1)",
    "gompertz(eta=2, beta=1)",
    "log_logistic(mu=0, sigma=1)",
    "wald(mu=1, lambda=1)",
    "weibull(shape=2, scale=1)"
]

function fixedShifted(shiftValue) {
    var shifted = [
        "shifted_exponential(lambda=1, shift=fixed(.shift))",
        "shifted_log_normal(mu=0, sigma=1, shift=fixed(.shift))",
        "shifted_gamma(alpha=2, theta=1, shift=fixed(.shift))",
        "shifted_inverse_gamma(alpha=2, theta=1, shift=fixed(.shift))",
        "shifted_log_logistic(mu=0, sigma=1, shift=fixed(.shift))",
        "shifted_wald(mu=1, lambda=1, shift=fixed(.shift))",
        "shifted_weibull(shape=2, scale=1, shift=fixed(.shift))"
    ]
    return shifted.map(function(dist) { return dist.replace(".shift", shiftValue) })
}