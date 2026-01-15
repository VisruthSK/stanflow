library(brms)
library(posterior)

# Core model pieces
mixture(0.2)
brms::mixture(0.3)

# Draws handling
as_draws(1)
posterior::as_draws(2)
brms:::as_draws(3)

# Noise from non-Stan packages
stats::lm(mpg ~ cyl, data = mtcars)
