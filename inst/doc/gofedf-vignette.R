## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(gofedf)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages('gofedf')

## ----eval=FALSE---------------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github('pnickchi/gofedf')

## -----------------------------------------------------------------------------
library(gofedf)

# Reproducible example
set.seed(123)

# Randomly generate some data from Normal distribution
n <- 50
x <- rnorm(n)

# Test if the data follows a Normal distribution by calculating the Cramer-von Mises statistic and approximate p-value of the test.
testNormal(x = x, method = 'cvm')

## -----------------------------------------------------------------------------
# Generate some random sample from a non Normal distribution.
x <- rgamma(n, shape = 3)
testNormal(x = x, method = 'cvm')

## -----------------------------------------------------------------------------
# Reproducible example
set.seed(123)

# Randomly generate some data
n <- 50
x <- rgamma(n, shape = 1)

# Test if the data follows a Gamma distribution, calculate Cramer-von Mises statistic and approximate p-value
testGamma(x = x, method = 'cvm')

## -----------------------------------------------------------------------------
# Reproducible example
set.seed(123)

# Create a set of explanatory variables and a response variable according to a linear model

# Sample size
n <- 50

# Number of explanatory variables
p <- 5

# Generate some coefficients
b <- runif(p)

# Simulate random explanatory variables
X <- matrix( runif(n*p), nrow = n, ncol = p)

# Generate some error terms from Normal distribution
e <- rnorm(n)

# Generate response variable according to the linear model
y <- X %*% b + e

# Test if the residuals of the model follows a Normal distribution, calculate Cramer-von Mises statistic and approximate p-value
testLMNormal(x = X, y)

## -----------------------------------------------------------------------------
# Or alternatively just pass 'lm.fit' object directly instead:
lm.fit <- lm(y ~ X, x = TRUE, y = TRUE)
testLMNormal(fit = lm.fit)

## -----------------------------------------------------------------------------
# Reproducible example
set.seed(123)

# Create a set of explanatory variables and a response variable according to a generalized linear model.

# Sample size
n <- 50

# Number of explanatory variables
p <- 5

# Simulate random explanatory variables
X <- matrix( rnorm(n*p, mean = 10, sd = 0.1), nrow = n, ncol = p)

# Generate some coefficients
b <- runif(p)

# Generate some error terms from Gamma distribution
e <- rgamma(n, shape = 3)

# Generate response variable according to the generalized linear model (log link function)
y <- exp(X %*% b) * e

# Test if the Gamma assumptions of the response variable holds by calculating the Cramer-von Mises statistic and approximate p-value
testGLMGamma(x=X, y, l = 'log', method = 'cvm')

## -----------------------------------------------------------------------------
# Or alternatively just pass 'glm.fit' object directly instead:
glm.fit <- glm2::glm2(y ~ X, family = Gamma(link = 'log'), x = TRUE, y = TRUE)
testGLMGamma(fit = glm.fit, l = 'log')

## -----------------------------------------------------------------------------
# Example: Inverse Gaussian (IG) distribution with weights

# Reproducible example
set.seed(123)

# Set the sample size
n <- 50

# Assign weights
weights <- runif(n, min = 5, max = 6)
weights <- weights / sum(weights)

# Set mean and shape parameters for IG distribution.
mio        <- 2
lambda     <- 2

# Generate a random sample from IG distribution with weighted shape.
y <- statmod::rinvgauss(n, mean = mio, shape = lambda * weights)

# Compute MLE of parameters, score matrix, and pit values.
theta_hat    <- IGMLE(obs = y,   w = weights)
score.matrix <- IGScore(obs = y, w = weights, mle = theta_hat)
pit.values   <- IGPIT(obs = y ,  w = weights, mle = theta_hat)

# Apply the goodness-of-fit test.
testYourModel(pit = pit.values, score = score.matrix)

