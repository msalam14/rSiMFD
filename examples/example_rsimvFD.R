# Load required package
library(mvtnorm)

# Example parameters
n <- 25
m_i <- rep(4, n)

# Spatial index: 5x5 grid on [0,1]^2
spat_indx <- as.matrix(expand.grid(seq(0, 1, length.out = 5),
                                   seq(0, 1, length.out = 5)))

# Functional argument (time points)
funARG <- seq(0, 1, length.out = 20)

# Mean function: p = 2 dimensional
meanFUN <- function(t) {
  c(sin(2 * pi * t), cos(2 * pi * t))
}

# Basis functions for p = 2
basisFUN <- list(
  function(t) cbind(sin(pi * t), cos(pi * t)),
  function(t) cbind(sin(2 * pi * t), cos(2 * pi * t))
)

# Variance of zeta (2 components)
zetaVAR <- c(1, 0.5)

# Correlation functions for xi
corrFUN <- list(
  function(r) corSpatialVal(r,d=0.3,nugget = 0.15,type = "exponential"),      # exponential correlation
  function(r) corSpatialVal(r,d=0.45,nugget = 0.05,type = "spherical") # slower decay
)

# Spatial variance and noise
spatVAR <- c(1, 0.5)
sigma2 <- 0.1

# Generate data
set.seed(123)
sim_data <- rsimvFD(n, m_i, spat_indx, funARG,
                    meanFUN, basisFUN, zetaVAR,
                    corrFUN, spatVAR, sigma2)

# Check dimensions
str(sim_data)

# Plot first subject, first function
image(sim_data$data[[1]][1:20, ], main = "Example simulated functional data")
