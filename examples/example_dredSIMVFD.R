set.seed(202)
# simulating training data
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
  function(r) corSpatialVal(r,d=0.5,nugget = 0.05,type = "exponential"),      # exponential correlation
  function(r) corSpatialVal(r,d=0.35,nugget = 0.01,type = "spherical") # slower decay
)

# Spatial variance and noise
spatVAR <- c(3, 2.5)
sigma2 <- 5


# ------------------------------------------------------------
# 2) Simulate training and test data via rsimvFD
# ------------------------------------------------------------
# Assumes rsimvFD() is already defined/loaded
fDATA <- rsimvFD(n, m_i, spat_indx, funARG,
                 meanFUN, basisFUN, zetaVAR, corrFUN, spatVAR, sigma2)

# Optional: a separate test set
n_test <- 8
m_test <- rep(3, n_test)
testFDATA <- rsimvFD(n_test, m_test, spat_indx, funARG,
                     meanFUN, basisFUN, zetaVAR, corrFUN, spatVAR, sigma2)

# ------------------------------------------------------------
# 3) Run the analysis pipeline with dredSIMVFD
# ------------------------------------------------------------
# Assumes dredSIMVFD() is already defined/loaded
PVE_vec <- c(0.95, 0.90)  # first-stage FPCA PVE, then stage-2 MFPCA PVE
corrM   <- "exponential"  # nlme::corSpatial type: "exponential"/"gaussian"/"spherical"

fit <- dredSIMVFD(
  fDATA      = fDATA$data,
  indxJ      = m_i,
  spat_indx  = spat_indx,
  funARG     = funARG,
  PVE        = PVE_vec,
  corrM      = corrM,
  center     = FALSE,
  mfpca_method = "HG",     # use Happ & Greven route (via fpca.sc on each margin)
  testFDATA  = testFDATA$data,
  test_indxJ = m_test
)

# Quick peek at key elements
cat("Subjects (train):", fit$n, " | Functions p:", fit$p, " | Spatial sites L:", fit$L, "\n")
cat("MFPCA subject score dims Zeta: ", paste(dim(fit$Zeta), collapse = " x "), "\n")
if (!is.null(fit$testRD)) {
  cat("Test RD dims: ", paste(dim(fit$testRD), collapse = " x "), "\n")
}

# ------------------------------------------------------------
# 4) Visualizations (ggplot)
#    a) Subject-level MFPCA scores (first two components)
# ------------------------------------------------------------
if (ncol(fit$Zeta) >= 2) {
  scores_df <- data.frame(Subject = 1:fit$n,
                          PC1 = fit$Zeta[, 1],
                          PC2 = fit$Zeta[, 2])
  p_scores <- ggplot(scores_df, aes(x = PC1, y = PC2)) +
    geom_point() +
    labs(title = "Subject scores (first two MFPCA components)",
         x = "PC1 score", y = "PC2 score") +
    theme_minimal(base_size = 12)
  print(p_scores)
}

# ------------------------------------------------------------
#    b) Spatial mean surface for the first component
#       (fit$spatMEAN is a list; each element is length L = # grid points)
# ------------------------------------------------------------
if (length(fit$spatMEAN) >= 1) {
  spat_df <- data.frame(spat_indx, Mean = fit$spatMEAN[[1]])
  p_spat <- ggplot(spat_df, aes(x = Row, y = Col, fill = Mean)) +
    geom_tile() +
    coord_equal() +
    labs(title = "Spatial mean (component 1)",
         x = "Row", y = "Col", fill = "Mean") +
    theme_minimal(base_size = 12)
  print(p_spat)
}

# ------------------------------------------------------------
#    c) Distribution of reduced-dimension (RD) quadratic forms across components
#       (per subject-occasion block)
# ------------------------------------------------------------
trainRD_long <- fit$trainRD %>%
  mutate(Idx = row_number()) %>%
  pivot_longer(cols = -Idx, names_to = "Component", values_to = "RD")

p_rd <- ggplot(trainRD_long, aes(x = Component, y = RD)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(title = "Train reduced-dimension (RD) values by component",
       x = "Component", y = "RD (quadratic form)") +
  theme_minimal(base_size = 12)
print(p_rd)

if (!is.null(fit$testRD)) {
  testRD_long <- fit$testRD %>%
    mutate(Idx = row_number()) %>%
    pivot_longer(cols = -Idx, names_to = "Component", values_to = "RD") %>%
    mutate(Set = "Test")
  
  trainRD_long2 <- trainRD_long %>% mutate(Set = "Train")
  both_long <- bind_rows(trainRD_long2, testRD_long)
  
  p_rd_cmp <- ggplot(both_long, aes(x = Component, y = RD, fill = Set)) +
    geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.2) +
    labs(title = "Train vs Test RD by component",
         x = "Component", y = "RD (quadratic form)", fill = NULL) +
    theme_minimal(base_size = 12)
  print(p_rd_cmp)
}

# ------------------------------------------------------------
# End of example
# ------------------------------------------------------------
