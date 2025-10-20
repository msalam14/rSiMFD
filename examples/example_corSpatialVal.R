# Parameters
d       <- 0.5    # range
nugget  <- 0.2    # nugget proportion
types   <- c("spherical", "gaussian", "exponential")

# Sanity checks at specific distances
r_checks <- c(0, d/2, d, 1.5*d)
names(r_checks) <- c("r=0", "r=d/2", "r=d", "r> d")

check_mat <- sapply(types, function(tp) {
  sapply(r_checks, function(r) corSpatialVal(r, d, nugget = nugget, type = tp))
})
print(round(check_mat, 4))

# Vectorized curve over distance
r_vals <- seq(0, 2*d, length.out = 400)

corr_sph  <- corSpatialVal(r_vals, d, nugget = nugget, type = "spherical")
corr_gau  <- corSpatialVal(r_vals, d, nugget = nugget, type = "gaussian")
corr_exp  <- corSpatialVal(r_vals, d, nugget = nugget, type = "exponential") # anything else defaults to exponential

# Plot the three correlation functions
df_plot <- tibble::tibble(
  r = r_vals,
  Spherical  = corr_sph,
  Gaussian   = corr_gau,
  Exponential= corr_exp
) %>%
  pivot_longer(cols = -r, names_to = "Type", values_to = "Correlation")

ggplot(df_plot, aes(x = r, y = Correlation, color = Type)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = d, linetype = 2) +
  labs(
    x = "Distance r",
    y = "Spatial correlation",
    title = "Spherical vs Gaussian vs Exponential (with nugget)",
    subtitle = paste0("range d = ", d, "   |   nugget = ", nugget),
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
