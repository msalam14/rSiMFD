d        <- 0.5     # range
nugget   <- 0.2     # nugget proportion
sill_cov <- 1.0     # sill variance

# covariance at specific distances
r_checks <- c(0, d/2, d, 1.5*d)
names(r_checks) <- c("r=0", "r=d/2", "r=d", "r> d")

types  <- c("spherical", "gaussian", "exponential")
labels <- c("Spherical", "Gaussian", "Exponential")

check_mat <- sapply(types, function(tp) {
  covSpatialVal(r_checks, d, nugget = nugget, sill_cov = sill_cov, type = tp)
})
colnames(check_mat) <- labels
print(round(check_mat, 4))

# Smooth covariance function of different types
r_vals <- seq(0, 2*d, length.out = 400)

df_list <- lapply(seq_along(types), function(i) {
  tp  <- types[i]
  lbl <- labels[i]
  data.frame(
    r = r_vals,
    Covariance = covSpatialVal(r_vals, d, nugget = nugget, sill_cov = sill_cov, type = tp),
    Type = lbl,
    stringsAsFactors = FALSE
  )
})
df_plot <- do.call(rbind, df_list)

# plotting of various autocovariance functions
ggplot(df_plot, aes(x = r, y = Covariance, color = Type)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = d, linetype = 2) +
  labs(
    x = "Distance r",
    y = "Spatial autocovariance",
    title = "Spherical vs Gaussian vs Exponential (with nugget)",
    subtitle = paste0("range d = ", d, "  |  nugget = ", nugget, "  |  sill = ", sill_cov),
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
