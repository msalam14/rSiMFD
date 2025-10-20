Cov0  <- 1.0     # total variance (sill)
rng <- 0.4     # range parameter
ngt <- 0.2    # nugget proportion

# Covariance at specific distances
h_vals<- c(0, rng/2, rng, 1.5*rng)
names(h_vals) <- c("h=0", "h=rng/2", "h=rng", "h>rng")

cov_val <- sapply(h_vals, function(h) COVspher(h, Cov0, rng, ngt))
print(round(cov_val, 4))

# Covariance function
h_val_seq  <- seq(0, 2*rng, length.out = 300)
cov_val_seq <- sapply(h_val_seq, function(h) COVspher(h, Cov0, rng, ngt))

# Plotting the resulting covariance
plot(h_val_seq, cov_val_seq, type = "l", lwd = 2,
     xlab = "Distance h", ylab = "Spherical covariance",
     main = "Spherical covariance with nugget")
abline(v = range, lty = 2)  # mark range
legend("topright",
       legend = c(paste0("range = ", range),
                  paste0("nugget = ", nugget),
                  paste0("Cov0 = ", Cov0)),
       bty = "n")

