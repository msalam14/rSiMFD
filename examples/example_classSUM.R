set.seed(123)
n <- 400
x <- rnorm(n)
# True probabilities by a logistic model
p_true <- plogis(-0.3 + 1.2 * x)
# Actual labels (0/1)
actual <- rbinom(n, 1, p_true)

# Predicted probabilities
logit_hat <- -0.3 + 1.1 * x + rnorm(n, sd = 0.5)
prd_scr <- plogis(logit_hat)

# classifications summaries at 0.5 threshold 
default_metrics <- classSUM(actual, prd_scr, thresh = 0.5)
names(default_metrics) <- c("Accuracy", "Sensitivity", "Specificity")
print(round(default_metrics, 3))

# a grid of thresholds
thr_grd <- seq(0, 1, by = 0.01)

metric_mat <- sapply(thr_grd, function(t) classSUM(actual, prd_scr, thresh = t))

# Find threshold that maximizes Youden's J = Sens + Spec - 1
youden <- metric_mat[2, ] + metric_mat[3, ] - 1
thr_opt <- thr_grd[which.max(youden)]
opt_metrics <- metric_mat[, which.max(youden)]
names(opt_metrics) <- c("Accuracy", "Sensitivity", "Specificity")

cat("Optimal threshold by Youden's J:", round(thr_opt, 3), "\n")
print(round(opt_metrics, 3))

as.data.frame(t(metric_mat)) %>%
  set_names("Accuracy","Sensitivity","Specificity") %>%
    mutate(threshold = thr_grd) %>%
      pivot_longer(-threshold,names_to = "metric",values_to = "value") %>%
  ggplot(aes(x = threshold, y = value, color = metric)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0.5, linetype = 2) +
  geom_vline(xintercept = thr_opt, linetype = 3) +
  labs(
    x = "Threshold",
    y = "Metric value",
    title = "Accuracy, Sensitivity, and Specificity vs Threshold",
    subtitle = paste0("Dashed: 0.5   |   Dotted: Youden-optimal = ", round(thr_opt, 3)),
    color = NULL
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
