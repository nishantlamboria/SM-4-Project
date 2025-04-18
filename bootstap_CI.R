# --- Compute Residuals ---
residual_mat <- trimmed_mat - district_fits_final

# --- Bootstrap Confidence Intervals for \alpha_i ---
set.seed(123)  # For reproducibility
B <- 1000  # Number of bootstrap replicates
alpha_boot <- matrix(NA, nrow = B, ncol = 19)

for (b in 1:B) {
  D_boot <- matrix(NA, nrow = 1308, ncol = 19)
  
  for (i in 1:19) {
    resample_resid <- sample(residual_mat[, i], size = 1308, replace = TRUE)
    for (t in 1:1308) {
      m_t <- months[t]
      D_boot[t, i] <- alphas[i] * state_trend[t] + seasonal_mat[m_t, i] + resample_resid[t]
    }
  }
  
  # Recompute alpha_i from bootstrap sample
  for (i in 1:19) {
    y_star <- D_boot[, i]
    seasonal_star <- sapply(1:1308, function(t) seasonal_mat[months[t], i])
    y_detrended <- y_star - seasonal_star
    alpha_boot[b, i] <- sum(y_detrended * state_trend) / sum(state_trend^2)
  }
}

# --- Construct 95% CI for alpha_i ---
alpha_CI_lower <- apply(alpha_boot, 2, quantile, probs = 0.025)
alpha_CI_upper <- apply(alpha_boot, 2, quantile, probs = 0.975)

alpha_CI_df <- data.frame(
  District = 1:19,
  Alpha_Estimate = alphas,
  CI_Lower = alpha_CI_lower,
  CI_Upper = alpha_CI_upper
)

# --- View or Export ---
print(alpha_CI_df)