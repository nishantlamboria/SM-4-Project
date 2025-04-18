# --- Step 1: Prepare the Data ---
state_trend <- trend                      # Length 1308
months <- rep(1:12, 109)                  # Month index for seasonal extraction

# --- Step 2: Estimate alpha_i ---
alphas <- numeric(19)
for (i in 1:19) {
  y_i <- trimmed_mat[, i]
  alphas[i] <- sum(y_i * state_trend) / sum(state_trend^2)
}

# --- Step 3: Estimate district-specific seasonal components ---
seasonal_mat <- matrix(0, nrow = 12, ncol = 19)

for (i in 1:19) {
  y_i <- trimmed_mat[, i]
  residual_i <- y_i - alphas[i] * state_trend
  for (m in 1:12) {
    seasonal_mat[m, i] <- mean(residual_i[months == m])
  }
}

# --- Step 4: Build fitted values and compute R-squared ---
district_fits_final <- matrix(0, nrow = 1308, ncol = 19)
r_squared_final <- numeric(19)

for (i in 1:19) {
  fitted_i <- numeric(1308)
  for (t in 1:1308) {
    m_t <- months[t]
    fitted_i[t] <- alphas[i] * state_trend[t] + seasonal_mat[m_t, i]
  }
  district_fits_final[, i] <- fitted_i
  actual <- trimmed_mat[, i]
  ss_res <- sum((actual - fitted_i)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared_final[i] <- 1 - ss_res / ss_tot
}

# --- Optional: Print R-squared ---
for (i in 1:19) {
  cat(sprintf("District %02d - R-squared (Final Model): %.6f\n", i, r_squared_final[i]))
}

# --- Plotting the fits ---

# Choose the districts you want to inspect (e.g., 3, 9, 14)
selected_districts <- 1:19

for (k in selected_districts) {
  actual_k <- trimmed_mat[, k]
  fitted_k <- district_fits_final[, k]
  
  df_k <- data.frame(
    index = 1:1308,
    actual = actual_k,
    fitted = fitted_k
  )
  
  g_k <- ggplot(df_k, aes(x = index)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.5, alpha = 0.6) +
    geom_line(aes(y = fitted, color = "Final Model Fit"), linewidth = 0.6) +
    scale_color_manual(values = c("Actual" = "#e74c3c", "Final Model Fit" = "#2ecc71")) +
    scale_x_continuous(
      breaks = year_breaks,
      labels = year_labels
    ) +
    labs(
      #title = paste("District", k, "Final Model Fit"),
      x = "Year", y = "Rainfall (mm)", color = ""
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.title = element_text(size = 13, face = "bold"),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      legend.position = "top",
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  print(g_k)
}

# --- Adjusted R-squared vector ---

adj_r_squared_final <- 1 - ((1 - r_squared_final) * (n - 1) / (n - 3))  # p = 2
