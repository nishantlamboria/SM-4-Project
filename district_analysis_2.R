# --- Initialize Storage ---

district_seasonals <- matrix(0, nrow = 12, ncol = 19)
district_estimates <- matrix(0, nrow = 1308, ncol = 19)
district_residuals <- matrix(0, nrow = 1308, ncol = 19)
r_squared_adj <- numeric(19)

month_index <- rep(1:12, times = 109)

# --- Fit Model for Each District ---
for (l in 1:19) {
  y_l <- trimmed_mat[, l]
  s_l <- numeric(12)
  
  for (m in 1:12) {
    s_l[m] <- mean(y_l[month_index == m] - trend[month_index == m])
  }
  
  # Save the seasonal component
  district_seasonals[, l] <- s_l
  
  # Reconstruct fitted values
  extended_s_l <- rep(s_l, times = 109)
  district_estimates[, l] <- trend + extended_s_l
  
  # Residuals and R-squared
  district_residuals[, l] <- y_l - district_estimates[, l]
  ss_res_l <- sum((y_l - district_estimates[, l])^2)
  ss_tot_l <- sum((y_l - mean(y_l))^2)
  r_squared_adj[l] <- 1 - ss_res_l / ss_tot_l
}

# --- Visualization and Reporting ---
for (l in 1:19) {
  plot_df_l <- data.frame(
    index = 1:1308,
    actual = trimmed_mat[, l],
    fitted = district_estimates[, l]
  )
  
  g_l <- ggplot(plot_df_l, aes(x = index)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.5, alpha = 0.6) +
    geom_line(aes(y = fitted, color = "District-Specific Seasonal Fit"), linewidth = 0.6) +
    scale_color_manual(values = c("Actual" = "#e74c3c", "District-Specific Seasonal Fit" = "#2ecc71")) +
    scale_x_continuous(
      breaks = year_breaks,
      labels = year_labels
    ) +
    labs(
      #title = paste("District", l, "District-Specific Seasonal Fit"),
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
  
  print(g_l)
  cat(sprintf("District %02d - R-squared: %.6f\n", l, r_squared_adj[l]))
}

# --- Adjusted R-squared vector ---

adj_r_squared_adj   <- 1 - ((1 - r_squared_adj)   * (n - 1) / (n - 2))  # p = 1
