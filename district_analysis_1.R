# --- Prepare Trimmed District Matrix (Removing NA from trend) ---
# List of all district vectors (named or unnamed)
district_vectors <- list(ambala_vec, bhiwani_vec, faridabad_vec, fatehabad_vec,
                         gurgaon_vec, hissar_vec, jhajjar_vec, jind_vec, kaithal_vec,
                         karnal_vec, kurukshetra_vec, mewat_vec,
                         palwal_vec, panipat_vec, rewari_vec,
                         rohtak_vec, sirsa_vec, sonepat_vec, yamunanagar_vec)

# Trim each vector from index 7 to 1314 (length 1308)
trimmed_vectors <- lapply(district_vectors, function(x) x[7:1314])

# Combine into a 1308 × 21 matrix (columns = districts)
trimmed_mat <- do.call(cbind, trimmed_vectors)

# Optional: Add column names
colnames(trimmed_mat) <- districts

# --- Initialize Storage ---
lambdas <- numeric(19)
estimates <- matrix(0, nrow = 1308, ncol = 19)
r_squared_init <- numeric(19)

# --- Compute Lambda_k and Estimates ---
for (k in 1:19) {
  y_k <- trimmed_mat[, k]
  lambdas[k] <- sum((y_k - trend) * extended_seasonal) / sum(extended_seasonal^2)
  estimates[, k] <- trend + extended_seasonal * lambdas[k]
}

# --- Compute and Plot R-squared for Each District ---
# --- Compute and Plot R-squared for Each District ---
for (k in 1:19) {
  y_actual <- trimmed_mat[, k]
  y_fit <- estimates[, k]
  
  # Compute R-squared
  ss_res_k <- sum((y_actual - y_fit)^2)
  ss_tot_k <- sum((y_actual - mean(y_actual))^2)
  r_squared_k <- 1 - (ss_res_k / ss_tot_k)
  r_squared_init[k] <- r_squared_k
  
  # Prepare plotting dataframe
  plot_df_k <- data.frame(
    index = 1:1308,  # numeric x-axis for months
    actual = y_actual,
    fitted = y_fit
  )
  
  
  # Plot
  g_k <- ggplot(plot_df_k, aes(x = index)) +
    geom_line(aes(y = actual, color = "Actual"), linewidth = 0.5, alpha = 0.6) +
    geom_line(aes(y = fitted, color = "District Model Fit"), linewidth = 0.6) +
    scale_color_manual(values = c("Actual" = "#e74c3c", "District Model Fit" = "#2ecc71")) +
    scale_x_continuous(
      breaks = year_breaks,
      labels = year_labels
    ) +
    labs(
      #title = paste("Rainfall Fit - District", k, "(1901 – 2010)"),
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
  cat(sprintf("District %02d - R-squared: %.6f\n", k, r_squared_k))
}


# --- Adjusted R-squared vector ---

adj_r_squared_init  <- 1 - ((1 - r_squared_init)  * (n - 1) / (n - 2))  # p = 1