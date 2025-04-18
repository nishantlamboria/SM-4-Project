library(ggplot2)

# --- Trend Extraction using 13-point Moving Average ---

trend <- rep(0, 1320)
residual <- rep(0, 1320)

for (i in 7:1314) {
  trend[i] <- (2 * sum(state_vec[(i - 6):(i + 6)]) - state_vec[i - 6] - state_vec[i + 6]) / 24
  residual[i] <- state_vec[i] - trend[i]
}

# --- Seasonal Component Estimation ---

seasonal <- numeric(12)

for (j in 1:12) {
  seasonal[j] <- sum(residual[seq(j, length(residual), by = 12)])/109
}


# --- Center Seasonal and Adjust Trend to Preserve Overall Level ---
# Adjust trend to preserve overall level after centering seasonal component

trend <- trend[7:1314] + mean(seasonal)        # Adjust trend to preserve total level
seasonal <- seasonal - mean(seasonal)  # Center the seasonal component

# --- Extend the seasonal component to match full series length ---
extended_seasonal <- rep(seasonal, times = 110)[7:1314]

# --- Build Data Frame for Plotting and Evaluation ---

fitted <- trend + extended_seasonal

fit_df <- data.frame(
  actual = state_vec[7:1314],
  fitted = fitted,
  residual = state_vec[7:1314] - fitted
)

# --- Goodness of Fit ---

# --- R-squared ---
ss_res <- sum((fit_df$actual - fit_df$fitted)^2)
ss_tot <- sum((fit_df$actual - mean(fit_df$actual))^2)
r_squared <- 1 - (ss_res / ss_tot)

# --- Adjusted R-squared ---
n <- nrow(fit_df)  # 48
p <- 2             # Trend and Seasonality
adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))

# Print both
cat("R-squared:", round(r_squared, 7), "\n")
cat("Adjusted R-squared:", round(adj_r_squared, 7), "\n")


# Assign readable month labels from Jul 2018 to Jun 2022
month_labels <- format(seq(as.Date("1901-07-01"), by = "month", length.out = 1308), "%b-%Y")
fit_df$month <- factor(month_labels, levels = month_labels, ordered = TRUE)

# --- Plot Actual vs Fitted Values ---

# Define yearly breaks and labels
year_breaks <- seq(1, nrow(fit_df), by = 12)  # Every 12 months (assuming monthly data)
year_labels <- seq(1901, 2010, by = 1)        # Adjust this to match your actual year range

# Truncate labels if your data is less than 110 years (or extend if more)
year_labels <- year_labels[seq_along(year_breaks)]

# --- Plot Actual vs Fitted Values for 110 years ---
# Define breaks and labels every 5 years (60 months apart)
year_breaks <- seq(1, nrow(fit_df), by = 60)
year_labels <- seq(1901, 2010, by = 5)
year_labels <- year_labels[seq_along(year_breaks)]

# --- Improved Actual vs Fitted Plot ---
ggplot(fit_df, aes(x = seq_along(month), group = 1)) +
  geom_area(aes(y = actual), fill = "#e74c3c", alpha = 0.15) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 0.6, alpha = 0.7) +
  geom_line(aes(y = fitted, color = "Trend + Seasonal Fit"), linewidth = 0.8) +
  scale_color_manual(
    values = c("Actual" = "#e74c3c", "Trend + Seasonal Fit" = "#3498db")
  ) +
  scale_x_continuous(
    breaks = year_breaks,
    labels = year_labels
  ) +
  labs(
    #title = "Rainfall Fit for Haryana (1901 – 2010)",
    #subtitle = "Monthly rainfall with trend + seasonal component",
    x = "Year", y = "Rainfall (mm)", color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.title = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    legend.position = "top",
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )





# --- Residual Plot ---

ggplot(fit_df, aes(x = seq_along(residual), y = residual)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  #geom_smooth(method = "loess", se = TRUE, color = "darkred", fill = "tomato", alpha = 0.2, linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
  scale_x_continuous(
    breaks = year_breaks,
    labels = year_labels
  ) +
  labs(
    #title = "Residual Plot for Haryana Rainfall Model (1901 – 2010)",
    #subtitle = "Deviation from trend + seasonal fit",
    x = "Year",
    y = "Residual (mm)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    panel.grid.minor = element_blank()
  )



# Plotting trends ans seasonals


# --- Trend Plot (1901–2010) ---

trend_df <- data.frame(
  year_month = seq_along(trend),
  trend = trend,
  month = month_labels  # Already computed in your code
)

ggplot(trend_df, aes(x = year_month, y = trend)) +
  geom_line(color = "#2c3e50", linewidth = 0.8) +
  scale_x_continuous(
    breaks = year_breaks,
    labels = year_labels
  ) +
  labs(
    #title = "Estimated Trend Component (1901–2010)",
    x = "Year", y = "Smoothed Rainfall Trend (mm)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )




# --- Seasonal Component Plot ---

seasonal_df <- data.frame(
  Month = factor(month.abb, levels = month.abb),  # Jan to Dec
  Seasonal = seasonal
)

ggplot(seasonal_df, aes(x = Month, y = Seasonal)) +
  geom_col(fill = "#3498db", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
  labs(
    #title = "Estimated Seasonal Component (Monthly Averages)",
    x = "Month", y = "Seasonal Effect (mm)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 11),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )



# Residual histogram

ggplot(fit_df, aes(x = residual)) +
  geom_histogram(
    bins = 40, fill = "#e74c3c", color = "white", alpha = 0.8
  ) +
  geom_vline(aes(xintercept = mean(residual)), color = "black", linetype = "dashed", linewidth = 0.7) +
  labs(
    #title = "Histogram of Residuals",
    x = "Residual (mm)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )
