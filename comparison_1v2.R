# --- Calculate Improvement ---

r_squared_improvement <- r_squared_adj - r_squared_init
print(mean(r_squared_improvement))

# --- Combine R-squared Values into a Data Frame ---
r2_df <- data.frame(
  District = factor(1:19),
  Initial_Model = r_squared_init,
  Improved_Model = r_squared_adj,
  Improvement = r_squared_improvement
)

# --- Reshape to Long Format for ggplot2 ---
library(tidyr)
r2_long <- pivot_longer(r2_df, cols = c("Initial_Model", "Improved_Model"),
                        names_to = "Model", values_to = "R_squared")
r2_long$Model <- factor(r2_long$Model, levels = c("Initial_Model", "Improved_Model"))


# --- Plot R-squared Comparison ---

ggplot(r2_long, aes(x = District, y = R_squared, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Initial_Model" = "#e67e22", "Improved_Model" = "#27ae60")) +
  labs(
    #title = "Comparison of R-squared Values by District",
    #subtitle = "Initial vs District-Specific Seasonal Adjustment Model",
    x = "District",
    y = expression(R^2),
    fill = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

# --- Plot the improvement ---

ggplot(r2_df, aes(x = District, y = Improvement)) +
  geom_col(fill = "#2980b9") +
  labs(
    title = "Improvement in R-squared with District-Specific Seasonality",
    x = "District",
    y = expression(Delta * R^2)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )