# Reshape to long format for ggplot
library(tidyr)

comparison_df <- data.frame(
  District = factor(1:19),
  Initial = r_squared_init,
  Seasonal_Adjustment = r_squared_adj,
  Final_Model = r_squared_final
)

long_df <- pivot_longer(comparison_df, 
                        cols = c(Initial, Seasonal_Adjustment, Final_Model),
                        names_to = "Model", 
                        values_to = "R_squared")

# --- Reorder factor levels for correct bar order ---
long_df$Model <- factor(long_df$Model, levels = c("Initial", "Seasonal_Adjustment", "Final_Model"))

# --- Plotting the results ---
library(ggplot2)
ggplot(long_df, aes(x = District, y = R_squared, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c(
    "Initial" = "#e74c3c", 
    "Seasonal_Adjustment" = "#3498db", 
    "Final_Model" = "#2ecc71"
  )) +
  labs(
    #title = "Model Comparison: R-squared across Districts",
    x = "District", y = "R-squared"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )