# Create data frame
adj_comparison_df <- data.frame(
  District = factor(1:19),
  Seasonal_Adjustment = adj_r_squared_adj,
  Final_Model = adj_r_squared_final
)

# Reshape to long format
adj_long_df <- pivot_longer(adj_comparison_df,
                            cols = c(Seasonal_Adjustment, Final_Model),
                            names_to = "Model",
                            values_to = "Adjusted_R_squared")

# Reorder factor levels
adj_long_df$Model <- factor(adj_long_df$Model, levels = c("Seasonal_Adjustment", "Final_Model"))

# --- Plotting ---

ggplot(adj_long_df, aes(x = District, y = Adjusted_R_squared, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c(
    "Seasonal_Adjustment" = "#3498db", 
    "Final_Model" = "#2ecc71"
  )) +
  labs(
    #title = "Adjusted R-squared Comparison: Seasonal vs Final Model",
    x = "District", y = "Adjusted R-squared"
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
