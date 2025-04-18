library(sf)
library(ggplot2)
library(dplyr)



Districts = c("Ambala", "Bhiwani", "Faridabad", "Fatehabad", "Gurgaon",
              "Hisar", "Jhajjar", "Jind", "Kaithal", "Karnal", "Kurukshetra",
              "Mewat", "Palwal", "Panipat", "Panchkula", "Mahendragarh",
              "Rewari", "Rohtak", "Sirsa", "Sonipat", "Yamunanagar")

trend_class <- rep(NA, length(alphas))

for (i in seq_along(alphas)) {
  if (alpha_CI_lower[i] > 1) {
    trend_class[i] <- "Above 1"
  } else if (alpha_CI_upper[i] < 1) {
    trend_class[i] <- "Below 1"
  } else {
    trend_class[i] <- "Around 1"
  }
}

# Data frame for plotting
map_data <- data.frame(
  District = tolower(Districts[c(-15, -16)]),
  alpha = alphas,
  CI_Lower = alpha_CI_lower,
  CI_Upper = alpha_CI_upper,
  Class = factor(trend_class, levels = c("Below 1", "Around 1", "Above 1"))
)





india <- st_read("IND_ADM2/geoBoundaries-IND-ADM2.shp")
# Subset to Haryana (only if needed — adjust this line based on your shapefile structure)
haryana_map <- india[india$shapeName %in% Districts, ]


# Ensure matching names
haryana_map$District <- tolower(haryana_map$shapeName)  # Adjust column name accordingly

haryana_merged <- haryana_map %>%
  left_join(map_data, by = "District")



# Add a new column with clean title-cased names for labeling
haryana_merged$label <- tools::toTitleCase(haryana_merged$shapeName)

ggplot(haryana_merged) +
  geom_sf(aes(fill = Class), color = "grey30", linewidth = 0.4) +
  geom_sf_text(aes(label = label), size = 3, color = "black", check_overlap = TRUE) +
  scale_fill_manual(
    values = c("Below 1" = "#d73027", "Around 1" = "#fee08b", "Above 1" = "#1a9850"),
    labels = c("Significantly Below 1", "Not Significantly Different", "Significantly Above 1")
  ) +
  labs(
    title = expression("District Classification Based on " ~ alpha[k] ~ " Trend Multipliers"),
    fill = "Trend Class"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
