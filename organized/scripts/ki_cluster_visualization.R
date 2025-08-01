# =============================================================================
# KI CLUSTER VISUALIZATION
# =============================================================================
# Erstellt eine Visualisierung der KI-Cluster basierend auf den Daten
# aus ki_specific_apa.png und cluster_prozent_tabelle.png

library(dplyr)
library(ggplot2)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Lade Daten...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data (FINISHED=1 and AB01=1 for KI group)
data_processed <- data %>% filter(FINISHED == 1)
data_ki <- data_processed %>% filter(AB01 == 1)

cat("✓ KI-Gruppe: n =", nrow(data_ki), "\n")

# =============================================================================
# PERFORM CLUSTERING
# =============================================================================

# Calculate composite scores
mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")

# Create composite scores
cluster_data <- data.frame(
  VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
)

# Remove missing values
cluster_data_clean <- cluster_data[complete.cases(cluster_data), ]

# Perform k-means clustering
set.seed(123)
kmeans_result <- kmeans(cluster_data_clean, centers = 3, nstart = 25)
clusters <- kmeans_result$cluster

# Calculate cluster means
cluster_means <- data.frame(
  cluster_number = 1:3,
  n = as.numeric(table(clusters)),
  VS = round(tapply(cluster_data_clean$VS, clusters, mean), 3),
  MN = round(tapply(cluster_data_clean$MN, clusters, mean), 3),
  ID = round(tapply(cluster_data_clean$ID, clusters, mean), 3),
  EA = round(tapply(cluster_data_clean$EA, clusters, mean), 3)
)

# Calculate overall mean for each cluster
cluster_means$overall_mean <- (cluster_means$VS + cluster_means$MN + 
                              cluster_means$ID + cluster_means$EA) / 4

# Sort by overall mean (highest to lowest)
cluster_means_sorted <- cluster_means[order(cluster_means$overall_mean, decreasing = TRUE), ]

# Assign names based on ranking
cluster_means_sorted$cluster_name <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")

cat("✓ Clustering abgeschlossen\n")
cat("Cluster-Verteilung:\n")
for(i in 1:nrow(cluster_means_sorted)) {
  cat("-", cluster_means_sorted$cluster_name[i], ":", cluster_means_sorted$n[i], 
      "(", round(cluster_means_sorted$n[i] / sum(cluster_means_sorted$n) * 100, 1), "%)\n")
}

# =============================================================================
# CREATE VISUALIZATION
# =============================================================================

cat("\n=== ERSTELLE KI CLUSTER VISUALISIERUNG ===\n")

# Prepare data for plotting - match the exact structure from the reference diagram
plot_data <- data.frame(
  Variable = rep(c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
                   "Identifikation", "Emotionale Ansprache"), each = 3),
  Cluster = rep(cluster_means_sorted$cluster_name, 4),
  Value = c(cluster_means_sorted$VS, cluster_means_sorted$MN, 
            cluster_means_sorted$ID, cluster_means_sorted$EA)
)

# Create the visualization matching the exact style
ki_cluster_plot <- ggplot(plot_data, aes(x = Cluster, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, 
           color = "black", linewidth = 0.3) +
  scale_fill_manual(values = c(
    "Vertrauen & Sympathie" = "#F1C682",      # Exakte Farbe
    "Menschlichkeit & Natürlichkeit" = "#BE4B5A",  # Exakte Farbe
    "Identifikation" = "#ABCD9B",             # Exakte Farbe
    "Emotionale Ansprache" = "#8DD3C8"        # Exakte Farbe
  )) +
  labs(x = "Cluster",
       y = "Mittelwert",
       fill = "Variable") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, family = "Times New Roman"),
    axis.text = element_text(size = 10, family = "Times New Roman"),
    axis.text.x = element_text(angle = 0, family = "Times New Roman", vjust = 0, margin = margin(t = 0, b = 0)),
    legend.title = element_text(size = 12, family = "Times New Roman"),
    legend.text = element_text(size = 10, family = "Times New Roman"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(0, "pt")
  ) +
  ylim(0, 5) +
  scale_y_continuous(breaks = seq(0, 5, 1), limits = c(0, 5))

# Save the visualization without outer border
dir.create("organized/images/clustering", recursive = TRUE, showWarnings = FALSE)
ggsave("organized/images/clustering/ki_cluster_visualization.png", 
       ki_cluster_plot, 
       width = 10, height = 6, dpi = 300, bg = "white", 
       limitsize = FALSE)

cat("✓ ki_cluster_visualization.png erstellt\n")

# =============================================================================
# DATA SUMMARY
# =============================================================================

cat("\n================================================================================\n")
cat("KI CLUSTER VISUALIZATION COMPLETED\n")
cat("================================================================================\n")
cat("Generated file: organized/images/clustering/ki_cluster_visualization.png\n")
cat("\nCluster data summary:\n")
print(cluster_means_sorted[, c("cluster_name", "n", "VS", "MN", "ID", "EA")])
cat("\n================================================================================\n") 