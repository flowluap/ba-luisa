# =============================================================================
# AGE CLUSTER VISUALIZATION
# =============================================================================
# Erstellt eine Visualisierung der Altersgruppen nach Clustern
# mit gleichen Farben und Style wie die anderen Visualisierungen

library(dplyr)
library(ggplot2)
library(tidyr)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Lade Daten...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data (FINISHED=1)
data_processed <- data %>% filter(FINISHED == 1)

cat("✓ Gesamte Stichprobe: n =", nrow(data_processed), "\n")

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
  VS = rowMeans(data_processed[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_processed[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_processed[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_processed[, ea_cols], na.rm = TRUE)
)

# Remove missing values
cluster_data_clean <- cluster_data[complete.cases(cluster_data), ]
data_clean <- data_processed[complete.cases(cluster_data), ]

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
cluster_means_sorted$cluster_name <- c("Offen", "Ambivalent", "Distanziert")

# Add cluster information to data
data_clean$Cluster <- clusters
data_clean$Cluster_Name <- cluster_means_sorted$cluster_name[clusters]

cat("✓ Clustering abgeschlossen\n")
cat("Cluster-Verteilung:\n")
for(i in 1:nrow(cluster_means_sorted)) {
  cat("-", cluster_means_sorted$cluster_name[i], ":", cluster_means_sorted$n[i], 
      "(", round(cluster_means_sorted$n[i] / sum(cluster_means_sorted$n) * 100, 1), "%)\n")
}

# =============================================================================
# CREATE AGE GROUPS
# =============================================================================

cat("\n=== ERSTELLE ALTERSGRUPPEN ===\n")

# Create age groups
data_clean$Age_Group <- cut(data_clean$SO01, 
                           breaks = c(17, 25, 35, 45, 55, 100),
                           labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
                           include.lowest = TRUE)

# Count persons by cluster and age group
age_cluster_counts <- data_clean %>%
  group_by(Cluster_Name, Age_Group) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(Age_Group %in% c("18-25", "26-35", "36-45", "46-55")) # Nur die gewünschten Altersgruppen

cat("Altersgruppen-Verteilung:\n")
print(age_cluster_counts)

# Debug: Check data structure
cat("\nDebug - SO01 Werte:\n")
print(table(data_clean$SO01))
cat("\nDebug - Age_Group Werte:\n")
print(table(data_clean$Age_Group))
cat("\nDebug - Cluster_Name Werte:\n")
print(table(data_clean$Cluster_Name))

# =============================================================================
# CREATE VISUALIZATION
# =============================================================================

cat("\n=== ERSTELLE ALTERSGRUPPEN-VISUALISIERUNG ===\n")

# Create the visualization matching the exact style
age_cluster_plot <- ggplot(age_cluster_counts, aes(x = Cluster_Name, y = Count, fill = Age_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, 
           color = "black", linewidth = 0.3) +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            size = 3, 
            family = "Times New Roman") +
  scale_fill_manual(values = c(
    "18-25" = "#F1C682",      # Gelb/Orange
    "26-35" = "#BE4B5A",      # Rot
    "36-45" = "#ABCD9B",      # Hellgrün
    "46-55" = "#8DD3C8"       # Hellblau/Türkis
  )) +
  labs(x = "Cluster",
       y = "Anzahl an Personen",
       fill = "Altersgruppe") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12, family = "Times New Roman"),
    axis.text = element_text(size = 10, family = "Times New Roman"),
    axis.text.x = element_text(angle = 0, family = "Times New Roman"),
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
    plot.margin = margin(0, 0, 0, 20),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(0, "pt")
  ) +
  scale_x_discrete(labels = c("Offen" = "Offen",
                             "Ambivalent" = "Ambivalent", 
                             "Distanziert" = "Distanziert"))

# Save the visualization
dir.create("organized/images/clustering", recursive = TRUE, showWarnings = FALSE)
ggsave("organized/images/clustering/age_cluster_visualization.png", 
       age_cluster_plot, width = 10, height = 6, dpi = 300, bg = "white", 
       limitsize = FALSE)

cat("✓ age_cluster_visualization.png erstellt\n")

# =============================================================================
# DATA SUMMARY
# =============================================================================

cat("\n================================================================================\n")
cat("AGE CLUSTER VISUALIZATION COMPLETED\n")
cat("================================================================================\n")
cat("Generated file: organized/images/clustering/age_cluster_visualization.png\n")
cat("\nAltersgruppen nach Cluster:\n")
print(age_cluster_counts)
cat("\nCluster data summary:\n")
print(cluster_means_sorted[, c("cluster_name", "n", "VS", "MN", "ID", "EA")])
cat("\n================================================================================\n") 