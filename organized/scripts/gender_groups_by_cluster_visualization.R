# =============================================================================
# GENDER GROUPS BY CLUSTER VISUALIZATION
# =============================================================================
# Erstellt Visualisierungen der Geschlechtergruppen für KI-Gruppe und Mensch-Gruppe
# getrennt, mit anderen Farben als üblich, aber gleichem Style

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

# Separate KI and Mensch groups
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("✓ KI-Gruppe: n =", nrow(data_ki), "\n")
cat("✓ Mensch-Gruppe: n =", nrow(data_mensch), "\n")

# =============================================================================
# PERFORM CLUSTERING FOR KI GROUP
# =============================================================================

cat("\n=== CLUSTERING FÜR KI-GRUPPE ===\n")

# Calculate composite scores for KI group
mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")

# Create composite scores for KI
cluster_data_ki <- data.frame(
  VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
)

# Remove missing values for KI
cluster_data_ki_clean <- cluster_data_ki[complete.cases(cluster_data_ki), ]
data_ki_clean <- data_ki[complete.cases(cluster_data_ki), ]

# Perform k-means clustering for KI
set.seed(123)
kmeans_result_ki <- kmeans(cluster_data_ki_clean, centers = 3, nstart = 25)
clusters_ki <- kmeans_result_ki$cluster

# Calculate cluster means for KI
cluster_means_ki <- data.frame(
  cluster_number = 1:3,
  n = as.numeric(table(clusters_ki)),
  VS = round(tapply(cluster_data_ki_clean$VS, clusters_ki, mean), 3),
  MN = round(tapply(cluster_data_ki_clean$MN, clusters_ki, mean), 3),
  ID = round(tapply(cluster_data_ki_clean$ID, clusters_ki, mean), 3),
  EA = round(tapply(cluster_data_ki_clean$EA, clusters_ki, mean), 3)
)

# Calculate overall mean for each cluster
cluster_means_ki$overall_mean <- (cluster_means_ki$VS + cluster_means_ki$MN + 
                                 cluster_means_ki$ID + cluster_means_ki$EA) / 4

# Sort by overall mean (highest to lowest)
cluster_means_ki_sorted <- cluster_means_ki[order(cluster_means_ki$overall_mean, decreasing = TRUE), ]

# Assign names based on ranking for KI
cluster_means_ki_sorted$cluster_name <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")

# Add cluster information to KI data
data_ki_clean$Cluster <- clusters_ki
data_ki_clean$Cluster_Name <- cluster_means_ki_sorted$cluster_name[clusters_ki]

cat("KI-Gruppe Cluster-Verteilung:\n")
for(i in 1:nrow(cluster_means_ki_sorted)) {
  cat("-", cluster_means_ki_sorted$cluster_name[i], ":", cluster_means_ki_sorted$n[i], 
      "(", round(cluster_means_ki_sorted$n[i] / sum(cluster_means_ki_sorted$n) * 100, 1), "%)\n")
}

# =============================================================================
# PERFORM CLUSTERING FOR MENSCH GROUP
# =============================================================================

cat("\n=== CLUSTERING FÜR MENSCH-GRUPPE ===\n")

# Create composite scores for Mensch
cluster_data_mensch <- data.frame(
  VS = rowMeans(data_mensch[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_mensch[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_mensch[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_mensch[, ea_cols], na.rm = TRUE)
)

# Remove missing values for Mensch
cluster_data_mensch_clean <- cluster_data_mensch[complete.cases(cluster_data_mensch), ]
data_mensch_clean <- data_mensch[complete.cases(cluster_data_mensch), ]

# Perform k-means clustering for Mensch
set.seed(123)
kmeans_result_mensch <- kmeans(cluster_data_mensch_clean, centers = 3, nstart = 25)
clusters_mensch <- kmeans_result_mensch$cluster

# Calculate cluster means for Mensch
cluster_means_mensch <- data.frame(
  cluster_number = 1:3,
  n = as.numeric(table(clusters_mensch)),
  VS = round(tapply(cluster_data_mensch_clean$VS, clusters_mensch, mean), 3),
  MN = round(tapply(cluster_data_mensch_clean$MN, clusters_mensch, mean), 3),
  ID = round(tapply(cluster_data_mensch_clean$ID, clusters_mensch, mean), 3),
  EA = round(tapply(cluster_data_mensch_clean$EA, clusters_mensch, mean), 3)
)

# Calculate overall mean for each cluster
cluster_means_mensch$overall_mean <- (cluster_means_mensch$VS + cluster_means_mensch$MN + 
                                     cluster_means_mensch$ID + cluster_means_mensch$EA) / 4

# Sort by overall mean (highest to lowest)
cluster_means_mensch_sorted <- cluster_means_mensch[order(cluster_means_mensch$overall_mean, decreasing = TRUE), ]

# Assign names based on ranking for Mensch
cluster_means_mensch_sorted$cluster_name <- c("Emotional Offen", "Ambivalent", "Emotional Distanziert")

# Add cluster information to Mensch data
data_mensch_clean$Cluster <- clusters_mensch
data_mensch_clean$Cluster_Name <- cluster_means_mensch_sorted$cluster_name[clusters_mensch]

cat("Mensch-Gruppe Cluster-Verteilung:\n")
for(i in 1:nrow(cluster_means_mensch_sorted)) {
  cat("-", cluster_means_mensch_sorted$cluster_name[i], ":", cluster_means_mensch_sorted$n[i], 
      "(", round(cluster_means_mensch_sorted$n[i] / sum(cluster_means_mensch_sorted$n) * 100, 1), "%)\n")
}

# =============================================================================
# CREATE GENDER GROUPS
# =============================================================================

cat("\n=== ERSTELLE GESCHLECHTERGRUPPEN ===\n")

# Create gender groups for KI
data_ki_clean$Gender_Group <- ifelse(data_ki_clean$SO02 == 1, "Männlich", "Weiblich")

# Create gender groups for Mensch
data_mensch_clean$Gender_Group <- ifelse(data_mensch_clean$SO02 == 1, "Männlich", "Weiblich")

# Count persons by cluster and gender group for KI
gender_cluster_counts_ki <- data_ki_clean %>%
  group_by(Cluster_Name, Gender_Group) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Group = "KI")

# Count persons by cluster and gender group for Mensch
gender_cluster_counts_mensch <- data_mensch_clean %>%
  group_by(Cluster_Name, Gender_Group) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Group = "Mensch")

cat("Geschlechtergruppen-Verteilung KI:\n")
print(gender_cluster_counts_ki)
cat("\nGeschlechtergruppen-Verteilung Mensch:\n")
print(gender_cluster_counts_mensch)

# =============================================================================
# CREATE VISUALIZATIONS
# =============================================================================

cat("\n=== ERSTELLE GESCHLECHTERGRUPPEN-VISUALISIERUNGEN ===\n")

# Create the KI group visualization with different colors
ki_gender_plot <- ggplot(gender_cluster_counts_ki, aes(x = Cluster_Name, y = Count, fill = Gender_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, 
           color = "black", linewidth = 0.3) +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            size = 3, 
            family = "Times New Roman") +
  scale_fill_manual(values = c(
    "Männlich" = "#98D8C8",      # Gedecktes Pastell-Grün
    "Weiblich" = "#B19CD9"       # Gedecktes Pastell-Lila
  )) +
  labs(x = "Cluster",
       y = "Anzahl an Personen",
       fill = "Geschlecht",
       title = "KI-Gruppe") +
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
    axis.ticks.length.x = unit(0, "pt"),
    plot.title = element_text(size = 14, family = "Times New Roman", hjust = 0.5)
  ) +
  ylim(0, max(gender_cluster_counts_ki$Count) * 1.2)

# Create the Mensch group visualization with different colors
mensch_gender_plot <- ggplot(gender_cluster_counts_mensch, aes(x = Cluster_Name, y = Count, fill = Gender_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, 
           color = "black", linewidth = 0.3) +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            size = 3, 
            family = "Times New Roman") +
  scale_fill_manual(values = c(
    "Männlich" = "#98D8C8",      # Gedecktes Pastell-Grün
    "Weiblich" = "#B19CD9"       # Gedecktes Pastell-Lila
  )) +
  labs(x = "Cluster",
       y = "Anzahl an Personen",
       fill = "Geschlecht",
       title = "Mensch-Gruppe") +
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
    axis.ticks.length.x = unit(0, "pt"),
    plot.title = element_text(size = 14, family = "Times New Roman", hjust = 0.5)
  ) +
  ylim(0, max(gender_cluster_counts_mensch$Count) * 1.2)

# Save the visualizations
dir.create("organized/images/clustering", recursive = TRUE, showWarnings = FALSE)

ggsave("organized/images/clustering/ki_gender_groups_visualization.png", 
       ki_gender_plot, width = 10, height = 6, dpi = 300, bg = "white", 
       limitsize = FALSE)

ggsave("organized/images/clustering/human_gender_groups_visualization.png", 
       mensch_gender_plot, width = 10, height = 6, dpi = 300, bg = "white", 
       limitsize = FALSE)

cat("✓ ki_gender_groups_visualization.png erstellt\n")
cat("✓ human_gender_groups_visualization.png erstellt\n")

# =============================================================================
# DATA SUMMARY
# =============================================================================

cat("\n================================================================================\n")
cat("GENDER GROUPS BY CLUSTER VISUALIZATION COMPLETED\n")
cat("================================================================================\n")
cat("Generated files:\n")
cat("- organized/images/clustering/ki_gender_groups_visualization.png\n")
cat("- organized/images/clustering/human_gender_groups_visualization.png\n")

cat("\nKI-Gruppe Geschlechtergruppen nach Cluster:\n")
print(gender_cluster_counts_ki)
cat("\nMensch-Gruppe Geschlechtergruppen nach Cluster:\n")
print(gender_cluster_counts_mensch)

cat("\nKI-Gruppe Cluster-Zusammenfassung:\n")
print(cluster_means_ki_sorted[, c("cluster_name", "n", "VS", "MN", "ID", "EA")])
cat("\nMensch-Gruppe Cluster-Zusammenfassung:\n")
print(cluster_means_mensch_sorted[, c("cluster_name", "n", "VS", "MN", "ID", "EA")])

cat("\n=== FARBEN VERWENDET ===\n")
cat("Männlich: #98D8C8 (Gedecktes Pastell-Grün)\n")
cat("Weiblich: #B19CD9 (Gedecktes Pastell-Lila)\n")
cat("(Andere Farben als üblich, aber gleicher Style)\n")

cat("\n================================================================================\n") 