# =============================================================================
# SEPARATE SILHOUETTE ANALYSIS
# =============================================================================
# Erstellt separate Silhouette-Analysen für KI- und Mensch-Gruppe im traditionellen Stil

library(dplyr)
library(ggplot2)
library(cluster)

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
# PREPARE CLUSTERING DATA
# =============================================================================

# Define variable columns
mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")

# Create composite scores for both groups
cluster_data_ki <- data.frame(
  VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
)

cluster_data_mensch <- data.frame(
  VS = rowMeans(data_mensch[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_mensch[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_mensch[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_mensch[, ea_cols], na.rm = TRUE)
)

# Remove missing values
cluster_data_ki_clean <- cluster_data_ki[complete.cases(cluster_data_ki), ]
cluster_data_mensch_clean <- cluster_data_mensch[complete.cases(cluster_data_mensch), ]

cat("✓ KI-Gruppe (nach Missing Values): n =", nrow(cluster_data_ki_clean), "\n")
cat("✓ Mensch-Gruppe (nach Missing Values): n =", nrow(cluster_data_mensch_clean), "\n")

# =============================================================================
# PERFORM CLUSTERING AND CALCULATE SILHOUETTE SCORES
# =============================================================================

cat("\n=== ERSTELLE SEPARATE SILHOUETTE-ANALYSEN ===\n")

# Perform k-means clustering with k=3 for both groups
set.seed(123)
kmeans_ki <- kmeans(cluster_data_ki_clean, centers = 3, nstart = 25)
kmeans_mensch <- kmeans(cluster_data_mensch_clean, centers = 3, nstart = 25)

# Calculate silhouette scores
sil_ki <- silhouette(kmeans_ki$cluster, dist(cluster_data_ki_clean))
sil_mensch <- silhouette(kmeans_mensch$cluster, dist(cluster_data_mensch_clean))

# =============================================================================
# CREATE TRADITIONAL SILHOUETTE PLOTS
# =============================================================================

# Function to create traditional silhouette plot with Readme styling
create_silhouette_plot <- function(sil_data, title, group_type = "ki") {
  # Convert silhouette object to data frame
  sil_df <- data.frame(
    cluster = sil_data[, 1],
    neighbor = sil_data[, 2],
    sil_width = sil_data[, 3]
  )
  
  # Create ordering: cluster first, then silhouette width within cluster
  sil_df$cluster_order <- sil_df$cluster
  sil_df$within_cluster_order <- ave(sil_df$sil_width, sil_df$cluster, FUN = function(x) rank(x, ties.method = "first"))
  sil_df$final_order <- sil_df$cluster_order * 1000 + sil_df$within_cluster_order
  sil_df <- sil_df[order(sil_df$final_order), ]
  
  # Create cluster labels based on group type
  cluster_labels <- if (group_type == "ki") {
    c("KI-Offen", "Ambivalent", "KI-Skeptisch")
  } else {
    c("Emotional Offen", "Ambivalent", "Emotional Distanziert")
  }
  
  # Create plot
  ggplot(sil_df, aes(x = sil_width, y = reorder(seq_along(sil_width), final_order), 
                     fill = factor(cluster, labels = cluster_labels))) +
    geom_bar(stat = "identity", width = 0.8, color = "black", linewidth = 0.3) +
    scale_fill_manual(values = c("#8DD3C8", "#ABCD9B", "#BE4B5A"), name = "Cluster") +
    labs(title = title,
         x = "Silhouette-Breite",
         y = "Beobachtungen",
         fill = "Cluster") +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    geom_vline(xintercept = mean(sil_df$sil_width), color = "red", linewidth = 0.8, linetype = "dashed") +
    geom_hline(yintercept = seq(5, nrow(sil_df), by = 5), color = "gray80", linewidth = 0.5, alpha = 0.7) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 11, family = "Times New Roman"),
      axis.text = element_text(size = 10, family = "Times New Roman"),
      axis.text.y = element_blank(),
      plot.title = element_text(size = 12, hjust = 0.5, face = "bold", family = "Times New Roman"),
      legend.title = element_text(size = 11, family = "Times New Roman"),
      legend.text = element_text(size = 10, family = "Times New Roman"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(10, 10, 10, 10),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.x = element_line(),
      axis.ticks.length.x = unit(0, "pt")
    ) +
    xlim(min(sil_df$sil_width) - 0.1, max(sil_df$sil_width) + 0.1)
}

# Create silhouette plots for both groups
sil_plot_ki <- create_silhouette_plot(sil_ki, "Silhouette-Analyse: KI-Gruppe", "ki")
sil_plot_mensch <- create_silhouette_plot(sil_mensch, "Silhouette-Analyse: Mensch-Gruppe", "mensch")

# =============================================================================
# SAVE PLOTS
# =============================================================================

cat("=== SPEICHERE PLOTS ===\n")

# Create directory
dir.create("organized/images/clustering", recursive = TRUE, showWarnings = FALSE)

# Save plots
ggsave("organized/images/clustering/ki_silhouette_analysis.png", 
       sil_plot_ki, width = 8, height = 6, dpi = 300, bg = "white", limitsize = FALSE)

ggsave("organized/images/clustering/mensch_silhouette_analysis.png", 
       sil_plot_mensch, width = 8, height = 6, dpi = 300, bg = "white", limitsize = FALSE)

cat("✓ ki_silhouette_analysis.png erstellt\n")
cat("✓ mensch_silhouette_analysis.png erstellt\n")

# =============================================================================
# STATISTICAL SUMMARY
# =============================================================================

cat("\n=== STATISTISCHE ZUSAMMENFASSUNG ===\n")

# Calculate average silhouette scores
avg_sil_ki <- mean(sil_ki[,3])
avg_sil_mensch <- mean(sil_mensch[,3])

# Calculate silhouette scores by cluster
sil_summary_ki <- aggregate(sil_ki[,3], by = list(cluster = sil_ki[,1]), FUN = mean)
sil_summary_mensch <- aggregate(sil_mensch[,3], by = list(cluster = sil_mensch[,1]), FUN = mean)

cat("KI-Gruppe:\n")
cat("- Durchschnittliche Silhouette-Breite:", round(avg_sil_ki, 3), "\n")
cat("- Interpretation:", ifelse(avg_sil_ki > 0.7, "Starke Cluster-Struktur", 
                               ifelse(avg_sil_ki > 0.5, "Mittlere Cluster-Struktur", 
                                      "Schwache Cluster-Struktur")), "\n")
cat("- Cluster-spezifische Werte:\n")
for(i in 1:nrow(sil_summary_ki)) {
  cat("  Cluster", sil_summary_ki$cluster[i], ":", round(sil_summary_ki$x[i], 3), "\n")
}

cat("\nMensch-Gruppe:\n")
cat("- Durchschnittliche Silhouette-Breite:", round(avg_sil_mensch, 3), "\n")
cat("- Interpretation:", ifelse(avg_sil_mensch > 0.7, "Starke Cluster-Struktur", 
                               ifelse(avg_sil_mensch > 0.5, "Mittlere Cluster-Struktur", 
                                      "Schwache Cluster-Struktur")), "\n")
cat("- Cluster-spezifische Werte:\n")
for(i in 1:nrow(sil_summary_mensch)) {
  cat("  Cluster", sil_summary_mensch$cluster[i], ":", round(sil_summary_mensch$x[i], 3), "\n")
}

cat("\n================================================================================\n")
cat("SEPARATE SILHOUETTE ANALYSIS COMPLETED\n")
cat("================================================================================\n")
cat("Generated files:\n")
cat("- organized/images/clustering/ki_silhouette_analysis.png\n")
cat("- organized/images/clustering/mensch_silhouette_analysis.png\n")

cat("\nSilhouette-Interpretation:\n")
cat("- > 0.7: Starke Cluster-Struktur\n")
cat("- 0.5-0.7: Mittlere Cluster-Struktur\n")
cat("- < 0.5: Schwache Cluster-Struktur\n")

cat("\n================================================================================\n") 