# =============================================================================
# CLUSTER MITTELWERTE KI-STYLE - KORREKTE DATEN
# =============================================================================

library(dplyr)
library(ggplot2)

cat("================================================================================\n")
cat("CLUSTER MITTELWERTE KI-STYLE - KORREKTE DATEN\n")
cat("================================================================================\n")

# Load real data (Bereinigte Daten von WhatsApp Business.csv)
cat("Lade Bereinigte Daten von WhatsApp Business.csv...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

cat("Daten erfolgreich geladen\n")
cat("Anzahl Zeilen:", nrow(data), "\n")
cat("Anzahl Spalten:", ncol(data), "\n")

# Filter data (FINISHED=1 and AB01=1 for KI group only)
data_processed <- data %>%
  filter(FINISHED == 1, AB01 == 1)

cat("Daten gefiltert (KI-Gruppe)\n")
cat("Anzahl KI-Fälle:", nrow(data_processed), "\n")

# Calculate composite scores for KI group
cat("\n=== COMPOSITE SCORES BERECHNUNG ===\n")

# MN composite
mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
mn_score <- rowMeans(data_processed[, mn_cols], na.rm = TRUE)

# VS composite
vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
vs_score <- rowMeans(data_processed[, vs_cols], na.rm = TRUE)

# EA composite
ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
ea_score <- rowMeans(data_processed[, ea_cols], na.rm = TRUE)

# ID composite
id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
id_score <- rowMeans(data_processed[, id_cols], na.rm = TRUE)

cat("✓ Composite Scores berechnet\n")

# Create clustering data
cluster_data <- data.frame(
  VS = vs_score,
  MN = mn_score,
  ID = id_score,
  EA = ea_score
)

# Perform k-means clustering
cat("\n=== K-MEANS CLUSTERING ===\n")
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(cluster_data, centers = 3, nstart = 25)
clusters <- kmeans_result$cluster

cat("✓ Clustering durchgeführt\n")
cat("Cluster-Verteilung:\n")
print(table(clusters))

# Calculate cluster means
cat("\n=== CLUSTER-MITTELWERTE ===\n")

cluster_means <- data.frame(
  Cluster = paste("Cluster", 1:3),
  n = as.numeric(table(clusters)),
  VS = round(tapply(vs_score, clusters, mean), 2),
  MN = round(tapply(mn_score, clusters, mean), 2),
  ID = round(tapply(id_score, clusters, mean), 2),
  EA = round(tapply(ea_score, clusters, mean), 2)
)

cat("Berechnete Cluster-Mittelwerte:\n")
print(cluster_means)

# Assign meaningful cluster names (logical approach based on actual values)
cat("\n=== CLUSTER-NAMEN ZUORDNUNG ===\n")

# Calculate overall mean for each cluster
cluster_means_raw <- data.frame(
  Cluster_Number = 1:3,
  n = as.numeric(table(clusters)),
  VS = round(tapply(vs_score, clusters, mean), 2),
  MN = round(tapply(mn_score, clusters, mean), 2),
  ID = round(tapply(id_score, clusters, mean), 2),
  EA = round(tapply(ea_score, clusters, mean), 2)
)

cluster_means_raw$overall_mean <- (cluster_means_raw$VS + cluster_means_raw$MN + 
                                   cluster_means_raw$ID + cluster_means_raw$EA) / 4

# Sort clusters by overall mean (highest to lowest)
cluster_ranking <- order(cluster_means_raw$overall_mean, decreasing = TRUE)

# Assign labels based on ranking
cluster_names <- character(3)
cluster_names[cluster_ranking[1]] <- "KI-Offen"      # Highest values
cluster_names[cluster_ranking[2]] <- "Ambivalent"    # Medium values
cluster_names[cluster_ranking[3]] <- "KI-Skeptisch"  # Lowest values

# Create final results table with logical cluster mapping
cluster_summary <- data.frame(
  Cluster_Number = 1:3,
  Cluster_Name = cluster_names,
  n = cluster_means_raw$n,
  VS = cluster_means_raw$VS,
  MN = cluster_means_raw$MN,
  ID = cluster_means_raw$ID,
  EA = cluster_means_raw$EA
)

cat("Finale Cluster-Ergebnisse:\n")
print(cluster_summary)

# Prepare data for plotting
plot_data <- data.frame(
  Cluster = rep(cluster_summary$Cluster_Name, 4),
  Variable = rep(c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
                   "Identifikation", "Emotionale Ansprache"), each = 3),
  Mittelwert = c(cluster_summary$VS, cluster_summary$MN, 
                 cluster_summary$ID, cluster_summary$EA)
)

# Set factor levels for proper ordering
plot_data$Cluster <- factor(plot_data$Cluster, 
                           levels = c("KI-Offen", "Ambivalent", "KI-Skeptisch"))
plot_data$Variable <- factor(plot_data$Variable, 
                            levels = c("Emotionale Ansprache", "Identifikation", 
                                     "Menschlichkeit & Natürlichkeit", "Vertrauen & Sympathie"))

cat("\n=== PLOT-DATEN VORBEREITET ===\n")
print(plot_data)

# Create the plot
cat("\n=== ERSTELLE CLUSTER-MITTELWERTE PLOT ===\n")

p <- ggplot(plot_data, aes(x = Cluster, y = Mittelwert, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("Emotionale Ansprache" = "turquoise", 
                               "Identifikation" = "limegreen",
                               "Menschlichkeit & Natürlichkeit" = "darkred",
                               "Vertrauen & Sympathie" = "gold")) +
  labs(x = "Cluster",
       y = "Mittelwert",
       fill = "Variable") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.major = element_line(color = "lightgray"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1))

# Create directory if it doesn't exist
dir.create("organized/output/images", recursive = TRUE, showWarnings = FALSE)

# Save plot
ggsave("organized/output/images/cluster_mittelwerte_ki_style.png", 
       plot = p, width = 10, height = 6, dpi = 300, bg = "white")

cat("\n✓ Cluster-Mittelwerte Plot erstellt: organized/output/images/cluster_mittelwerte_ki_style.png\n")

cat("\n================================================================================\n")
cat("CLUSTER MITTELWERTE KI-STYLE ABGESCHLOSSEN\n")
cat("================================================================================\n")
cat("• cluster_mittelwerte_ki_style.png (KI-Cluster-Mittelwerte Visualisierung)\n")
cat("• Gruppiertes Balkendiagramm mit 4 Variablen pro Cluster\n")
cat("• Cluster-Namen konsistent mit README.md\n")
cat("• Verwendet Bereinigte Daten von WhatsApp Business.csv\n")
cat("• N-Werte: KI =", nrow(data_processed), "\n")
cat("• Farbschema: turquoise, limegreen, darkred, gold\n")
cat("================================================================================\n") 