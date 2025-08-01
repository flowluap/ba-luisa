# =============================================================================
# CLUSTER PROZENT TABELLE - LOGISCHE CLUSTER-ZUORDNUNG (KORRIGIERT)
# =============================================================================
# Erstellt Cluster-Prozent-Tabelle mit korrekter logischer Cluster-Namen-Zuordnung

library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

cat("================================================================================\n")
cat("CLUSTER PROZENT TABELLE - LOGISCHE CLUSTER-ZUORDNUNG (KORRIGIERT)\n")
cat("================================================================================\n")

# Function to perform cluster analysis with correct logical naming
perform_cluster_analysis_correct <- function(data, group_name, k = 3) {
  cat("\n=== CLUSTER-ANALYSE FÜR", group_name, "===\n")
  
  # Calculate composite scores
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  
  # Create composite scores
  cluster_data <- data.frame(
    VS = rowMeans(data[, vs_cols], na.rm = TRUE),
    MN = rowMeans(data[, mn_cols], na.rm = TRUE),
    ID = rowMeans(data[, id_cols], na.rm = TRUE),
    EA = rowMeans(data[, ea_cols], na.rm = TRUE)
  )
  
  # Remove rows with missing values
  cluster_data_clean <- cluster_data[complete.cases(cluster_data), ]
  
  cat("Anzahl gültige Fälle für Clustering:", nrow(cluster_data_clean), "\n")
  
  if(nrow(cluster_data_clean) < k) {
    cat("Zu wenige Fälle für", k, "Cluster. Reduziere auf", nrow(cluster_data_clean), "Cluster.\n")
    k <- max(1, nrow(cluster_data_clean) - 1)
  }
  
  # STANDARDIZE variables (like individual scripts)
  cluster_data_scaled <- scale(cluster_data_clean)
  
  # Perform k-means clustering
  set.seed(123)  # For reproducibility
  if(k > 1) {
    kmeans_result <- kmeans(cluster_data_scaled, centers = k, nstart = 25)
    clusters <- kmeans_result$cluster
  } else {
    clusters <- rep(1, nrow(cluster_data_clean))
  }
  
  cat("Cluster-Verteilung:\n")
  print(table(clusters))
  
  # Calculate cluster means for logical naming
  cluster_means <- data.frame(
    Cluster_Number = 1:k,
    n = as.numeric(table(clusters)),
    VS = round(tapply(cluster_data_clean$VS, clusters, mean), 2),
    MN = round(tapply(cluster_data_clean$MN, clusters, mean), 2),
    ID = round(tapply(cluster_data_clean$ID, clusters, mean), 2),
    EA = round(tapply(cluster_data_clean$EA, clusters, mean), 2)
  )
  
  # Calculate overall mean for each cluster
  cluster_means$overall_mean <- (cluster_means$VS + cluster_means$MN + 
                                cluster_means$ID + cluster_means$EA) / 4
  
  # Sort clusters by overall mean (highest to lowest)
  cluster_ranking <- order(cluster_means$overall_mean, decreasing = TRUE)
  
  # Assign logical names based on ranking
  if(group_name == "KI") {
    cluster_names <- character(k)
    cluster_names[cluster_ranking[1]] <- "KI-Offen"      # Highest values
    cluster_names[cluster_ranking[2]] <- "Ambivalent"    # Medium values
    cluster_names[cluster_ranking[3]] <- "KI-Skeptisch"  # Lowest values
  } else {  # Mensch
    cluster_names <- character(k)
    cluster_names[cluster_ranking[1]] <- "Emotional Offen"      # Highest values
    cluster_names[cluster_ranking[2]] <- "Ambivalent"           # Medium values
    cluster_names[cluster_ranking[3]] <- "Emotional Distanziert" # Lowest values
  }
  
  cat("Logische Cluster-Zuordnung:\n")
  for(i in 1:k) {
    cat("Cluster", i, "→", cluster_names[i], "(Gesamtdurchschnitt:", round(cluster_means$overall_mean[i], 2), ")\n")
  }
  
  # Create mapping from cluster number to cluster name
  cluster_name_mapping <- data.frame(
    cluster_number = 1:k,
    cluster_name = cluster_names,
    stringsAsFactors = FALSE
  )
  
  return(list(
    clusters = clusters,
    data = cluster_data_clean,
    n_cases = nrow(cluster_data_clean),
    cluster_names = cluster_names,
    cluster_means = cluster_means,
    cluster_name_mapping = cluster_name_mapping
  ))
}

# Load data
cat("Lade Daten...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

cat("Daten erfolgreich geladen\n")
cat("Anzahl Zeilen:", nrow(data), "\n")
cat("Anzahl Spalten:", ncol(data), "\n")

# Filter data (FINISHED=1)
data_processed <- data %>%
  filter(FINISHED == 1)

cat("Daten gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data_processed), "\n")

# Split by group
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("Anzahl KI (AB01=1):", nrow(data_ki), "\n")
cat("Anzahl Mensch (AB01=2):", nrow(data_mensch), "\n")

# Perform cluster analysis for both groups with correct logical naming
ki_clusters <- perform_cluster_analysis_correct(data_ki, "KI", k = 3)
mensch_clusters <- perform_cluster_analysis_correct(data_mensch, "MENSCH", k = 3)

# Create result table with correct logical cluster names
results <- data.frame(
  Cluster = c(ki_clusters$cluster_names, mensch_clusters$cluster_names),
  Gruppe = c(rep("KI", 3), rep("Mensch", 3)),
  N = numeric(6),
  Prozent = character(6),
  stringsAsFactors = FALSE
)

# Calculate counts and percentages for KI clusters using correct mapping
ki_cluster_counts <- table(ki_clusters$clusters)
for(i in 1:3) {
  cluster_name <- ki_clusters$cluster_names[i]
  # Find the cluster number that corresponds to this name
  cluster_number <- ki_clusters$cluster_name_mapping$cluster_number[ki_clusters$cluster_name_mapping$cluster_name == cluster_name]
  count <- ki_cluster_counts[as.character(cluster_number)]
  percentage <- round((count / ki_clusters$n_cases) * 100, 1)
  results$N[i] <- count
  results$Prozent[i] <- paste0(percentage, "%")
}

# Calculate counts and percentages for Mensch clusters using correct mapping
mensch_cluster_counts <- table(mensch_clusters$clusters)
for(i in 1:3) {
  cluster_name <- mensch_clusters$cluster_names[i]
  # Find the cluster number that corresponds to this name
  cluster_number <- mensch_clusters$cluster_name_mapping$cluster_number[mensch_clusters$cluster_name_mapping$cluster_name == cluster_name]
  count <- mensch_cluster_counts[as.character(cluster_number)]
  percentage <- round((count / mensch_clusters$n_cases) * 100, 1)
  results$N[i + 3] <- count
  results$Prozent[i + 3] <- paste0(percentage, "%")
}

cat("\nCluster-Ergebnisse (korrekte logische Zuordnung):\n")
print(results)

# Create APA table with simple tableGrob
cluster_apa_grob <- tableGrob(
  results,
  rows = NULL,
  theme = ttheme_default(
    base_family = "Times New Roman",
    base_size = 12,
    core = list(
      fg_params = list(hjust = 0, x = 0.1),
      bg_params = list(fill = NA),
      padding = unit(c(6, 1), "mm")
    ),
                     colhead = list(
         fg_params = list(fontface = "bold", hjust = 0, x = 0.1),
         bg_params = list(fill = NA),
         padding = unit(c(0.1, 1), "mm")
       ),
    rowhead = list(
      fg_params = list(hjust = 0, x = 0.1),
      bg_params = list(fill = NA),
      padding = unit(c(6, 1), "mm")
    )
  )
)

# Add borders
cluster_apa_grob <- gtable_add_grob(
  cluster_apa_grob,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 2)
  ),
  t = 1, b = 1, l = 1, r = ncol(results)
)

# Adjust header height to prevent cutoff
cluster_apa_grob$heights[1] <- unit(10, "mm")

# Add bottom border
cluster_apa_grob <- gtable_add_grob(
  cluster_apa_grob,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 3, col = "black")
  ),
  t = nrow(results) + 1, b = nrow(results) + 1, l = 1, r = ncol(results)
)

# Create directory if it doesn't exist
dir.create("organized/images/clustering", recursive = TRUE, showWarnings = FALSE)

# Save table with increased height to prevent cutoff
png("organized/images/clustering/cluster_prozent_tabelle.png", 
    width = 1200, height = 400, res = 300, bg = "white")

# Draw table directly
grid.newpage()
grid.draw(cluster_apa_grob)

dev.off()

cat("\n✓ Cluster-Prozent-Tabelle erstellt: cluster_prozent_tabelle.png\n")

cat("\n================================================================================\n")
cat("CLUSTER PROZENT TABELLE ABGESCHLOSSEN (KORRIGIERT)\n")
cat("================================================================================\n")
cat("• cluster_prozent_tabelle.png (Cluster-Prozent-Tabelle mit korrekter logischer Zuordnung)\n")
cat("• Logische Cluster-Namen basierend auf tatsächlichen Werten\n")
cat("• Verwendet Bereinigte Daten von WhatsApp Business.csv\n")
cat("• KI-Gruppe: N =", nrow(data_ki), "\n")
cat("• Mensch-Gruppe: N =", nrow(data_mensch), "\n")
cat("================================================================================\n") 