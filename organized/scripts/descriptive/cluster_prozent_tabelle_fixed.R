# =============================================================================
# CLUSTER PROZENT TABELLE - KOMPLETT KORRIGIERT
# =============================================================================
# Erstellt Cluster-Prozent-Tabelle mit garantiert korrekter Cluster-Zuordnung

library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

cat("================================================================================\n")
cat("CLUSTER PROZENT TABELLE - KOMPLETT KORRIGIERT\n")
cat("================================================================================\n")

# Load data
cat("Lade Daten...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data (FINISHED=1)
data_processed <- data %>% filter(FINISHED == 1)

# Split by group
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("✓ KI-Gruppe: n =", nrow(data_ki), "\n")
cat("✓ Mensch-Gruppe: n =", nrow(data_mensch), "\n")

# Function to perform clustering with correct naming
perform_clustering_fixed <- function(data, group_name) {
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
  if(group_name == "KI") {
    cluster_means_sorted$cluster_name <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")
  } else {
    cluster_means_sorted$cluster_name <- c("Emotional Offen", "Ambivalent", "Emotional Distanziert")
  }
  
  cat("\nCluster-Zuordnung für", group_name, ":\n")
  for(i in 1:3) {
    cat("Rang", i, ":", cluster_means_sorted$cluster_name[i], 
        "(n=", cluster_means_sorted$n[i], 
        ", Durchschnitt=", round(cluster_means_sorted$overall_mean[i], 2), ")\n")
  }
  
  return(list(
    clusters = clusters,
    cluster_means = cluster_means_sorted,
    total_n = nrow(cluster_data_clean)
  ))
}

# Perform clustering for both groups
ki_result <- perform_clustering_fixed(data_ki, "KI")
mensch_result <- perform_clustering_fixed(data_mensch, "MENSCH")

# Create result table
results <- data.frame(
  Cluster = c(ki_result$cluster_means$cluster_name, mensch_result$cluster_means$cluster_name),
  Gruppe = c(rep("KI", 3), rep("Mensch", 3)),
  N = c(ki_result$cluster_means$n, mensch_result$cluster_means$n),
  Prozent = c(
    paste0(round(ki_result$cluster_means$n / ki_result$total_n * 100, 1), "%"),
    paste0(round(mensch_result$cluster_means$n / mensch_result$total_n * 100, 1), "%")
  ),
  stringsAsFactors = FALSE
)

cat("\nKORREKTE CLUSTER-ERGEBNISSE:\n")
print(results)

# Create APA table
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

# Adjust header height - make it even closer to other rows
cluster_apa_grob$heights[1] <- unit(1.5, "mm")

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

# Save table
png("organized/images/clustering/cluster_prozent_tabelle.png", 
    width = 1200, height = 200, res = 300, bg = "white")

# Draw table directly
grid.newpage()
grid.draw(cluster_apa_grob)

dev.off()

cat("\n✓ Cluster-Prozent-Tabelle erstellt: cluster_prozent_tabelle.png\n")

cat("\n================================================================================\n")
cat("CLUSTER PROZENT TABELLE ABGESCHLOSSEN (KOMPLETT KORRIGIERT)\n")
cat("================================================================================\n")
cat("• cluster_prozent_tabelle.png (Cluster-Prozent-Tabelle mit garantiert korrekter Zuordnung)\n")
cat("• Logische Cluster-Namen basierend auf tatsächlichen Werten\n")
cat("• Verwendet Bereinigte Daten von WhatsApp Business.csv\n")
cat("• KI-Gruppe: N =", nrow(data_ki), "\n")
cat("• Mensch-Gruppe: N =", nrow(data_mensch), "\n")
cat("================================================================================\n") 