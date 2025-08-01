# =============================================================================
# WISSENSCHAFTLICHE CLUSTER-ANALYSE
# =============================================================================
# Implementiert wissenschaftlich fundierte Cluster-Bildung mit:
# - Elbow-Methode und Silhouette-Analyse
# - Cluster-Validierung und Stabilitätstests
# - Signifikanz-Tests und Effektstärken
# - Transparente Dokumentation

library(dplyr)
library(ggplot2)
library(cluster)

# Install and load required packages if not available
if (!require(fpc)) {
  install.packages("fpc")
  library(fpc)
}
if (!require(effectsize)) {
  install.packages("effectsize")
  library(effectsize)
}

library(gridExtra)
library(grid)
library(gtable)

cat("================================================================================\n")
cat("WISSENSCHAFTLICHE CLUSTER-ANALYSE\n")
cat("================================================================================\n")

# =============================================================================
# DATEN LADEN UND VORBEREITEN
# =============================================================================

# Load real data
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

# Calculate composite scores
calculate_composites <- function(data) {
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  
  return(data.frame(
    VS = rowMeans(data[, vs_cols], na.rm = TRUE),
    MN = rowMeans(data[, mn_cols], na.rm = TRUE),
    ID = rowMeans(data[, id_cols], na.rm = TRUE),
    EA = rowMeans(data[, ea_cols], na.rm = TRUE)
  ))
}

composites_ki <- calculate_composites(data_ki)
composites_mensch <- calculate_composites(data_mensch)

# Remove missing values
composites_ki_clean <- composites_ki[complete.cases(composites_ki), ]
composites_mensch_clean <- composites_mensch[complete.cases(composites_mensch), ]

cat("✓ KI-Gruppe nach Missing-Value-Filter: n =", nrow(composites_ki_clean), "\n")
cat("✓ Mensch-Gruppe nach Missing-Value-Filter: n =", nrow(composites_mensch_clean), "\n")

# =============================================================================
# WISSENSCHAFTLICHE CLUSTER-ANALYSE FUNKTIONEN
# =============================================================================

# Function to determine optimal number of clusters
determine_optimal_clusters <- function(data, max_k = 10) {
  cat("\n=== OPTIMALE CLUSTER-ANZAHL BESTIMMEN ===\n")
  
  # Standardize data
  data_scaled <- scale(data)
  
  # Elbow method
  wss <- sapply(1:max_k, function(k) {
    set.seed(123)
    kmeans(data_scaled, k, nstart = 25)$tot.withinss
  })
  
  # Silhouette analysis
  sil_width <- sapply(2:max_k, function(k) {
    set.seed(123)
    km <- kmeans(data_scaled, k, nstart = 25)
    sil <- silhouette(km$cluster, dist(data_scaled))
    mean(sil[, 3])
  })
  
  # Find optimal k
  optimal_k_elbow <- which.min(diff(wss)) + 1
  optimal_k_silhouette <- which.max(sil_width) + 1
  
  cat("Elbow-Methode empfiehlt:", optimal_k_elbow, "Cluster\n")
  cat("Silhouette-Analyse empfiehlt:", optimal_k_silhouette, "Cluster\n")
  
  # Choose the most common recommendation
  optimal_k <- ifelse(optimal_k_elbow == optimal_k_silhouette, 
                     optimal_k_elbow, 
                     round(mean(c(optimal_k_elbow, optimal_k_silhouette))))
  
  cat("Gewählte Cluster-Anzahl:", optimal_k, "\n")
  
  return(list(
    optimal_k = optimal_k,
    wss = wss,
    sil_width = sil_width,
    data_scaled = data_scaled
  ))
}

# Function to perform scientific clustering
perform_scientific_clustering <- function(data, group_name, k = 3) {
  cat("\n=== WISSENSCHAFTLICHE CLUSTER-ANALYSE FÜR", group_name, "===\n")
  
  # Determine optimal number of clusters
  cluster_analysis <- determine_optimal_clusters(data, max_k = 6)
  
  # Use optimal k or specified k
  final_k <- ifelse(k == 3, cluster_analysis$optimal_k, k)
  cat("Verwende k =", final_k, "für Clustering\n")
  
  # Perform k-means clustering
  set.seed(123)
  kmeans_result <- kmeans(cluster_analysis$data_scaled, centers = final_k, nstart = 25)
  clusters <- kmeans_result$cluster
  
  # Cluster validation
  cat("\n=== CLUSTER-VALIDIERUNG ===\n")
  
  # Cluster statistics
  cluster_stats <- cluster.stats(dist(cluster_analysis$data_scaled), clusters)
  cat("Average Silhouette Width:", round(cluster_stats$avg.silwidth, 3), "\n")
  cat("Calinski-Harabasz Index:", round(cluster_stats$ch, 3), "\n")
  cat("Dunn Index:", round(cluster_stats$dunn, 3), "\n")
  
  # Bootstrap stability test
  cat("\n=== BOOTSTRAP-STABILITÄTSTEST ===\n")
  tryCatch({
    boot_result <- clusterboot(cluster_analysis$data_scaled, B = 50, 
                              bootmethod = "boot", clustermethod = claraCBI, 
                              k = final_k, seed = 123)
    cat("Bootstrap-Stabilität:", round(mean(boot_result$bootmean), 3), "\n")
  }, error = function(e) {
    cat("Bootstrap-Test konnte nicht durchgeführt werden\n")
  })
  
  # Calculate cluster means
  cluster_means <- data.frame(
    Cluster_Number = 1:final_k,
    n = as.numeric(table(clusters)),
    VS = round(tapply(data$VS, clusters, mean), 3),
    MN = round(tapply(data$MN, clusters, mean), 3),
    ID = round(tapply(data$ID, clusters, mean), 3),
    EA = round(tapply(data$EA, clusters, mean), 3)
  )
  
  # Calculate overall mean for logical naming
  cluster_means$overall_mean <- (cluster_means$VS + cluster_means$MN + 
                                cluster_means$ID + cluster_means$EA) / 4
  
  # Sort clusters by overall mean (highest to lowest)
  cluster_ranking <- order(cluster_means$overall_mean, decreasing = TRUE)
  
  # Assign logical names based on ranking
  if(group_name == "KI") {
    cluster_names <- character(final_k)
    cluster_names[cluster_ranking[1]] <- "KI-Offen"      # Highest values
    cluster_names[cluster_ranking[2]] <- "Ambivalent"    # Medium values
    cluster_names[cluster_ranking[3]] <- "KI-Skeptisch"  # Lowest values
  } else {  # Mensch
    cluster_names <- character(final_k)
    cluster_names[cluster_ranking[1]] <- "Emotional Offen"      # Highest values
    cluster_names[cluster_ranking[2]] <- "Ambivalent"           # Medium values
    cluster_names[cluster_ranking[3]] <- "Emotional Distanziert" # Lowest values
  }
  
  cluster_means$Cluster_Name <- cluster_names
  
  # Statistical significance tests
  cat("\n=== SIGNIFIKANZ-TESTS ===\n")
  
  # Prepare data for ANOVA
  long_data <- data.frame(
    value = c(data$VS, data$MN, data$ID, data$EA),
    variable = rep(c("VS", "MN", "ID", "EA"), each = nrow(data)),
    cluster = rep(clusters, 4)
  )
  
  # ANOVA for each variable
  variables <- c("VS", "MN", "ID", "EA")
  anova_results <- list()
  
  for(var in variables) {
    var_data <- data.frame(
      value = data[[var]],
      cluster = as.factor(clusters)
    )
    
    anova_result <- aov(value ~ cluster, data = var_data)
    anova_results[[var]] <- anova_result
    
    # Calculate effect size
    eta_sq <- eta_squared(anova_result)
    
    cat(var, "F(", anova_result$Df[1], ",", anova_result$Df[2], ") =", 
        round(anova_result$`F value`[1], 3), 
        ", p =", round(anova_result$`Pr(>F)`[1], 4),
        ", η² =", round(eta_sq$Eta2, 3), "\n")
  }
  
  # Post-hoc tests
  cat("\n=== POST-HOC TESTS (Tukey HSD) ===\n")
  for(var in variables) {
    cat("\n", var, ":\n")
    tukey_result <- TukeyHSD(anova_results[[var]])
    print(round(tukey_result$cluster, 4))
  }
  
  return(list(
    clusters = clusters,
    cluster_means = cluster_means,
    cluster_stats = cluster_stats,
    anova_results = anova_results,
    data_scaled = cluster_analysis$data_scaled,
    optimal_k = final_k
  ))
}

# =============================================================================
# CLUSTER-ANALYSE DURCHFÜHREN
# =============================================================================

# Perform scientific clustering for both groups
ki_clustering <- perform_scientific_clustering(composites_ki_clean, "KI", k = 3)
mensch_clustering <- perform_scientific_clustering(composites_mensch_clean, "MENSCH", k = 3)

# =============================================================================
# ERGEBNISSE AUSGEBEN
# =============================================================================

cat("\n================================================================================\n")
cat("WISSENSCHAFTLICHE CLUSTER-ANALYSE ERGEBNISSE\n")
cat("================================================================================\n")

cat("\nKI-Gruppe Cluster-Ergebnisse:\n")
print(ki_clustering$cluster_means)

cat("\nMensch-Gruppe Cluster-Ergebnisse:\n")
print(mensch_clustering$cluster_means)

cat("\n================================================================================\n")
cat("WISSENSCHAFTLICHE CLUSTER-ANALYSE ABGESCHLOSSEN\n")
cat("================================================================================\n")
cat("• Optimale Cluster-Anzahl bestimmt (Elbow + Silhouette)\n")
cat("• Cluster-Validierung durchgeführt\n")
cat("• Signifikanz-Tests und Effektstärken berechnet\n")
cat("• Post-hoc Tests durchgeführt\n")
cat("• Logische Cluster-Namen basierend auf Daten zugeordnet\n")
cat("================================================================================\n") 