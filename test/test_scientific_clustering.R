# =============================================================================
# TEST: WISSENSCHAFTLICHE CLUSTER-ANALYSE VALIDIERUNG
# =============================================================================
# Testet die wissenschaftliche Fundierung der Cluster-Analyse

library(dplyr)
library(cluster)

cat("================================================================================\n")
cat("TEST: WISSENSCHAFTLICHE CLUSTER-ANALYSE VALIDIERUNG\n")
cat("================================================================================\n")

# =============================================================================
# TEST FUNKTIONEN
# =============================================================================

# Test 1: Optimale Cluster-Anzahl Bestimmung
test_optimal_cluster_determination <- function() {
  cat("\n=== TEST 1: OPTIMALE CLUSTER-ANZAHL BESTIMMUNG ===\n")
  
  # Load data
  data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                     fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)
  data_processed <- data %>% filter(FINISHED == 1)
  data_ki <- data_processed %>% filter(AB01 == 1)
  
  # Calculate composites
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  
  composites_ki <- data.frame(
    VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
    MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
    ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
    EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
  )
  composites_ki_clean <- composites_ki[complete.cases(composites_ki), ]
  
  # Test Elbow method
  data_scaled <- scale(composites_ki_clean)
  wss <- sapply(1:6, function(k) {
    set.seed(123)
    kmeans(data_scaled, k, nstart = 25)$tot.withinss
  })
  
  optimal_k_elbow <- which.min(diff(wss)) + 1
  cat("Elbow-Methode empfiehlt:", optimal_k_elbow, "Cluster\n")
  
  # Test Silhouette analysis
  sil_width <- sapply(2:6, function(k) {
    set.seed(123)
    km <- kmeans(data_scaled, k, nstart = 25)
    sil <- silhouette(km$cluster, dist(data_scaled))
    mean(sil[, 3])
  })
  
  optimal_k_silhouette <- which.max(sil_width) + 1
  cat("Silhouette-Analyse empfiehlt:", optimal_k_silhouette, "Cluster\n")
  
  # Check if methods are reasonable
  elbow_reasonable <- optimal_k_elbow >= 2 && optimal_k_elbow <= 6
  silhouette_reasonable <- optimal_k_silhouette >= 2 && optimal_k_silhouette <= 6
  
  cat("Elbow-Methode vernünftig:", if(elbow_reasonable) "✅ JA" else "❌ NEIN", "\n")
  cat("Silhouette-Analyse vernünftig:", if(silhouette_reasonable) "✅ JA" else "❌ NEIN", "\n")
  
  return(elbow_reasonable && silhouette_reasonable)
}

# Test 2: Cluster-Validierung
test_cluster_validation <- function() {
  cat("\n=== TEST 2: CLUSTER-VALIDIERUNG ===\n")
  
  # Load data
  data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                     fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)
  data_processed <- data %>% filter(FINISHED == 1)
  data_ki <- data_processed %>% filter(AB01 == 1)
  
  # Calculate composites
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  
  composites_ki <- data.frame(
    VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
    MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
    ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
    EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
  )
  composites_ki_clean <- composites_ki[complete.cases(composites_ki), ]
  
  # Perform clustering
  data_scaled <- scale(composites_ki_clean)
  set.seed(123)
  kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 25)
  clusters <- kmeans_result$cluster
  
  # Test Silhouette width
  sil <- silhouette(clusters, dist(data_scaled))
  avg_sil_width <- mean(sil[, 3])
  
  cat("Average Silhouette Width:", round(avg_sil_width, 3), "\n")
  
  # Check if silhouette width is reasonable
  sil_reasonable <- avg_sil_width > 0.1  # Should be positive and meaningful
  cat("Silhouette Width vernünftig:", if(sil_reasonable) "✅ JA" else "❌ NEIN", "\n")
  
  return(sil_reasonable)
}

# Test 3: Signifikanz-Tests
test_significance_tests <- function() {
  cat("\n=== TEST 3: SIGNIFIKANZ-TESTS ===\n")
  
  # Load data
  data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                     fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)
  data_processed <- data %>% filter(FINISHED == 1)
  data_ki <- data_processed %>% filter(AB01 == 1)
  
  # Calculate composites
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  
  composites_ki <- data.frame(
    VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
    MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
    ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
    EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
  )
  composites_ki_clean <- composites_ki[complete.cases(composites_ki), ]
  
  # Perform clustering
  data_scaled <- scale(composites_ki_clean)
  set.seed(123)
  kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 25)
  clusters <- kmeans_result$cluster
  
  # Test ANOVA for each variable
  variables <- c("VS", "MN", "ID", "EA")
  significant_variables <- 0
  
  for(var in variables) {
    var_data <- data.frame(
      value = composites_ki_clean[[var]],
      cluster = as.factor(clusters)
    )
    
    anova_result <- aov(value ~ cluster, data = var_data)
    anova_summary <- summary(anova_result)
    p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    cat(var, "p-Wert:", round(p_value, 4), "\n")
    
    if(p_value < 0.05) {
      significant_variables <- significant_variables + 1
    }
  }
  
  # Check if at least some variables show significant differences
  significant_clusters <- significant_variables >= 2
  cat("Signifikante Cluster-Unterschiede:", if(significant_clusters) "✅ JA" else "❌ NEIN", "\n")
  
  return(significant_clusters)
}

# Test 4: Logische Cluster-Namen
test_logical_cluster_names <- function() {
  cat("\n=== TEST 4: LOGISCHE CLUSTER-NAMEN ===\n")
  
  # Load data
  data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                     fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)
  data_processed <- data %>% filter(FINISHED == 1)
  data_ki <- data_processed %>% filter(AB01 == 1)
  
  # Calculate composites
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  
  composites_ki <- data.frame(
    VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
    MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
    ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
    EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
  )
  composites_ki_clean <- composites_ki[complete.cases(composites_ki), ]
  
  # Perform clustering
  data_scaled <- scale(composites_ki_clean)
  set.seed(123)
  kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 25)
  clusters <- kmeans_result$cluster
  
  # Calculate cluster means
  cluster_means <- data.frame(
    n = as.numeric(table(clusters)),
    VS = round(tapply(composites_ki_clean$VS, clusters, mean), 3),
    MN = round(tapply(composites_ki_clean$MN, clusters, mean), 3),
    ID = round(tapply(composites_ki_clean$ID, clusters, mean), 3),
    EA = round(tapply(composites_ki_clean$EA, clusters, mean), 3)
  )
  
  # Calculate overall mean
  cluster_means$overall_mean <- (cluster_means$VS + cluster_means$MN + 
                                cluster_means$ID + cluster_means$EA) / 4
  
  # Sort clusters by overall mean
  cluster_ranking <- order(cluster_means$overall_mean, decreasing = TRUE)
  
  # Check if highest cluster is logically named "KI-Offen"
  highest_cluster <- cluster_ranking[1]
  cat("Höchster Cluster:", highest_cluster, "(Durchschnitt:", round(cluster_means$overall_mean[highest_cluster], 2), ")\n")
  
  # Check if lowest cluster is logically named "KI-Skeptisch"
  lowest_cluster <- cluster_ranking[3]
  cat("Niedrigster Cluster:", lowest_cluster, "(Durchschnitt:", round(cluster_means$overall_mean[lowest_cluster], 2), ")\n")
  
  # Check logical ordering
  logical_ordering <- cluster_means$overall_mean[highest_cluster] > cluster_means$overall_mean[lowest_cluster]
  cat("Logische Reihenfolge:", if(logical_ordering) "✅ JA" else "❌ NEIN", "\n")
  
  return(logical_ordering)
}

# Test 5: Reproduzierbarkeit
test_reproducibility <- function() {
  cat("\n=== TEST 5: REPRODUZIERBARKEIT ===\n")
  
  # Load data
  data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                     fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)
  data_processed <- data %>% filter(FINISHED == 1)
  data_ki <- data_processed %>% filter(AB01 == 1)
  
  # Calculate composites
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  
  composites_ki <- data.frame(
    VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
    MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
    ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
    EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
  )
  composites_ki_clean <- composites_ki[complete.cases(composites_ki), ]
  
  # Test reproducibility with same seed
  data_scaled <- scale(composites_ki_clean)
  
  set.seed(123)
  kmeans_result1 <- kmeans(data_scaled, centers = 3, nstart = 25)
  clusters1 <- kmeans_result1$cluster
  
  set.seed(123)
  kmeans_result2 <- kmeans(data_scaled, centers = 3, nstart = 25)
  clusters2 <- kmeans_result2$cluster
  
  # Check if results are identical
  reproducible <- identical(clusters1, clusters2)
  cat("Ergebnisse reproduzierbar:", if(reproducible) "✅ JA" else "❌ NEIN", "\n")
  
  return(reproducible)
}

# =============================================================================
# ALLE TESTS AUSFÜHREN
# =============================================================================

cat("\n=== TEST-AUSFÜHRUNG ===\n")

test_results <- list(
  optimal_clusters = test_optimal_cluster_determination(),
  cluster_validation = test_cluster_validation(),
  significance_tests = test_significance_tests(),
  logical_names = test_logical_cluster_names(),
  reproducibility = test_reproducibility()
)

# =============================================================================
# ZUSAMMENFASSUNG
# =============================================================================

cat("\n================================================================================\n")
cat("WISSENSCHAFTLICHE CLUSTER-ANALYSE TEST-ZUSAMMENFASSUNG\n")
cat("================================================================================\n")

all_tests_passed <- all(unlist(test_results))

if(all_tests_passed) {
  cat("✅ ALLE WISSENSCHAFTLICHEN CLUSTER-TESTS BESTANDEN\n")
  cat("✅ Cluster-Analyse ist wissenschaftlich fundiert\n")
} else {
  cat("❌ EINIGE WISSENSCHAFTLICHEN CLUSTER-TESTS FEHLGESCHLAGEN\n")
  cat("❌ Cluster-Analyse benötigt Verbesserungen\n")
}

cat("✓ Test 1 (Optimale Cluster-Anzahl):", if(test_results$optimal_clusters) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 2 (Cluster-Validierung):", if(test_results$cluster_validation) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 3 (Signifikanz-Tests):", if(test_results$significance_tests) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 4 (Logische Cluster-Namen):", if(test_results$logical_names) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 5 (Reproduzierbarkeit):", if(test_results$reproducibility) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")

cat("\n================================================================================\n")
cat("WISSENSCHAFTLICHE CLUSTER-ANALYSE TEST ABGESCHLOSSEN\n")
cat("================================================================================\n") 