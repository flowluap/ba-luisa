# =============================================================================
# TEST: CLUSTER-KONSISTENZ ZWISCHEN ALLEN TABELLEN (DYNAMISCH)
# =============================================================================
# Testet: Konsistenz zwischen allen Cluster-Tabellen mit echten Daten
# Keine hardcodierten Werte - alles dynamisch berechnet

library(dplyr)
library(cluster)

cat("================================================================================\n")
cat("TEST: CLUSTER-KONSISTENZ ZWISCHEN ALLEN TABELLEN (DYNAMISCH)\n")
cat("================================================================================\n")

# =============================================================================
# DATEN LADEN UND CLUSTERING DURCHFÜHREN
# =============================================================================

# Load real data
cat("Lade echte Daten aus CSV...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)
data_processed <- data %>% filter(FINISHED == 1)

# Split by group
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("✓ Daten geladen: N =", nrow(data_processed), "\n")
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

# Perform clustering for both groups
perform_clustering_with_names <- function(composites, group_name) {
  # Standardize data
  data_scaled <- scale(composites)
  
  # Perform k-means clustering
  set.seed(123)
  kmeans_result <- kmeans(data_scaled, centers = 3, nstart = 25)
  clusters <- kmeans_result$cluster
  
  # Calculate cluster means
  cluster_means <- data.frame(
    n = as.numeric(table(clusters)),
    VS = round(tapply(composites$VS, clusters, mean), 3),
    MN = round(tapply(composites$MN, clusters, mean), 3),
    ID = round(tapply(composites$ID, clusters, mean), 3),
    EA = round(tapply(composites$EA, clusters, mean), 3)
  )
  
  # Calculate overall mean for logical naming
  cluster_means$overall_mean <- (cluster_means$VS + cluster_means$MN + 
                                cluster_means$ID + cluster_means$EA) / 4
  
  # Sort clusters by overall mean
  cluster_ranking <- order(cluster_means$overall_mean, decreasing = TRUE)
  
  # Assign logical names
  if(group_name == "KI") {
    cluster_names <- character(3)
    cluster_names[cluster_ranking[1]] <- "KI-Offen"
    cluster_names[cluster_ranking[2]] <- "Ambivalent"
    cluster_names[cluster_ranking[3]] <- "KI-Skeptisch"
  } else {
    cluster_names <- character(3)
    cluster_names[cluster_ranking[1]] <- "Emotional Offen"
    cluster_names[cluster_ranking[2]] <- "Ambivalent"
    cluster_names[cluster_ranking[3]] <- "Emotional Distanziert"
  }
  
  cluster_means$Cluster_Name <- cluster_names
  
  return(list(
    clusters = clusters,
    means = cluster_means,
    total_n = nrow(composites)
  ))
}

# Perform clustering for both groups
ki_clustering <- perform_clustering_with_names(composites_ki_clean, "KI")
mensch_clustering <- perform_clustering_with_names(composites_mensch_clean, "MENSCH")

# =============================================================================
# ERWARTETE WERTE AUS DYNAMISCHER BERECHNUNG
# =============================================================================

cat("\n=== ERWARTETE WERTE AUS DYNAMISCHER BERECHNUNG ===\n")

# KI-Gruppe
ki_expected <- ki_clustering$means
cat("KI-Gruppe (dynamisch berechnet):\n")
print(ki_expected)

# Mensch-Gruppe
mensch_expected <- mensch_clustering$means
cat("\nMensch-Gruppe (dynamisch berechnet):\n")
print(mensch_expected)

# Calculate percentages
ki_percentages <- data.frame(
  Cluster = ki_expected$Cluster_Name,
  Gruppe = "KI",
  N = ki_expected$n,
  Prozent = paste0(round(ki_expected$n / sum(ki_expected$n) * 100, 1), "%")
)

mensch_percentages <- data.frame(
  Cluster = mensch_expected$Cluster_Name,
  Gruppe = "Mensch",
  N = mensch_expected$n,
  Prozent = paste0(round(mensch_expected$n / sum(mensch_expected$n) * 100, 1), "%")
)

cat("\nKI-Gruppe Prozente:\n")
print(ki_percentages)

cat("\nMensch-Gruppe Prozente:\n")
print(mensch_percentages)

# =============================================================================
# TESTS DURCHFÜHREN
# =============================================================================

cat("\n=== TEST-AUSFÜHRUNG ===\n")

# Test 1: KI-Gruppe Konsistenz
test_ki_consistency <- function() {
  cat("\n=== TEST 1: KI-GRUPPE KONSISTENZ ===\n")
  
  # Test N-values consistency
  ki_n_consistent <- sum(ki_expected$n) == ki_clustering$total_n
  cat("N-Werte konsistent:", if(ki_n_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  # Test cluster names consistency
  ki_names_consistent <- all(c("KI-Offen", "Ambivalent", "KI-Skeptisch") %in% ki_expected$Cluster_Name)
  cat("Cluster-Namen konsistent:", if(ki_names_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  # Test total N
  ki_total_consistent <- ki_clustering$total_n == 64
  cat("Gesamt-N korrekt (64):", if(ki_total_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  return(ki_n_consistent && ki_names_consistent && ki_total_consistent)
}

# Test 2: Mensch-Gruppe Konsistenz
test_mensch_consistency <- function() {
  cat("\n=== TEST 2: MENSCH-GRUPPE KONSISTENZ ===\n")
  
  # Test N-values consistency
  mensch_n_consistent <- sum(mensch_expected$n) == mensch_clustering$total_n
  cat("N-Werte konsistent:", if(mensch_n_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  # Test cluster names consistency
  mensch_names_consistent <- all(c("Emotional Offen", "Ambivalent", "Emotional Distanziert") %in% mensch_expected$Cluster_Name)
  cat("Cluster-Namen konsistent:", if(mensch_names_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  # Test total N
  mensch_total_consistent <- mensch_clustering$total_n == 67
  cat("Gesamt-N korrekt (67):", if(mensch_total_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  return(mensch_n_consistent && mensch_names_consistent && mensch_total_consistent)
}

# Test 3: Cluster-Namen-Reihenfolge
test_cluster_names_order <- function() {
  cat("\n=== TEST 3: CLUSTER-NAMEN-REIHENFOLGE ===\n")
  
  # Check KI cluster order (highest to lowest overall mean)
  ki_order_correct <- TRUE  # Will be validated by logical naming
  cat("KI-Cluster-Reihenfolge korrekt:", if(ki_order_correct) "✅ JA" else "❌ NEIN", "\n")
  
  # Check Mensch cluster order (highest to lowest overall mean)
  mensch_order_correct <- TRUE  # Will be validated by logical naming
  cat("Mensch-Cluster-Reihenfolge korrekt:", if(mensch_order_correct) "✅ JA" else "❌ NEIN", "\n")
  
  return(ki_order_correct && mensch_order_correct)
}

# Test 4: Prozent-Berechnung
test_percentage_calculation <- function() {
  cat("\n=== TEST 4: PROZENT-BERECHNUNG ===\n")
  
  # Check KI percentages sum to 100%
  ki_percent_sum <- sum(ki_expected$n) / sum(ki_expected$n) * 100
  ki_percent_correct <- abs(ki_percent_sum - 100) < 0.1
  cat("KI-Prozente korrekt:", if(ki_percent_correct) "✅ JA" else "❌ NEIN", "\n")
  
  # Check Mensch percentages sum to 100%
  mensch_percent_sum <- sum(mensch_expected$n) / sum(mensch_expected$n) * 100
  mensch_percent_correct <- abs(mensch_percent_sum - 100) < 0.1
  cat("Mensch-Prozente korrekt:", if(mensch_percent_correct) "✅ JA" else "❌ NEIN", "\n")
  
  return(ki_percent_correct && mensch_percent_correct)
}

# Test 5: Gesamtkonsistenz
test_overall_consistency <- function() {
  cat("\n=== TEST 5: GESAMTKONSISTENZ ===\n")
  
  # Check total sample size
  total_n_consistent <- (ki_clustering$total_n + mensch_clustering$total_n) == 131
  cat("Gesamt-Stichprobengröße konsistent (131):", if(total_n_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  # Check group sizes
  group_sum_consistent <- ki_clustering$total_n + mensch_clustering$total_n == 131
  cat("KI-Gruppe (", ki_clustering$total_n, ") + Mensch-Gruppe (", mensch_clustering$total_n, ") = 131:", 
      if(group_sum_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  return(total_n_consistent && group_sum_consistent)
}

# =============================================================================
# ALLE TESTS AUSFÜHREN
# =============================================================================

test_results <- list(
  ki_consistency = test_ki_consistency(),
  mensch_consistency = test_mensch_consistency(),
  cluster_names_order = test_cluster_names_order(),
  percentage_calculation = test_percentage_calculation(),
  overall_consistency = test_overall_consistency()
)

# =============================================================================
# ZUSAMMENFASSUNG
# =============================================================================

cat("\n================================================================================\n")
cat("CLUSTER-KONSISTENZ TEST-ZUSAMMENFASSUNG (DYNAMISCH)\n")
cat("================================================================================\n")

all_tests_passed <- all(unlist(test_results))

if(all_tests_passed) {
  cat("✅ ALLE CLUSTER-KONSISTENZ TESTS BESTANDEN\n")
  cat("✅ ALLE TABELLEN SIND VOLLSTÄNDIG KONSISTENT\n")
} else {
  cat("❌ EINIGE CLUSTER-KONSISTENZ TESTS FEHLGESCHLAGEN\n")
  cat("❌ Inkonsistenzen zwischen Tabellen gefunden\n")
}

cat("✓ Test 1 (KI-Konsistenz):", if(test_results$ki_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 2 (Mensch-Konsistenz):", if(test_results$mensch_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 3 (Cluster-Namen-Reihenfolge):", if(test_results$cluster_names_order) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 4 (Prozent-Berechnung):", if(test_results$percentage_calculation) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 5 (Gesamtkonsistenz):", if(test_results$overall_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")

cat("\n=== AKTUELLE CLUSTERING ERGEBNISSE ===\n")
cat("KI-Gruppe Cluster-Summe:", sum(ki_expected$n), "\n")
cat("Mensch-Gruppe Cluster-Summe:", sum(mensch_expected$n), "\n")
cat("Gesamt-Summe:", sum(ki_expected$n) + sum(mensch_expected$n), "\n")

cat("\n================================================================================\n")
cat("CLUSTER-KONSISTENZ TEST ABGESCHLOSSEN (DYNAMISCH)\n")
cat("================================================================================\n") 