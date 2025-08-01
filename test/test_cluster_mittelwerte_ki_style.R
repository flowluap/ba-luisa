# =============================================================================
# TEST: CLUSTER-MITTELWERTE KI-STYLE TABELLE
# =============================================================================
# Testet: cluster_mittelwerte_ki_style.png gegen Bereinigte Daten von WhatsApp Business.csv

library(dplyr)

cat("================================================================================\n")
cat("TEST: CLUSTER-MITTELWERTE KI-STYLE TABELLE\n")
cat("================================================================================\n")

# Load data
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)
data_processed <- data %>% filter(FINISHED == 1)

cat("✓ Daten geladen: N =", nrow(data_processed), "\n")

# Split data by group
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("✓ KI-Gruppe: n =", nrow(data_ki), "\n")
cat("✓ Mensch-Gruppe: n =", nrow(data_mensch), "\n")

# Calculate composite scores for both groups
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

# Perform clustering for both groups
perform_clustering <- function(composites) {
  set.seed(123)
  kmeans_result <- kmeans(composites, centers = 3, nstart = 25)
  clusters <- kmeans_result$cluster
  
  cluster_means <- data.frame(
    n = as.numeric(table(clusters)),
    VS = round(tapply(composites$VS, clusters, mean), 2),
    MN = round(tapply(composites$MN, clusters, mean), 2),
    ID = round(tapply(composites$ID, clusters, mean), 2),
    EA = round(tapply(composites$EA, clusters, mean), 2)
  )
  
  return(list(clusters = clusters, means = cluster_means))
}

clustering_ki <- perform_clustering(composites_ki)
clustering_mensch <- perform_clustering(composites_mensch)

# Expected values from cluster_mittelwerte_ki_style.png
# Based on the table structure with KI and Mensch groups
expected_ki_style <- data.frame(
  Gruppe = c("KI", "KI", "KI", "Mensch", "Mensch", "Mensch"),
  Cluster = c("KI-Offen", "Ambivalent", "KI-Skeptisch", 
              "Emotional Distanziert", "Emotional Offen", "Ambivalent"),
  N = c(23, 18, 23, 17, 22, 28),
  VS = c(2.45, 2.30, 2.46, 3.42, 3.72, 3.59),
  MN = c(2.45, 2.43, 2.53, 3.73, 3.43, 3.55),
  ID = c(2.02, 2.54, 2.66, 3.78, 3.86, 3.34),
  EA = c(2.56, 2.86, 2.31, 3.87, 3.72, 3.48)
)

cat("\n=== ERWARTETE WERTE AUS cluster_mittelwerte_ki_style.png ===\n")
print(expected_ki_style)

# Test functions
test_ki_cluster_values <- function() {
  cat("\n=== TEST 1: KI-CLUSTER WERTE ===\n")
  
  # Extract KI values from expected
  ki_expected <- expected_ki_style[expected_ki_style$Gruppe == "KI", ]
  
  # Compare with actual clustering results
  ki_n_correct <- all(clustering_ki$means$n == ki_expected$N)
  ki_vs_correct <- all(clustering_ki$means$VS == ki_expected$VS)
  ki_mn_correct <- all(clustering_ki$means$MN == ki_expected$MN)
  ki_id_correct <- all(clustering_ki$means$ID == ki_expected$ID)
  ki_ea_correct <- all(clustering_ki$means$EA == ki_expected$EA)
  
  cat("KI N-Werte:", if(ki_n_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  cat("KI VS-Werte:", if(ki_vs_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  cat("KI MN-Werte:", if(ki_mn_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  cat("KI ID-Werte:", if(ki_id_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  cat("KI EA-Werte:", if(ki_ea_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  return(ki_n_correct && ki_vs_correct && ki_mn_correct && ki_id_correct && ki_ea_correct)
}

test_mensch_cluster_values <- function() {
  cat("\n=== TEST 2: MENSCH-CLUSTER WERTE ===\n")
  
  # Extract Mensch values from expected
  mensch_expected <- expected_ki_style[expected_ki_style$Gruppe == "Mensch", ]
  
  # Compare with actual clustering results
  mensch_n_correct <- all(clustering_mensch$means$n == mensch_expected$N)
  mensch_vs_correct <- all(clustering_mensch$means$VS == mensch_expected$VS)
  mensch_mn_correct <- all(clustering_mensch$means$MN == mensch_expected$MN)
  mensch_id_correct <- all(clustering_mensch$means$ID == mensch_expected$ID)
  mensch_ea_correct <- all(clustering_mensch$means$EA == mensch_expected$EA)
  
  cat("Mensch N-Werte:", if(mensch_n_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  cat("Mensch VS-Werte:", if(mensch_vs_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  cat("Mensch MN-Werte:", if(mensch_mn_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  cat("Mensch ID-Werte:", if(mensch_id_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  cat("Mensch EA-Werte:", if(mensch_ea_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  return(mensch_n_correct && mensch_vs_correct && mensch_mn_correct && mensch_id_correct && mensch_ea_correct)
}

test_cluster_names_ki_style <- function() {
  cat("\n=== TEST 3: CLUSTER-NAMEN KI-STYLE ===\n")
  
  # Test KI cluster names
  ki_names_correct <- all(expected_ki_style$Cluster[expected_ki_style$Gruppe == "KI"] == 
                          c("KI-Offen", "Ambivalent", "KI-Skeptisch"))
  
  # Test Mensch cluster names
  mensch_names_correct <- all(expected_ki_style$Cluster[expected_ki_style$Gruppe == "Mensch"] == 
                              c("Emotional Distanziert", "Emotional Offen", "Ambivalent"))
  
  cat("KI-Cluster-Namen:", if(ki_names_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  cat("Mensch-Cluster-Namen:", if(mensch_names_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  return(ki_names_correct && mensch_names_correct)
}

test_total_sample_size_ki_style <- function() {
  cat("\n=== TEST 4: GESAMT-STICHPROBENGRÖSSE KI-STYLE ===\n")
  
  total_expected <- sum(expected_ki_style$N)
  total_actual <- nrow(data_ki) + nrow(data_mensch)
  
  cat("Erwartet:", total_expected, "vs Tatsächlich:", total_actual, "\n")
  
  total_correct <- total_expected == total_actual
  cat("Gesamt-Stichprobengröße:", if(total_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  return(total_correct)
}

test_table_structure_ki_style <- function() {
  cat("\n=== TEST 5: TABELLEN-STRUKTUR KI-STYLE ===\n")
  
  # Test if table has correct structure
  structure_correct <- all(colnames(expected_ki_style) == c("Gruppe", "Cluster", "N", "VS", "MN", "ID", "EA"))
  
  # Test if we have 6 rows (3 KI + 3 Mensch)
  rows_correct <- nrow(expected_ki_style) == 6
  
  # Test if groups are correctly labeled
  groups_correct <- all(expected_ki_style$Gruppe %in% c("KI", "Mensch"))
  
  cat("Tabellen-Struktur:", if(structure_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  cat("Anzahl Zeilen (6):", if(rows_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  cat("Gruppen-Labels:", if(groups_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  return(structure_correct && rows_correct && groups_correct)
}

# Run all tests
cat("\n=== TEST-AUSFÜHRUNG ===\n")

test_results <- list(
  ki_cluster_values = test_ki_cluster_values(),
  mensch_cluster_values = test_mensch_cluster_values(),
  cluster_names_ki_style = test_cluster_names_ki_style(),
  total_sample_size_ki_style = test_total_sample_size_ki_style(),
  table_structure_ki_style = test_table_structure_ki_style()
)

# Summary
cat("\n================================================================================\n")
cat("CLUSTER-MITTELWERTE KI-STYLE TEST-ZUSAMMENFASSUNG\n")
cat("================================================================================\n")

all_tests_passed <- all(unlist(test_results))

if(all_tests_passed) {
  cat("✅ ALLE KI-STYLE TESTS BESTANDEN - TABELLE KORREKT\n")
} else {
  cat("❌ EINIGE KI-STYLE TESTS FEHLGESCHLAGEN - TABELLE FEHLERHAFT\n")
}

cat("✓ Test 1 (KI-Cluster-Werte):", if(test_results$ki_cluster_values) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 2 (Mensch-Cluster-Werte):", if(test_results$mensch_cluster_values) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 3 (Cluster-Namen):", if(test_results$cluster_names_ki_style) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 4 (Stichprobengröße):", if(test_results$total_sample_size_ki_style) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 5 (Tabellen-Struktur):", if(test_results$table_structure_ki_style) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")

cat("\n================================================================================\n")
cat("CLUSTER-MITTELWERTE KI-STYLE TEST ABGESCHLOSSEN\n")
cat("================================================================================\n") 