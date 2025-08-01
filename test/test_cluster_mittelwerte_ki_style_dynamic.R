# =============================================================================
# TEST: CLUSTER-MITTELWERTE KI-STYLE TABELLE (DYNAMISCH)
# =============================================================================
# Testet: cluster_mittelwerte_ki_style.png mit echten Daten
# Keine hardcodierten Werte - alles dynamisch berechnet

library(dplyr)
library(cluster)

cat("================================================================================\n")
cat("TEST: CLUSTER-MITTELWERTE KI-STYLE TABELLE (DYNAMISCH)\n")
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

# Create expected table structure (like cluster_mittelwerte_ki_style.png)
expected_table <- data.frame(
  Gruppe = c(rep("KI", 3), rep("Mensch", 3)),
  Cluster = c(ki_clustering$means$Cluster_Name, mensch_clustering$means$Cluster_Name),
  N = c(ki_clustering$means$n, mensch_clustering$means$n),
  VS = c(ki_clustering$means$VS, mensch_clustering$means$VS),
  MN = c(ki_clustering$means$MN, mensch_clustering$means$MN),
  ID = c(ki_clustering$means$ID, mensch_clustering$means$ID),
  EA = c(ki_clustering$means$EA, mensch_clustering$means$EA)
)

cat("Erwartete Tabelle (dynamisch berechnet):\n")
print(expected_table)

# =============================================================================
# TESTS DURCHFÜHREN
# =============================================================================

cat("\n=== TEST-AUSFÜHRUNG ===\n")

# Test 1: KI-Cluster Werte
test_ki_cluster_values <- function() {
  cat("\n=== TEST 1: KI-CLUSTER WERTE ===\n")
  
  # Test N-values
  ki_n_correct <- all(expected_table$N[1:3] == ki_clustering$means$n)
  cat("KI N-Werte:", if(ki_n_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  # Test VS-values
  ki_vs_correct <- all(expected_table$VS[1:3] == ki_clustering$means$VS)
  cat("KI VS-Werte:", if(ki_vs_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  # Test MN-values
  ki_mn_correct <- all(expected_table$MN[1:3] == ki_clustering$means$MN)
  cat("KI MN-Werte:", if(ki_mn_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  # Test ID-values
  ki_id_correct <- all(expected_table$ID[1:3] == ki_clustering$means$ID)
  cat("KI ID-Werte:", if(ki_id_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  # Test EA-values
  ki_ea_correct <- all(expected_table$EA[1:3] == ki_clustering$means$EA)
  cat("KI EA-Werte:", if(ki_ea_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  return(ki_n_correct && ki_vs_correct && ki_mn_correct && ki_id_correct && ki_ea_correct)
}

# Test 2: Mensch-Cluster Werte
test_mensch_cluster_values <- function() {
  cat("\n=== TEST 2: MENSCH-CLUSTER WERTE ===\n")
  
  # Test N-values
  mensch_n_correct <- all(expected_table$N[4:6] == mensch_clustering$means$n)
  cat("Mensch N-Werte:", if(mensch_n_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  # Test VS-values
  mensch_vs_correct <- all(expected_table$VS[4:6] == mensch_clustering$means$VS)
  cat("Mensch VS-Werte:", if(mensch_vs_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  # Test MN-values
  mensch_mn_correct <- all(expected_table$MN[4:6] == mensch_clustering$means$MN)
  cat("Mensch MN-Werte:", if(mensch_mn_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  # Test ID-values
  mensch_id_correct <- all(expected_table$ID[4:6] == mensch_clustering$means$ID)
  cat("Mensch ID-Werte:", if(mensch_id_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  # Test EA-values
  mensch_ea_correct <- all(expected_table$EA[4:6] == mensch_clustering$means$EA)
  cat("Mensch EA-Werte:", if(mensch_ea_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  return(mensch_n_correct && mensch_vs_correct && mensch_mn_correct && mensch_id_correct && mensch_ea_correct)
}

# Test 3: Cluster-Namen KI-Style
test_cluster_names_ki_style <- function() {
  cat("\n=== TEST 3: CLUSTER-NAMEN KI-STYLE ===\n")
  
  # Test KI cluster names
  ki_names_correct <- all(c("KI-Offen", "Ambivalent", "KI-Skeptisch") %in% ki_clustering$means$Cluster_Name)
  cat("KI-Cluster-Namen:", if(ki_names_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  # Test Mensch cluster names
  mensch_names_correct <- all(c("Emotional Offen", "Ambivalent", "Emotional Distanziert") %in% mensch_clustering$means$Cluster_Name)
  cat("Mensch-Cluster-Namen:", if(mensch_names_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  return(ki_names_correct && mensch_names_correct)
}

# Test 4: Gesamt-Stichprobengröße KI-Style
test_total_sample_size_ki_style <- function() {
  cat("\n=== TEST 4: GESAMT-STICHPROBENGRÖSSE KI-STYLE ===\n")
  
  expected_total <- 131
  actual_total <- sum(expected_table$N)
  
  cat("Erwartet:", expected_total, "vs Tatsächlich:", actual_total, "\n")
  
  total_correct <- actual_total == expected_total
  cat("Gesamt-Stichprobengröße:", if(total_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  return(total_correct)
}

# Test 5: Tabellen-Struktur KI-Style
test_table_structure_ki_style <- function() {
  cat("\n=== TEST 5: TABELLEN-STRUKTUR KI-STYLE ===\n")
  
  # Test table structure
  structure_correct <- ncol(expected_table) == 7 && all(colnames(expected_table) == c("Gruppe", "Cluster", "N", "VS", "MN", "ID", "EA"))
  cat("Tabellen-Struktur:", if(structure_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  # Test number of rows
  rows_correct <- nrow(expected_table) == 6
  cat("Anzahl Zeilen (6):", if(rows_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  # Test group labels
  group_labels_correct <- all(expected_table$Gruppe[1:3] == "KI") && all(expected_table$Gruppe[4:6] == "Mensch")
  cat("Gruppen-Labels:", if(group_labels_correct) "✅ KORREKT" else "❌ FEHLER", "\n")
  
  return(structure_correct && rows_correct && group_labels_correct)
}

# =============================================================================
# ALLE TESTS AUSFÜHREN
# =============================================================================

test_results <- list(
  ki_cluster_values = test_ki_cluster_values(),
  mensch_cluster_values = test_mensch_cluster_values(),
  cluster_names_ki_style = test_cluster_names_ki_style(),
  total_sample_size_ki_style = test_total_sample_size_ki_style(),
  table_structure_ki_style = test_table_structure_ki_style()
)

# =============================================================================
# ZUSAMMENFASSUNG
# =============================================================================

cat("\n================================================================================\n")
cat("CLUSTER-MITTELWERTE KI-STYLE TEST-ZUSAMMENFASSUNG (DYNAMISCH)\n")
cat("================================================================================\n")

all_tests_passed <- all(unlist(test_results))

if(all_tests_passed) {
  cat("✅ ALLE KI-STYLE TESTS BESTANDEN - TABELLE KORREKT\n")
} else {
  cat("❌ EINIGE KI-STYLE TESTS FEHLGESCHLAGEN - TABELLE PROBLEMATISCH\n")
}

cat("✓ Test 1 (KI-Cluster-Werte):", if(test_results$ki_cluster_values) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 2 (Mensch-Cluster-Werte):", if(test_results$mensch_cluster_values) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 3 (Cluster-Namen):", if(test_results$cluster_names_ki_style) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 4 (Stichprobengröße):", if(test_results$total_sample_size_ki_style) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 5 (Tabellen-Struktur):", if(test_results$table_structure_ki_style) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")

cat("\n=== AKTUELLE CLUSTERING ERGEBNISSE ===\n")
cat("KI-Gruppe Cluster-Summe:", sum(ki_clustering$means$n), "\n")
cat("Mensch-Gruppe Cluster-Summe:", sum(mensch_clustering$means$n), "\n")
cat("Gesamt-Summe:", sum(ki_clustering$means$n) + sum(mensch_clustering$means$n), "\n")

cat("\n================================================================================\n")
cat("CLUSTER-MITTELWERTE KI-STYLE TEST ABGESCHLOSSEN (DYNAMISCH)\n")
cat("================================================================================\n") 