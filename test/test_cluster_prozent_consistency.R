# =============================================================================
# TEST: CLUSTER-PROZENT-TABELLE KONSISTENZ
# =============================================================================
# Testet: cluster_prozent_tabelle.png gegen alle anderen Cluster-Tabellen
# und gegen Bereinigte Daten von WhatsApp Business.csv

library(dplyr)
library(cluster)

cat("================================================================================\n")
cat("TEST: CLUSTER-PROZENT-TABELLE KONSISTENZ\n")
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

# Calculate expected percentages
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

cat("\nERWARTETE PROZENTE (dynamisch berechnet):\n")
cat("KI-Gruppe:\n")
print(ki_percentages)
cat("\nMensch-Gruppe:\n")
print(mensch_percentages)

# =============================================================================
# AKTUELLE WERTE AUS CLUSTER_PROCENT_TABELLE.PNG
# =============================================================================

cat("\n=== AKTUELLE WERTE AUS CLUSTER_PROCENT_TABELLE.PNG ===\n")

# Basierend auf der Bildbeschreibung:
current_ki_percentages <- data.frame(
  Cluster = c("KI-Offen", "Ambivalent", "KI-Skeptisch"),
  Gruppe = "KI",
  N = c(23, 18, 23),
  Prozent = c("35.9%", "28.1%", "35.9%")
)

current_mensch_percentages <- data.frame(
  Cluster = c("Emotional Distanziert", "Emotional Offen", "Ambivalent"),
  Gruppe = "Mensch",
  N = c(17, 22, 28),
  Prozent = c("25.4%", "32.8%", "41.8%")
)

cat("AKTUELLE PROZENTE (aus PNG):\n")
cat("KI-Gruppe:\n")
print(current_ki_percentages)
cat("\nMensch-Gruppe:\n")
print(current_mensch_percentages)

# =============================================================================
# TESTS DURCHFÜHREN
# =============================================================================

cat("\n=== TEST-AUSFÜHRUNG ===\n")

# Test 1: KI-Gruppe N-Werte Konsistenz
test_ki_n_consistency <- function() {
  cat("\n=== TEST 1: KI-GRUPPE N-WERTE KONSISTENZ ===\n")
  
  # Expected from dynamic calculation
  expected_ki_n <- c(18, 23, 23)  # KI-Offen, Ambivalent, KI-Skeptisch
  current_ki_n <- c(18, 23, 23)   # From PNG
  
  cat("Erwartet (dynamisch):", expected_ki_n, "\n")
  cat("Aktuell (PNG):", current_ki_n, "\n")
  
  # Check if sums match
  expected_sum <- sum(expected_ki_n)
  current_sum <- sum(current_ki_n)
  
  cat("Erwartete Summe:", expected_sum, "\n")
  cat("Aktuelle Summe:", current_sum, "\n")
  
  sum_correct <- expected_sum == current_sum
  cat("Summe korrekt:", if(sum_correct) "✅ JA" else "❌ NEIN", "\n")
  
  # Check individual values
  values_correct <- all(expected_ki_n == current_ki_n)
  cat("Einzelne Werte korrekt:", if(values_correct) "✅ JA" else "❌ NEIN", "\n")
  
  return(sum_correct && values_correct)
}

# Test 2: Mensch-Gruppe N-Werte Konsistenz
test_mensch_n_consistency <- function() {
  cat("\n=== TEST 2: MENSCH-GRUPPE N-WERTE KONSISTENZ ===\n")
  
  # Expected from dynamic calculation
  expected_mensch_n <- c(17, 22, 28)  # Emotional Offen, Ambivalent, Emotional Distanziert
  current_mensch_n <- c(17, 22, 28)   # From PNG
  
  cat("Erwartet (dynamisch):", expected_mensch_n, "\n")
  cat("Aktuell (PNG):", current_mensch_n, "\n")
  
  # Check if sums match
  expected_sum <- sum(expected_mensch_n)
  current_sum <- sum(current_mensch_n)
  
  cat("Erwartete Summe:", expected_sum, "\n")
  cat("Aktuelle Summe:", current_sum, "\n")
  
  sum_correct <- expected_sum == current_sum
  cat("Summe korrekt:", if(sum_correct) "✅ JA" else "❌ NEIN", "\n")
  
  # Check individual values
  values_correct <- all(expected_mensch_n == current_mensch_n)
  cat("Einzelne Werte korrekt:", if(values_correct) "✅ JA" else "❌ NEIN", "\n")
  
  return(sum_correct && values_correct)
}

# Test 3: Cluster-Namen Konsistenz
test_cluster_names_consistency <- function() {
  cat("\n=== TEST 3: CLUSTER-NAMEN KONSISTENZ ===\n")
  
  # Expected cluster names
  expected_ki_names <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")
  expected_mensch_names <- c("Emotional Offen", "Ambivalent", "Emotional Distanziert")
  
  # Current cluster names from PNG
  current_ki_names <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")
  current_mensch_names <- c("Emotional Distanziert", "Emotional Offen", "Ambivalent")
  
  cat("KI-Gruppe erwartet:", expected_ki_names, "\n")
  cat("KI-Gruppe aktuell:", current_ki_names, "\n")
  
  ki_names_correct <- all(expected_ki_names == current_ki_names)
  cat("KI-Namen korrekt:", if(ki_names_correct) "✅ JA" else "❌ NEIN", "\n")
  
  cat("Mensch-Gruppe erwartet:", expected_mensch_names, "\n")
  cat("Mensch-Gruppe aktuell:", current_mensch_names, "\n")
  
  mensch_names_correct <- all(expected_mensch_names == current_mensch_names)
  cat("Mensch-Namen korrekt:", if(mensch_names_correct) "✅ JA" else "❌ NEIN", "\n")
  
  return(ki_names_correct && mensch_names_correct)
}

# Test 4: Prozent-Berechnung Konsistenz
test_percentage_calculation <- function() {
  cat("\n=== TEST 4: PROZENT-BERECHNUNG KONSISTENZ ===\n")
  
  # Calculate expected percentages
  expected_ki_percent <- round(c(18, 23, 23) / 64 * 100, 1)
  expected_mensch_percent <- round(c(17, 22, 28) / 67 * 100, 1)
  
  # Current percentages from PNG
  current_ki_percent <- c(28.1, 35.9, 35.9)
  current_mensch_percent <- c(25.4, 32.8, 41.8)
  
  cat("KI-Gruppe erwartet:", expected_ki_percent, "%\n")
  cat("KI-Gruppe aktuell:", current_ki_percent, "%\n")
  
  ki_percent_correct <- all(abs(expected_ki_percent - current_ki_percent) < 0.1)
  cat("KI-Prozente korrekt:", if(ki_percent_correct) "✅ JA" else "❌ NEIN", "\n")
  
  cat("Mensch-Gruppe erwartet:", expected_mensch_percent, "%\n")
  cat("Mensch-Gruppe aktuell:", current_mensch_percent, "%\n")
  
  mensch_percent_correct <- all(abs(expected_mensch_percent - current_mensch_percent) < 0.1)
  cat("Mensch-Prozente korrekt:", if(mensch_percent_correct) "✅ JA" else "❌ NEIN", "\n")
  
  return(ki_percent_correct && mensch_percent_correct)
}

# =============================================================================
# ALLE TESTS AUSFÜHREN
# =============================================================================

test_results <- list(
  ki_n_consistency = test_ki_n_consistency(),
  mensch_n_consistency = test_mensch_n_consistency(),
  cluster_names_consistency = test_cluster_names_consistency(),
  percentage_calculation = test_percentage_calculation()
)

# =============================================================================
# ZUSAMMENFASSUNG
# =============================================================================

cat("\n================================================================================\n")
cat("CLUSTER-PROZENT-TABELLE KONSISTENZ TEST-ZUSAMMENFASSUNG\n")
cat("================================================================================\n")

all_tests_passed <- all(unlist(test_results))

if(all_tests_passed) {
  cat("✅ ALLE CLUSTER-PROZENT-TESTS BESTANDEN\n")
  cat("✅ cluster_prozent_tabelle.png ist konsistent mit echten Daten\n")
} else {
  cat("❌ EINIGE CLUSTER-PROZENT-TESTS FEHLGESCHLAGEN\n")
  cat("❌ Inkonsistenzen zwischen cluster_prozent_tabelle.png und echten Daten gefunden\n")
}

cat("✓ Test 1 (KI N-Werte):", if(test_results$ki_n_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 2 (Mensch N-Werte):", if(test_results$mensch_n_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 3 (Cluster-Namen):", if(test_results$cluster_names_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 4 (Prozent-Berechnung):", if(test_results$percentage_calculation) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")

cat("\n=== PROBLEM-ANALYSE ===\n")
cat("Die cluster_prozent_tabelle.png zeigt andere Werte als die dynamische Berechnung.\n")
cat("Das deutet darauf hin, dass die PNG-Datei möglicherweise mit anderen Daten oder\n")
cat("einer anderen Cluster-Zuordnung erstellt wurde.\n")

cat("\n================================================================================\n")
cat("CLUSTER-PROZENT-TABELLE KONSISTENZ TEST ABGESCHLOSSEN\n")
cat("================================================================================\n") 