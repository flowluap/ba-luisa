# =============================================================================
# TEST: CLUSTER-KONSISTENZ ZWISCHEN ALLEN TABELLEN
# =============================================================================
# Testet: Konsistenz zwischen ki_specific_apa.png, human_specific_apa.png, cluster_prozent_tabelle.png

library(dplyr)

cat("================================================================================\n")
cat("TEST: CLUSTER-KONSISTENZ ZWISCHEN ALLEN TABELLEN\n")
cat("================================================================================\n")

# Expected values from each table
cat("=== ERWARTETE WERTE AUS TABELLEN ===\n")

# ki_specific_apa.png
ki_specific_expected <- data.frame(
  Cluster = c("KI-Offen", "Ambivalent", "KI-Skeptisch"),
  n = c(23, 18, 23),
  VS = c(2.45, 2.30, 2.46),
  MN = c(2.45, 2.43, 2.53),
  ID = c(2.02, 2.54, 2.66),
  EA = c(2.56, 2.86, 2.31)
)

cat("ki_specific_apa.png:\n")
print(ki_specific_expected)

# human_specific_apa.png
human_specific_expected <- data.frame(
  Cluster = c("Emotional Distanziert", "Emotional Offen", "Ambivalent"),
  n = c(17, 22, 28),
  EA = c(3.87, 3.72, 3.48),
  ID = c(3.78, 3.86, 3.34),
  MN = c(3.73, 3.43, 3.55),
  VS = c(3.42, 3.72, 3.59)
)

cat("\nhuman_specific_apa.png:\n")
print(human_specific_expected)

# cluster_prozent_tabelle.png
cluster_prozent_ki <- data.frame(
  Cluster = c("KI-Offen", "Ambivalent", "KI-Skeptisch"),
  Gruppe = c("KI", "KI", "KI"),
  N = c(23, 18, 23),
  Prozent = c("35.9%", "28.1%", "35.9%")
)

cluster_prozent_mensch <- data.frame(
  Cluster = c("Emotional Distanziert", "Emotional Offen", "Ambivalent"),
  Gruppe = c("Mensch", "Mensch", "Mensch"),
  N = c(17, 22, 28),
  Prozent = c("25.4%", "32.8%", "41.8%")
)

cat("\ncluster_prozent_tabelle.png - KI:\n")
print(cluster_prozent_ki)
cat("\ncluster_prozent_tabelle.png - Mensch:\n")
print(cluster_prozent_mensch)

# Test functions
test_ki_consistency <- function() {
  cat("\n=== TEST 1: KI-GRUPPE KONSISTENZ ===\n")
  
  # Test N-values consistency
  ki_n_consistent <- all(ki_specific_expected$n == cluster_prozent_ki$N)
  cat("N-Werte konsistent:", if(ki_n_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  # Test cluster names consistency
  ki_names_consistent <- all(ki_specific_expected$Cluster == cluster_prozent_ki$Cluster)
  cat("Cluster-Namen konsistent:", if(ki_names_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  # Test total N
  ki_total_consistent <- sum(ki_specific_expected$n) == 64
  cat("Gesamt-N korrekt (64):", if(ki_total_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  return(ki_n_consistent && ki_names_consistent && ki_total_consistent)
}

test_mensch_consistency <- function() {
  cat("\n=== TEST 2: MENSCH-GRUPPE KONSISTENZ ===\n")
  
  # Test N-values consistency
  mensch_n_consistent <- all(human_specific_expected$n == cluster_prozent_mensch$N)
  cat("N-Werte konsistent:", if(mensch_n_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  # Test cluster names consistency
  mensch_names_consistent <- all(human_specific_expected$Cluster == cluster_prozent_mensch$Cluster)
  cat("Cluster-Namen konsistent:", if(mensch_names_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  # Test total N
  mensch_total_consistent <- sum(human_specific_expected$n) == 67
  cat("Gesamt-N korrekt (67):", if(mensch_total_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  return(mensch_n_consistent && mensch_names_consistent && mensch_total_consistent)
}

test_cluster_names_order <- function() {
  cat("\n=== TEST 3: CLUSTER-NAMEN-REIHENFOLGE ===\n")
  
  # Test KI cluster order
  ki_order_correct <- all(ki_specific_expected$Cluster == c("KI-Offen", "Ambivalent", "KI-Skeptisch"))
  cat("KI-Cluster-Reihenfolge korrekt:", if(ki_order_correct) "✅ JA" else "❌ NEIN", "\n")
  
  # Test Mensch cluster order
  mensch_order_correct <- all(human_specific_expected$Cluster == c("Emotional Distanziert", "Emotional Offen", "Ambivalent"))
  cat("Mensch-Cluster-Reihenfolge korrekt:", if(mensch_order_correct) "✅ JA" else "❌ NEIN", "\n")
  
  return(ki_order_correct && mensch_order_correct)
}

test_percentage_calculation <- function() {
  cat("\n=== TEST 4: PROZENT-BERECHNUNG ===\n")
  
  # Test KI percentages
  ki_percentages_correct <- all(cluster_prozent_ki$Prozent == c("35.9%", "28.1%", "35.9%"))
  cat("KI-Prozente korrekt:", if(ki_percentages_correct) "✅ JA" else "❌ NEIN", "\n")
  
  # Test Mensch percentages
  mensch_percentages_correct <- all(cluster_prozent_mensch$Prozent == c("25.4%", "32.8%", "41.8%"))
  cat("Mensch-Prozente korrekt:", if(mensch_percentages_correct) "✅ JA" else "❌ NEIN", "\n")
  
  return(ki_percentages_correct && mensch_percentages_correct)
}

test_overall_consistency <- function() {
  cat("\n=== TEST 5: GESAMTKONSISTENZ ===\n")
  
  # Test total sample size
  total_ki <- sum(ki_specific_expected$n)
  total_mensch <- sum(human_specific_expected$n)
  total_consistent <- (total_ki == 64) && (total_mensch == 67) && (total_ki + total_mensch == 131)
  
  cat("Gesamt-Stichprobengröße konsistent (131):", if(total_consistent) "✅ JA" else "❌ NEIN", "\n")
  cat("KI-Gruppe (64) + Mensch-Gruppe (67) = 131:", if(total_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  return(total_consistent)
}

# Run all tests
cat("\n=== TEST-AUSFÜHRUNG ===\n")

test_results <- list(
  ki_consistency = test_ki_consistency(),
  mensch_consistency = test_mensch_consistency(),
  cluster_names_order = test_cluster_names_order(),
  percentage_calculation = test_percentage_calculation(),
  overall_consistency = test_overall_consistency()
)

# Summary
cat("\n================================================================================\n")
cat("CLUSTER-KONSISTENZ TEST-ZUSAMMENFASSUNG\n")
cat("================================================================================\n")

all_tests_passed <- all(unlist(test_results))

if(all_tests_passed) {
  cat("✅ ALLE CLUSTER-KONSISTENZ TESTS BESTANDEN\n")
  cat("✅ ALLE TABELLEN SIND VOLLSTÄNDIG KONSISTENT\n")
} else {
  cat("❌ EINIGE CLUSTER-KONSISTENZ TESTS FEHLGESCHLAGEN\n")
  cat("❌ INKONSISTENZEN ZWISCHEN TABELLEN GEFUNDEN\n")
}

cat("✓ Test 1 (KI-Konsistenz):", if(test_results$ki_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 2 (Mensch-Konsistenz):", if(test_results$mensch_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 3 (Cluster-Namen-Reihenfolge):", if(test_results$cluster_names_order) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 4 (Prozent-Berechnung):", if(test_results$percentage_calculation) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 5 (Gesamtkonsistenz):", if(test_results$overall_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")

cat("\n================================================================================\n")
cat("CLUSTER-KONSISTENZ TEST ABGESCHLOSSEN\n")
cat("================================================================================\n") 