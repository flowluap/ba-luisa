# =============================================================================
# TEST: ALTE CLUSTER_MITTELWERTE_KI_STYLE.PNG VALIDIERUNG
# =============================================================================
# Testet die alte Version der cluster_mittelwerte_ki_style.png

library(dplyr)

cat("================================================================================\n")
cat("TEST: ALTE CLUSTER_MITTELWERTE_KI_STYLE.PNG VALIDIERUNG\n")
cat("================================================================================\n")

# Standard-Reihenfolge
STANDARD_KI_CLUSTER_ORDER <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")

# Basierend auf der Beschreibung der alten Version (die Ihnen optisch besser gefällt)
# Die alte Version hatte: Ambivalent, KI-Offen, KI-Skeptisch (falsche Reihenfolge)
old_cluster_order <- c("Ambivalent", "KI-Offen", "KI-Skeptisch")
old_ea_values <- c(2.86, 2.56, 2.31)

cat("=== ALTE VERSION ANALYSE ===\n")
cat("Aktuelle Reihenfolge:", paste(old_cluster_order, collapse = " → "), "\n")
cat("EA-Werte:", paste(old_ea_values, collapse = ", "), "\n")

# Test der Reihenfolge
cat("\n=== REIHENFOLGE-TEST ===\n")
order_correct <- all(old_cluster_order == STANDARD_KI_CLUSTER_ORDER)
cat("Reihenfolge korrekt:", if(order_correct) "✅ JA" else "❌ NEIN", "\n")

if(!order_correct) {
  cat("Aktuelle Reihenfolge:", paste(old_cluster_order, collapse = " → "), "\n")
  cat("Erwartete Reihenfolge:", paste(STANDARD_KI_CLUSTER_ORDER, collapse = " → "), "\n")
  cat("PROBLEM: Die alte Version hat die falsche Reihenfolge!\n")
}

# Test der Werte-Konsistenz
cat("\n=== WERTE-KONSISTENZ ===\n")
# Vergleiche mit ki_specific_apa.png
ki_specific_ea_values <- c(2.56, 2.86, 2.31)  # KI-Offen, Ambivalent, KI-Skeptisch

# Sortiere beide nach Cluster-Namen für Vergleich
old_sorted <- data.frame(
  Cluster = old_cluster_order,
  EA = old_ea_values
) %>% arrange(Cluster)

ki_specific_sorted <- data.frame(
  Cluster = STANDARD_KI_CLUSTER_ORDER,
  EA = ki_specific_ea_values
) %>% arrange(Cluster)

values_consistent <- all(old_sorted$EA == ki_specific_sorted$EA)
cat("Werte konsistent (nach Sortierung):", if(values_consistent) "✅ JA" else "❌ NEIN", "\n")

if(values_consistent) {
  cat("✅ Die Werte sind identisch, nur die Reihenfolge ist unterschiedlich\n")
} else {
  cat("❌ Es gibt auch Unterschiede in den Werten selbst\n")
}

cat("\n================================================================================\n")
cat("FAZIT: ALTE VERSION\n")
cat("================================================================================\n")

if(!order_correct && values_consistent) {
  cat("❌ Die alte Version hat die falsche Reihenfolge\n")
  cat("✅ Aber die Werte sind korrekt (nach Sortierung)\n")
  cat("💡 Sie mögen die Optik, aber die Reihenfolge ist inkonsistent\n")
} else if(order_correct) {
  cat("✅ Die alte Version ist korrekt\n")
} else {
  cat("❌ Die alte Version hat sowohl Reihenfolge- als auch Werte-Probleme\n")
}

cat("================================================================================\n")
cat("TEST ABGESCHLOSSEN\n")
cat("================================================================================\n") 