# =============================================================================
# TEST: CLUSTER-REIHENFOLGE PROBLEM ANALYSE
# =============================================================================
# Analysiert das Problem mit vertauschten Clustern zwischen Tabellen

library(dplyr)

cat("================================================================================\n")
cat("ANALYSE: CLUSTER-REIHENFOLGE PROBLEM\n")
cat("================================================================================\n")

# Werte aus ki_specific_apa.png
ki_specific_apa <- data.frame(
  Cluster = c("KI-Offen", "Ambivalent", "KI-Skeptisch"),
  n = c(23, 18, 23),
  VS = c(2.45, 2.30, 2.46),
  MN = c(2.45, 2.43, 2.53),
  ID = c(2.02, 2.54, 2.66),
  EA = c(2.56, 2.86, 2.31)
)

cat("=== WERTE AUS ki_specific_apa.png ===\n")
print(ki_specific_apa)

# Werte aus cluster_mittelwerte_ki_style.png (basierend auf dem Diagramm)
cluster_mittelwerte_ki_style <- data.frame(
  Cluster = c("Ambivalent", "KI-Offen", "KI-Skeptisch"),  # Reihenfolge wie im Diagramm
  n = c(18, 23, 23),
  VS = c(2.30, 2.45, 2.46),
  MN = c(2.43, 2.45, 2.53),
  ID = c(2.54, 2.02, 2.66),
  EA = c(2.86, 2.56, 2.31)  # Hier ist EA für Ambivalent = 2.86 (nicht 2.8)
)

cat("\n=== WERTE AUS cluster_mittelwerte_ki_style.png (Diagramm-Reihenfolge) ===\n")
print(cluster_mittelwerte_ki_style)

# Analyse der EA-Werte
cat("\n=== EA-WERTE VERGLEICH ===\n")
cat("ki_specific_apa.png:\n")
for(i in 1:3) {
  cat("  ", ki_specific_apa$Cluster[i], ": EA = ", ki_specific_apa$EA[i], "\n")
}

cat("\ncluster_mittelwerte_ki_style.png:\n")
for(i in 1:3) {
  cat("  ", cluster_mittelwerte_ki_style$Cluster[i], ": EA = ", cluster_mittelwerte_ki_style$EA[i], "\n")
}

# Test der Reihenfolge
cat("\n=== REIHENFOLGE-ANALYSE ===\n")

# Prüfe ob die EA-Werte in der gleichen Reihenfolge sind
ea_order_ki_specific <- ki_specific_apa$EA
ea_order_ki_style <- cluster_mittelwerte_ki_style$EA

cat("EA-Werte ki_specific_apa.png: ", paste(ea_order_ki_specific, collapse = ", "), "\n")
cat("EA-Werte ki_style.png: ", paste(ea_order_ki_style, collapse = ", "), "\n")

# Prüfe ob die Reihenfolge identisch ist
order_identical <- all(ea_order_ki_specific == ea_order_ki_style)
cat("EA-Reihenfolge identisch:", if(order_identical) "✅ JA" else "❌ NEIN", "\n")

if(!order_identical) {
  cat("\n=== PROBLEM IDENTIFIZIERT ===\n")
  cat("Die Cluster sind in unterschiedlicher Reihenfolge dargestellt!\n")
  
  # Zeige die korrekte Zuordnung
  cat("\nKorrekte Zuordnung basierend auf EA-Werten:\n")
  for(i in 1:3) {
    ki_specific_cluster <- ki_specific_apa$Cluster[i]
    ki_specific_ea <- ki_specific_apa$EA[i]
    
    # Finde den entsprechenden Cluster in ki_style
    matching_index <- which(cluster_mittelwerte_ki_style$EA == ki_specific_ea)
    ki_style_cluster <- cluster_mittelwerte_ki_style$Cluster[matching_index]
    
    cat("  ", ki_specific_cluster, " (EA=", ki_specific_ea, ") entspricht ", 
        ki_style_cluster, " in ki_style\n")
  }
}

# Prüfe alle Werte auf Konsistenz
cat("\n=== VOLLSTÄNDIGE WERTE-KONSISTENZ ===\n")

# Sortiere beide Tabellen nach EA-Werten für Vergleich
ki_specific_sorted <- ki_specific_apa[order(ki_specific_apa$EA), ]
ki_style_sorted <- cluster_mittelwerte_ki_style[order(cluster_mittelwerte_ki_style$EA), ]

cat("Nach EA-Werten sortiert:\n")
cat("ki_specific_apa.png:\n")
print(ki_specific_sorted)
cat("\nki_style.png:\n")
print(ki_style_sorted)

# Prüfe ob die sortierten Werte identisch sind
values_consistent <- all(
  ki_specific_sorted$n == ki_style_sorted$n &
  ki_specific_sorted$VS == ki_style_sorted$VS &
  ki_specific_sorted$MN == ki_style_sorted$MN &
  ki_specific_sorted$ID == ki_style_sorted$ID &
  ki_specific_sorted$EA == ki_style_sorted$EA
)

cat("\nWerte konsistent (nach Sortierung):", if(values_consistent) "✅ JA" else "❌ NEIN", "\n")

if(values_consistent) {
  cat("✅ Die Werte sind identisch, nur die Reihenfolge ist unterschiedlich!\n")
} else {
  cat("❌ Es gibt auch Unterschiede in den Werten selbst!\n")
}

cat("\n================================================================================\n")
cat("ANALYSE ABGESCHLOSSEN\n")
cat("================================================================================\n") 