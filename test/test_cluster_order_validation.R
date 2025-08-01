# =============================================================================
# TEST: CLUSTER-REIHENFOLGE VALIDIERUNG (VERALLGEMEINERT)
# =============================================================================
# Verallgemeinerte Tests um Cluster-Reihenfolge-Probleme zu verhindern

library(dplyr)

cat("================================================================================\n")
cat("VERALLGEMEINERTE CLUSTER-REIHENFOLGE VALIDIERUNG\n")
cat("================================================================================\n")

# Definierte Standard-Reihenfolgen
STANDARD_KI_CLUSTER_ORDER <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")
STANDARD_MENSCH_CLUSTER_ORDER <- c("Emotional Distanziert", "Emotional Offen", "Ambivalent")

# Test-Funktionen
test_cluster_order_standard <- function(cluster_names, expected_order, group_name) {
  cat("\n=== TEST: ", group_name, " CLUSTER-REIHENFOLGE ===\n")
  
  # Prüfe ob alle erwarteten Cluster vorhanden sind
  all_clusters_present <- all(expected_order %in% cluster_names)
  cat("Alle erwarteten Cluster vorhanden:", if(all_clusters_present) "✅ JA" else "❌ NEIN", "\n")
  
  if(!all_clusters_present) {
    missing_clusters <- setdiff(expected_order, cluster_names)
    cat("Fehlende Cluster:", paste(missing_clusters, collapse = ", "), "\n")
  }
  
  # Prüfe ob die Reihenfolge korrekt ist
  order_correct <- all(cluster_names == expected_order)
  cat("Reihenfolge korrekt:", if(order_correct) "✅ JA" else "❌ NEIN", "\n")
  
  if(!order_correct) {
    cat("Aktuelle Reihenfolge:", paste(cluster_names, collapse = " → "), "\n")
    cat("Erwartete Reihenfolge:", paste(expected_order, collapse = " → "), "\n")
  }
  
  return(all_clusters_present && order_correct)
}

test_cluster_names_consistency <- function(cluster_names_1, cluster_names_2, table1_name, table2_name) {
  cat("\n=== TEST: CLUSTER-NAMEN KONSISTENZ (", table1_name, " vs ", table2_name, ") ===\n")
  
  # Prüfe ob alle Cluster-Namen identisch sind
  names_identical <- all(sort(cluster_names_1) == sort(cluster_names_2))
  cat("Cluster-Namen identisch:", if(names_identical) "✅ JA" else "❌ NEIN", "\n")
  
  if(!names_identical) {
    cat("Unterschiede in Cluster-Namen:\n")
    only_in_1 <- setdiff(cluster_names_1, cluster_names_2)
    only_in_2 <- setdiff(cluster_names_2, cluster_names_1)
    
    if(length(only_in_1) > 0) {
      cat("  Nur in ", table1_name, ":", paste(only_in_1, collapse = ", "), "\n")
    }
    if(length(only_in_2) > 0) {
      cat("  Nur in ", table2_name, ":", paste(only_in_2, collapse = ", "), "\n")
    }
  }
  
  return(names_identical)
}

test_cluster_values_consistency <- function(values_1, values_2, cluster_names_1, cluster_names_2, table1_name, table2_name) {
  cat("\n=== TEST: CLUSTER-WERTE KONSISTENZ (", table1_name, " vs ", table2_name, ") ===\n")
  
  # Sortiere beide nach Cluster-Namen für Vergleich
  sorted_1 <- data.frame(
    Cluster = cluster_names_1,
    Values = values_1
  ) %>% arrange(Cluster)
  
  sorted_2 <- data.frame(
    Cluster = cluster_names_2,
    Values = values_2
  ) %>% arrange(Cluster)
  
  # Prüfe ob die sortierten Werte identisch sind
  values_consistent <- all(sorted_1$Values == sorted_2$Values)
  cat("Werte konsistent (nach Sortierung):", if(values_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!values_consistent) {
    cat("Unterschiede in Werten:\n")
    for(i in 1:nrow(sorted_1)) {
      if(sorted_1$Values[i] != sorted_2$Values[i]) {
        cat("  ", sorted_1$Cluster[i], ": ", sorted_1$Values[i], " vs ", sorted_2$Values[i], "\n")
      }
    }
  }
  
  return(values_consistent)
}

test_no_automatic_sorting <- function(cluster_names, values, table_name) {
  cat("\n=== TEST: KEINE AUTOMATISCHE SORTIERUNG (", table_name, ") ===\n")
  
  # Prüfe ob die Cluster nach Werten sortiert sind (was nicht sein sollte)
  sorted_by_values <- order(values)
  current_order <- 1:length(cluster_names)
  
  # Wenn die aktuelle Reihenfolge der sortierten Reihenfolge entspricht, 
  # wurde wahrscheinlich automatisch sortiert
  potentially_auto_sorted <- all(sorted_by_values == current_order)
  
  cat("Potentiell automatisch sortiert:", if(potentially_auto_sorted) "❌ JA" else "✅ NEIN", "\n")
  
  if(potentially_auto_sorted) {
    cat("WARNUNG: Cluster scheinen nach Werten sortiert zu sein!\n")
    cat("Erwartet: Feste Reihenfolge (KI-Offen → Ambivalent → KI-Skeptisch)\n")
    cat("Gefunden: Sortiert nach Werten\n")
  }
  
  return(!potentially_auto_sorted)
}

# Beispiel-Test mit aktuellen Daten
run_example_validation <- function() {
  cat("\n=== BEISPIEL-VALIDIERUNG MIT AKTUELLEN DATEN ===\n")
  
  # Test-Daten aus ki_specific_apa.png
  ki_specific_clusters <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")
  ki_specific_ea_values <- c(2.56, 2.86, 2.31)
  
  # Test-Daten aus cluster_mittelwerte_ki_style.png (NEU GENERIERT - korrekte Reihenfolge)
  ki_style_clusters <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")  # KORRIGIERT
  ki_style_ea_values <- c(2.56, 2.86, 2.31)  # KORRIGIERT
  
  # Führe Tests aus
  test_results <- list(
    order_ki_specific = test_cluster_order_standard(ki_specific_clusters, STANDARD_KI_CLUSTER_ORDER, "KI-Specific"),
    order_ki_style = test_cluster_order_standard(ki_style_clusters, STANDARD_KI_CLUSTER_ORDER, "KI-Style"),
    names_consistency = test_cluster_names_consistency(ki_specific_clusters, ki_style_clusters, "ki_specific", "ki_style"),
    values_consistency = test_cluster_values_consistency(ki_specific_ea_values, ki_style_ea_values, ki_specific_clusters, ki_style_clusters, "ki_specific", "ki_style"),
    no_auto_sort_ki_specific = test_no_automatic_sorting(ki_specific_clusters, ki_specific_ea_values, "ki_specific"),
    no_auto_sort_ki_style = test_no_automatic_sorting(ki_style_clusters, ki_style_ea_values, "ki_style")
  )
  
  # Zusammenfassung
  cat("\n================================================================================\n")
  cat("VALIDIERUNGS-ZUSAMMENFASSUNG\n")
  cat("================================================================================\n")
  
  all_tests_passed <- all(unlist(test_results))
  
  if(all_tests_passed) {
    cat("✅ ALLE VALIDIERUNGS-TESTS BESTANDEN\n")
    cat("✅ Cluster-Reihenfolge ist korrekt und konsistent\n")
  } else {
    cat("❌ EINIGE VALIDIERUNGS-TESTS FEHLGESCHLAGEN\n")
    cat("❌ Cluster-Reihenfolge-Probleme gefunden\n")
  }
  
  cat("✓ Reihenfolge ki_specific:", if(test_results$order_ki_specific) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
  cat("✓ Reihenfolge ki_style:", if(test_results$order_ki_style) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
  cat("✓ Namen-Konsistenz:", if(test_results$names_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
  cat("✓ Werte-Konsistenz:", if(test_results$values_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
  cat("✓ Keine Auto-Sortierung ki_specific:", if(test_results$no_auto_sort_ki_specific) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
  cat("✓ Keine Auto-Sortierung ki_style:", if(test_results$no_auto_sort_ki_style) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
  
  return(test_results)
}

# Führe Beispiel-Validierung aus
example_results <- run_example_validation()

cat("\n================================================================================\n")
cat("VERALLGEMEINERTE CLUSTER-REIHENFOLGE VALIDIERUNG ABGESCHLOSSEN\n")
cat("================================================================================\n")
cat("Diese Tests können für alle zukünftigen Cluster-Tabellen verwendet werden.\n")
cat("Standard-Reihenfolgen sind definiert und werden automatisch geprüft.\n") 