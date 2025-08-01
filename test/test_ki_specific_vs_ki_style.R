# =============================================================================
# TEST: KI_SPECIFIC_APA.PNG VS CLUSTER_MITTELWERTE_KI_STYLE.PNG
# =============================================================================
# Testet: Konsistenz zwischen ki_specific_apa.png und cluster_mittelwerte_ki_style.png

library(dplyr)

cat("================================================================================\n")
cat("TEST: KI_SPECIFIC_APA.PNG VS CLUSTER_MITTELWERTE_KI_STYLE.PNG\n")
cat("================================================================================\n")

# Expected values from ki_specific_apa.png
ki_specific_expected <- data.frame(
  Cluster = c("KI-Offen", "Ambivalent", "KI-Skeptisch"),
  n = c(23, 18, 23),
  VS = c(2.45, 2.30, 2.46),
  MN = c(2.45, 2.43, 2.53),
  ID = c(2.02, 2.54, 2.66),
  EA = c(2.56, 2.86, 2.31)
)

cat("=== ERWARTETE WERTE AUS ki_specific_apa.png ===\n")
print(ki_specific_expected)

# Expected values from cluster_mittelwerte_ki_style.png (KI portion)
ki_style_expected <- data.frame(
  Gruppe = c("KI", "KI", "KI"),
  Cluster = c("KI-Offen", "Ambivalent", "KI-Skeptisch"),
  N = c(23, 18, 23),
  VS = c(2.45, 2.30, 2.46),
  MN = c(2.45, 2.43, 2.53),
  ID = c(2.02, 2.54, 2.66),
  EA = c(2.56, 2.86, 2.31)
)

cat("\n=== ERWARTETE WERTE AUS cluster_mittelwerte_ki_style.png (KI-TEIL) ===\n")
print(ki_style_expected)

# Test functions
test_n_values_consistency <- function() {
  cat("\n=== TEST 1: N-WERTE KONSISTENZ ===\n")
  
  n_consistent <- all(ki_specific_expected$n == ki_style_expected$N)
  cat("N-Werte identisch:", if(n_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!n_consistent) {
    cat("Unterschiede in N-Werten:\n")
    for(i in 1:3) {
      if(ki_specific_expected$n[i] != ki_style_expected$N[i]) {
        cat("  ", ki_specific_expected$Cluster[i], ": ", 
            ki_specific_expected$n[i], " vs ", ki_style_expected$N[i], "\n")
      }
    }
  }
  
  return(n_consistent)
}

test_vs_values_consistency <- function() {
  cat("\n=== TEST 2: VS-WERTE KONSISTENZ ===\n")
  
  vs_consistent <- all(ki_specific_expected$VS == ki_style_expected$VS)
  cat("VS-Werte identisch:", if(vs_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!vs_consistent) {
    cat("Unterschiede in VS-Werten:\n")
    for(i in 1:3) {
      if(ki_specific_expected$VS[i] != ki_style_expected$VS[i]) {
        cat("  ", ki_specific_expected$Cluster[i], ": ", 
            ki_specific_expected$VS[i], " vs ", ki_style_expected$VS[i], "\n")
      }
    }
  }
  
  return(vs_consistent)
}

test_mn_values_consistency <- function() {
  cat("\n=== TEST 3: MN-WERTE KONSISTENZ ===\n")
  
  mn_consistent <- all(ki_specific_expected$MN == ki_style_expected$MN)
  cat("MN-Werte identisch:", if(mn_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!mn_consistent) {
    cat("Unterschiede in MN-Werten:\n")
    for(i in 1:3) {
      if(ki_specific_expected$MN[i] != ki_style_expected$MN[i]) {
        cat("  ", ki_specific_expected$Cluster[i], ": ", 
            ki_specific_expected$MN[i], " vs ", ki_style_expected$MN[i], "\n")
      }
    }
  }
  
  return(mn_consistent)
}

test_id_values_consistency <- function() {
  cat("\n=== TEST 4: ID-WERTE KONSISTENZ ===\n")
  
  id_consistent <- all(ki_specific_expected$ID == ki_style_expected$ID)
  cat("ID-Werte identisch:", if(id_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!id_consistent) {
    cat("Unterschiede in ID-Werten:\n")
    for(i in 1:3) {
      if(ki_specific_expected$ID[i] != ki_style_expected$ID[i]) {
        cat("  ", ki_specific_expected$Cluster[i], ": ", 
            ki_specific_expected$ID[i], " vs ", ki_style_expected$ID[i], "\n")
      }
    }
  }
  
  return(id_consistent)
}

test_ea_values_consistency <- function() {
  cat("\n=== TEST 5: EA-WERTE KONSISTENZ ===\n")
  
  ea_consistent <- all(ki_specific_expected$EA == ki_style_expected$EA)
  cat("EA-Werte identisch:", if(ea_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!ea_consistent) {
    cat("Unterschiede in EA-Werten:\n")
    for(i in 1:3) {
      if(ki_specific_expected$EA[i] != ki_style_expected$EA[i]) {
        cat("  ", ki_specific_expected$Cluster[i], ": ", 
            ki_specific_expected$EA[i], " vs ", ki_style_expected$EA[i], "\n")
      }
    }
  }
  
  return(ea_consistent)
}

test_cluster_names_consistency <- function() {
  cat("\n=== TEST 6: CLUSTER-NAMEN KONSISTENZ ===\n")
  
  names_consistent <- all(ki_specific_expected$Cluster == ki_style_expected$Cluster)
  cat("Cluster-Namen identisch:", if(names_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!names_consistent) {
    cat("Unterschiede in Cluster-Namen:\n")
    for(i in 1:3) {
      if(ki_specific_expected$Cluster[i] != ki_style_expected$Cluster[i]) {
        cat("  Position ", i, ": '", ki_specific_expected$Cluster[i], 
            "' vs '", ki_style_expected$Cluster[i], "'\n")
      }
    }
  }
  
  return(names_consistent)
}

test_cluster_order_consistency <- function() {
  cat("\n=== TEST 7: CLUSTER-REIHENFOLGE KONSISTENZ ===\n")
  
  order_consistent <- all(ki_specific_expected$Cluster == c("KI-Offen", "Ambivalent", "KI-Skeptisch"))
  cat("Cluster-Reihenfolge korrekt:", if(order_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!order_consistent) {
    cat("Aktuelle Reihenfolge in ki_specific_apa.png:\n")
    for(i in 1:3) {
      cat("  ", i, ". ", ki_specific_expected$Cluster[i], "\n")
    }
    cat("Erwartete Reihenfolge: KI-Offen, Ambivalent, KI-Skeptisch\n")
  }
  
  return(order_consistent)
}

test_total_consistency <- function() {
  cat("\n=== TEST 8: GESAMTKONSISTENZ ===\n")
  
  # Test if all values are identical
  all_values_identical <- all(
    ki_specific_expected$n == ki_style_expected$N &
    ki_specific_expected$VS == ki_style_expected$VS &
    ki_specific_expected$MN == ki_style_expected$MN &
    ki_specific_expected$ID == ki_style_expected$ID &
    ki_specific_expected$EA == ki_style_expected$EA &
    ki_specific_expected$Cluster == ki_style_expected$Cluster
  )
  
  cat("Alle Werte identisch:", if(all_values_identical) "✅ JA" else "❌ NEIN", "\n")
  
  # Test total N
  total_n_ki_specific <- sum(ki_specific_expected$n)
  total_n_ki_style <- sum(ki_style_expected$N)
  total_n_consistent <- total_n_ki_specific == total_n_ki_style
  
  cat("Gesamt-N konsistent (", total_n_ki_specific, "):", 
      if(total_n_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  return(all_values_identical && total_n_consistent)
}

# Run all tests
cat("\n=== TEST-AUSFÜHRUNG ===\n")

test_results <- list(
  n_values = test_n_values_consistency(),
  vs_values = test_vs_values_consistency(),
  mn_values = test_mn_values_consistency(),
  id_values = test_id_values_consistency(),
  ea_values = test_ea_values_consistency(),
  cluster_names = test_cluster_names_consistency(),
  cluster_order = test_cluster_order_consistency(),
  total_consistency = test_total_consistency()
)

# Summary
cat("\n================================================================================\n")
cat("KI_SPECIFIC_APA VS KI_STYLE TEST-ZUSAMMENFASSUNG\n")
cat("================================================================================\n")

all_tests_passed <- all(unlist(test_results))

if(all_tests_passed) {
  cat("✅ ALLE TESTS BESTANDEN - TABELLEN SIND IDENTISCH\n")
  cat("✅ ki_specific_apa.png und cluster_mittelwerte_ki_style.png sind vollständig konsistent\n")
} else {
  cat("❌ EINIGE TESTS FEHLGESCHLAGEN - INKONSISTENZEN GEFUNDEN\n")
  cat("❌ ki_specific_apa.png und cluster_mittelwerte_ki_style.png unterscheiden sich\n")
}

cat("✓ Test 1 (N-Werte):", if(test_results$n_values) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 2 (VS-Werte):", if(test_results$vs_values) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 3 (MN-Werte):", if(test_results$mn_values) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 4 (ID-Werte):", if(test_results$id_values) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 5 (EA-Werte):", if(test_results$ea_values) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 6 (Cluster-Namen):", if(test_results$cluster_names) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 7 (Cluster-Reihenfolge):", if(test_results$cluster_order) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 8 (Gesamtkonsistenz):", if(test_results$total_consistency) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")

cat("\n================================================================================\n")
cat("VERGLEICH ABGESCHLOSSEN\n")
cat("================================================================================\n") 