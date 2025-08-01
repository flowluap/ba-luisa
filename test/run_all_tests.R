# =============================================================================
# MASTER TEST SCRIPT - FÜHRT ALLE TESTS AUS
# =============================================================================

cat("================================================================================\n")
cat("MASTER TEST SCRIPT - DATENINTEGRITÄT ALLER TABELLEN\n")
cat("================================================================================\n")

# Run dynamic data validation test
cat("\n1. DYNAMISCHE DATENVALIDIERUNG\n")
cat("================================================================================\n")
source("test/test_dynamic_data_validation.R")

# Run data integrity test (completely dynamic)
cat("\n2. DATENINTEGRITÄT TEST (KOMPLETT DYNAMISCH)\n")
cat("================================================================================\n")
source("test/test_data_integrity_dynamic.R")

cat("\n\n3. CLUSTER-KONSISTENZ TEST (DYNAMISCH)\n")
cat("================================================================================\n")
source("test/test_cluster_consistency_dynamic.R")

cat("\n\n4. CLUSTER-MITTELWERTE KI-STYLE TEST (DYNAMISCH)\n")
cat("================================================================================\n")
source("test/test_cluster_mittelwerte_ki_style_dynamic.R")

cat("\n\n5. CLUSTER-REIHENFOLGE VALIDIERUNG\n")
cat("================================================================================\n")
source("test/test_cluster_order_validation.R")

cat("\n\n6. LOGISCHE CLUSTER-ZUORDNUNG VALIDIERUNG\n")
cat("================================================================================\n")
source("test/test_logical_cluster_validation.R")

cat("\n\n7. WISSENSCHAFTLICHE CLUSTER-ANALYSE VALIDIERUNG\n")
cat("================================================================================\n")
source("test/test_scientific_clustering.R")

cat("\n\n================================================================================\n")
cat("ALLE TESTS ABGESCHLOSSEN\n")
cat("================================================================================\n")
cat("✓ test_dynamic_data_validation.R - Dynamische Datenvalidierung\n")
cat("✓ test_data_integrity_dynamic.R - Datenintegrität (komplett dynamisch)\n")
cat("✓ test_cluster_consistency_dynamic.R - Konsistenz zwischen Tabellen (dynamisch)\n")
cat("✓ test_cluster_mittelwerte_ki_style_dynamic.R - KI-Style Tabelle (dynamisch)\n")
cat("✓ test_cluster_order_validation.R - Cluster-Reihenfolge Validierung\n")
cat("✓ test_logical_cluster_validation.R - Logische Cluster-Zuordnung\n")
cat("✓ test_scientific_clustering.R - Wissenschaftliche Cluster-Analyse\n")
cat("✓ run_all_tests.R - Master-Script\n")
cat("================================================================================\n") 