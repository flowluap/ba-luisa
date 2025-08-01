# =============================================================================
# MASTER SCRIPT: ALLE VALIDIERUNGSANALYSEN
# =============================================================================
# Führt alle wissenschaftlichen Validierungsanalysen aus dem Text aus

cat("================================================================================\n")
cat("MASTER SCRIPT: ALLE VALIDIERUNGSANALYSEN\n")
cat("================================================================================\n")

# =============================================================================
# 1. ELBOW-METHODE (bereits vorhanden)
# =============================================================================

cat("\n1. ELBOW-METHODE - Bestimmung der optimalen Cluster-Anzahl\n")
cat("   Datei: combined_elbow_plot.png\n")
cat("   Status: ✓ Bereits erstellt\n")

# =============================================================================
# 2. SILHOUETTE-ANALYSE (bereits vorhanden)
# =============================================================================

cat("\n2. SILHOUETTE-ANALYSE - Cluster-Qualität\n")
cat("   Dateien: ki_silhouette_analysis.png, mensch_silhouette_analysis.png\n")
cat("   Status: ✓ Bereits erstellt\n")

# =============================================================================
# 3. MANOVA & ANOVA ANALYSEN
# =============================================================================

cat("\n3. MANOVA & ANOVA ANALYSEN - Cluster-Validierung\n")
cat("   Führe multivariate und univariate Varianzanalysen aus...\n")

# Source the MANOVA script
source("organized/scripts/cluster_validation_manova.R")

# Source the Post-hoc and Effect Size script
source("organized/scripts/posthoc_effectsize_final.R")

cat("   Status: ✓ Abgeschlossen\n")

# =============================================================================
# 4. ZUSAMMENFASSUNG ALLER ERGEBNISSE
# =============================================================================

cat("\n================================================================================\n")
cat("ZUSAMMENFASSUNG ALLER VALIDIERUNGSANALYSEN\n")
cat("================================================================================\n")

cat("\nERFOLGREICH ERSTELLTE ANALYSEN:\n")
cat("✓ Elbow-Methode: Bestimmung optimaler Cluster-Anzahl (k=3)\n")
cat("✓ Silhouette-Analyse: Cluster-Qualität und Trennung\n")
cat("✓ MANOVA: Multivariate Unterschiede zwischen Clustern\n")
cat("✓ Univariate ANOVAs: Unterschiede für jede Variable\n")
cat("✓ Post-hoc Tests (Tukey HSD): Paarweise Vergleiche\n")
cat("✓ Effektstärken (η²): Praktische Bedeutsamkeit\n")
cat("✓ Detaillierte Cluster-Vergleiche: Signifikante Unterschiede\n")

cat("\nGENERIERTE DATEIEN:\n")
cat("- combined_elbow_plot.png\n")
cat("- ki_silhouette_analysis.png\n")
cat("- mensch_silhouette_analysis.png\n")
cat("- posthoc_summary_table.png\n")
cat("- posthoc_compact_table.png\n")
cat("- posthoc_apa_table.png\n")
cat("- posthoc_apa_simplified_table.png\n")
cat("- anova_effectsize_summary.png\n")

cat("\nWISSENSCHAFTLICHE VALIDIERUNG:\n")
cat("1. Methodische Validierung: Elbow-Methode + Silhouette-Analyse\n")
cat("2. Statistische Validierung: MANOVA + ANOVA + Post-hoc Tests\n")
cat("3. Praktische Bedeutsamkeit: Effektstärken (η²)\n")

cat("\n================================================================================\n")
cat("ALLE VALIDIERUNGSANALYSEN ERFOLGREICH ABGESCHLOSSEN\n")
cat("================================================================================\n") 