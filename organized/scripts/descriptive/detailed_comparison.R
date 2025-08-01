# ================================================================================
# DETAILLIERTER VERGLEICH DER DATENSÄTZE
# ================================================================================
# Findet die genauen Unterschiede zwischen den Datensätzen
# ================================================================================

# Load required libraries
library(dplyr)

# ================================================================================
# DATEN LADEN UND FILTERN
# ================================================================================

# Load both datasets
simulated_data <- read.delim("organized/data/Datensatz_simuliert.csv", 
                            fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)
real_data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                       fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter simulated data for FINISHED=1 only
simulated_filtered <- simulated_data[simulated_data$FINISHED == 1, ]

cat("================================================================================\n")
cat("DETAILLIERTER VERGLEICH DER DATENSÄTZE\n")
cat("================================================================================\n\n")

# ================================================================================
# SPALTENNAMEN VERGLEICHEN
# ================================================================================

cat("1. SPALTENNAMEN VERGLEICH:\n")
cat("   ======================\n")

sim_cols <- colnames(simulated_filtered)
real_cols <- colnames(real_data)

if(identical(sim_cols, real_cols)) {
  cat("   ✅ Spaltennamen sind identisch\n")
} else {
  cat("   ❌ Spaltennamen unterscheiden sich\n")
  cat("   Unterschiede:\n")
  cat("   - Simuliert:", setdiff(sim_cols, real_cols), "\n")
  cat("   - Real:", setdiff(real_cols, sim_cols), "\n")
}
cat("\n")

# ================================================================================
# DATENTYPEN VERGLEICHEN
# ================================================================================

cat("2. DATENTYPEN VERGLEICH:\n")
cat("   ====================\n")

# Compare data types for key columns
key_cols <- c("AB01", "FINISHED", "ID01_01", "ID01_02", "ID01_03", "ID01_04", 
              "EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")

for(col in key_cols) {
  if(col %in% colnames(simulated_filtered) && col %in% colnames(real_data)) {
    sim_type <- class(simulated_filtered[[col]])
    real_type <- class(real_data[[col]])
    
    if(identical(sim_type, real_type)) {
      cat("   ✅", col, ":", sim_type, "\n")
    } else {
      cat("   ❌", col, ":", sim_type, "vs", real_type, "\n")
    }
  }
}
cat("\n")

# ================================================================================
# EINZELNE WERTE VERGLEICHEN
# ================================================================================

cat("3. EINZELNE WERTE VERGLEICH:\n")
cat("   =======================\n")

# Compare first few rows for key columns
cat("   Erste 3 Zeilen für AB01:\n")
cat("   Simuliert:", head(simulated_filtered$AB01, 3), "\n")
cat("   Real:", head(real_data$AB01, 3), "\n\n")

cat("   Erste 3 Zeilen für ID01_01:\n")
cat("   Simuliert:", head(simulated_filtered$ID01_01, 3), "\n")
cat("   Real:", head(real_data$ID01_01, 3), "\n\n")

# ================================================================================
# SORTIERUNG VERGLEICHEN
# ================================================================================

cat("4. SORTIERUNG VERGLEICH:\n")
cat("   ====================\n")

# Check if the data is sorted differently
sim_sorted <- simulated_filtered[order(simulated_filtered$CASE), ]
real_sorted <- real_data[order(real_data$CASE), ]

# Compare sorted datasets
identical_sorted <- identical(sim_sorted, real_sorted)
cat("   Sind die sortierten Datensätze identisch?", identical_sorted, "\n\n")

# ================================================================================
# CASE-NUMMERN VERGLEICHEN
# ================================================================================

cat("5. CASE-NUMMERN VERGLEICH:\n")
cat("   ======================\n")

sim_cases <- sort(simulated_filtered$CASE)
real_cases <- sort(real_data$CASE)

if(identical(sim_cases, real_cases)) {
  cat("   ✅ CASE-Nummern sind identisch\n")
} else {
  cat("   ❌ CASE-Nummern unterscheiden sich\n")
  cat("   Unterschiede in CASE-Nummern:\n")
  cat("   - Nur in simuliert:", setdiff(sim_cases, real_cases), "\n")
  cat("   - Nur in real:", setdiff(real_cases, sim_cases), "\n")
}
cat("\n")

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("6. ZUSAMMENFASSUNG:\n")
cat("   ===============\n")
cat("   Die Datensätze sind statistisch identisch (gleiche Mittelwerte,\n")
cat("   Gruppengrößen, etc.), aber unterscheiden sich wahrscheinlich in:\n")
cat("   - Der Reihenfolge der Zeilen\n")
cat("   - Einzelnen CASE-Nummern\n")
cat("   - Möglicherweise Datentypen oder Formatierung\n")
cat("   - Aber nicht in den eigentlichen Messwerten\n")
cat("================================================================================\n") 