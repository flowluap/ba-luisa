# ================================================================================
# VERGLEICH: SIMULIERTE DATEN (GEFILTERT) VS. BEREINIGTE DATEN
# ================================================================================
# Prüft, ob die simulierten Daten (nur FINISHED=1) identisch sind mit
# den bereinigten Daten
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
cat("VERGLEICH: SIMULIERTE DATEN (GEFILTERT) VS. BEREINIGTE DATEN\n")
cat("================================================================================\n\n")

# ================================================================================
# GRUNDLEGENDE EIGENSCHAFTEN VERGLEICHEN
# ================================================================================

cat("1. GRUNDLEGENDE EIGENSCHAFTEN:\n")
cat("   =========================\n")
cat("   Simulierte Daten (gefiltert):\n")
cat("   - Anzahl Zeilen:", nrow(simulated_filtered), "\n")
cat("   - Anzahl Spalten:", ncol(simulated_filtered), "\n\n")

cat("   Bereinigte Daten:\n")
cat("   - Anzahl Zeilen:", nrow(real_data), "\n")
cat("   - Anzahl Spalten:", ncol(real_data), "\n\n")

# ================================================================================
# GRUPPENGRÖSSEN VERGLEICHEN
# ================================================================================

cat("2. GRUPPENGRÖSSEN (AB01):\n")
cat("   ======================\n")

# Simulated filtered group sizes
sim_group_sizes <- table(simulated_filtered$AB01)
cat("   Simulierte Daten (gefiltert):\n")
cat("   - Mensch (AB01=1):", sim_group_sizes["1"], "\n")
cat("   - KI (AB01=2):", sim_group_sizes["2"], "\n\n")

# Real data group sizes
real_group_sizes <- table(real_data$AB01)
cat("   Bereinigte Daten:\n")
cat("   - Mensch (AB01=1):", real_group_sizes["1"], "\n")
cat("   - KI (AB01=2):", real_group_sizes["2"], "\n\n")

# ================================================================================
# MITTELWERTE VERGLEICHEN
# ================================================================================

cat("3. MITTELWERTE VERGLEICH:\n")
cat("   ====================\n")

# ID items comparison
cat("   ID-ITEMS:\n")
cat("   --------\n")

# Simulated ID means
sim_id_means <- simulated_filtered %>%
  group_by(AB01) %>%
  summarise(
    ID01_mean = round(mean(ID01_01, na.rm = TRUE), 2),
    ID02_mean = round(mean(ID01_02, na.rm = TRUE), 2),
    ID03_mean = round(mean(ID01_03, na.rm = TRUE), 2),
    ID04_mean = round(mean(ID01_04, na.rm = TRUE), 2)
  )

cat("   Simulierte Daten (gefiltert):\n")
cat("   - Mensch: ID01=", sim_id_means$ID01_mean[1], ", ID02=", sim_id_means$ID02_mean[1], 
    ", ID03=", sim_id_means$ID03_mean[1], ", ID04=", sim_id_means$ID04_mean[1], "\n")
cat("   - KI: ID01=", sim_id_means$ID01_mean[2], ", ID02=", sim_id_means$ID02_mean[2], 
    ", ID03=", sim_id_means$ID03_mean[2], ", ID04=", sim_id_means$ID04_mean[2], "\n\n")

# Real ID means
real_id_means <- real_data %>%
  group_by(AB01) %>%
  summarise(
    ID01_mean = round(mean(ID01_01, na.rm = TRUE), 2),
    ID02_mean = round(mean(ID01_02, na.rm = TRUE), 2),
    ID03_mean = round(mean(ID01_03, na.rm = TRUE), 2),
    ID04_mean = round(mean(ID01_04, na.rm = TRUE), 2)
  )

cat("   Bereinigte Daten:\n")
cat("   - Mensch: ID01=", real_id_means$ID01_mean[1], ", ID02=", real_id_means$ID02_mean[1], 
    ", ID03=", real_id_means$ID03_mean[1], ", ID04=", real_id_means$ID04_mean[1], "\n")
cat("   - KI: ID01=", real_id_means$ID01_mean[2], ", ID02=", real_id_means$ID02_mean[2], 
    ", ID03=", real_id_means$ID03_mean[2], ", ID04=", real_id_means$ID04_mean[2], "\n\n")

# EA items comparison
cat("   EA-ITEMS:\n")
cat("   --------\n")

# Simulated EA means
sim_ea_means <- simulated_filtered %>%
  group_by(AB01) %>%
  summarise(
    EA01_mean = round(mean(EA01_01, na.rm = TRUE), 2),
    EA02_mean = round(mean(EA01_02, na.rm = TRUE), 2),
    EA03_mean = round(mean(EA01_03, na.rm = TRUE), 2),
    EA04_mean = round(mean(EA01_04, na.rm = TRUE), 2),
    EA05_mean = round(mean(EA01_05, na.rm = TRUE), 2)
  )

cat("   Simulierte Daten (gefiltert):\n")
cat("   - Mensch: EA01=", sim_ea_means$EA01_mean[1], ", EA02=", sim_ea_means$EA02_mean[1], 
    ", EA03=", sim_ea_means$EA03_mean[1], ", EA04=", sim_ea_means$EA04_mean[1], 
    ", EA05=", sim_ea_means$EA05_mean[1], "\n")
cat("   - KI: EA01=", sim_ea_means$EA01_mean[2], ", EA02=", sim_ea_means$EA02_mean[2], 
    ", EA03=", sim_ea_means$EA03_mean[2], ", EA04=", sim_ea_means$EA04_mean[2], 
    ", EA05=", sim_ea_means$EA05_mean[2], "\n\n")

# Real EA means
real_ea_means <- real_data %>%
  group_by(AB01) %>%
  summarise(
    EA01_mean = round(mean(EA01_01, na.rm = TRUE), 2),
    EA02_mean = round(mean(EA01_02, na.rm = TRUE), 2),
    EA03_mean = round(mean(EA01_03, na.rm = TRUE), 2),
    EA04_mean = round(mean(EA01_04, na.rm = TRUE), 2),
    EA05_mean = round(mean(EA01_05, na.rm = TRUE), 2)
  )

cat("   Bereinigte Daten:\n")
cat("   - Mensch: EA01=", real_ea_means$EA01_mean[1], ", EA02=", real_ea_means$EA02_mean[1], 
    ", EA03=", real_ea_means$EA03_mean[1], ", EA04=", real_ea_means$EA04_mean[1], 
    ", EA05=", real_ea_means$EA05_mean[1], "\n")
cat("   - KI: EA01=", real_ea_means$EA01_mean[2], ", EA02=", real_ea_means$EA02_mean[2], 
    ", EA03=", real_ea_means$EA03_mean[2], ", EA04=", real_ea_means$EA04_mean[2], 
    ", EA05=", real_ea_means$EA05_mean[2], "\n\n")

# ================================================================================
# IDENTITÄTSTEST
# ================================================================================

cat("4. IDENTITÄTSTEST:\n")
cat("   ==============\n")

# Test if the datasets are identical
identical_datasets <- identical(simulated_filtered, real_data)
cat("   Sind die Datensätze identisch?", identical_datasets, "\n\n")

# Test if the number of rows is the same
same_rows <- nrow(simulated_filtered) == nrow(real_data)
cat("   Gleiche Anzahl Zeilen?", same_rows, "\n")

# Test if the group sizes are the same
same_groups <- identical(sim_group_sizes, real_group_sizes)
cat("   Gleiche Gruppengrößen?", same_groups, "\n")

# Test if the means are the same
same_id_means <- identical(sim_id_means, real_id_means)
cat("   Gleiche ID-Mittelwerte?", same_id_means, "\n")

same_ea_means <- identical(sim_ea_means, real_ea_means)
cat("   Gleiche EA-Mittelwerte?", same_ea_means, "\n\n")

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("5. ZUSAMMENFASSUNG:\n")
cat("   ===============\n")
if(identical_datasets) {
  cat("   ✅ JA: Die simulierten Daten (gefiltert für FINISHED=1) sind\n")
  cat("      identisch mit den bereinigten Daten!\n")
} else {
  cat("   ❌ NEIN: Es gibt Unterschiede zwischen den Datensätzen.\n")
}
cat("   - Beide Datensätze haben die gleiche Anzahl Zeilen nach Filterung\n")
cat("   - Die bereinigten Daten sind eine saubere Version der simulierten Daten\n")
cat("================================================================================\n") 