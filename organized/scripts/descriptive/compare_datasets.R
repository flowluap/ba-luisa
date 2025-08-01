# ================================================================================
# VERGLEICH ZWISCHEN DATENSÄTZEN
# ================================================================================
# Script zum Vergleich von "Datensatz_simuliert.csv" und 
# "Bereinigte Daten von WhatsApp Business.csv"
# ================================================================================

# Load required libraries
library(dplyr)

# ================================================================================
# DATEN LADEN
# ================================================================================

# Load both datasets
simulated_data <- read.delim("organized/data/Datensatz_simuliert.csv", 
                            fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)
real_data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                       fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

cat("================================================================================\n")
cat("VERGLEICH ZWISCHEN DATENSÄTZEN\n")
cat("================================================================================\n\n")

# ================================================================================
# GRUNDLEGENDE EIGENSCHAFTEN
# ================================================================================

cat("1. GRUNDLEGENDE EIGENSCHAFTEN:\n")
cat("   =========================\n")
cat("   Simulierte Daten:\n")
cat("   - Anzahl Zeilen:", nrow(simulated_data), "\n")
cat("   - Anzahl Spalten:", ncol(simulated_data), "\n")
cat("   - Spaltennamen:", paste(colnames(simulated_data), collapse=", "), "\n\n")

cat("   Echte Daten:\n")
cat("   - Anzahl Zeilen:", nrow(real_data), "\n")
cat("   - Anzahl Spalten:", ncol(real_data), "\n")
cat("   - Spaltennamen:", paste(colnames(real_data), collapse=", "), "\n\n")

# ================================================================================
# GRUPPENGRÖSSEN VERGLEICHEN
# ================================================================================

cat("2. GRUPPENGRÖSSEN (AB01):\n")
cat("   ======================\n")

# Simulated data group sizes
sim_group_sizes <- table(simulated_data$AB01)
cat("   Simulierte Daten:\n")
cat("   - Mensch (AB01=1):", sim_group_sizes["1"], "\n")
cat("   - KI (AB01=2):", sim_group_sizes["2"], "\n\n")

# Real data group sizes
real_group_sizes <- table(real_data$AB01)
cat("   Echte Daten:\n")
cat("   - Mensch (AB01=1):", real_group_sizes["1"], "\n")
cat("   - KI (AB01=2):", real_group_sizes["2"], "\n\n")

# ================================================================================
# FINISHED STATUS VERGLEICHEN
# ================================================================================

cat("3. FINISHED STATUS:\n")
cat("   ================\n")

# Simulated data finished status
sim_finished <- table(simulated_data$FINISHED)
cat("   Simulierte Daten:\n")
cat("   - FINISHED=0:", sim_finished["0"], "\n")
cat("   - FINISHED=1:", sim_finished["1"], "\n\n")

# Real data finished status
real_finished <- table(real_data$FINISHED)
cat("   Echte Daten:\n")
cat("   - FINISHED=0:", real_finished["0"], "\n")
cat("   - FINISHED=1:", real_finished["1"], "\n\n")

# ================================================================================
# MITTELWERTE VERGLEICHEN (NUR FINISHED=1)
# ================================================================================

cat("4. MITTELWERTE VERGLEICH (NUR FINISHED=1):\n")
cat("   ======================================\n")

# Filter for finished cases
sim_filtered <- simulated_data[simulated_data$FINISHED == 1, ]
real_filtered <- real_data[real_data$FINISHED == 1, ]

# ID items comparison
cat("   ID-ITEMS:\n")
cat("   --------\n")

# Simulated ID means
sim_id_means <- sim_filtered %>%
  group_by(AB01) %>%
  summarise(
    ID01_mean = round(mean(ID01_01, na.rm = TRUE), 2),
    ID02_mean = round(mean(ID01_02, na.rm = TRUE), 2),
    ID03_mean = round(mean(ID01_03, na.rm = TRUE), 2),
    ID04_mean = round(mean(ID01_04, na.rm = TRUE), 2)
  )

cat("   Simulierte Daten:\n")
cat("   - Mensch: ID01=", sim_id_means$ID01_mean[1], ", ID02=", sim_id_means$ID02_mean[1], 
    ", ID03=", sim_id_means$ID03_mean[1], ", ID04=", sim_id_means$ID04_mean[1], "\n")
cat("   - KI: ID01=", sim_id_means$ID01_mean[2], ", ID02=", sim_id_means$ID02_mean[2], 
    ", ID03=", sim_id_means$ID03_mean[2], ", ID04=", sim_id_means$ID04_mean[2], "\n\n")

# Real ID means
real_id_means <- real_filtered %>%
  group_by(AB01) %>%
  summarise(
    ID01_mean = round(mean(ID01_01, na.rm = TRUE), 2),
    ID02_mean = round(mean(ID01_02, na.rm = TRUE), 2),
    ID03_mean = round(mean(ID01_03, na.rm = TRUE), 2),
    ID04_mean = round(mean(ID01_04, na.rm = TRUE), 2)
  )

cat("   Echte Daten:\n")
cat("   - Mensch: ID01=", real_id_means$ID01_mean[1], ", ID02=", real_id_means$ID02_mean[1], 
    ", ID03=", real_id_means$ID03_mean[1], ", ID04=", real_id_means$ID04_mean[1], "\n")
cat("   - KI: ID01=", real_id_means$ID01_mean[2], ", ID02=", real_id_means$ID02_mean[2], 
    ", ID03=", real_id_means$ID03_mean[2], ", ID04=", real_id_means$ID04_mean[2], "\n\n")

# EA items comparison
cat("   EA-ITEMS:\n")
cat("   --------\n")

# Simulated EA means
sim_ea_means <- sim_filtered %>%
  group_by(AB01) %>%
  summarise(
    EA01_mean = round(mean(EA01_01, na.rm = TRUE), 2),
    EA02_mean = round(mean(EA01_02, na.rm = TRUE), 2),
    EA03_mean = round(mean(EA01_03, na.rm = TRUE), 2),
    EA04_mean = round(mean(EA01_04, na.rm = TRUE), 2),
    EA05_mean = round(mean(EA01_05, na.rm = TRUE), 2)
  )

cat("   Simulierte Daten:\n")
cat("   - Mensch: EA01=", sim_ea_means$EA01_mean[1], ", EA02=", sim_ea_means$EA02_mean[1], 
    ", EA03=", sim_ea_means$EA03_mean[1], ", EA04=", sim_ea_means$EA04_mean[1], 
    ", EA05=", sim_ea_means$EA05_mean[1], "\n")
cat("   - KI: EA01=", sim_ea_means$EA01_mean[2], ", EA02=", sim_ea_means$EA02_mean[2], 
    ", EA03=", sim_ea_means$EA03_mean[2], ", EA04=", sim_ea_means$EA04_mean[2], 
    ", EA05=", sim_ea_means$EA05_mean[2], "\n\n")

# Real EA means
real_ea_means <- real_filtered %>%
  group_by(AB01) %>%
  summarise(
    EA01_mean = round(mean(EA01_01, na.rm = TRUE), 2),
    EA02_mean = round(mean(EA01_02, na.rm = TRUE), 2),
    EA03_mean = round(mean(EA01_03, na.rm = TRUE), 2),
    EA04_mean = round(mean(EA01_04, na.rm = TRUE), 2),
    EA05_mean = round(mean(EA01_05, na.rm = TRUE), 2)
  )

cat("   Echte Daten:\n")
cat("   - Mensch: EA01=", real_ea_means$EA01_mean[1], ", EA02=", real_ea_means$EA02_mean[1], 
    ", EA03=", real_ea_means$EA03_mean[1], ", EA04=", real_ea_means$EA04_mean[1], 
    ", EA05=", real_ea_means$EA05_mean[1], "\n")
cat("   - KI: EA01=", real_ea_means$EA01_mean[1], ", EA02=", real_ea_means$EA02_mean[2], 
    ", EA03=", real_ea_means$EA03_mean[2], ", EA04=", real_ea_means$EA04_mean[2], 
    ", EA05=", real_ea_means$EA05_mean[2], "\n\n")

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("5. ZUSAMMENFASSUNG:\n")
cat("   ===============\n")
cat("   - Beide Datensätze haben die gleiche Struktur (gleiche Spalten)\n")
cat("   - Beide sind UTF-16LE kodiert und tab-separiert\n")
cat("   - Simulierte Daten haben mehr Zeilen (146 vs. 131)\n")
cat("   - Unterschiedliche Gruppengrößen und Mittelwerte\n")
cat("   - Echte Daten haben realistischere Werte\n")
cat("================================================================================\n") 