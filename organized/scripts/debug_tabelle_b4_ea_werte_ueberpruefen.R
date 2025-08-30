#!/usr/bin/env Rscript

# ================================================================================
# DEBUG: TABELLE B4 EA-WERTE 100% GENAU ÜBERPRÜFEN
# ================================================================================

cat("================================================================================\n")
cat("DEBUG: TABELLE B4 EA-WERTE 100% GENAU ÜBERPRÜFEN\n")
cat("================================================================================\n")

# Pakete laden
library(dplyr)

# Datensatz laden
cat("\n=== DATENSATZ LADEN ===\n")
tryCatch({
  data <- read.delim("/Users/luisa.claussen/Desktop/Digitaler Anhang /Bereinigte Daten.csv",
                     fileEncoding = "UTF-16",
                     sep = "\t",
                     stringsAsFactors = FALSE)
  cat("✓ Daten geladen. Gesamtstichprobe: n =", nrow(data), "\n")
}, error = function(e) {
  cat("❌ Fehler beim Laden der Daten:", e$message, "\n")
  stop("Kann nicht fortfahren ohne echte Daten")
})

# AB01 (Gruppe) identifizieren
cat("\n=== GRUPPEN IDENTIFIZIEREN ===\n")
if("AB01" %in% names(data)) {
  cat("✓ AB01 (Gruppe) gefunden\n")
  cat("  Eindeutige Werte in AB01:", paste(sort(unique(data$AB01)), collapse = ", "), "\n")
  cat("  Häufigkeiten:\n")
  print(table(data$AB01, useNA = "ifany"))
} else {
  cat("❌ AB01 nicht gefunden\n")
  stop("AB01 Spalte nicht gefunden")
}

# EA Items identifizieren
cat("\n=== EA ITEMS IDENTIFIZIEREN ===\n")
ea_items <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")

# Verfügbare Items prüfen
available_ea_items <- ea_items[ea_items %in% names(data)]
cat("✓ Verfügbare EA Items:", paste(available_ea_items, collapse = ", "), "\n")

if(length(available_ea_items) == 0) {
  cat("❌ Keine EA Items gefunden\n")
  stop("Keine EA Items gefunden")
}

# Gruppen aufteilen
ki_data <- data[data$AB01 == 1, ]
mensch_data <- data[data$AB01 == 2, ]

cat("\nKI-Avatar Gruppe (AB01 = 1): n =", nrow(ki_data), "\n")
cat("Mensch Gruppe (AB01 = 2): n =", nrow(mensch_data), "\n")

# Detaillierte Überprüfung jedes Items
cat("\n=== DETAILLIERTE ÜBERPRÜFUNG JEDES ITEMS ===\n")

for(i in 1:length(available_ea_items)) {
  item <- available_ea_items[i]
  cat("\n---", item, "---\n")
  
  # KI-Avatar Gruppe
  if(item %in% names(ki_data)) {
    ki_values <- as.numeric(as.character(ki_data[[item]]))
    ki_values_clean <- ki_values[!is.na(ki_values)]
    
    cat("KI-Avatar (AB01 = 1):\n")
    cat("  Rohwerte (erste 10):", paste(head(ki_values, 10), collapse = ", "), "\n")
    cat("  Anzahl gültige Werte:", length(ki_values_clean), "\n")
    cat("  Anzahl fehlende Werte:", sum(is.na(ki_values)), "\n")
    cat("  Wertebereich:", min(ki_values_clean), "bis", max(ki_values_clean), "\n")
    cat("  Einzelwerte:", paste(sort(unique(ki_values_clean)), collapse = ", "), "\n")
    
    if(length(ki_values_clean) > 0) {
      ki_mean <- mean(ki_values_clean, na.rm = TRUE)
      ki_sd <- sd(ki_values_clean, na.rm = TRUE)
      cat("  Mittelwert:", ki_mean, "\n")
      cat("  Standardabweichung:", ki_sd, "\n")
      
      # Manuelle Berechnung zur Überprüfung
      ki_sum <- sum(ki_values_clean)
      ki_n <- length(ki_values_clean)
      ki_mean_manual <- ki_sum / ki_n
      cat("  Manuelle Berechnung: Summe =", ki_sum, "/ n =", ki_n, "=", ki_mean_manual, "\n")
      cat("  Stimmt überein:", abs(ki_mean - ki_mean_manual) < 0.0001, "\n")
    }
  } else {
    cat("❌ Item nicht in KI-Daten gefunden\n")
  }
  
  # Mensch Gruppe
  if(item %in% names(mensch_data)) {
    mensch_values <- as.numeric(as.character(mensch_data[[item]]))
    mensch_values_clean <- mensch_values[!is.na(mensch_values)]
    
    cat("Mensch (AB01 = 2):\n")
    cat("  Rohwerte (erste 10):", paste(head(mensch_values, 10), collapse = ", "), "\n")
    cat("  Anzahl gültige Werte:", length(mensch_values_clean), "\n")
    cat("  Anzahl fehlende Werte:", sum(is.na(mensch_values)), "\n")
    cat("  Wertebereich:", min(mensch_values_clean), "bis", max(mensch_values_clean), "\n")
    cat("  Einzelwerte:", paste(sort(unique(mensch_values_clean)), collapse = ", "), "\n")
    
    if(length(mensch_values_clean) > 0) {
      mensch_mean <- mean(mensch_values_clean, na.rm = TRUE)
      mensch_sd <- sd(mensch_values_clean, na.rm = TRUE)
      cat("  Mittelwert:", mensch_mean, "\n")
      cat("  Standardabweichung:", mensch_sd, "\n")
      
      # Manuelle Berechnung zur Überprüfung
      mensch_sum <- sum(mensch_values_clean)
      mensch_n <- length(mensch_values_clean)
      mensch_mean_manual <- mensch_sum / mensch_n
      cat("  Manuelle Berechnung: Summe =", mensch_sum, "/ n =", mensch_n, "=", mensch_mean_manual, "\n")
      cat("  Stimmt überein:", abs(mensch_mean - mensch_mean_manual) < 0.0001, "\n")
    }
  } else {
    cat("❌ Item nicht in Mensch-Daten gefunden\n")
  }
}

# Zusammenfassung der finalen Werte
cat("\n=== ZUSAMMENFASSUNG DER FINALEN WERTE ===\n")

cat("\nKI-Avatar Gruppe (AB01 = 1):\n")
for(i in 1:length(available_ea_items)) {
  item <- available_ea_items[i]
  if(item %in% names(ki_data)) {
    ki_values <- as.numeric(as.character(ki_data[[item]]))
    ki_values_clean <- ki_values[!is.na(ki_values)]
    if(length(ki_values_clean) > 0) {
      ki_mean <- mean(ki_values_clean, na.rm = TRUE)
      cat("  ", item, ": M =", ki_mean, ", n =", length(ki_values_clean), "\n")
    }
  }
}

cat("\nMensch Gruppe (AB01 = 2):\n")
for(i in 1:length(available_ea_items)) {
  item <- available_ea_items[i]
  if(item %in% names(mensch_data)) {
    mensch_values <- as.numeric(as.character(mensch_data[[item]]))
    mensch_values_clean <- mensch_values[!is.na(mensch_values)]
    if(length(mensch_values_clean) > 0) {
      mensch_mean <- mean(mensch_values_clean, na.rm = TRUE)
      cat("  ", item, ": M =", mensch_mean, ", n =", length(mensch_values_clean), "\n")
    }
  }
}

# Überprüfung der Tabelle B4 Werte
cat("\n=== ÜBERPRÜFUNG DER TABELLE B4 WERTE ===\n")

# Erwartete Werte aus Tabelle B4
expected_ki <- c(2.641, 2.516, 2.609, 2.641, 2.359)
expected_mensch <- c(3.701, 3.672, 3.627, 3.582, 3.701)

cat("\nErwartete Werte aus Tabelle B4:\n")
cat("KI-Avatar:", paste(expected_ki, collapse = ", "), "\n")
cat("Mensch:", paste(expected_mensch, collapse = ", "), "\n")

# Aktuelle Berechnung
cat("\nAktuelle Berechnung:\n")
for(i in 1:length(available_ea_items)) {
  item <- available_ea_items[i]
  
  # KI-Werte
  ki_values <- as.numeric(as.character(ki_data[[item]]))
  ki_values_clean <- ki_values[!is.na(ki_values)]
  ki_mean_current <- if(length(ki_values_clean) > 0) mean(ki_values_clean, na.rm = TRUE) else NA
  
  # Mensch-Werte
  mensch_values <- as.numeric(as.character(mensch_data[[item]]))
  mensch_values_clean <- mensch_values[!is.na(mensch_values)]
  mensch_mean_current <- if(length(mensch_values_clean) > 0) mean(mensch_values_clean, na.rm = TRUE) else NA
  
  cat("  ", item, ":\n")
  cat("    KI: Erwartet =", expected_ki[i], ", Berechnet =", ki_mean_current, "\n")
  cat("    Mensch: Erwartet =", expected_mensch[i], ", Berechnet =", mensch_mean_current, "\n")
  
  # Überprüfung der Übereinstimmung
  ki_match <- if(!is.na(ki_mean_current)) abs(ki_mean_current - expected_ki[i]) < 0.001 else FALSE
  mensch_match <- if(!is.na(mensch_mean_current)) abs(mensch_mean_current - expected_mensch[i]) < 0.001 else FALSE
  
  cat("    KI stimmt überein:", ki_match, "\n")
  cat("    Mensch stimmt überein:", mensch_match, "\n")
}

cat("\n================================================================================\n")
cat("DEBUG: TABELLE B4 EA-WERTE ÜBERPRÜFUNG ABGESCHLOSSEN\n")
cat("================================================================================\n") 