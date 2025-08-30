# =============================================================================
# DEBUG: DATENSATZ ÜBERPRÜFEN
# =============================================================================
# Überprüft den Datensatz und die Gruppierung nach AB01

library(dplyr)

cat("================================================================================\n")
cat("DEBUG: DATENSATZ ÜBERPRÜFUNG\n")
cat("================================================================================\n")

# =============================================================================
# DATENSATZ LADEN UND ÜBERPRÜFEN
# =============================================================================

# Daten einlesen (Datei aus "Digitaler Anhang" auf dem Desktop)
data_file <- "/Users/luisa.claussen/Desktop/Digitaler Anhang /Bereinigte Daten.csv"
cat("Verwende Datensatz:", data_file, "\n")

# Versuche verschiedene Lade-Methoden
cat("Versuche verschiedene Lade-Methoden...\n")

# Methode 1: UTF-16 mit Tabulator (TSV)
tryCatch({
  data <- read.delim(data_file, header = TRUE, fileEncoding = "UTF-16")
  cat("✓ Methode 1 erfolgreich: UTF-16 mit Tabulator (TSV)\n")
}, error = function(e) {
  cat("❌ Methode 1 fehlgeschlagen:", e$message, "\n")
  
  # Methode 2: UTF-16 mit explizitem Tabulator
  tryCatch({
    data <- read.csv(data_file, sep = "\t", header = TRUE, fileEncoding = "UTF-16")
    cat("✓ Methode 2 erfolgreich: UTF-16 mit explizitem Tabulator\n")
  }, error = function(e) {
    cat("❌ Methode 2 fehlgeschlagen:", e$message, "\n")
    
    # Methode 3: UTF-8-BOM mit Tabulator
    tryCatch({
      data <- read.delim(data_file, header = TRUE, fileEncoding = "UTF-8-BOM")
      cat("✓ Methode 3 erfolgreich: UTF-8-BOM mit Tabulator\n")
    }, error = function(e) {
      cat("❌ Methode 3 fehlgeschlagen:", e$message, "\n")
      
      # Methode 4: Standard UTF-8 mit Tabulator
      tryCatch({
        data <- read.delim(data_file, header = TRUE, encoding = "UTF-8")
        cat("✓ Methode 4 erfolgreich: Standard UTF-8 mit Tabulator\n")
      }, error = function(e) {
        cat("❌ Methode 4 fehlgeschlagen:", e$message, "\n")
        data <- NULL
      })
    })
  })
})

if (is.null(data)) {
  cat("❌ Alle Lade-Methoden fehlgeschlagen\n")
  quit()
}

# =============================================================================
# DATENSATZ STRUKTUR ANALYSIEREN
# =============================================================================

cat("\n=== DATENSATZ STRUKTUR ===\n")
cat("Anzahl Zeilen:", nrow(data), "\n")
cat("Anzahl Spalten:", ncol(data), "\n")

cat("\nVerfügbare Spalten:\n")
print(colnames(data))

cat("\nErste 5 Zeilen:\n")
print(head(data, 5))

# =============================================================================
# AB01 SPALTE ÜBERPRÜFEN
# =============================================================================

cat("\n=== AB01 SPALTE ÜBERPRÜFUNG ===\n")

# Suche nach AB01 Spalte
ab01_col <- NULL
if ("AB01" %in% colnames(data)) {
  ab01_col <- "AB01"
  cat("✓ AB01 Spalte gefunden\n")
} else {
  # Suche nach ähnlichen Spaltennamen
  ab01_matches <- grep("AB01|ab01|Ab01", colnames(data), value = TRUE)
  if (length(ab01_matches) > 0) {
    ab01_col <- ab01_matches[1]
    cat("✓ Ähnliche Spalte gefunden:", ab01_col, "\n")
  } else {
    cat("❌ Keine AB01 Spalte gefunden\n")
    cat("Verfügbare Spalten mit 'AB':\n")
    ab_spalten <- grep("AB|ab|Ab", colnames(data), value = TRUE)
    if (length(ab_spalten) > 0) {
      print(ab_spalten)
    } else {
      cat("Keine Spalten mit 'AB' gefunden\n")
    }
  }
}

if (!is.null(ab01_col)) {
  cat("\nAB01 Spalte Inhalt:\n")
  print(table(data[[ab01_col]], useNA = "ifany"))
  
  cat("\nAB01 Spalte Details:\n")
  print(summary(data[[ab01_col]]))
  
  cat("\nAB01 Spalte einzigartige Werte:\n")
  print(unique(data[[ab01_col]]))
  
  # Überprüfe Gruppierung
  ki_avatar_count <- sum(data[[ab01_col]] == 1, na.rm = TRUE)
  mensch_count <- sum(data[[ab01_col]] == 2, na.rm = TRUE)
  na_count <- sum(is.na(data[[ab01_col]]))
  
  cat("\nGruppierung nach AB01:\n")
  cat("KI-Avatar (AB01 = 1):", ki_avatar_count, "\n")
  cat("Mensch (AB01 = 2):", mensch_count, "\n")
  cat("Fehlende Werte:", na_count, "\n")
}

# =============================================================================
# ZIELVARIABLEN ÜBERPRÜFEN
# =============================================================================

cat("\n=== ZIELVARIABLEN ÜBERPRÜFUNG ===\n")

target_vars <- c("MN", "VS", "EA", "ID", "KI01", "KI02")
available_vars <- target_vars[target_vars %in% colnames(data)]

cat("Verfügbare Zielvariablen:", paste(available_vars, collapse = ", "), "\n")

if (length(available_vars) > 0) {
  cat("\nZusammenfassung der verfügbaren Zielvariablen:\n")
  for (var in available_vars) {
    cat("\n", var, ":\n")
    print(summary(data[[var]]))
  }
}

# =============================================================================
# GRUPPIERTE ANALYSE TESTEN
# =============================================================================

if (!is.null(ab01_col) && length(available_vars) > 0) {
  cat("\n=== GRUPPIERTE ANALYSE TEST ===\n")
  
  # Gruppiere Daten
  ki_avatar_data <- data[data[[ab01_col]] == 1, ]
  mensch_data <- data[data[[ab01_col]] == 2, ]
  
  cat("KI-Avatar Gruppe (n =", nrow(ki_avatar_data), "):\n")
  if (nrow(ki_avatar_data) > 0) {
    for (var in available_vars) {
      values <- ki_avatar_data[[var]]
      values_clean <- values[!is.na(values)]
      if (length(values_clean) > 0) {
        mw <- mean(values_clean)
        sd_val <- sd(values_clean)
        cat("  ", var, ": M =", round(mw, 3), "SD =", round(sd_val, 3), "\n")
      }
    }
  }
  
  cat("\nMensch Gruppe (n =", nrow(mensch_data), "):\n")
  if (nrow(mensch_data) > 0) {
    for (var in available_vars) {
      values <- mensch_data[[var]]
      values_clean <- values[!is.na(values)]
      if (length(values_clean) > 0) {
        mw <- mean(values_clean)
        sd_val <- sd(values_clean)
        cat("  ", var, ": M =", round(mw, 3), "SD =", round(sd_val, 3), "\n")
      }
    }
  }
}

# =============================================================================
# ALTERNATIVE GRUPPIERUNGSVERSUCHE
# =============================================================================

cat("\n=== ALTERNATIVE GRUPPIERUNGSVERSUCHE ===\n")

# Suche nach anderen möglichen Gruppierungsspalten
group_candidates <- c("Gruppe", "gruppe", "Group", "group", "Bedingung", "bedingung", "Condition", "condition")
for (candidate in group_candidates) {
  if (candidate %in% colnames(data)) {
    cat("Mögliche Gruppierungsspalte gefunden:", candidate, "\n")
    cat("Inhalt:\n")
    print(table(data[[candidate]], useNA = "ifany"))
  }
}

cat("\n================================================================================\n")
cat("DEBUG ABGESCHLOSSEN\n")
cat("================================================================================\n") 