#!/usr/bin/env Rscript

# ================================================================================
# DEBUG: KORRELATIONEN ÜBERPRÜFEN
# ================================================================================

cat("================================================================================\n")
cat("DEBUG: KORRELATIONEN ÜBERPRÜFEN\n")
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
  cat("Spalten:", paste(names(data), collapse = ", "), "\n")
}, error = function(e) {
  cat("❌ Fehler beim Laden der Daten:", e$message, "\n")
  stop("Kann nicht fortfahren ohne echte Daten")
})

# Zentralen Konstrukte identifizieren
cat("\n=== ZENTRALE KONSTRUKTE IDENTIFIZIEREN ===\n")
central_vars <- c("MN", "VS", "EA", "ID", "KI01", "KI02")
available_vars <- central_vars[central_vars %in% names(data)]

if(length(available_vars) > 0) {
  cat("✓ Verfügbare zentrale Konstrukte:", paste(available_vars, collapse = ", "), "\n")
} else {
  cat("❌ Keine zentralen Konstrukte gefunden!\n")
  cat("Verfügbare Spalten:\n")
  print(names(data))
  stop("Keine zentralen Konstrukte verfügbar")
}

# Datenqualität überprüfen
cat("\n=== DATENQUALITÄT ÜBERPRÜFEN ===\n")
for(var in available_vars) {
  cat("\n", var, ":\n")
  cat("  N:", sum(!is.na(data[[var]])), "\n")
  cat("  Mittelwert:", round(mean(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  SD:", round(sd(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  Min:", round(min(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  Max:", round(max(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  NA:", sum(is.na(data[[var]])), "\n")
}

# Korrelationsmatrix manuell berechnen
cat("\n=== KORRELATIONSMATRIX MANUELL BERECHNEN ===\n")
cor_matrix <- matrix(NA, nrow = length(available_vars), ncol = length(available_vars))
rownames(cor_matrix) <- available_vars
colnames(cor_matrix) <- available_vars

p_matrix <- matrix(NA, nrow = length(available_vars), ncol = length(available_vars))
rownames(p_matrix) <- available_vars
colnames(p_matrix) <- available_vars

for(i in 1:length(available_vars)) {
  for(j in 1:length(available_vars)) {
    if(i == j) {
      cor_matrix[i, j] <- 1
      p_matrix[i, j] <- 1
    } else {
      var1 <- available_vars[i]
      var2 <- available_vars[j]
      
      # Nur vollständige Fälle verwenden
      complete_cases <- complete.cases(data[[var1]], data[[var2]])
      n_complete <- sum(complete_cases)
      
      if(n_complete > 3) {
        x <- data[[var1]][complete_cases]
        y <- data[[var2]][complete_cases]
        
        # Pearson-Korrelation manuell berechnen
        n <- length(x)
        mean_x <- mean(x)
        mean_y <- mean(y)
        
        numerator <- sum((x - mean_x) * (y - mean_y))
        denominator_x <- sqrt(sum((x - mean_x)^2))
        denominator_y <- sqrt(sum((y - mean_y)^2))
        
        if(denominator_x > 0 && denominator_y > 0) {
          cor_val <- numerator / (denominator_x * denominator_y)
          cor_matrix[i, j] <- cor_val
          
          # T-Test für Signifikanz
          if(n > 2) {
            t_stat <- cor_val * sqrt((n-2) / (1 - cor_val^2))
            p_val <- 2 * pt(-abs(t_stat), df = n-2)
            p_matrix[i, j] <- p_val
          }
        }
      }
    }
  }
}

cat("\n=== KORRELATIONSMATRIX (ROH) ===\n")
print(round(cor_matrix, 4))

cat("\n=== P-WERTE ===\n")
print(round(p_matrix, 4))

# Mit R's cor() Funktion vergleichen
cat("\n=== VERGLEICH MIT R's cor() FUNKTION ===\n")
correlation_data <- data[, available_vars, drop = FALSE]
r_cor_matrix <- cor(correlation_data, use = "pairwise.complete.obs", method = "pearson")
print(round(r_cor_matrix, 4))

# Unterschiede identifizieren
cat("\n=== UNTERSCHIEDE IDENTIFIZIEREN ===\n")
differences <- abs(cor_matrix - r_cor_matrix)
max_diff <- max(differences, na.rm = TRUE)
cat("Maximaler Unterschied zwischen manueller und R-Berechnung:", max_diff, "\n")

if(max_diff > 0.001) {
  cat("⚠️  WARNUNG: Große Unterschiede gefunden!\n")
  print(round(differences, 6))
}

# Spezifische Korrelationen überprüfen
cat("\n=== SPEZIFISCHE KORRELATIONEN ÜBERPRÜFEN ===\n")
cat("MN ↔ VS:\n")
cat("  Manuell:", round(cor_matrix["MN", "VS"], 4), "\n")
cat("  R-Funktion:", round(r_cor_matrix["MN", "VS"], 4), "\n")
cat("  P-Wert:", round(p_matrix["MN", "VS"], 4), "\n")

cat("\nMN ↔ EA:\n")
cat("  Manuell:", round(cor_matrix["MN", "EA"], 4), "\n")
cat("  R-Funktion:", round(r_cor_matrix["MN", "EA"], 4), "\n")
cat("  P-Wert:", round(p_matrix["MN", "EA"], 4), "\n")

cat("\nVS ↔ EA:\n")
cat("  Manuell:", round(cor_matrix["VS", "EA"], 4), "\n")
cat("  R-Funktion:", round(r_cor_matrix["VS", "EA"], 4), "\n")
cat("  P-Wert:", round(p_matrix["VS", "EA"], 4), "\n")

# Datenstruktur überprüfen
cat("\n=== DATENSTRUKTUR ÜBERPRÜFEN ===\n")
cat("Datentypen:\n")
for(var in available_vars) {
  cat("  ", var, ":", class(data[[var]]), "\n")
}

cat("\nErste 5 Werte jeder Variable:\n")
for(var in available_vars) {
  cat("  ", var, ":", head(data[[var]], 5), "\n")
}

cat("\n================================================================================\n")
cat("DEBUG ABGESCHLOSSEN\n")
cat("================================================================================\n") 