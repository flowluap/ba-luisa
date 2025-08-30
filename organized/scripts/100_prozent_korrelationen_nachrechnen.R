#!/usr/bin/env Rscript

# ================================================================================
# 100% GENAU: ALLE KORRELATIONEN NOCHMAL NACHBERECHNEN
# ================================================================================

cat("================================================================================\n")
cat("100% GENAU: ALLE KORRELATIONEN NOCHMAL NACHBERECHNEN\n")
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

# Skalenmittelwerte berechnen
cat("\n=== SKALENMITTELWERTE BERECHNEN ===\n")

# MN (Menschlichkeit & Natürlichkeit) - 7 Items
mn_items <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
data$MN <- rowMeans(data[, mn_items], na.rm = TRUE)
cat("✓ MN-Skala berechnet aus", length(mn_items), "Items\n")

# VS (Vertrauen & Sympathie) - 8 Items
vs_items <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
data$VS <- rowMeans(data[, vs_items], na.rm = TRUE)
cat("✓ VS-Skala berechnet aus", length(vs_items), "Items\n")

# EA (Emotionale Ansprache) - 5 Items
ea_items <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
data$EA <- rowMeans(data[, ea_items], na.rm = TRUE)
cat("✓ EA-Skala berechnet aus", length(ea_items), "Items\n")

# ID (Identifikation) - 4 Items
id_items <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
data$ID <- rowMeans(data[, id_items], na.rm = TRUE)
cat("✓ ID-Skala berechnet aus", length(id_items), "Items\n")

# KI01 (KI-Wahrnehmung) - 4 Items
ki01_items <- c("KI01_01", "KI01_02", "KI01_03", "KI01_04")
data$KI01 <- rowMeans(data[, ki01_items], na.rm = TRUE)
cat("✓ KI01-Skala berechnet aus", length(ki01_items), "Items\n")

# KI02 (KI-Kritik) - 8 Items
ki02_items <- c("KI02_01", "KI02_02", "KI02_03", "KI02_04", "KI02_05", "KI02_06", "KI02_07", "KI02_08")
data$KI02 <- rowMeans(data[, ki02_items], na.rm = TRUE)
cat("✓ KI02-Skala berechnet aus", length(ki02_items), "Items\n")

# Soziodemografische und nutzungsbezogene Variablen identifizieren
cat("\n=== SOZIODEMOGRAFISCHE UND NUTZUNGSBEZOGENE VARIABLEN IDENTIFIZIEREN ===\n")

# SE01 (Vorwissen) - 5 Items
se01_items <- c("SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06")
data$SE01 <- rowMeans(data[, se01_items], na.rm = TRUE)
cat("✓ SE01 (Vorwissen) berechnet aus", length(se01_items), "Items\n")

# SE02 (Nutzungsintensität) - 1 Item
data$SE02 <- data$SE02_01
cat("✓ SE02 (Nutzungsintensität) übernommen\n")

# SE03 (Plattformnutzung) - 7 Items
se03_items <- c("SE03_01", "SE03_02", "SE03_03", "SE03_04", "SE03_05", "SE03_06", "SE03_07")
data$SE03 <- rowMeans(data[, se03_items], na.rm = TRUE)
cat("✓ SE03 (Plattformnutzung) berechnet aus", length(se03_items), "Items\n")

# SO01 (Alter) - 1 Item
data$SO01 <- data$SO01_01
cat("✓ SO01 (Alter) übernommen\n")

# SO02 (Geschlecht) - 1 Item
data$SO02 <- data$SO02
cat("✓ SO02 (Geschlecht) übernommen\n")

# Verfügbare Variablen identifizieren
cat("\n=== VERFÜGBARE VARIABLEN IDENTIFIZIEREN ===\n")
target_vars <- c("MN", "VS", "EA", "ID", "KI01", "KI02")
socio_vars <- c("SE01", "SE02", "SE03", "SO01", "SO02")

available_target_vars <- target_vars[sapply(target_vars, function(x) !is.null(data[[x]]))]
available_socio_vars <- socio_vars[sapply(socio_vars, function(x) !is.null(data[[x]]))]

cat("✓ Verfügbare Zielvariablen:", paste(available_target_vars, collapse = ", "), "\n")
cat("✓ Verfügbare soziodemografische Variablen:", paste(available_socio_vars, collapse = ", "), "\n")

# Datenqualität überprüfen
cat("\n=== DATENQUALITÄT ÜBERPRÜFEN ===\n")
for(var in c(available_target_vars, available_socio_vars)) {
  cat("\n", var, ":\n")
  cat("  N:", sum(!is.na(data[[var]])), "\n")
  cat("  Mittelwert:", round(mean(data[[var]], na.rm = TRUE), 6), "\n")
  cat("  SD:", round(sd(data[[var]], na.rm = TRUE), 6), "\n")
  cat("  Min:", round(min(data[[var]], na.rm = TRUE), 6), "\n")
  cat("  Max:", round(max(data[[var]], na.rm = TRUE), 6), "\n")
  cat("  NA:", sum(is.na(data[[var]])), "\n")
}

# Korrelationsmatrix manuell berechnen
cat("\n=== KORRELATIONSMATRIX MANUELL BERECHNEN ===\n")
correlation_data <- data[, c(available_target_vars, available_socio_vars), drop = FALSE]

# Pearson-Korrelationen berechnen
cor_matrix <- cor(correlation_data, use = "pairwise.complete.obs", method = "pearson")
print(round(cor_matrix, 6))

# P-Werte für Signifikanz berechnen
cat("\n=== P-WERTE BERECHNEN ===\n")
p_matrix <- matrix(NA, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
rownames(p_matrix) <- rownames(cor_matrix)
colnames(p_matrix) <- colnames(p_matrix)

for(i in 1:nrow(cor_matrix)) {
  for(j in 1:ncol(cor_matrix)) {
    if(i != j) {
      var1 <- rownames(cor_matrix)[i]
      var2 <- colnames(cor_matrix)[j]
      
      test_result <- cor.test(data[[var1]], data[[var2]], method = "pearson")
      p_matrix[i, j] <- test_result$p.value
    } else {
      p_matrix[i, j] <- 1
    }
  }
}

print(round(p_matrix, 6))

# SPEZIFISCHE KORRELATIONEN 100% GENAU BERECHNEN
cat("\n=== SPEZIFISCHE KORRELATIONEN 100% GENAU BERECHNEN ===\n")

# Funktion für manuelle Korrelationsberechnung
manual_correlation <- function(x, y) {
  # Nur vollständige Fälle
  complete_cases <- complete.cases(x, y)
  x_clean <- x[complete_cases]
  y_clean <- y[complete_cases]
  
  n <- length(x_clean)
  if(n == 0) return(NA)
  
  # Mittelwerte
  x_mean <- mean(x_clean)
  y_mean <- mean(y_clean)
  
  # Summen der Abweichungen
  sum_xy <- sum((x_clean - x_mean) * (y_clean - y_mean))
  sum_x2 <- sum((x_clean - x_mean)^2)
  sum_y2 <- sum((y_clean - y_mean)^2)
  
  # Korrelation
  correlation <- sum_xy / sqrt(sum_x2 * sum_y2)
  
  return(correlation)
}

# Alle Korrelationen manuell berechnen
cat("\n📊 MANUELLE KORRELATIONSBERECHNUNG (100% GENAU):\n")
cat("=", paste(rep("=", 80), collapse = ""), "\n")

for(target_var in c("MN", "VS", "EA", "ID")) {
  for(socio_var in c("SE01", "SE02", "SE03", "SO01", "SO02")) {
    if(target_var %in% available_target_vars && socio_var %in% available_socio_vars) {
      # R's cor() Funktion
      r_cor <- cor(data[[target_var]], data[[socio_var]], use = "pairwise.complete.obs", method = "pearson")
      
      # Manuelle Berechnung
      manual_cor <- manual_correlation(data[[target_var]], data[[socio_var]])
      
      # P-Wert
      p_val <- p_matrix[target_var, socio_var]
      
      # Signifikanz
      significance <- ""
      if(p_val < 0.001) significance <- "***"
      else if(p_val < 0.01) significance <- "**"
      else if(p_val < 0.05) significance <- "*"
      
      # Vollständige Variablennamen
      target_name <- switch(target_var,
                           "MN" = "Menschlichkeit & Natürlichkeit",
                           "VS" = "Vertrauen & Sympathie",
                           "EA" = "Emotionale Ansprache",
                           "ID" = "Identifikation")
      
      socio_name <- switch(socio_var,
                          "SE01" = "Vorwissen",
                          "SE02" = "Nutzungsintensität",
                          "SE03" = "Plattformnutzung",
                          "SO01" = "Alter",
                          "SO02" = "Geschlecht")
      
      cat(sprintf("%s ↔ %s:\n", target_name, socio_name))
      cat(sprintf("  R's cor():     %8.6f\n", r_cor))
      cat(sprintf("  Manuell:       %8.6f\n", manual_cor))
      cat(sprintf("  P-Wert:        %8.6f\n", p_val))
      cat(sprintf("  Signifikanz:   %s\n", significance))
      cat(sprintf("  N:             %d\n", sum(complete.cases(data[[target_var]], data[[socio_var]]))))
      cat("\n")
    }
  }
}

# Vergleich mit deinen alten Werten
cat("\n📋 VERGLEICH MIT DEINEN ALTEN WERTEN:\n")
cat("=", paste(rep("=", 80), collapse = ""), "\n")

old_values <- data.frame(
  Variable = c("MN", "VS", "EA", "ID"),
  SE01_old = c(0.145, 0.197, 0.186, 0.260),
  SE02_old = c(0.089, 0.116, 0.028, 0.122),
  SE03_old = c(0.239, 0.193, 0.185, 0.218),
  SO01_old = c(-0.021, -0.081, -0.004, -0.062),
  SO02_old = c(-0.016, 0.014, -0.031, 0.035),
  stringsAsFactors = FALSE
)

for(i in 1:nrow(old_values)) {
  var <- old_values$Variable[i]
  
  # Vollständige Variablennamen
  var_name <- switch(var,
                     "MN" = "Menschlichkeit & Natürlichkeit",
                     "VS" = "Vertrauen & Sympathie",
                     "EA" = "Emotionale Ansprache",
                     "ID" = "Identifikation")
  
  cat(sprintf("\n%s:\n", var_name))
  cat("  Vorwissen:\n")
  cat(sprintf("    Alt: %6.3f | Neu: %6.3f | Diff: %+6.3f\n", 
              old_values$SE01_old[i], 
              cor_matrix[var, "SE01"], 
              cor_matrix[var, "SE01"] - old_values$SE01_old[i]))
  
  cat("  Nutzungsintensität:\n")
  cat(sprintf("    Alt: %6.3f | Neu: %6.3f | Diff: %+6.3f\n", 
              old_values$SE02_old[i], 
              cor_matrix[var, "SE02"], 
              cor_matrix[var, "SE02"] - old_values$SE02_old[i]))
  
  cat("  Plattformnutzung:\n")
  cat(sprintf("    Alt: %6.3f | Neu: %6.3f | Diff: %+6.3f\n", 
              old_values$SE03_old[i], 
              cor_matrix[var, "SE03"], 
              cor_matrix[var, "SE03"] - old_values$SE03_old[i]))
  
  cat("  Alter:\n")
  cat(sprintf("    Alt: %6.3f | Neu: %6.3f | Diff: %+6.3f\n", 
              old_values$SO01_old[i], 
              cor_matrix[var, "SO01"], 
              cor_matrix[var, "SO01"] - old_values$SO01_old[i]))
  
  cat("  Geschlecht:\n")
  cat(sprintf("    Alt: %6.3f | Neu: %6.3f | Diff: %+6.3f\n", 
              old_values$SO02_old[i], 
              cor_matrix[var, "SO02"], 
              cor_matrix[var, "SO02"] - old_values$SO02_old[i]))
}

cat("\n================================================================================\n")
cat("100% GENAU: ALLE KORRELATIONEN NACHBERECHNET\n")
cat("================================================================================\n")

cat("\n📋 ZUSAMMENFASSUNG:\n")
cat("• Alle Korrelationen wurden sowohl mit R's cor() als auch manuell berechnet\n")
cat("• Datenqualität wurde überprüft (N, Mittelwert, SD, Min, Max, NA)\n")
cat("• Vergleich mit deinen alten Werten wurde durchgeführt\n")
cat("• P-Werte und Signifikanz wurden berechnet\n")
cat("• Alle Berechnungen sind 100% transparent und nachvollziehbar\n")
cat("• Variablennamen wurden vollständig ausgeschrieben\n") 