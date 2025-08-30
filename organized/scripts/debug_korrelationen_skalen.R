#!/usr/bin/env Rscript

# ================================================================================
# DEBUG: KORRELATIONEN ÜBERPRÜFEN (MIT SKALENMITTELWERTEN)
# ================================================================================

cat("================================================================================\n")
cat("DEBUG: KORRELATIONEN ÜBERPRÜFEN (MIT SKALENMITTELWERTEN)\n")
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

# Skalenmittelwerte berechnen
cat("\n=== SKALENMITTELWERTE BERECHNEN ===\n")

# MN (Menschlichkeit & Natürlichkeit) - 7 Items
mn_items <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
if(all(mn_items %in% names(data))) {
  data$MN <- rowMeans(data[, mn_items], na.rm = TRUE)
  cat("✓ MN-Skala berechnet aus", length(mn_items), "Items\n")
} else {
  cat("❌ MN-Items nicht gefunden\n")
}

# VS (Vertrauen & Sympathie) - 8 Items
vs_items <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
if(all(vs_items %in% names(data))) {
  data$VS <- rowMeans(data[, vs_items], na.rm = TRUE)
  cat("✓ VS-Skala berechnet aus", length(vs_items), "Items\n")
} else {
  cat("❌ VS-Items nicht gefunden\n")
}

# EA (Emotionale Ansprache) - 5 Items
ea_items <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
if(all(ea_items %in% names(data))) {
  data$EA <- rowMeans(data[, ea_items], na.rm = TRUE)
  cat("✓ EA-Skala berechnet aus", length(ea_items), "Items\n")
} else {
  cat("❌ EA-Items nicht gefunden\n")
}

# ID (Identifikation) - 4 Items
id_items <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
if(all(id_items %in% names(data))) {
  data$ID <- rowMeans(data[, id_items], na.rm = TRUE)
  cat("✓ ID-Skala berechnet aus", length(id_items), "Items\n")
} else {
  cat("❌ ID-Items nicht gefunden\n")
}

# KI01 (KI-Wahrnehmung) - 4 Items
ki01_items <- c("KI01_01", "KI01_02", "KI01_03", "KI01_04")
if(all(ki01_items %in% names(data))) {
  data$KI01 <- rowMeans(data[, ki01_items], na.rm = TRUE)
  cat("✓ KI01-Skala berechnet aus", length(ki01_items), "Items\n")
} else {
  cat("❌ KI01-Items nicht gefunden\n")
}

# KI02 (KI-Kritik) - 8 Items
ki02_items <- c("KI02_01", "KI02_02", "KI02_03", "KI02_04", "KI02_05", "KI02_06", "KI02_07", "KI02_08")
if(all(ki02_items %in% names(data))) {
  data$KI02 <- rowMeans(data[, ki02_items], na.rm = TRUE)
  cat("✓ KI02-Skala berechnet aus", length(ki02_items), "Items\n")
} else {
  cat("❌ KI02-Items nicht gefunden\n")
}

# Verfügbare Skalen identifizieren
cat("\n=== VERFÜGBARE SKALEN IDENTIFIZIEREN ===\n")
scale_vars <- c("MN", "VS", "EA", "ID", "KI01", "KI02")
available_scales <- scale_vars[sapply(scale_vars, function(x) !is.null(data[[x]]))]

if(length(available_scales) > 0) {
  cat("✓ Verfügbare Skalen:", paste(available_scales, collapse = ", "), "\n")
} else {
  cat("❌ Keine Skalen verfügbar!\n")
  stop("Keine Skalen berechnet")
}

# Datenqualität der Skalen überprüfen
cat("\n=== DATENQUALITÄT DER SKALEN ÜBERPRÜFEN ===\n")
for(var in available_scales) {
  cat("\n", var, ":\n")
  cat("  N:", sum(!is.na(data[[var]])), "\n")
  cat("  Mittelwert:", round(mean(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  SD:", round(sd(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  Min:", round(min(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  Max:", round(max(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  NA:", sum(is.na(data[[var]])), "\n")
}

# Korrelationsmatrix der Skalen berechnen
cat("\n=== KORRELATIONSMATRIX DER SKALEN BERECHNEN ===\n")
correlation_data <- data[, available_scales, drop = FALSE]

# Pearson-Korrelationen berechnen
cor_matrix <- cor(correlation_data, use = "pairwise.complete.obs", method = "pearson")
print(round(cor_matrix, 4))

# P-Werte für Signifikanz berechnen
cat("\n=== P-WERTE BERECHNEN ===\n")
p_matrix <- matrix(NA, nrow = length(available_scales), ncol = length(available_scales))
rownames(p_matrix) <- available_scales
colnames(p_matrix) <- available_scales

for(i in 1:length(available_scales)) {
  for(j in 1:length(available_scales)) {
    if(i == j) {
      p_matrix[i, j] <- 1
    } else {
      var1 <- available_scales[i]
      var2 <- available_scales[j]
      
      test_result <- cor.test(data[[var1]], data[[var2]], method = "pearson")
      p_matrix[i, j] <- test_result$p.value
    }
  }
}

print(round(p_matrix, 4))

# Spezifische Korrelationen überprüfen
cat("\n=== SPEZIFISCHE KORRELATIONEN ÜBERPRÜFEN ===\n")
if("MN" %in% available_scales && "VS" %in% available_scales) {
  cat("MN ↔ VS:\n")
  cat("  Korrelation:", round(cor_matrix["MN", "VS"], 4), "\n")
  cat("  P-Wert:", round(p_matrix["MN", "VS"], 4), "\n")
  cat("  Signifikanz:", ifelse(p_matrix["MN", "VS"] < 0.001, "***", 
                               ifelse(p_matrix["MN", "VS"] < 0.01, "**",
                                      ifelse(p_matrix["MN", "VS"] < 0.05, "*", "n.s.")))), "\n")
}

if("MN" %in% available_scales && "EA" %in% available_scales) {
  cat("\nMN ↔ EA:\n")
  cat("  Korrelation:", round(cor_matrix["MN", "EA"], 4), "\n")
  cat("  P-Wert:", round(p_matrix["MN", "EA"], 4), "\n")
  cat("  Signifikanz:", ifelse(p_matrix["MN", "EA"] < 0.001, "***", 
                               ifelse(p_matrix["MN", "EA"] < 0.01, "**",
                                      ifelse(p_matrix["MN", "EA"] < 0.05, "*", "n.s.")))), "\n")
}

if("VS" %in% available_scales && "EA" %in% available_scales) {
  cat("\nVS ↔ EA:\n")
  cat("  Korrelation:", round(cor_matrix["VS", "EA"], 4), "\n")
  cat("  P-Wert:", round(p_matrix["VS", "EA"], 4), "\n")
  cat("  Signifikanz:", ifelse(p_matrix["VS", "EA"] < 0.001, "***", 
                               ifelse(p_matrix["VS", "EA"] < 0.01, "**",
                                      ifelse(p_matrix["VS", "EA"] < 0.05, "*", "n.s.")))), "\n")
}

# Vergleich mit den Werten aus Tabelle 9
cat("\n=== VERGLEICH MIT TABELLE 9 ===\n")
cat("Tabelle 9 zeigte:\n")
cat("  MN ↔ VS: 0.371***\n")
cat("  MN ↔ EA: 0.468***\n")
cat("  VS ↔ EA: 0.522***\n")

cat("\nTatsächliche Werte:\n")
if("MN" %in% available_scales && "VS" %in% available_scales) {
  cat("  MN ↔ VS:", round(cor_matrix["MN", "VS"], 4), "\n")
}
if("MN" %in% available_scales && "EA" %in% available_scales) {
  cat("  MN ↔ EA:", round(cor_matrix["MN", "EA"], 4), "\n")
}
if("VS" %in% available_scales && "EA" %in% available_scales) {
  cat("  VS ↔ EA:", round(cor_matrix["VS", "EA"], 4), "\n")
}

cat("\n================================================================================\n")
cat("DEBUG MIT SKALENMITTELWERTEN ABGESCHLOSSEN\n")
cat("================================================================================\n") 