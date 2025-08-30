#!/usr/bin/env Rscript

# ================================================================================
# DEBUG: TABELLE 10 KORRELATIONEN ÜBERPRÜFEN
# ================================================================================

cat("================================================================================\n")
cat("DEBUG: TABELLE 10 KORRELATIONEN ÜBERPRÜFEN\n")
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
if(all(se01_items %in% names(data))) {
  data$SE01 <- rowMeans(data[, se01_items], na.rm = TRUE)
  cat("✓ SE01 (Vorwissen) berechnet aus", length(se01_items), "Items\n")
} else {
  cat("❌ SE01-Items nicht gefunden\n")
}

# SE02 (Nutzungsintensität) - 1 Item
if("SE02_01" %in% names(data)) {
  data$SE02 <- data$SE02_01
  cat("✓ SE02 (Nutzungsintensität) übernommen\n")
} else {
  cat("❌ SE02_01 nicht gefunden\n")
}

# SE03 (Plattformnutzung) - 7 Items
se03_items <- c("SE03_01", "SE03_02", "SE03_03", "SE03_04", "SE03_05", "SE03_06", "SE03_07")
if(all(se03_items %in% names(data))) {
  data$SE03 <- rowMeans(data[, se03_items], na.rm = TRUE)
  cat("✓ SE03 (Plattformnutzung) berechnet aus", length(se03_items), "Items\n")
} else {
  cat("❌ SE03-Items nicht gefunden\n")
}

# SO01 (Alter) - 1 Item
if("SO01_01" %in% names(data)) {
  data$SO01 <- data$SO01_01
  cat("✓ SO01 (Alter) übernommen\n")
} else {
  cat("❌ SO01_01 nicht gefunden\n")
}

# SO02 (Geschlecht) - 1 Item
if("SO02" %in% names(data)) {
  data$SO02 <- data$SO02
  cat("✓ SO02 (Geschlecht) übernommen\n")
} else {
  cat("❌ SO02 nicht gefunden\n")
}

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
  cat("  Mittelwert:", round(mean(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  SD:", round(sd(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  Min:", round(min(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  Max:", round(max(data[[var]], na.rm = TRUE), 3), "\n")
  cat("  NA:", sum(is.na(data[[var]])), "\n")
}

# Korrelationsmatrix manuell berechnen
cat("\n=== KORRELATIONSMATRIX MANUELL BERECHNEN ===\n")
correlation_data <- data[, c(available_target_vars, available_socio_vars), drop = FALSE]

# Pearson-Korrelationen berechnen
cor_matrix <- cor(correlation_data, use = "pairwise.complete.obs", method = "pearson")
print(round(cor_matrix, 4))

# P-Werte für Signifikanz berechnen
cat("\n=== P-WERTE BERECHNEN ===\n")
p_matrix <- matrix(NA, nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
rownames(p_matrix) <- rownames(cor_matrix)
colnames(p_matrix) <- colnames(cor_matrix)

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

print(round(p_matrix, 4))

# Spezifische Korrelationen überprüfen
cat("\n=== SPEZIFISCHE KORRELATIONEN ÜBERPRÜFEN ===\n")
cat("MN ↔ SE01 (Vorwissen):\n")
if("MN" %in% available_target_vars && "SE01" %in% available_socio_vars) {
  cat("  Korrelation:", round(cor_matrix["MN", "SE01"], 4), "\n")
  cat("  P-Wert:", round(p_matrix["MN", "SE01"], 4), "\n")
  cat("  Signifikanz:", ifelse(p_matrix["MN", "SE01"] < 0.001, "***", 
                               ifelse(p_matrix["MN", "SE01"] < 0.01, "**",
                                      ifelse(p_matrix["MN", "SE01"] < 0.05, "*", "n.s.")))), "\n")
}

cat("\nVS ↔ SE02 (Nutzungsintensität):\n")
if("VS" %in% available_target_vars && "SE02" %in% available_socio_vars) {
  cat("  Korrelation:", round(cor_matrix["VS", "SE02"], 4), "\n")
  cat("  P-Wert:", round(p_matrix["VS", "SE02"], 4), "\n")
  cat("  Signifikanz:", ifelse(p_matrix["VS", "SE02"] < 0.001, "***", 
                               ifelse(p_matrix["VS", "SE02"] < 0.01, "**",
                                      ifelse(p_matrix["VS", "SE02"] < 0.05, "*", "n.s.")))), "\n")
}

cat("\nEA ↔ SE03 (Plattformnutzung):\n")
if("EA" %in% available_target_vars && "SE03" %in% available_socio_vars) {
  cat("  Korrelation:", round(cor_matrix["EA", "SE03"], 4), "\n")
  cat("  P-Wert:", round(p_matrix["EA", "SE03"], 4), "\n")
  cat("  Signifikanz:", ifelse(p_matrix["EA", "SE03"] < 0.001, "***", 
                               ifelse(p_matrix["EA", "SE03"] < 0.01, "**",
                                      ifelse(p_matrix["EA", "SE03"] < 0.05, "*", "n.s.")))), "\n")
}

cat("\nID ↔ SO01 (Alter):\n")
if("ID" %in% available_target_vars && "SO01" %in% available_socio_vars) {
  cat("  Korrelation:", round(cor_matrix["ID", "SO01"], 4), "\n")
  cat("  P-Wert:", round(p_matrix["ID", "SO01"], 4), "\n")
  cat("  Signifikanz:", ifelse(p_matrix["ID", "SO01"] < 0.001, "***", 
                               ifelse(p_matrix["ID", "SO01"] < 0.01, "**",
                                      ifelse(p_matrix["ID", "SO01"] < 0.05, "*", "n.s.")))), "\n")
}

cat("\nKI01 ↔ SO02 (Geschlecht):\n")
if("KI01" %in% available_target_vars && "SO02" %in% available_socio_vars) {
  cat("  Korrelation:", round(cor_matrix["KI01", "SO02"], 4), "\n")
  cat("  P-Wert:", round(p_matrix["KI01", "SO02"], 4), "\n")
  cat("  Signifikanz:", ifelse(p_matrix["KI01", "SO02"] < 0.001, "***", 
                               ifelse(p_matrix["KI01", "SO02"] < 0.01, "**",
                                      ifelse(p_matrix["KI01", "SO02"] < 0.05, "*", "n.s.")))), "\n")
}

# Datenstruktur überprüfen
cat("\n=== DATENSTRUKTUR ÜBERPRÜFEN ===\n")
cat("Datentypen:\n")
for(var in c(available_target_vars, available_socio_vars)) {
  cat("  ", var, ":", class(data[[var]]), "\n")
}

cat("\nErste 5 Werte jeder Variable:\n")
for(var in c(available_target_vars, available_socio_vars)) {
  cat("  ", var, ":", head(data[[var]], 5), "\n")
}

cat("\n================================================================================\n")
cat("DEBUG ABGESCHLOSSEN\n")
cat("================================================================================\n") 