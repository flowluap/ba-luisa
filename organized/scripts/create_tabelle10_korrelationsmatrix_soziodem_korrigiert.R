#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 10: PEARSON-KORRELATIONSMATRIX MIT SOZIODEMOGRAFISCHEN VARIABLEN (KORRIGIERT)
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 10: PEARSON-KORRELATIONSMATRIX MIT SOZIODEMOGRAFISCHEN VARIABLEN (KORRIGIERT)\n")
cat("================================================================================\n")

# Pakete laden
library(dplyr)
library(gt)

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

# Korrelationsmatrix berechnen
cat("\n=== KORRELATIONSMATRIX BERECHNEN ===\n")
correlation_data <- data[, c(available_target_vars, available_socio_vars), drop = FALSE]

# Pearson-Korrelationen berechnen
cor_matrix <- cor(correlation_data, use = "pairwise.complete.obs", method = "pearson")

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

cat("✓ Korrelationsmatrix berechnet\n")
cat("✓ P-Werte für Signifikanz berechnet\n")

# Korrelationsmatrix mit Signifikanz-Markierung erstellen
cat("\n=== KORRELATIONSMATRIX MIT SIGNIFIKANZ-MARKIERUNG ===\n")

# Daten für GT-Tabelle vorbereiten
cor_table_data <- data.frame(
  Variable = available_target_vars,
  stringsAsFactors = FALSE
)

# Korrelationswerte und Signifikanz-Markierung hinzufügen
for(var in available_target_vars) {
  var_index <- which(rownames(cor_matrix) == var)
  cor_values <- cor_matrix[var_index, available_socio_vars]
  p_values <- p_matrix[var_index, available_socio_vars]
  
  # Signifikanz-Markierung hinzufügen
  marked_values <- sapply(1:length(cor_values), function(j) {
    cor_val <- cor_values[j]
    p_val <- p_values[j]
    
    if(p_val < 0.001) {
      return(sprintf("%.2f***", cor_val))
    } else if(p_val < 0.01) {
      return(sprintf("%.2f**", cor_val))
    } else if(p_val < 0.05) {
      return(sprintf("%.2f*", cor_val))
    } else {
      return(sprintf("%.2f", cor_val))
    }
  })
  
  # Spalten zu Tabelle hinzufügen
  for(k in 1:length(marked_values)) {
    col_name <- names(cor_values)[k]
    cor_table_data[[col_name]] <- marked_values[k]
  }
}

# GT Tabelle erstellen
tabelle10 <- gt(cor_table_data) %>%
  # Titel und Untertitel
  tab_header(
    title = "Tabelle 10",
    subtitle = "Pearson-Korrelationsmatrix der Zielvariablen mit soziodemografischen und nutzungsbezogenen Variablen"
  ) %>%
  # Spaltenlabels
  cols_label(
    Variable = "Variable",
    SE01 = "SE01 (Vorwissen)",
    SE02 = "SE02 (Nutzungsintensität)",
    SE03 = "SE03 (Plattformnutzung)",
    SO01 = "SO01 (Alter)",
    SO02 = "SO02 (Geschlecht)"
  ) %>%
  # Spaltenausrichtung
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  cols_align(
    align = "center",
    columns = available_socio_vars
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(10),
    table.width = px(900),
    column_labels.font.weight = "bold",
    data_row.padding = px(4),
    footnotes.padding = px(4),
    table_body.hlines.style = "none",
    table_body.hlines.color = "transparent",
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  ) %>%
  # Fußnote mit Legende
  tab_footnote(
    footnote = "*** p < 0.001, ** p < 0.01, * p < 0.05",
    placement = "left"
  ) %>%
  # Spaltenbreiten
  cols_width(
    Variable ~ px(180),
    SE01 ~ px(120),
    SE02 ~ px(140),
    SE03 ~ px(140),
    SO01 ~ px(100),
    SO02 ~ px(120)
  )

# Tabelle anzeigen
print(tabelle10)

# HTML exportieren
cat("\n=== TABELLE EXPORTIEREN ===\n")
html_file <- "organized/images/clustering/tabelle10_korrelationsmatrix_soziodem_korrigiert.html"
tabelle10 %>% gtsave(html_file)
cat("✓ HTML-Export:", html_file, "\n")

cat("\n=== INTERPRETATION DER ERGEBNISSE ===\n")

cat("\n📊 KORRELATIONSINTERPRETATION:\n")
cat("• r ≥ 0.70: Sehr hohe Korrelation\n")
cat("• r ≥ 0.50: Hohe Korrelation\n")
cat("• r ≥ 0.30: Mittlere Korrelation\n")
cat("• r ≥ 0.10: Niedrige Korrelation\n")
cat("• r < 0.10: Sehr niedrige Korrelation\n")

cat("\n🔍 IHRE ERGEBNISSE:\n")
cat("Zielvariablen:", paste(available_target_vars, collapse = ", "), "\n")
cat("Soziodemografische Variablen:", paste(available_socio_vars, collapse = ", "), "\n")
cat("Stichprobengröße: n =", nrow(correlation_data), "\n")

# Wichtige Korrelationen hervorheben
cat("\n📈 WICHTIGE KORRELATIONEN:\n")
for(i in 1:length(available_target_vars)) {
  for(j in 1:length(available_socio_vars)) {
    var1 <- available_target_vars[i]
    var2 <- available_socio_vars[j]
    cor_val <- cor_matrix[var1, var2]
    p_val <- p_matrix[var1, var2]
    
    if(abs(cor_val) >= 0.10) {
      significance <- ""
      if(p_val < 0.001) significance <- "***"
      else if(p_val < 0.01) significance <- "**"
      else if(p_val < 0.05) significance <- "*"
      
      cat("•", var1, "↔", var2, ":", sprintf("%.2f", cor_val), significance, "\n")
    }
  }
}

cat("\n================================================================================\n")
cat("TABELLE 10: KORRELATIONSMATRIX MIT SOZIODEMOGRAFISCHEN VARIABLEN KORRIGIERT ERSTELLT\n")
cat("================================================================================\n")

cat("\n📋 ZUSAMMENFASSUNG:\n")
cat("• Tabelle 10 mit ECHTEN Daten erstellt\n")
cat("• Pearson-Korrelationsmatrix der Zielvariablen mit soziodemografischen Variablen\n")
cat("• Signifikanz-Markierung mit ***, **, *\n")
cat("• Legende für Signifikanzniveaus hinzugefügt\n")
cat("• HTML-Export für weitere Verwendung verfügbar\n")

cat("\n⚠️  WICHTIG: Vorherige Tabelle 10 zeigte identische Werte (Fehler)!\n")
cat("Diese Version verwendet die echten Korrelationswerte aus dem Datensatz.\n") 