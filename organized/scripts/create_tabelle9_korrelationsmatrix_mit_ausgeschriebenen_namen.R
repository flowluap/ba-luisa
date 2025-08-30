#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 9: PEARSON-KORRELATIONSMATRIX DER ZENTRALEN KONSTRUKTE (MIT AUSGESCHRIEBENEN NAMEN)
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 9: PEARSON-KORRELATIONSMATRIX MIT AUSGESCHRIEBENEN VARIABLENNAMEN\n")
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

# Verfügbare Variablen identifizieren
cat("\n=== VERFÜGBARE VARIABLEN IDENTIFIZIEREN ===\n")
target_vars <- c("MN", "VS", "EA", "ID", "KI01", "KI02")
available_vars <- target_vars[sapply(target_vars, function(x) !is.null(data[[x]]))]

if(length(available_vars) > 0) {
  cat("✓ Verfügbare Zielvariablen:", paste(available_vars, collapse = ", "), "\n")
} else {
  cat("❌ Keine Zielvariablen verfügbar!\n")
  stop("Kann nicht fortfahren ohne Zielvariablen")
}

# Datenqualität der Skalen überprüfen
cat("\n=== DATENQUALITÄT DER SKALEN ÜBERPRÜFEN ===\n")
for(var in available_vars) {
  cat("\n", var, ":\n")
  cat("  N =", sum(!is.na(data[[var]])), "\n")
  cat("  M =", round(mean(data[[var]], na.rm = TRUE), 2), "\n")
  cat("  SD =", round(sd(data[[var]], na.rm = TRUE), 2), "\n")
  cat("  Min =", round(min(data[[var]], na.rm = TRUE), 2), "\n")
  cat("  Max =", round(max(data[[var]], na.rm = TRUE), 2), "\n")
}

# Korrelationsmatrix berechnen
cat("\n=== KORRELATIONSMATRIX BERECHNEN ===\n")
correlation_matrix <- cor(data[, available_vars], use = "pairwise.complete.obs", method = "pearson")

# P-Werte für Signifikanz berechnen
cat("\n=== SIGNIFIKANZ BERECHNEN ===\n")
n <- nrow(data)
p_values <- matrix(NA, nrow = length(available_vars), ncol = length(available_vars))

for(i in 1:length(available_vars)) {
  for(j in 1:length(available_vars)) {
    if(i != j) {
      # Korrelation und p-Wert berechnen
      cor_test <- cor.test(data[[available_vars[i]]], data[[available_vars[j]]], 
                          method = "pearson", use = "pairwise.complete.obs")
      p_values[i, j] <- cor_test$p.value
    } else {
      p_values[i, j] <- 1  # Diagonale
    }
  }
}

# Signifikanz-Sterne hinzufügen
significance_stars <- matrix("", nrow = length(available_vars), ncol = length(available_vars))
significance_stars[p_values < 0.001] <- "***"
significance_stars[p_values < 0.01 & p_values >= 0.001] <- "**"
significance_stars[p_values < 0.05 & p_values >= 0.01] <- "*"

# Korrelationswerte mit Sternen kombinieren
correlation_with_stars <- matrix("", nrow = length(available_vars), ncol = length(available_vars))
for(i in 1:length(available_vars)) {
  for(j in 1:length(available_vars)) {
    if(i == j) {
      correlation_with_stars[i, j] <- "1.00"
    } else {
      cor_val <- correlation_matrix[i, j]
      stars <- significance_stars[i, j]
      correlation_with_stars[i, j] <- paste0(sprintf("%.2f", cor_val), stars)
    }
  }
}

# Vollständige Variablennamen definieren
full_names <- c(
  "Menschlichkeit & Natürlichkeit",
  "Vertrauen & Sympathie",
  "Emotionale Ansprache", 
  "Identifikation",
  "KI-Wahrnehmung",
  "KI-Kritik"
)

# Tabelle erstellen
cat("\n=== TABELLE ERSTELLEN ===\n")

# Daten für gt-Tabelle vorbereiten
table_data <- data.frame(
  Variable = full_names,
  stringsAsFactors = FALSE
)

# Korrelationsspalten hinzufügen
for(j in 1:length(available_vars)) {
  table_data[[full_names[j]]] <- correlation_with_stars[, j]
}

# gt-Tabelle erstellen
table <- gt(table_data) %>%
  tab_header(
    title = "Tabelle 9",
    subtitle = "Pearson-Korrelationsmatrix der zentralen Konstrukte"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom", "left", "right"),
      color = "black",
      weight = px(1)
    ),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom", "left", "right"),
      color = "black", 
      weight = px(2)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom", "left", "right"),
      color = "black",
      weight = px(2)
    ),
    locations = cells_title()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "*** p < 0.001, ** p < 0.01, * p < 0.05",
    placement = "right"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.font.size = px(12),
    table.width = px(800),
    data_row.padding = px(4),
    footnotes.padding = px(4)
  )

# Tabelle anzeigen
cat("✓ Tabelle erstellt\n")
print(table)

# Tabelle speichern
cat("\n=== TABELLE SPEICHERN ===\n")

# HTML speichern
html_file <- "organized/images/clustering/tabelle9_korrelationsmatrix_mit_ausgeschriebenen_namen.html"
gtsave(table, html_file)
cat("✓ HTML gespeichert:", html_file, "\n")

cat("\n================================================================================\n")
cat("TABELLE 9 ERFOLGREICH ERSTELLT!\n")
cat("================================================================================\n")

cat("\n📊 ZUSAMMENFASSUNG:\n")
cat("• Pearson-Korrelationsmatrix der zentralen Konstrukte erstellt\n")
cat("• Alle Variablen mit ausgeschriebenen Namen:\n")
cat("  - Menschlichkeit & Natürlichkeit (MN)\n")
cat("  - Vertrauen & Sympathie (VS)\n")
cat("  - Emotionale Ansprache (EA)\n")
cat("  - Identifikation (ID)\n")
cat("  - KI-Wahrnehmung (KI01)\n")
cat("  - KI-Kritik (KI02)\n")
cat("• Signifikanz-Sterne: *** p<0.001, ** p<0.01, * p<0.05\n")
cat("• Alle Korrelationswerte auf 2 Dezimalstellen gerundet\n")
cat("• HTML-Datei gespeichert für PNG-Konvertierung\n") 