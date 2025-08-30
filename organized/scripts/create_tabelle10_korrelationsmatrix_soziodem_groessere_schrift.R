#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 10: PEARSON-KORRELATIONSMATRIX DER ZIELVARIABLEN MIT SOZIODEMOGRAFISCHEN 
# UND NUTZUNGSBEZOGENEN VARIABLEN (GRÖSSERE SCHRIFTGRÖSSE)
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 10: PEARSON-KORRELATIONSMATRIX MIT GRÖSSERER SCHRIFTGRÖSSE\n")
cat("================================================================================\n")

# Pakete laden
library(gt)

# ================================================================================
# TABELLE MIT EXAKTEN WERTEN AUS DEM BILD ERSTELLEN
# ================================================================================

cat("\n=== TABELLE MIT EXAKTEN WERTEN ERSTELLEN ===\n")

# Daten für gt-Tabelle vorbereiten (exakte Werte aus dem Bild)
table_data <- data.frame(
  Variable = c(
    "Menschlichkeit & Natürlichkeit",
    "Vertrauen & Sympathie",
    "Emotionale Ansprache",
    "Identifikation",
    "KI-Wahrnehmung",
    "KI-Kritik"
  ),
  Vorwissen = c(-0.01, 0.01, -0.05, -0.01, -0.01, -0.04),
  Nutzungsintensitaet = c(0.09, 0.12, 0.03, 0.12, 0.13, 0.01),
  Plattformnutzung = c("0.55***", "0.56***", "0.56***", "0.56***", -0.11, -0.03),
  Alter = c(-0.02, -0.08, -0.00, -0.06, -0.04, -0.00),
  Geschlecht = c(-0.02, 0.01, -0.03, 0.04, 0.07, 0.13),
  stringsAsFactors = FALSE
)

cat("✓ Tabellendaten vorbereitet\n")

# gt-Tabelle erstellen
table <- gt(table_data) %>%
  tab_header(
    title = "Tabelle 10",
    subtitle = "Pearson-Korrelationsmatrix der Zielvariablen mit soziodemografischen und nutzungsbezogenen Variablen"
  ) %>%
  cols_label(
    Variable = "Variable",
    Vorwissen = "Vorwissen",
    Nutzungsintensitaet = "Nutzungsintensität",
    Plattformnutzung = "Plattformnutzung",
    Alter = "Alter",
    Geschlecht = "Geschlecht"
  ) %>%
  fmt_number(
    columns = c("Vorwissen", "Nutzungsintensitaet", "Alter", "Geschlecht"),
    decimals = 2
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "gray",
      weight = px(1)
    ),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "gray",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title()
  ) %>%
  cols_align(
    align = "left",
    columns = "Variable"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Vorwissen", "Nutzungsintensitaet", "Plattformnutzung", "Alter", "Geschlecht")
  ) %>%
  tab_options(
    table.font.size = px(18),  # Größere Schriftgröße für bessere Lesbarkeit
    table.width = px(1000),
    data_row.padding = px(8),
    column_labels.padding = px(10)
  )

# Tabelle anzeigen
cat("✓ Tabelle erstellt\n")
print(table)

# Tabelle speichern
cat("\n=== TABELLE SPEICHERN ===\n")

# HTML speichern
html_file <- "organized/images/clustering/tabelle10_korrelationsmatrix_soziodem_groessere_schrift.html"
gtsave(table, html_file)
cat("✓ HTML gespeichert:", html_file, "\n")

cat("\n================================================================================\n")
cat("TABELLE 10 ERFOLGREICH ERSTELLT!\n")
cat("================================================================================\n")

cat("\n📊 ZUSAMMENFASSUNG:\n")
cat("• Pearson-Korrelationsmatrix der Zielvariablen mit soziodemografischen Variablen erstellt\n")
cat("• Exakte Werte aus dem Bild verwendet\n")
cat("• Alle Variablen:\n")
cat("  - Zielvariablen: MN, VS, EA, ID, KI01, KI02\n")
cat("  - Soziodemografische: SE01, SE02, SE03, SO01, SO02\n")
cat("• Korrelationskoeffizienten mit 2 Nachkommastellen\n")
cat("• Signifikanzmarkierungen (***) für p < 0.001 beibehalten\n")
cat("• Schriftgröße auf 18px erhöht für optimale Lesbarkeit\n")
cat("• Ursprüngliches Design mit Strichen zwischen den Zeilen beibehalten\n")
cat("• HTML-Datei gespeichert für PNG-Konvertierung\n") 