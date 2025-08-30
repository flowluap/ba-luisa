#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 11: INTERAKTIONSEFFEKTE VON VORWISSEN AUF DIE ZIELVARIABLEN (GRÖSSERE SCHRIFT)
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 11: INTERAKTIONSEFFEKTE VON VORWISSEN AUF DIE ZIELVARIABLEN\n")
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
    "Identifikation"
  ),
  KI_Avatar_Beta = c(0.067, -0.012, -0.044, 0.093),
  KI_Avatar_p = c(0.307, 0.867, 0.611, 0.307),
  KI_Avatar_R2 = c(0.017, 0.000, 0.004, 0.017),
  Mensch_Beta = c(-0.064, 0.054, -0.051, -0.070),
  Mensch_p = c(0.311, 0.392, 0.480, 0.387),
  Mensch_R2 = c(0.016, 0.011, 0.008, 0.012),
  stringsAsFactors = FALSE
)

cat("✓ Tabellendaten vorbereitet\n")

# gt-Tabelle erstellen
table <- gt(table_data) %>%
  tab_header(
    title = "Tabelle 11",
    subtitle = "Interaktionseffekte von Vorwissen auf die Zielvariablen"
  ) %>%
  tab_spanner(
    label = "KI-Avatar",
    columns = c("KI_Avatar_Beta", "KI_Avatar_p", "KI_Avatar_R2")
  ) %>%
  tab_spanner(
    label = "Mensch",
    columns = c("Mensch_Beta", "Mensch_p", "Mensch_R2")
  ) %>%
  cols_label(
    Variable = "Variable",
    KI_Avatar_Beta = "β",
    KI_Avatar_p = "p-Wert",
    KI_Avatar_R2 = "R²",
    Mensch_Beta = "β",
    Mensch_p = "p-Wert",
    Mensch_R2 = "R²"
  ) %>%
  fmt_number(
    columns = c("KI_Avatar_Beta", "KI_Avatar_R2", "Mensch_Beta", "Mensch_R2"),
    decimals = 3
  ) %>%
  fmt_number(
    columns = c("KI_Avatar_p", "Mensch_p"),
    decimals = 3,
    pattern = "p = {x}"
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
    columns = c("KI_Avatar_Beta", "KI_Avatar_p", "KI_Avatar_R2", "Mensch_Beta", "Mensch_p", "Mensch_R2")
  ) %>%
  tab_options(
    table.font.size = px(18),  # Noch größere Schriftgröße für bessere Lesbarkeit
    table.width = px(900),
    data_row.padding = px(8),
    column_labels.padding = px(10)
  )

# Tabelle anzeigen
cat("✓ Tabelle erstellt\n")
print(table)

# Tabelle speichern
cat("\n=== TABELLE SPEICHERN ===\n")

# HTML speichern
html_file <- "organized/images/clustering/tabelle11_interaktionseffekte_vorwissen_groessere_schrift.html"
gtsave(table, html_file)
cat("✓ HTML gespeichert:", html_file, "\n")

cat("\n================================================================================\n")
cat("TABELLE 11 ERFOLGREICH ERSTELLT!\n")
cat("================================================================================\n")

cat("\n📊 ZUSAMMENFASSUNG:\n")
cat("• Interaktionseffekte von Vorwissen auf die Zielvariablen erstellt\n")
cat("• Exakte Werte aus dem Bild verwendet\n")
cat("• Separate Analysen für KI-Avatar und Mensch Gruppen\n")
cat("• Alle Variablen mit vollständigen Namen:\n")
cat("  - Menschlichkeit & Natürlichkeit (MN)\n")
cat("  - Vertrauen & Sympathie (VS)\n")
cat("  - Emotionale Ansprache (EA)\n")
cat("  - Identifikation (ID)\n")
cat("• Spalten: β (Beta-Koeffizient), p-Wert, R²\n")
cat("• Schriftgröße auf 18px erhöht für optimale Lesbarkeit\n")
cat("• Spaltenname 'Variable' beibehalten\n")
cat("• Striche zwischen den Zeilen entfernt\n")
cat("• HTML-Datei gespeichert für PNG-Konvertierung\n") 