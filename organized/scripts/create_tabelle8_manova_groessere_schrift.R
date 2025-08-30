#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 8: MANOVA-ERGEBNISSE (GRÖSSERE SCHRIFTGRÖSSE)
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 8: MANOVA-ERGEBNISSE MIT GRÖSSERER SCHRIFTGRÖSSE\n")
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
    "Pillais Trace",
    "Wilks Lambda", 
    "Hotellings Trace"
  ),
  KI_Influencer_Wert = c(0.85, 0.15, 5.67),
  KI_Influencer_F = c(45.20, 45.20, 45.20),
  KI_Influencer_Sig = c("p < 0.001", "p < 0.001", "p < 0.001"),
  Mensch_Wert = c(0.78, 0.22, 3.55),
  Mensch_F = c(38.50, 38.50, 38.50),
  Mensch_Sig = c("p < 0.001", "p < 0.001", "p < 0.001"),
  stringsAsFactors = FALSE
)

cat("✓ Tabellendaten vorbereitet\n")

# gt-Tabelle erstellen
table <- gt(table_data) %>%
  tab_header(
    title = "Tabelle 8",
    subtitle = "Ergebnisse der multivariaten Varianzanalyse (MANOVA)"
  ) %>%
  tab_spanner(
    label = "KI-Influencer",
    columns = c("KI_Influencer_Wert", "KI_Influencer_F", "KI_Influencer_Sig")
  ) %>%
  tab_spanner(
    label = "Mensch",
    columns = c("Mensch_Wert", "Mensch_F", "Mensch_Sig")
  ) %>%
  cols_label(
    Variable = "Variable",
    KI_Influencer_Wert = "Wert",
    KI_Influencer_F = "F-Wert",
    KI_Influencer_Sig = "Sig.",
    Mensch_Wert = "Wert",
    Mensch_F = "F-Wert",
    Mensch_Sig = "Sig."
  ) %>%
  fmt_number(
    columns = c("KI_Influencer_Wert", "Mensch_Wert"),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c("KI_Influencer_F", "Mensch_F"),
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
    columns = c("Variable", "KI_Influencer_Sig", "Mensch_Sig")
  ) %>%
  cols_align(
    align = "right",
    columns = c("KI_Influencer_Wert", "KI_Influencer_F", "Mensch_Wert", "Mensch_F")
  ) %>%
  tab_options(
    table.font.size = px(18),  # Größere Schriftgröße für bessere Lesbarkeit
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
html_file <- "organized/images/clustering/tabelle8_manova_groessere_schrift.html"
gtsave(table, html_file)
cat("✓ HTML gespeichert:", html_file, "\n")

cat("\n================================================================================\n")
cat("TABELLE 8 ERFOLGREICH ERSTELLT!\n")
cat("================================================================================\n")

cat("\n📊 ZUSAMMENFASSUNG:\n")
cat("• MANOVA-Ergebnisse erstellt\n")
cat("• Exakte Werte aus dem Bild verwendet\n")
cat("• Separate Analysen für KI-Influencer und Mensch Gruppen\n")
cat("• Alle Variablen:\n")
cat("  - Pillais Trace\n")
cat("  - Wilks Lambda\n")
cat("  - Hotellings Trace\n")
cat("• Spalten: Wert, F-Wert, Sig.\n")
cat("• Schriftgröße auf 18px erhöht für optimale Lesbarkeit\n")
cat("• Alle Werte auf 2 Nachkommastellen gerundet\n")
cat("• Ursprüngliches Design mit Strichen zwischen den Zeilen beibehalten\n")
cat("• HTML-Datei gespeichert für PNG-Konvertierung\n") 