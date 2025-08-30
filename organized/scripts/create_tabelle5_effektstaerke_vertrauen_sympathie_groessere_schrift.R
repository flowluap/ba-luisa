#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 5: EFFEKTSTÄRKE UND SIGNIFIKANZANALYSE FÜR VERTRAUEN UND SYMPATHIE 
# (GRÖSSERE SCHRIFTGRÖSSE - 20PX, 2 NACHKOMMASTELLEN)
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 5: EFFEKTSTÄRKE UND SIGNIFIKANZANALYSE FÜR VERTRAUEN UND SYMPATHIE\n")
cat("================================================================================\n")

# Pakete laden
library(gt)

# ================================================================================
# TABELLE MIT EXAKTEN WERTEN AUS DEM BILD ERSTELLEN
# ================================================================================

cat("\n=== TABELLE MIT EXAKTEN WERTEN ERSTELLEN ===\n")

# Daten für gt-Tabelle vorbereiten (exakte Werte aus dem Bild)
table_data <- data.frame(
  Skala = c("Vertrauen und Sympathie"),
  Cohens_d = c(4.710),
  t = c(-26.943),
  p = c("p < 0.001"),
  KI_Avatar_M = c(2.41),
  KI_Avatar_SD = c(0.26),
  KI_Avatar_KI = c("[2.35, 2.47]"),
  Mensch_M = c(3.59),
  Mensch_SD = c(0.24),
  Mensch_KI = c("[3.53, 3.65]"),
  stringsAsFactors = FALSE
)

cat("✓ Tabellendaten vorbereitet\n")

# gt-Tabelle erstellen
table <- gt(table_data) %>%
  tab_header(
    title = "Tabelle 5",
    subtitle = "Effektstärke und Signifikanzanalyse für Vertrauen und Sympathie"
  ) %>%
  tab_spanner(
    label = "KI-Avatar",
    columns = c("KI_Avatar_M", "KI_Avatar_SD", "KI_Avatar_KI")
  ) %>%
  tab_spanner(
    label = "Mensch",
    columns = c("Mensch_M", "Mensch_SD", "Mensch_KI")
  ) %>%
  cols_label(
    Skala = "Skala",
    Cohens_d = "Cohens d",
    t = "t",
    p = "p",
    KI_Avatar_M = "M",
    KI_Avatar_SD = "SD",
    KI_Avatar_KI = "KI",
    Mensch_M = "M",
    Mensch_SD = "SD",
    Mensch_KI = "KI"
  ) %>%
  fmt_number(
    columns = c("Cohens_d", "t", "KI_Avatar_M", "KI_Avatar_SD", "Mensch_M", "Mensch_SD"),
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
    columns = c("Skala", "KI_Avatar_KI", "Mensch_KI")
  ) %>%
  cols_align(
    align = "center",
    columns = c("Cohens_d", "t", "p", "KI_Avatar_M", "KI_Avatar_SD", "Mensch_M", "Mensch_SD")
  ) %>%
  tab_options(
    table.font.size = px(20),  # Noch größere Schriftgröße für optimale Lesbarkeit
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
html_file <- "organized/images/clustering/tabelle5_effektstaerke_vertrauen_sympathie_groessere_schrift.html"
gtsave(table, html_file)
cat("✓ HTML gespeichert:", html_file, "\n")

cat("\n================================================================================\n")
cat("TABELLE 5 ERFOLGREICH ERSTELLT!\n")
cat("================================================================================\n")

cat("\n📊 ZUSAMMENFASSUNG:\n")
cat("• Effektstärke und Signifikanzanalyse für Vertrauen und Sympathie erstellt\n")
cat("• Exakte Werte aus dem Bild verwendet\n")
cat("• Separate Analysen für KI-Avatar und Mensch Gruppen\n")
cat("• Alle Variablen:\n")
cat("  - Vertrauen und Sympathie\n")
cat("• Spalten: Cohens d, t, p, M, SD, KI (95% Konfidenzintervall)\n")
cat("• Schriftgröße auf 20px erhöht für optimale Lesbarkeit\n")
cat("• Alle numerischen Werte mit 2 Nachkommastellen\n")
cat("• Ursprüngliches Design mit Strichen zwischen den Zeilen beibehalten\n")
cat("• HTML-Datei gespeichert für PNG-Konvertierung\n") 