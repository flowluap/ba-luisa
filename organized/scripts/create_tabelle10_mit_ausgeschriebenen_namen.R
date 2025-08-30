#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 10: MIT AUSGESCHRIEBENEN VARIABLENNAMEN
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 10: MIT AUSGESCHRIEBENEN VARIABLENNAMEN\n")
cat("================================================================================\n")

# Pakete laden
library(gt)

# Echte Korrelationswerte aus dem Debug direkt eintragen
cat("\n=== ECHTE KORRELATIONSWERTE MIT AUSGESCHRIEBENEN NAMEN ===\n")

# Daten für GT-Tabelle vorbereiten
cor_table_data <- data.frame(
  Variable = c("Menschlichkeit & Natürlichkeit", "Vertrauen & Sympathie", "Emotionale Ansprache", "Identifikation", "KI-Wahrnehmung", "KI-Kritik"),
  Vorwissen = c("-0.01", "0.01", "-0.05", "-0.01", "-0.01", "-0.04"),
  Nutzungsintensität = c("0.09", "0.12", "0.03", "0.12", "0.13", "0.01"),
  Plattformnutzung = c("0.55***", "0.56***", "0.56***", "0.56***", "-0.11", "-0.03"),
  Alter = c("-0.02", "-0.08", "-0.00", "-0.06", "-0.04", "-0.00"),
  Geschlecht = c("-0.02", "0.01", "-0.03", "0.04", "0.07", "0.13"),
  stringsAsFactors = FALSE
)

cat("✓ Echte Korrelationswerte mit ausgeschriebenen Namen eingetragen\n")

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
    Vorwissen = "Vorwissen",
    Nutzungsintensität = "Nutzungsintensität",
    Plattformnutzung = "Plattformnutzung",
    Alter = "Alter",
    Geschlecht = "Geschlecht"
  ) %>%
  # Spaltenausrichtung
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  cols_align(
    align = "center",
    columns = c("Vorwissen", "Nutzungsintensität", "Plattformnutzung", "Alter", "Geschlecht")
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(10),
    table.width = px(760),
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
    Vorwissen ~ px(90),
    Nutzungsintensität ~ px(110),
    Plattformnutzung ~ px(110),
    Alter ~ px(80),
    Geschlecht ~ px(90)
  )

# Tabelle anzeigen
print(tabelle10)

# HTML exportieren
cat("\n=== TABELLE EXPORTIEREN ===\n")
html_file <- "organized/images/clustering/tabelle10_mit_ausgeschriebenen_namen.html"
tabelle10 %>% gtsave(html_file)
cat("✓ HTML-Export:", html_file, "\n")

cat("\n================================================================================\n")
cat("TABELLE 10: MIT AUSGESCHRIEBENEN VARIABLENNAMEN ERSTELLT\n")
cat("================================================================================\n")

cat("\n📋 ZUSAMMENFASSUNG:\n")
cat("• Tabelle 10 mit ausgeschriebenen Variablennamen erstellt\n")
cat("• Alle Spaltenüberschriften sind vollständig ausgeschrieben\n")
cat("• Alle Zeilenvariablen sind vollständig ausgeschrieben\n")
cat("• HTML-Export für weitere Verwendung verfügbar\n") 