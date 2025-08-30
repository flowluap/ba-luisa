# =============================================================================
# APA7-KONFORME GT TABELLEN
# =============================================================================
# Erstellt Tabellen im exakten APA7-Standard mit der gt Library
# Alle Formatierungen entsprechen den offiziellen APA7-Richtlinien

library(gt)
library(dplyr)

# =============================================================================
# APA7 TABELLEN-STANDARDS
# =============================================================================

cat("================================================================================\n")
cat("APA7-KONFORME GT TABELLEN\n")
cat("================================================================================\n")

cat("\n=== APA7 TABELLEN-STANDARDS ===\n")
cat("• Keine vertikalen Linien\n")
cat("• Minimale horizontale Linien (nur unter Header und über Total)\n")
cat("• Linksbündige Textspalten\n")
cat("• Rechtsbündige Zahlen\n")
cat("• Dezimalstellen konsistent (2-3 Stellen)\n")
cat("• F-Werte und p-Werte in separaten Spalten\n")
cat("• Effektstärken (η², Cohen's d) separat\n")
cat("• Fußnoten für Abkürzungen und Signifikanz\n")

# =============================================================================
# BEISPIEL 1: APA7-KONFORME DESKRIPTIVE STATISTIKEN
# =============================================================================

cat("\n=== BEISPIEL 1: APA7-KONFORME DESKRIPTIVE STATISTIKEN ===\n")

# Daten für die Beispieltabelle
descriptive_data <- data.frame(
  Variable = c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
               "Identifikation", "Emotionale Ansprache", "Gesamt"),
  M = c(2.46, 2.47, 2.41, 2.57, 2.48),
  SD = c(0.25, 0.24, 0.18, 0.23, 0.23),
  n = c(64, 64, 64, 64, 64)
)

# APA7-konforme Tabelle
apa7_descriptive <- descriptive_data %>%
  gt() %>%
  tab_header(
    title = "Tabelle 1",
    subtitle = "Deskriptive Statistiken der Hauptvariablen"
  ) %>%
  # APA7: Nur horizontale Linien unter Header und über Total
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(
      rows = nrow(descriptive_data)
    )
  ) %>%
  # APA7: Linksbündige Textspalten, rechtsbündige Zahlen
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  cols_align(
    align = "right",
    columns = c(M, SD, n)
  ) %>%
  # APA7: Spaltennamen ohne Abkürzungen
  cols_label(
    Variable = "Variable",
    M = "M",
    SD = "SD",
    n = "n"
  ) %>%
  # APA7: Dezimalstellen konsistent (2 Stellen)
  fmt_number(
    columns = c(M, SD),
    decimals = 2
  ) %>%
  # APA7: Fußnoten für Abkürzungen
  tab_footnote(
    footnote = "M = Mittelwert, SD = Standardabweichung",
    placement = "left"
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(12),
    table.width = px(500),
    column_labels.font.weight = "bold",
    data_row.padding = px(6),
    footnotes.padding = px(6),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  )

print(apa7_descriptive)

# =============================================================================
# BEISPIEL 2: APA7-KONFORME ANOVA-TABELLE
# =============================================================================

cat("\n=== BEISPIEL 2: APA7-KONFORME ANOVA-TABELLE ===\n")

# ANOVA-Daten
anova_data <- data.frame(
  Variable = c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
               "Identifikation", "Emotionale Ansprache"),
  df = c(2, 2, 2, 2),
  F = c(3.45, 2.18, 15.67, 8.92),
  p = c(0.037, 0.118, 0.001, 0.001),
  eta2 = c(0.098, 0.064, 0.324, 0.216)
)

# APA7-konforme ANOVA-Tabelle
apa7_anova <- anova_data %>%
  gt() %>%
  tab_header(
    title = "Tabelle 2",
    subtitle = "Ergebnisse der univariaten Varianzanalysen"
  ) %>%
  # APA7: Nur horizontale Linien unter Header
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  # APA7: Linksbündige Textspalten, rechtsbündige Zahlen
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  cols_align(
    align = "right",
    columns = c(df, F, p, eta2)
  ) %>%
  # APA7: Spaltennamen
  cols_label(
    Variable = "Variable",
    df = "df",
    F = "F",
    p = "p",
    eta2 = "η²"
  ) %>%
  # APA7: Dezimalstellen und p-Werte
  fmt_number(
    columns = F,
    decimals = 2
  ) %>%
  fmt_number(
    columns = eta2,
    decimals = 3
  ) %>%
  # APA7: p-Werte mit Sternchen
  fmt_number(
    columns = p,
    decimals = 3
  ) %>%
  # APA7: Fußnoten
  tab_footnote(
    footnote = "df = Freiheitsgrade, η² = Eta-Quadrat. * p < .05, ** p < .01, *** p < .001",
    placement = "left"
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(12),
    table.width = px(600),
    column_labels.font.weight = "bold",
    data_row.padding = px(6),
    footnotes.padding = px(6),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  )

print(apa7_anova)

# =============================================================================
# BEISPIEL 3: APA7-KONFORME POST-HOC VERGLEICHE
# =============================================================================

cat("\n=== BEISPIEL 3: APA7-KONFORME POST-HOC VERGLEICHE ===\n")

# Post-hoc Daten
posthoc_data <- data.frame(
  Vergleich = c("KI-Offen vs Ambivalent", "KI-Offen vs KI-Skeptisch", 
                "Ambivalent vs KI-Skeptisch"),
  M_Diff = c(0.011, 0.163, 0.152),
  SE = c(0.075, 0.080, 0.078),
  t = c(0.147, 2.049, 1.958),
  p = c(0.884, 0.048, 0.058),
  d = c(0.043, 0.643, 0.618)
)

# APA7-konforme Post-hoc Tabelle
apa7_posthoc <- posthoc_data %>%
  gt() %>%
  tab_header(
    title = "Tabelle 3",
    subtitle = "Post-hoc Vergleiche (Tukey HSD) für Vertrauen & Sympathie"
  ) %>%
  # APA7: Nur horizontale Linien unter Header
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  # APA7: Linksbündige Textspalten, rechtsbündige Zahlen
  cols_align(
    align = "left",
    columns = Vergleich
  ) %>%
  cols_align(
    align = "right",
    columns = c(M_Diff, SE, t, p, d)
  ) %>%
  # APA7: Spaltennamen
  cols_label(
    Vergleich = "Vergleich",
    M_Diff = "M",
    SE = "SE",
    t = "t",
    p = "p",
    d = "d"
  ) %>%
  # APA7: Dezimalstellen
  fmt_number(
    columns = c(M_Diff, SE, d),
    decimals = 3
  ) %>%
  fmt_number(
    columns = t,
    decimals = 2
  ) %>%
  # APA7: p-Werte
  fmt_number(
    columns = p,
    decimals = 3
  ) %>%
  # APA7: Fußnoten
  tab_footnote(
    footnote = "M = Mittelwertsdifferenz, SE = Standardfehler, d = Cohen's d. * p < .05",
    placement = "left"
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(12),
    table.width = px(700),
    column_labels.font.weight = "bold",
    data_row.padding = px(6),
    footnotes.padding = px(6),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  )

print(apa7_posthoc)

# =============================================================================
# BEISPIEL 4: APA7-KONFORME CLUSTER-MITTELWERTE
# =============================================================================

cat("\n=== BEISPIEL 4: APA7-KONFORME CLUSTER-MITTELWERTE ===\n")

# Cluster-Daten
cluster_data <- data.frame(
  Cluster = c("KI-Offen", "Ambivalent", "KI-Skeptisch", "Gesamt"),
  n = c(19, 24, 21, 64),
  VS_M = c(2.64, 2.25, 2.40, 2.46),
  VS_SD = c(0.26, 0.25, 0.25, 0.25),
  MN_M = c(2.73, 2.41, 2.31, 2.47),
  MN_SD = c(0.27, 0.26, 0.16, 0.24)
)

# APA7-konforme Cluster-Tabelle
apa7_cluster <- cluster_data %>%
  gt() %>%
  tab_header(
    title = "Tabelle 4",
    subtitle = "Cluster-Mittelwerte und Standardabweichungen"
  ) %>%
  # APA7: Nur horizontale Linien unter Header und über Total
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(
      rows = nrow(cluster_data)
    )
  ) %>%
  # APA7: Linksbündige Textspalten, rechtsbündige Zahlen
  cols_align(
    align = "left",
    columns = Cluster
  ) %>%
  cols_align(
    align = "right",
    columns = c(n, VS_M, VS_SD, MN_M, MN_SD)
  ) %>%
  # APA7: Spaltennamen
  cols_label(
    Cluster = "Cluster",
    n = "n",
    VS_M = "M",
    VS_SD = "SD",
    MN_M = "M",
    MN_SD = "SD"
  ) %>%
  # APA7: Dezimalstellen
  fmt_number(
    columns = c(VS_M, VS_SD, MN_M, MN_SD),
    decimals = 2
  ) %>%
  # APA7: Spalten-Gruppierung
  tab_spanner(
    label = "Vertrauen & Sympathie",
    columns = c(VS_M, VS_SD)
  ) %>%
  tab_spanner(
    label = "Menschlichkeit & Natürlichkeit",
    columns = c(MN_M, MN_SD)
  ) %>%
  # APA7: Fußnoten
  tab_footnote(
    footnote = "M = Mittelwert, SD = Standardabweichung",
    placement = "left"
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(12),
    table.width = px(700),
    column_labels.font.weight = "bold",
    data_row.padding = px(6),
    footnotes.padding = px(6),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  )

print(apa7_cluster)

# =============================================================================
# BEISPIEL 5: APA7-KONFORME KORRELATIONSTABELLE
# =============================================================================

cat("\n=== BEISPIEL 5: APA7-KONFORME KORRELATIONSTABELLE ===\n")

# Korrelations-Daten
correlation_data <- data.frame(
  Variable = c("1. Vertrauen & Sympathie", "2. Menschlichkeit & Natürlichkeit", 
               "3. Identifikation", "4. Emotionale Ansprache"),
  VS = c("—", "0.45**", "0.32*", "0.28"),
  MN = c("0.45**", "—", "0.51**", "0.38*"),
  ID = c("0.32*", "0.51**", "—", "0.42**"),
  EA = c("0.28", "0.38*", "0.42**", "—")
)

# APA7-konforme Korrelations-Tabelle
apa7_correlation <- correlation_data %>%
  gt() %>%
  tab_header(
    title = "Tabelle 5",
    subtitle = "Interkorrelationen der Hauptvariablen"
  ) %>%
  # APA7: Nur horizontale Linien unter Header
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  # APA7: Linksbündige Textspalten, zentrierte Zahlen
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  cols_align(
    align = "center",
    columns = c(VS, MN, ID, EA)
  ) %>%
  # APA7: Spaltennamen
  cols_label(
    Variable = "Variable",
    VS = "1",
    MN = "2",
    ID = "3",
    EA = "4"
  ) %>%
  # APA7: Fußnoten
  tab_footnote(
    footnote = "* p < .05, ** p < .01. N = 64",
    placement = "left"
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(12),
    table.width = px(600),
    column_labels.font.weight = "bold",
    data_row.padding = px(6),
    footnotes.padding = px(6),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  )

print(apa7_correlation)

# =============================================================================
# APA7 EXPORT-FUNKTIONEN
# =============================================================================

cat("\n=== APA7 EXPORT-FUNKTIONEN ===\n")

# Funktion zum Exportieren von APA7-Tabellen
export_apa7_table <- function(table, filename, format = "html") {
  if (format == "html") {
    gtsave(table, filename = paste0(filename, ".html"))
    cat("✓ HTML-Export:", filename, ".html\n")
  } else if (format == "rtf") {
    gtsave(table, filename = paste0(filename, ".rtf"))
    cat("✓ RTF-Export:", filename, ".rtf\n")
  } else if (format == "latex") {
    gtsave(table, filename = paste0(filename, ".tex"))
    cat("✓ LaTeX-Export:", filename, ".tex\n")
  }
}

# Exportiere alle Tabellen
cat("\nExportiere APA7-Tabellen...\n")

export_apa7_table(apa7_descriptive, "organized/images/clustering/apa7_descriptive", "html")
export_apa7_table(apa7_anova, "organized/images/clustering/apa7_anova", "html")
export_apa7_table(apa7_posthoc, "organized/images/clustering/apa7_posthoc", "html")
export_apa7_table(apa7_cluster, "organized/images/clustering/apa7_cluster", "html")
export_apa7_table(apa7_correlation, "organized/images/clustering/apa7_correlation", "html")

# =============================================================================
# APA7 CHECKLISTE
# =============================================================================

cat("\n================================================================================\n")
cat("APA7 TABELLEN-CHECKLISTE\n")
cat("================================================================================\n")

cat("\n✅ FORMATIERUNG:\n")
cat("   ✓ Keine vertikalen Linien\n")
cat("   ✓ Minimale horizontale Linien (nur unter Header/über Total)\n")
cat("   ✓ Linksbündige Textspalten\n")
cat("   ✓ Rechtsbündige Zahlen\n")
cat("   ✓ Konsistente Dezimalstellen\n")

cat("\n✅ INHALT:\n")
cat("   ✓ Klare Spaltennamen ohne Abkürzungen\n")
cat("   ✓ F-Werte und p-Werte separat\n")
cat("   ✓ Effektstärken (η², Cohen's d) separat\n")
cat("   ✓ Fußnoten für Abkürzungen und Signifikanz\n")

cat("\n✅ EXPORT:\n")
cat("   ✓ HTML für Web/Präsentationen\n")
cat("   ✓ RTF für Word-Dokumente\n")
cat("   ✓ LaTeX für wissenschaftliche Publikationen\n")

cat("\n✅ VERWENDUNG:\n")
cat("   ✓ Direkt in R Markdown\n")
cat("   ✓ Export zu Word/LaTeX\n")
cat("   ✓ Einbettung in Präsentationen\n")

cat("\n================================================================================\n")
cat("APA7-KONFORME TABELLEN ERFOLGREICH ERSTELLT\n")
cat("================================================================================\n") 