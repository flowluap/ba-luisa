# =============================================================================
# TABELLE 2: SKALENMITTELWERTE
# =============================================================================
# Erstellt eine APA7-konforme Tabelle mit 2 Rubriken: Mensch & KI
# Variablen: MN, VS, EA, ID, KI01, KI02
# Spalten: Mittelwerte, SD

library(gt)
library(dplyr)
library(moments)

# =============================================================================
# DATEN LADEN
# =============================================================================

cat("================================================================================\n")
cat("TABELLE 2: SKALENMITTELWERTE\n")
cat("================================================================================\n")

# Daten einlesen (Datei aus "Digitaler Anhang" auf dem Desktop)
data_file <- "/Users/luisa.claussen/Desktop/Digitaler Anhang /Bereinigte Daten.csv"
cat("Verwende Datensatz:", data_file, "\n")

# Lade Daten als TSV (Tabulator-getrennt) mit UTF-16 Encoding
tryCatch({
  data <- read.delim(data_file, header = TRUE, fileEncoding = "UTF-16")
  cat("✓ Datensatz erfolgreich geladen\n")
}, error = function(e) {
  cat("❌ Fehler beim Laden des Datensatzes:", e$message, "\n")
  quit()
})

cat("✓ Daten geladen. Anzahl Zeilen:", nrow(data), "\n")
cat("✓ Anzahl Spalten:", ncol(data), "\n")

# =============================================================================
# ZIELVARIABLEN IDENTIFIZIEREN UND BERECHNEN
# =============================================================================

cat("\n=== ZIELVARIABLEN BERECHNEN ===\n")

# Überprüfe verfügbare Spalten
cat("Verfügbare Spalten im Datensatz:\n")
print(colnames(data))

# Suche nach AB01 Spalte für Gruppierung
if (!("AB01" %in% colnames(data))) {
  cat("❌ AB01 Spalte nicht gefunden\n")
  quit()
}

cat("✓ AB01 Spalte gefunden\n")

# Gruppiere Daten nach AB01
ki_avatar_data <- data[data$AB01 == 1, ]  # Gruppe 1 = KI-Avatar
mensch_data <- data[data$AB01 == 2, ]     # Gruppe 2 = Mensch

cat("Anzahl Teilnehmer KI-Avatar Gruppe:", nrow(ki_avatar_data), "\n")
cat("Anzahl Teilnehmer Mensch Gruppe:", nrow(mensch_data), "\n")

# =============================================================================
# SKALEN BILDEN UND DESKRIPTIVE STATISTIKEN BERECHNEN
# =============================================================================

cat("\n=== SKALEN BILDEN UND STATISTIKEN BERECHNEN ===\n")

# Berechne Skalenmittelwerte für beide Gruppen
descriptive_stats <- data.frame(
  Variable = character(),
  KI_Avatar_M = numeric(),
  KI_Avatar_SD = numeric(),
  Mensch_M = numeric(),
  Mensch_SD = numeric(),
  stringsAsFactors = FALSE
)

# Funktion zum Berechnen von Skalenmittelwerten
calculate_scale_mean <- function(data, item_prefix, n_items) {
  items <- paste0(item_prefix, sprintf("%02d", 1:n_items))
  available_items <- items[items %in% colnames(data)]
  
  if (length(available_items) > 0) {
    # Konvertiere zu numerischen Werten
    numeric_data <- data[, available_items, drop = FALSE]
    numeric_data <- apply(numeric_data, 2, function(x) as.numeric(as.character(x)))
    
    # Berechne Zeilenmittelwerte
    row_means <- rowMeans(numeric_data, na.rm = TRUE)
    return(row_means)
  } else {
    return(rep(NA, nrow(data)))
  }
}

# Skalen berechnen
cat("Berechne Skalen...\n")

# MN (Menschlichkeit & Natürlichkeit) - 7 Items
mn_ki <- calculate_scale_mean(ki_avatar_data, "MN01_", 7)
mn_mensch <- calculate_scale_mean(mensch_data, "MN01_", 7)

# VS (Vertrauen & Sympathie) - 8 Items  
vs_ki <- calculate_scale_mean(ki_avatar_data, "VS01_", 8)
vs_mensch <- calculate_scale_mean(mensch_data, "VS01_", 8)

# EA (Emotionale Ansprache) - 5 Items
ea_ki <- calculate_scale_mean(ki_avatar_data, "EA01_", 5)
ea_mensch <- calculate_scale_mean(mensch_data, "EA01_", 5)

# ID (Identifikation) - 4 Items
id_ki <- calculate_scale_mean(ki_avatar_data, "ID01_", 4)
id_mensch <- calculate_scale_mean(mensch_data, "ID01_", 4)

# KI01 (KI Wahrnehmung) - 4 Items
ki01_ki <- calculate_scale_mean(ki_avatar_data, "KI01_", 4)
ki01_mensch <- calculate_scale_mean(mensch_data, "KI01_", 4)

# KI02 (KI Kritik) - 8 Items
ki02_ki <- calculate_scale_mean(ki_avatar_data, "KI02_", 8)
ki02_mensch <- calculate_scale_mean(mensch_data, "KI02_", 8)

# Statistiken berechnen und in Dataframe einfügen
variable_names <- c(
  "Menschlichkeit & Natürlichkeit",
  "Vertrauen & Sympathie", 
  "Emotionale Ansprache",
  "Identifikation",
  "KI Wahrnehmung",
  "KI Kritik"
)

# MN
descriptive_stats <- rbind(descriptive_stats, data.frame(
  Variable = variable_names[1],
  KI_Avatar_M = mean(mn_ki, na.rm = TRUE),
  KI_Avatar_SD = sd(mn_ki, na.rm = TRUE),
  Mensch_M = mean(mn_mensch, na.rm = TRUE),
  Mensch_SD = sd(mn_mensch, na.rm = TRUE),
  stringsAsFactors = FALSE
))

# VS
descriptive_stats <- rbind(descriptive_stats, data.frame(
  Variable = variable_names[2],
  KI_Avatar_M = mean(vs_ki, na.rm = TRUE),
  KI_Avatar_SD = sd(vs_ki, na.rm = TRUE),
  Mensch_M = mean(vs_mensch, na.rm = TRUE),
  Mensch_SD = sd(vs_mensch, na.rm = TRUE),
  stringsAsFactors = FALSE
))

# EA
descriptive_stats <- rbind(descriptive_stats, data.frame(
  Variable = variable_names[3],
  KI_Avatar_M = mean(ea_ki, na.rm = TRUE),
  KI_Avatar_SD = sd(ea_ki, na.rm = TRUE),
  Mensch_M = mean(ea_mensch, na.rm = TRUE),
  Mensch_SD = sd(ea_mensch, na.rm = TRUE),
  stringsAsFactors = FALSE
))

# ID
descriptive_stats <- rbind(descriptive_stats, data.frame(
  Variable = variable_names[4],
  KI_Avatar_M = mean(id_ki, na.rm = TRUE),
  KI_Avatar_SD = sd(id_ki, na.rm = TRUE),
  Mensch_M = mean(id_mensch, na.rm = TRUE),
  Mensch_SD = sd(id_mensch, na.rm = TRUE),
  stringsAsFactors = FALSE
))

# KI01
descriptive_stats <- rbind(descriptive_stats, data.frame(
  Variable = variable_names[5],
  KI_Avatar_M = mean(ki01_ki, na.rm = TRUE),
  KI_Avatar_SD = sd(ki01_ki, na.rm = TRUE),
  Mensch_M = mean(ki01_mensch, na.rm = TRUE),
  Mensch_SD = sd(ki01_mensch, na.rm = TRUE),
  stringsAsFactors = FALSE
))

# KI02
descriptive_stats <- rbind(descriptive_stats, data.frame(
  Variable = variable_names[6],
  KI_Avatar_M = mean(ki02_ki, na.rm = TRUE),
  KI_Avatar_SD = sd(ki02_ki, na.rm = TRUE),
  Mensch_M = mean(ki02_mensch, na.rm = TRUE),
  Mensch_SD = sd(ki02_mensch, na.rm = TRUE),
  stringsAsFactors = FALSE
))

# Zeige berechnete Statistiken
cat("\nBerechnete Statistiken:\n")
for (i in 1:nrow(descriptive_stats)) {
  var_name <- descriptive_stats$Variable[i]
  ki_mw <- descriptive_stats$KI_Avatar_M[i]
  ki_sd <- descriptive_stats$KI_Avatar_SD[i]
  mensch_mw <- descriptive_stats$Mensch_M[i]
  mensch_sd <- descriptive_stats$Mensch_SD[i]
  cat("✓", var_name, "- KI-Avatar: M=", round(ki_mw, 3), "SD=", round(ki_sd, 3), 
      "| Mensch: M=", round(mensch_mw, 3), "SD=", round(mensch_sd, 3), "\n")
}

# =============================================================================
# APA7-KONFORME GT TABELLE ERSTELLEN
# =============================================================================

cat("\n=== APA7-KONFORME TABELLE ERSTELLEN ===\n")

# Erstelle die APA7-konforme Tabelle mit 2 Rubriken
apa7_skalenmittelwerte <- descriptive_stats %>%
  gt() %>%
  tab_header(
    title = "Tabelle 2",
    subtitle = "Skalenmittelwerte"
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
    columns = c(KI_Avatar_M, KI_Avatar_SD, Mensch_M, Mensch_SD)
  ) %>%
  # APA7: Spaltennamen
  cols_label(
    Variable = "Variable",
    KI_Avatar_M = "M",
    KI_Avatar_SD = "SD",
    Mensch_M = "M",
    Mensch_SD = "SD"
  ) %>%
  # APA7: Spalten-Gruppierung mit Rubriken
  tab_spanner(
    label = "KI-Avatar",
    columns = c(KI_Avatar_M, KI_Avatar_SD)
  ) %>%
  tab_spanner(
    label = "Mensch",
    columns = c(Mensch_M, Mensch_SD)
  ) %>%
  # APA7: Dezimalstellen konsistent
  fmt_number(
    columns = c(KI_Avatar_M, KI_Avatar_SD, Mensch_M, Mensch_SD),
    decimals = 2
  ) %>%
  # APA7: Fußnoten für Abkürzungen
  tab_footnote(
    footnote = paste("M = Mittelwert, SD = Standardabweichung. KI-Avatar n =", nrow(ki_avatar_data), 
                     ", Mensch n =", nrow(mensch_data)),
    placement = "left"
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(12),
    table.width = px(600),
    column_labels.font.weight = "bold",
    data_row.padding = px(6),
    footnotes.padding = px(6),
    table_body.hlines.style = "none",
    table_body.hlines.color = "transparent",
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  ) %>%
  # Spaltenbreiten kompakter für nähere Zusammenstellung
  cols_width(
    Variable ~ px(200),
    KI_Avatar_M ~ px(75),
    KI_Avatar_SD ~ px(75),
    Mensch_M ~ px(75),
    Mensch_SD ~ px(75)
  )

# Tabelle anzeigen
print(apa7_skalenmittelwerte)

# =============================================================================
# EXPORT DER TABELLE
# =============================================================================

cat("\n=== TABELLE EXPORTIEREN ===\n")

# HTML Export
gtsave(apa7_skalenmittelwerte, filename = "organized/images/clustering/tabelle2_skalenmittelwerte.html")
cat("✓ HTML-Export: organized/images/clustering/tabelle2_skalenmittelwerte.html\n")

# =============================================================================
# INTERPRETATION DER ERGEBNISSE
# =============================================================================

cat("\n=== INTERPRETATION DER ERGEBNISSE ===\n")

cat("\n🤖 KI-AVATAR GRUPPE:\n")
for (i in 1:nrow(descriptive_stats)) {
  var_name <- descriptive_stats$Variable[i]
  ki_mw <- descriptive_stats$KI_Avatar_M[i]
  ki_sd <- descriptive_stats$KI_Avatar_SD[i]
  cat("•", var_name, "- M =", round(ki_mw, 2), "SD =", round(ki_sd, 2), "\n")
}

cat("\n👤 MENSCH GRUPPE:\n")
for (i in 1:nrow(descriptive_stats)) {
  var_name <- descriptive_stats$Variable[i]
  mensch_mw <- descriptive_stats$Mensch_M[i]
  mensch_sd <- descriptive_stats$Mensch_SD[i]
  cat("•", var_name, "- M =", round(mensch_mw, 2), "SD =", round(mensch_sd, 2), "\n")
}

cat("\n================================================================================\n")
cat("TABELLE 2: SKALENMITTELWERTE ERFOLGREICH ERSTELLT\n")
cat("================================================================================\n")

# Zusammenfassung anzeigen
cat("\n📋 ZUSAMMENFASSUNG:\n")
cat("• Tabelle 2 im APA7-Standard erstellt\n")
cat("• 2 Rubriken: KI-Avatar (n =", nrow(ki_avatar_data), ") & Mensch (n =", nrow(mensch_data), ")\n") 
cat("• Alle 6 Zielvariablen analysiert: MN, VS, EA, ID, KI01, KI02\n")
cat("• Spalten: Mittelwerte (M) und Standardabweichungen (SD)\n")
cat("• HTML-Export für weitere Verwendung verfügbar\n")
cat("• Kompaktes Layout mit optimierten Spaltenabständen\n") 