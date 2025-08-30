# =============================================================================
# TABELLE 3: RELIABILITÄTSKENNWERTE DER ZIELVARIABLEN (KORRIGIERT)
# =============================================================================
# Erstellt eine APA7-konforme Tabelle mit Cronbachs Alpha für alle Skalen
# Verwendet die bewährte psych::alpha() Funktion

library(gt)
library(dplyr)

# =============================================================================
# DATEN LADEN
# =============================================================================

cat("================================================================================\n")
cat("TABELLE 3: RELIABILITÄTSKENNWERTE DER ZIELVARIABLEN (KORRIGIERT)\n")
cat("================================================================================\n")

# Daten einlesen
data_file <- "/Users/luisa.claussen/Desktop/Digitaler Anhang /Bereinigte Daten.csv"
cat("Verwende Datensatz:", data_file, "\n")

data <- read.delim(data_file, header = TRUE, fileEncoding = "UTF-16")
cat("✓ Daten geladen. Gesamtstichprobe: n =", nrow(data), "\n")

# =============================================================================
# CRONBACHS ALPHA MIT PSYCH-PAKET BERECHNEN
# =============================================================================

cat("\n=== CRONBACHS ALPHA MIT PSYCH-PAKET BERECHNEN ===\n")

# Funktion zum Berechnen von Cronbachs Alpha mit psych::alpha()
calculate_cronbach_alpha_psych <- function(data, item_prefix, n_items, scale_name) {
  cat("\n---", scale_name, "---\n")
  
  # Alle Items finden
  items <- paste0(item_prefix, sprintf("%02d", 1:n_items))
  available_items <- items[items %in% colnames(data)]
  
  cat("Verfügbare Items:", paste(available_items, collapse = ", "), "\n")
  
  if (length(available_items) > 0) {
    # Daten extrahieren
    numeric_data <- data[, available_items, drop = FALSE]
    
    # Konvertiere zu numerisch
    numeric_data <- apply(numeric_data, 2, function(x) as.numeric(as.character(x)))
    
    # Zeige Zusammenfassung
    cat("Anzahl gültige Fälle:", sum(complete.cases(numeric_data)), "von", nrow(numeric_data), "\n")
    
    # Cronbachs Alpha mit psych::alpha() berechnen
    if (ncol(numeric_data) > 1) {
      # Prüfe ob psych Paket verfügbar ist
      if (!require(psych, quietly = TRUE)) {
        cat("❌ psych Paket nicht verfügbar. Installiere...\n")
        install.packages("psych", repos = "https://cran.rstudio.com/")
        library(psych)
      }
      
      # Alpha berechnen
      alpha_result <- alpha(numeric_data, check.keys = TRUE)
      
      cat("Cronbachs Alpha (psych):", round(alpha_result$total$raw_alpha, 3), "\n")
      cat("Standardized Alpha:", round(alpha_result$total$std.alpha, 3), "\n")
      
      return(alpha_result$total$raw_alpha)
    } else {
      cat("❌ Zu wenige Items für Alpha-Berechnung\n")
      return(NA)
    }
  } else {
    cat("❌ Keine Items gefunden!\n")
    return(NA)
  }
}

# Alle Skalen berechnen
cat("\n=== ALPHA-BERECHNUNG FÜR ALLE SKALEN ===\n")

# MN (Menschlichkeit & Natürlichkeit) - 7 Items
mn_alpha <- calculate_cronbach_alpha_psych(data, "MN01_", 7, "Menschlichkeit & Natürlichkeit (MN)")

# VS (Vertrauen & Sympathie) - 8 Items  
vs_alpha <- calculate_cronbach_alpha_psych(data, "VS01_", 8, "Vertrauen & Sympathie (VS)")

# EA (Emotionale Ansprache) - 5 Items
ea_alpha <- calculate_cronbach_alpha_psych(data, "EA01_", 5, "Emotionale Ansprache (EA)")

# ID (Identifikation) - 4 Items
id_alpha <- calculate_cronbach_alpha_psych(data, "ID01_", 4, "Identifikation (ID)")

# KI01 (KI Wahrnehmung) - 4 Items
ki01_alpha <- calculate_cronbach_alpha_psych(data, "KI01_", 4, "KI Wahrnehmung (KI01)")

# KI02 (KI Kritik) - 8 Items
ki02_alpha <- calculate_cronbach_alpha_psych(data, "KI02_", 8, "KI Kritik (KI02)")

# =============================================================================
# ERGEBNISSE ZUSAMMENFASSEN
# =============================================================================

cat("\n=== ZUSAMMENFASSUNG ALLER ALPHA-WERTE ===\n")

# Erstelle Dataframe für die Tabelle
reliability_stats <- data.frame(
  Variable = c(
    "Menschlichkeit & Natürlichkeit",
    "Vertrauen & Sympathie", 
    "Emotionale Ansprache",
    "Identifikation",
    "KI Wahrnehmung",
    "KI Kritik"
  ),
  Cronbachs_Alpha = c(mn_alpha, vs_alpha, ea_alpha, id_alpha, ki01_alpha, ki02_alpha),
  stringsAsFactors = FALSE
)

# Zeige alle Alpha-Werte
for (i in 1:nrow(reliability_stats)) {
  var_name <- reliability_stats$Variable[i]
  alpha_val <- reliability_stats$Cronbachs_Alpha[i]
  cat("✓", var_name, "- α =", round(alpha_val, 3), "\n")
}

# =============================================================================
# APA7-KONFORME GT TABELLE ERSTELLEN
# =============================================================================

cat("\n=== APA7-KONFORME TABELLE ERSTELLEN ===\n")

# Erstelle die APA7-konforme Tabelle
apa7_reliabilitaet <- reliability_stats %>%
  gt() %>%
  tab_header(
    title = "Tabelle 3",
    subtitle = "Reliabilitätskennwerte der Zielvariablen"
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
    columns = Cronbachs_Alpha
  ) %>%
  # APA7: Spaltennamen
  cols_label(
    Variable = "Variable",
    Cronbachs_Alpha = "Cronbachs Alpha (α)"
  ) %>%
  # APA7: Dezimalstellen konsistent
  fmt_number(
    columns = Cronbachs_Alpha,
    decimals = 2
  ) %>%
  # APA7: Fußnoten für Abkürzungen
  tab_footnote(
    footnote = paste("α = Cronbachs Alpha. Gesamtstichprobe n =", nrow(data)),
    placement = "left"
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(12),
    table.width = px(500),
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
  # Spaltenbreiten
  cols_width(
    Variable ~ px(300),
    Cronbachs_Alpha ~ px(150)
  )

# Tabelle anzeigen
print(apa7_reliabilitaet)

# =============================================================================
# EXPORT DER TABELLE
# =============================================================================

cat("\n=== TABELLE EXPORTIEREN ===\n")

# HTML Export
gtsave(apa7_reliabilitaet, filename = "organized/images/clustering/tabelle3_reliabilitaet_corrected.html")
cat("✓ HTML-Export: organized/images/clustering/tabelle3_reliabilitaet_corrected.html\n")

# =============================================================================
# INTERPRETATION DER ALPHA-WERTE
# =============================================================================

cat("\n=== INTERPRETATION DER ALPHA-WERTE ===\n")

cat("\n📊 RELIABILITÄTSBEWERTUNG:\n")
cat("• α ≥ 0.90: Exzellent\n")
cat("• α ≥ 0.80: Gut\n")
cat("• α ≥ 0.70: Akzeptabel\n")
cat("• α ≥ 0.60: Fragwürdig\n")
cat("• α < 0.60: Unzureichend\n")

cat("\n🔍 IHRE SKALEN:\n")
for (i in 1:nrow(reliability_stats)) {
  var_name <- reliability_stats$Variable[i]
  alpha_val <- reliability_stats$Cronbachs_Alpha[i]
  
  if (is.na(alpha_val)) {
    bewertung <- "NICHT BERECHENBAR"
  } else if (alpha_val >= 0.90) {
    bewertung <- "EXZELLENT"
  } else if (alpha_val >= 0.80) {
    bewertung <- "GUT"
  } else if (alpha_val >= 0.70) {
    bewertung <- "AKZEPTABEL"
  } else if (alpha_val >= 0.60) {
    bewertung <- "FRAGWÜRDIG"
  } else {
    bewertung <- "UNZUREICHEND"
  }
  
  cat("•", var_name, "- α =", round(alpha_val, 3), "→", bewertung, "\n")
}

cat("\n================================================================================\n")
cat("TABELLE 3: RELIABILITÄTSKENNWERTE (KORRIGIERT) ERFOLGREICH ERSTELLT\n")
cat("================================================================================\n")

# Zusammenfassung anzeigen
cat("\n📋 ZUSAMMENFASSUNG:\n")
cat("• Tabelle 3 im APA7-Standard erstellt (korrigiert)\n")
cat("• Cronbachs Alpha mit psych::alpha() berechnet\n")
cat("• Gesamtstichprobe n =", nrow(data), "verwendet\n")
cat("• HTML-Export für weitere Verwendung verfügbar\n")
cat("• Korrekte Interpretation der Alpha-Werte\n") 