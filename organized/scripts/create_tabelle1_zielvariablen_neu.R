#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 1: DESKRIPTIVE STATISTIKEN DER ZIELVARIABLEN (NEU) - KORRIGIERT
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 1: DESKRIPTIVE STATISTIKEN DER ZIELVARIABLEN (NEU) - KORRIGIERT\n")
cat("================================================================================\n")

# Pakete laden
library(dplyr)
library(gt)
library(moments)  # Für Schiefe-Berechnung

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

# ================================================================================
# DESKRIPTIVE STATISTIKEN BERECHNEN
# ================================================================================

cat("\n=== DESKRIPTIVE STATISTIKEN BERECHNEN ===\n")

# Funktion für Shapiro-Wilk Test
shapiro_test <- function(x) {
  # Entferne NA-Werte
  x_clean <- x[!is.na(x)]
  
  # Shapiro-Wilk Test (funktioniert nur für n zwischen 3 und 5000)
  if (length(x_clean) >= 3 && length(x_clean) <= 5000) {
    test_result <- shapiro.test(x_clean)
    return(test_result$p.value)
  } else {
    return(NA)
  }
}

# Berechne alle Statistiken
descriptive_stats <- data.frame(
  Variable = character(),
  N = numeric(),
  M = numeric(),
  SD = numeric(),
  Min = numeric(),
  Max = numeric(),
  Schiefe = numeric(),
  Shapiro_Wilk_p = numeric(),
  stringsAsFactors = FALSE
)

# Vollständige Variablennamen
variable_names <- c(
  "MN" = "Menschlichkeit & Natürlichkeit",
  "VS" = "Vertrauen & Sympathie", 
  "EA" = "Emotionale Ansprache",
  "ID" = "Identifikation",
  "KI01" = "KI-Wahrnehmung",
  "KI02" = "KI-Kritik"
)

for (var in available_vars) {
  values <- data[[var]]
  values_clean <- values[!is.na(values)]
  
  if (length(values_clean) > 0) {
    # Berechne Statistiken
    n <- length(values_clean)
    m <- mean(values_clean)
    sd_val <- sd(values_clean)
    min_val <- min(values_clean)
    max_val <- max(values_clean)
    skew <- skewness(values_clean)
    shapiro_p <- shapiro_test(values_clean)
    
    # Formatiere p-Wert korrekt
    if (!is.na(shapiro_p)) {
      if (shapiro_p < 0.001) {
        shapiro_formatted <- "< 0.001"
      } else {
        shapiro_formatted <- sprintf("%.3f", shapiro_p)
      }
    } else {
      shapiro_formatted <- "n/a"
    }
    
    # Füge zur Tabelle hinzu
    descriptive_stats <- rbind(descriptive_stats, data.frame(
      Variable = variable_names[var],
      N = n,
      M = round(m, 2),
      SD = round(sd_val, 2),
      Min = round(min_val, 2),
      Max = round(max_val, 2),
      Schiefe = round(skew, 3),
      Shapiro_Wilk_p = shapiro_formatted,
      stringsAsFactors = FALSE
    ))
    
    cat("✓", var, "Statistiken berechnet\n")
  } else {
    cat("⚠️", var, "hat keine gültigen Werte\n")
  }
}

# Tabelle anzeigen
cat("\n=== ROHDATEN DER TABELLE ===\n")
print(descriptive_stats)

# ================================================================================
# APA7-KONFORME TABELLE ERSTELLEN
# ================================================================================

cat("\n=== APA7-KONFORME TABELLE ERSTELLEN ===\n")

# gt-Tabelle erstellen
table <- gt(descriptive_stats) %>%
  tab_header(
    title = "Tabelle 1",
    subtitle = "Deskriptive Statistiken der Zielvariablen"
  ) %>%
  cols_label(
    Variable = "Variable",
    N = "N",
    M = "M",
    SD = "SD",
    Min = "Min",
    Max = "Max",
    Schiefe = "Schiefe",
    Shapiro_Wilk_p = "Shapiro-Wilk p"
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
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "M = Mittelwert, SD = Standardabweichung, Min = Minimum, Max = Maximum, Schiefe = Schiefe der Verteilung, Shapiro-Wilk p = p-Wert des Shapiro-Wilk Tests auf Normalverteilung",
    placement = "right"
  ) %>%
  cols_align(
    align = "center",
    columns = c("N", "M", "SD", "Min", "Max", "Schiefe", "Shapiro_Wilk_p")
  ) %>%
  cols_align(
    align = "left",
    columns = "Variable"
  ) %>%
  tab_options(
    table.font.size = px(12),
    table.width = px(900),
    data_row.padding = px(4),
    footnotes.padding = px(4)
  )

# Tabelle anzeigen
cat("✓ Tabelle erstellt\n")
print(table)

# Tabelle speichern
cat("\n=== TABELLE SPEICHERN ===\n")

# HTML speichern
html_file <- "organized/images/clustering/tabelle1_zielvariablen_neu_korrigiert.html"
gtsave(table, html_file)
cat("✓ HTML gespeichert:", html_file, "\n")

cat("\n================================================================================\n")
cat("TABELLE 1 ERFOLGREICH KORRIGIERT!\n")
cat("================================================================================\n")

cat("\n📊 ZUSAMMENFASSUNG:\n")
cat("• Deskriptive Statistiken der Zielvariablen korrigiert\n")
cat("• Shapiro-Wilk Spalte zeigt nur p-Werte (nicht den kompletten Test)\n")
cat("• Alle Variablen mit vollständigen Namen:\n")
cat("  - Menschlichkeit & Natürlichkeit (MN)\n")
cat("  - Vertrauen & Sympathie (VS)\n")
cat("  - Emotionale Ansprache (EA)\n")
cat("  - Identifikation (ID)\n")
cat("  - KI-Wahrnehmung (KI01)\n")
cat("  - KI-Kritik (KI02)\n")
cat("• Statistiken: N, M, SD, Min, Max, Schiefe, Shapiro-Wilk p\n")
cat("• Alle Werte auf 2-3 Dezimalstellen gerundet\n")
cat("• HTML-Datei gespeichert für PNG-Konvertierung\n") 