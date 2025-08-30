#!/usr/bin/env Rscript

# ================================================================================
# TABELLE B1: MINIMAL- UND MAXIMALWERTE DER AGGREGIERTEN SKALEN
# ================================================================================

cat("================================================================================\n")
cat("TABELLE B1: MINIMAL- UND MAXIMALWERTE DER AGGREGIERTEN SKALEN\n")
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

# AB01 (Gruppe) identifizieren
cat("\n=== GRUPPEN IDENTIFIZIEREN ===\n")
if("AB01" %in% names(data)) {
  data$AB01 <- data$AB01
  cat("✓ AB01 (Gruppe) übernommen\n")
} else {
  cat("❌ AB01 nicht gefunden\n")
  stop("AB01 Spalte nicht gefunden")
}

# Gruppen identifizieren
if(all(c(1, 2) %in% unique(data$AB01))) {
  cat("✓ Gruppen gefunden: 1 = KI-Avatar, 2 = Mensch\n")
  cat("  KI-Avatar (AB01 = 1): n =", sum(data$AB01 == 1, na.rm = TRUE), "\n")
  cat("  Mensch (AB01 = 2): n =", sum(data$AB01 == 2, na.rm = TRUE), "\n")
} else {
  cat("❌ Gruppen nicht korrekt kodiert\n")
  stop("Gruppen nicht korrekt kodiert")
}

# Minima und Maxima der Skalenmittelwerte für jede Gruppe berechnen
cat("\n=== MINIMA UND MAXIMA DER SKALENMITTELWERTE BERECHNEN ===\n")

target_vars <- c("MN", "VS", "EA", "ID")
var_names <- c("Menschlichkeit & Natürlichkeit", "Vertrauen & Sympathie", "Emotionale Ansprache", "Identifikation")

# KI-Avatar Gruppe (AB01 = 1)
ki_data <- data[data$AB01 == 1, ]
cat("\nKI-Avatar Gruppe (n =", nrow(ki_data), "):\n")

ki_min_max <- list()
for(var in target_vars) {
  # Skalenmittelwert der jeweiligen Variable
  var_data <- ki_data[[var]]
  var_data_clean <- var_data[!is.na(var_data)]
  
  if(length(var_data_clean) > 0) {
    min_val <- min(var_data_clean)
    max_val <- max(var_data_clean)
    ki_min_max[[var]] <- list(min = min_val, max = max_val)
    
    var_name <- var_names[which(target_vars == var)]
    cat("  ", var_name, ": Min =", round(min_val, 3), ", Max =", round(max_val, 3), "\n")
  } else {
    ki_min_max[[var]] <- list(min = NA, max = NA)
    cat("  ", var_names[which(target_vars == var)], ": Keine gültigen Daten\n")
  }
}

# Mensch Gruppe (AB01 = 2)
mensch_data <- data[data$AB01 == 2, ]
cat("\nMensch Gruppe (n =", nrow(mensch_data), "):\n")

mensch_min_max <- list()
for(var in target_vars) {
  # Skalenmittelwert der jeweiligen Variable
  var_data <- mensch_data[[var]]
  var_data_clean <- var_data[!is.na(var_data)]
  
  if(length(var_data_clean) > 0) {
    min_val <- min(var_data_clean)
    max_val <- max(var_data_clean)
    mensch_min_max[[var]] <- list(min = min_val, max = max_val)
    
    var_name <- var_names[which(target_vars == var)]
    cat("  ", var_name, ": Min =", round(min_val, 3), ", Max =", round(max_val, 3), "\n")
  } else {
    mensch_min_max[[var]] <- list(min = NA, max = NA)
    cat("  ", var_names[which(target_vars == var)], ": Keine gültigen Daten\n")
  }
}

# Daten für GT-Tabelle vorbereiten
cat("\n=== DATEN FÜR GT-TABELLE VORBEREITEN ===\n")

  # Tabelle erstellen
  table_data <- data.frame(
    Variable = var_names,
    KI_Avatar_Min = sapply(target_vars, function(x) {
      if(is.na(ki_min_max[[x]]$min)) return("NA")
      sprintf("%.2f", ki_min_max[[x]]$min)
    }),
    KI_Avatar_Max = sapply(target_vars, function(x) {
      if(is.na(ki_min_max[[x]]$max)) return("NA")
      sprintf("%.2f", ki_min_max[[x]]$max)
    }),
    Mensch_Min = sapply(target_vars, function(x) {
      if(is.na(mensch_min_max[[x]]$min)) return("NA")
      sprintf("%.2f", mensch_min_max[[x]]$min)
    }),
    Mensch_Max = sapply(target_vars, function(x) {
      if(is.na(mensch_min_max[[x]]$max)) return("NA")
      sprintf("%.2f", mensch_min_max[[x]]$max)
    }),
    stringsAsFactors = FALSE
  )

# GT Tabelle erstellen
cat("\n=== GT-TABELLE ERSTELLEN ===\n")

tabelle_b1 <- gt(table_data) %>%
  # Titel und Untertitel
  tab_header(
    title = "Tabelle B1",
    subtitle = "Minimal- und Maximalwerte der Skalenmittelwerte für die Mensch- und die KI-Gruppe"
  ) %>%
  # Spalten gruppieren
  tab_spanner(
    label = "KI-Avatar",
    columns = c("KI_Avatar_Min", "KI_Avatar_Max")
  ) %>%
  tab_spanner(
    label = "Mensch",
    columns = c("Mensch_Min", "Mensch_Max")
  ) %>%
  # Spaltenlabels
  cols_label(
    Variable = "Variable",
    KI_Avatar_Min = "Minima",
    KI_Avatar_Max = "Maxima",
    Mensch_Min = "Minima",
    Mensch_Max = "Maxima"
  ) %>%
  # Spaltenausrichtung
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  cols_align(
    align = "center",
    columns = c("KI_Avatar_Min", "KI_Avatar_Max", "Mensch_Min", "Mensch_Max")
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(10),
    table.width = px(600),
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
  # Spaltenbreiten
  cols_width(
    Variable ~ px(180),
    KI_Avatar_Min ~ px(60),
    KI_Avatar_Max ~ px(60),
    Mensch_Min ~ px(60),
    Mensch_Max ~ px(60)
  )

# Tabelle anzeigen
print(tabelle_b1)

# HTML exportieren
cat("\n=== TABELLE EXPORTIEREN ===\n")
html_file <- "organized/images/clustering/tabelle_b1_min_max_werte.html"
tabelle_b1 %>% gtsave(html_file)
cat("✓ HTML-Export:", html_file, "\n")

cat("\n=== INTERPRETATION DER ERGEBNISSE ===\n")

cat("\n📊 INTERPRETATION DER MIN-MAX-WERTE:\n")
cat("• Minima: Niedrigster Wert in der jeweiligen Gruppe\n")
cat("• Maxima: Höchster Wert in der jeweiligen Gruppe\n")
cat("• Spannweite: Differenz zwischen Maximum und Minimum\n")

cat("\n🔍 IHRE ERGEBNISSE:\n")
cat("Zielvariablen:", paste(var_names, collapse = ", "), "\n")
cat("Gruppen: KI-Avatar vs. Mensch\n")

  # Spannweiten berechnen und anzeigen
  cat("\n📈 SPANNWEITEN (MAX - MIN):\n")
  for(var in target_vars) {
    var_name <- var_names[which(target_vars == var)]
    
    # KI-Avatar Spannweite
    ki_min <- ki_min_max[[var]]$min
    ki_max <- ki_min_max[[var]]$max
    if(!is.na(ki_min) && !is.na(ki_max)) {
      ki_range <- ki_max - ki_min
      cat("• KI-Avatar -", var_name, ":", sprintf("Spannweite = %.2f", ki_range), "\n")
    }
    
    # Mensch Spannweite
    mensch_min <- mensch_min_max[[var]]$min
    mensch_max <- mensch_min_max[[var]]$max
    if(!is.na(mensch_min) && !is.na(mensch_max)) {
      mensch_range <- mensch_max - mensch_min
      cat("• Mensch -", var_name, ":", sprintf("Spannweite = %.2f", mensch_range), "\n")
    }
  }

cat("\n================================================================================\n")
cat("TABELLE B1: MINIMAL- UND MAXIMALWERTE ERSTELLT\n")
cat("================================================================================\n")

cat("\n📋 ZUSAMMENFASSUNG:\n")
cat("• Tabelle B1 mit Min-Max-Werten der aggregierten Skalen erstellt\n")
cat("• Separate Werte für KI-Avatar und Mensch Gruppe\n")
cat("• Alle Zielvariablen (MN, VS, EA, ID) berücksichtigt\n")
cat("• HTML-Export für weitere Verwendung verfügbar\n") 