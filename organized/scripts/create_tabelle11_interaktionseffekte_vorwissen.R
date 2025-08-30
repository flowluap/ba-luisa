#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 11: INTERAKTIONSEFFEKTE VON VORWISSEN AUF DIE ZIELVARIABLEN
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 11: INTERAKTIONSEFFEKTE VON VORWISSEN AUF DIE ZIELVARIABLEN\n")
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

# Soziodemografische und nutzungsbezogene Variablen identifizieren
cat("\n=== SOZIODEMOGRAFISCHE UND NUTZUNGSBEZOGENE VARIABLEN IDENTIFIZIEREN ===\n")

# SE01 (Vorwissen) - 5 Items
se01_items <- c("SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06")
data$SE01 <- rowMeans(data[, se01_items], na.rm = TRUE)
cat("✓ SE01 (Vorwissen) berechnet aus", length(se01_items), "Items\n")

# AB01 (Gruppe) - 1 Item
if("AB01" %in% names(data)) {
  data$AB01 <- data$AB01
  cat("✓ AB01 (Gruppe) übernommen\n")
} else {
  cat("❌ AB01 nicht gefunden\n")
  stop("AB01 Spalte nicht gefunden")
}

# Verfügbare Variablen identifizieren
cat("\n=== VERFÜGBARE VARIABLEN IDENTIFIZIEREN ===\n")
target_vars <- c("MN", "VS", "EA", "ID")
cat("✓ Verfügbare Zielvariablen:", paste(target_vars, collapse = ", "), "\n")

# Gruppen identifizieren
cat("\n=== GRUPPEN IDENTIFIZIEREN ===\n")
if(all(c(1, 2) %in% unique(data$AB01))) {
  cat("✓ Gruppen gefunden: 1 = KI-Avatar, 2 = Mensch\n")
  cat("  KI-Avatar (AB01 = 1): n =", sum(data$AB01 == 1, na.rm = TRUE), "\n")
  cat("  Mensch (AB01 = 2): n =", sum(data$AB01 == 2, na.rm = TRUE), "\n")
} else {
  cat("❌ Gruppen nicht korrekt kodiert\n")
  stop("Gruppen nicht korrekt kodiert")
}

# Regressionsanalysen für jede Gruppe durchführen
cat("\n=== REGRESSIONSANALYSEN DURCHFÜHREN ===\n")

# Funktion für Regressionsanalyse
run_regression <- function(dependent_var, independent_var, group_data) {
  # Vollständige Fälle
  complete_cases <- complete.cases(group_data[[dependent_var]], group_data[[independent_var]])
  if(sum(complete_cases) < 3) {
    return(list(beta = NA, p_value = NA, r_squared = NA))
  }
  
  # Regressionsmodell
  model <- lm(as.formula(paste(dependent_var, "~", independent_var)), data = group_data[complete_cases, ])
  
  # Ergebnisse extrahieren
  summary_model <- summary(model)
  
  # Beta-Koeffizient
  beta <- coef(model)[2]
  
  # P-Wert
  p_value <- summary_model$coefficients[2, 4]
  
  # R²
  r_squared <- summary_model$r.squared
  
  return(list(beta = beta, p_value = p_value, r_squared = r_squared))
}

# Ergebnisse für beide Gruppen sammeln
results <- list()

for(var in target_vars) {
  cat("\nAnalysiere", var, "...\n")
  
  # KI-Avatar Gruppe (AB01 = 1)
  ki_data <- data[data$AB01 == 1, ]
  ki_result <- run_regression(var, "SE01", ki_data)
  
  # Mensch Gruppe (AB01 = 2)
  mensch_data <- data[data$AB01 == 2, ]
  mensch_result <- run_regression(var, "SE01", mensch_data)
  
  results[[var]] <- list(
    ki = ki_result,
    mensch = mensch_result
  )
  
  cat("  KI-Avatar: β =", round(ki_result$beta, 3), ", p =", round(ki_result$p_value, 3), ", R² =", round(ki_result$r_squared, 3), "\n")
  cat("  Mensch: β =", round(mensch_result$beta, 3), ", p =", round(mensch_result$p_value, 3), ", R² =", round(mensch_result$r_squared, 3), "\n")
}

# Daten für GT-Tabelle vorbereiten
cat("\n=== DATEN FÜR GT-TABELLE VORBEREITEN ===\n")

# Vollständige Variablennamen
var_names <- c("Menschlichkeit & Natürlichkeit", "Vertrauen & Sympathie", "Emotionale Ansprache", "Identifikation")

# Tabelle erstellen
table_data <- data.frame(
  Variable = var_names,
  KI_Avatar_Beta = sapply(target_vars, function(x) round(results[[x]]$ki$beta, 3)),
  KI_Avatar_p = sapply(target_vars, function(x) {
    p_val <- results[[x]]$ki$p_value
    if(is.na(p_val)) return("NA")
    if(p_val < 0.001) return("p < 0.001")
    if(p_val < 0.01) return("p < 0.01")
    if(p_val < 0.05) return("p < 0.05")
    return(sprintf("p = %.3f", p_val))
  }),
  KI_Avatar_R2 = sapply(target_vars, function(x) round(results[[x]]$ki$r_squared, 3)),
  Mensch_Beta = sapply(target_vars, function(x) round(results[[x]]$mensch$beta, 3)),
  Mensch_p = sapply(target_vars, function(x) {
    p_val <- results[[x]]$mensch$p_value
    if(is.na(p_val)) return("NA")
    if(p_val < 0.001) return("p < 0.001")
    if(p_val < 0.01) return("p < 0.01")
    if(p_val < 0.05) return("p < 0.05")
    return(sprintf("p = %.3f", p_val))
  }),
  Mensch_R2 = sapply(target_vars, function(x) round(results[[x]]$mensch$r_squared, 3)),
  stringsAsFactors = FALSE
)

# GT Tabelle erstellen
cat("\n=== GT-TABELLE ERSTELLEN ===\n")

tabelle11 <- gt(table_data) %>%
  # Titel und Untertitel
  tab_header(
    title = "Tabelle 11",
    subtitle = "Interaktionseffekte von Vorwissen auf die Zielvariablen"
  ) %>%
  # Spalten gruppieren
  tab_spanner(
    label = "KI-Avatar",
    columns = c("KI_Avatar_Beta", "KI_Avatar_p", "KI_Avatar_R2")
  ) %>%
  tab_spanner(
    label = "Mensch",
    columns = c("Mensch_Beta", "Mensch_p", "Mensch_R2")
  ) %>%
  # Spaltenlabels
  cols_label(
    Variable = "Variable",
    KI_Avatar_Beta = "β",
    KI_Avatar_p = "p-Wert",
    KI_Avatar_R2 = "R²",
    Mensch_Beta = "β",
    Mensch_p = "p-Wert",
    Mensch_R2 = "R²"
  ) %>%
  # Spaltenausrichtung
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  cols_align(
    align = "center",
    columns = c("KI_Avatar_Beta", "KI_Avatar_p", "KI_Avatar_R2", "Mensch_Beta", "Mensch_p", "Mensch_R2")
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(10),
    table.width = px(780),
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
    KI_Avatar_Beta ~ px(60),
    KI_Avatar_p ~ px(80),
    KI_Avatar_R2 ~ px(60),
    Mensch_Beta ~ px(60),
    Mensch_p ~ px(80),
    Mensch_R2 ~ px(60)
  )

# Tabelle anzeigen
print(tabelle11)

# HTML exportieren
cat("\n=== TABELLE EXPORTIEREN ===\n")
html_file <- "organized/images/clustering/tabelle11_interaktionseffekte_vorwissen.html"
tabelle11 %>% gtsave(html_file)
cat("✓ HTML-Export:", html_file, "\n")

cat("\n=== INTERPRETATION DER ERGEBNISSE ===\n")

cat("\n📊 INTERPRETATION DER BETA-KOEFFIZIENTEN:\n")
cat("• β > 0: Positiver Zusammenhang (höheres Vorwissen → höhere Zielvariable)\n")
cat("• β < 0: Negativer Zusammenhang (höheres Vorwissen → niedrigere Zielvariable)\n")
cat("• β ≈ 0: Kein Zusammenhang\n")

cat("\n🔍 IHRE ERGEBNISSE:\n")
cat("Zielvariablen:", paste(var_names, collapse = ", "), "\n")
cat("Prädiktor: Vorwissen (SE01)\n")
cat("Gruppen: KI-Avatar vs. Mensch\n")

# Wichtige Effekte hervorheben
cat("\n📈 WICHTIGE EFFEKTE:\n")
for(var in target_vars) {
  var_name <- var_names[which(target_vars == var)]
  
  # KI-Avatar Effekte
  ki_beta <- results[[var]]$ki$beta
  ki_p <- results[[var]]$ki$p_value
  ki_r2 <- results[[var]]$ki$r_squared
  
  if(!is.na(ki_beta) && !is.na(ki_p)) {
    significance <- ""
    if(ki_p < 0.001) significance <- "***"
    else if(ki_p < 0.01) significance <- "**"
    else if(ki_p < 0.05) significance <- "*"
    
    cat("• KI-Avatar -", var_name, ":", sprintf("β = %.3f", ki_beta), significance, "\n")
  }
  
  # Mensch Effekte
  mensch_beta <- results[[var]]$mensch$beta
  mensch_p <- results[[var]]$mensch$p_value
  mensch_r2 <- results[[var]]$mensch$r_squared
  
  if(!is.na(mensch_beta) && !is.na(mensch_p)) {
    significance <- ""
    if(mensch_p < 0.001) significance <- "***"
    else if(mensch_p < 0.01) significance <- "**"
    else if(mensch_p < 0.05) significance <- "*"
    
    cat("• Mensch -", var_name, ":", sprintf("β = %.3f", mensch_beta), significance, "\n")
  }
}

cat("\n================================================================================\n")
cat("TABELLE 11: INTERAKTIONSEFFEKTE VON VORWISSEN ERSTELLT\n")
cat("================================================================================\n")

cat("\n📋 ZUSAMMENFASSUNG:\n")
cat("• Tabelle 11 mit Interaktionseffekten von Vorwissen auf Zielvariablen erstellt\n")
cat("• Separate Analysen für KI-Avatar und Mensch Gruppe\n")
cat("• Beta-Koeffizienten, p-Werte und R² für jede Gruppe\n")
cat("• HTML-Export für weitere Verwendung verfügbar\n") 