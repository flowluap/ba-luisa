#!/usr/bin/env Rscript

# Moderationsanalyse: Interaktion Gruppe X Vorwissen
# Basierend auf Datensatz_simuliert.csv, gefiltert für FINISHED=1

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)

cat("=== MODERATIONSANALYSE: GRUPPE X VORWISSEN ===\n")

# Daten laden
cat("Lade Datensatz_simuliert.csv...\n")
data <- tryCatch({
  read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-16", check.names=FALSE)
}, error = function(e) {
  cat("Fehler beim Laden mit UTF-16, versuche UTF-8-BOM...\n")
  read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-8-BOM", check.names=FALSE)
})

# Nur abgeschlossene Fälle
cat("Filtere abgeschlossene Fälle (FINISHED=1)...\n")
data_valid <- data[data$FINISHED == 1, ]
cat("Anzahl abgeschlossener Fälle:", nrow(data_valid), "\n")

# Gruppen aufteilen
data_ki <- data_valid[data_valid$AB01 == 1, ]
data_mensch <- data_valid[data_valid$AB01 == 2, ]

cat("Anzahl KI (AB01=1):", nrow(data_ki), "\n")
cat("Anzahl Mensch (AB01=2):", nrow(data_mensch), "\n")

# Funktion für Moderationsanalyse
perform_moderation_analysis <- function(data_group, group_name, target_vars) {
  cat("\n=== MODERATIONSANALYSE FÜR", group_name, "===\n")
  
  results <- list()
  
  for (var_name in target_vars) {
    cat("\n--- Analyse für", var_name, "---\n")
    
    # Skalenmittelwerte berechnen
    if (var_name == "VS") {
      var_cols <- sapply(c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08"), 
                         function(x) as.numeric(as.character(data_group[[x]])))
    } else if (var_name == "MN") {
      var_cols <- sapply(c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07"), 
                         function(x) as.numeric(as.character(data_group[[x]])))
    } else if (var_name == "ID") {
      var_cols <- sapply(c("ID01_01", "ID01_02", "ID01_03", "ID01_04"), 
                         function(x) as.numeric(as.character(data_group[[x]])))
    } else if (var_name == "EA") {
      var_cols <- sapply(c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05"), 
                         function(x) as.numeric(as.character(data_group[[x]])))
    }
    
    # Zielvariable berechnen
    data_group[[paste0(var_name, "_mean")]] <- rowMeans(var_cols, na.rm=TRUE)
    
    # Vorwissen (SE01 und SE02)
    se01_cols <- sapply(c("SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06"), 
                        function(x) as.numeric(as.character(data_group[[x]])))
    se02_cols <- sapply(c("SE02_01"), 
                        function(x) as.numeric(as.character(data_group[[x]])))
    
    # Mittelwerte für SE01 und SE02
    data_group$SE01_mean <- rowMeans(se01_cols, na.rm=TRUE)
    data_group$SE02_mean <- rowMeans(se02_cols, na.rm=TRUE)
    
    # Gesamtes Vorwissen (Mittelwert von SE01 und SE02)
    data_group$Vorwissen <- rowMeans(cbind(data_group$SE01_mean, data_group$SE02_mean), na.rm=TRUE)
    
    # Modell erstellen
    model_formula <- as.formula(paste0(var_name, "_mean ~ Vorwissen"))
    model <- lm(model_formula, data = data_group)
    
    # Ergebnisse speichern
    results[[var_name]] <- list(
      model = model,
      summary = summary(model),
      data = data_group
    )
    
    cat("Modell für", var_name, "erstellt\n")
    cat("R² =", round(summary(model)$r.squared, 3), "\n")
    cat("Vorwissen Beta =", round(coef(model)[2], 3), "\n")
    cat("Vorwissen p =", round(summary(model)$coefficients[2,4], 3), "\n")
  }
  
  return(results)
}

# Moderationsanalyse für beide Gruppen
target_vars <- c("VS", "MN", "ID", "EA")

ki_results <- perform_moderation_analysis(data_ki, "KI", target_vars)
mensch_results <- perform_moderation_analysis(data_mensch, "MENSCH", target_vars)

# Vergleich der Effekte
cat("\n=== VERGLEICH DER EFFEKTE ===\n")

comparison_data <- data.frame(
  Variable = rep(target_vars, 2),
  Gruppe = rep(c("KI", "Mensch"), each = length(target_vars)),
  Beta = c(
    sapply(ki_results, function(x) coef(x$model)[2]),
    sapply(mensch_results, function(x) coef(x$model)[2])
  ),
  P_Wert = c(
    sapply(ki_results, function(x) x$summary$coefficients[2,4]),
    sapply(mensch_results, function(x) x$summary$coefficients[2,4])
  ),
  R_Quadrat = c(
    sapply(ki_results, function(x) x$summary$r.squared),
    sapply(mensch_results, function(x) x$summary$r.squared)
  )
)

comparison_data$Signifikant <- ifelse(comparison_data$P_Wert < 0.05, "Ja", "Nein")
comparison_data$Beta_Rounded <- round(comparison_data$Beta, 3)
comparison_data$P_Rounded <- round(comparison_data$P_Wert, 3)
comparison_data$R2_Rounded <- round(comparison_data$R_Quadrat, 3)

cat("\nVergleich der Vorwissen-Effekte:\n")
print(comparison_data[, c("Variable", "Gruppe", "Beta_Rounded", "P_Rounded", "R2_Rounded", "Signifikant")])

# APA-konforme Tabelle erstellen
apa_table_data <- comparison_data %>%
  mutate(
    Variable = Variable,
    Gruppe = Gruppe,
    `β` = Beta_Rounded,
    `p` = P_Rounded,
    `R²` = R2_Rounded
  ) %>%
  select(Variable, Gruppe, `β`, `p`, `R²`)

# Tabelle als gridExtra erstellen
apa_tab <- tableGrob(apa_table_data, 
                     rows = NULL,
                     theme = ttheme_default(
                       base_size = 11,
                       base_family = "Times",
                       core = list(
                         fg_params = list(hjust = 0.5, x = 0.5, fontsize = 10),
                         bg_params = list(fill = "white")
                       ),
                       colhead = list(
                         fg_params = list(fontface = "bold", hjust = 0.5, x = 0.5, fontsize = 11),
                         bg_params = list(fill = "white")
                       )
                     ))

# Spaltenbreiten anpassen
apa_tab$widths <- unit(c(0.25, 0.25, 0.17, 0.17, 0.16), "npc")

# Striche oben und unten hinzufügen
apa_tab <- gtable_add_rows(apa_tab, heights = unit(0.1, "cm"), pos = 0)
apa_tab <- gtable_add_grob(apa_tab, 
                           segmentsGrob(x0 = 0, y0 = 0, x1 = 1, y1 = 0, 
                                       gp = gpar(lwd = 2)), 
                           t = 1, l = 1, r = ncol(apa_tab))
apa_tab <- gtable_add_grob(apa_tab, 
                           segmentsGrob(x0 = 0, y0 = 0, x1 = 1, y1 = 0, 
                                       gp = gpar(lwd = 2)), 
                           t = nrow(apa_tab), l = 1, r = ncol(apa_tab))

# Tabelle speichern
png("../output/images/moderationsanalyse_tabelle.png", 
    width = 8, height = 6, units = "in", res = 300, bg = "white")
grid.draw(apa_tab)
dev.off()

cat("\nPNG gespeichert: ../output/images/moderationsanalyse_tabelle.png\n")

# Interpretation
cat("\n=== INTERPRETATION ===\n")

cat("\nVorwissen schwächt die Bedingung, wenn:\n")
cat("1. Der Beta-Koeffizient negativ ist (Vorwissen reduziert die abhängige Variable)\n")
cat("2. Der Effekt in der KI-Gruppe stärker ist als in der Mensch-Gruppe\n")
cat("3. Der Effekt signifikant ist (p < 0.05)\n")

cat("\nErgebnisse:\n")
for (var in target_vars) {
  ki_beta <- comparison_data$Beta[comparison_data$Variable == var & comparison_data$Gruppe == "KI"]
  mensch_beta <- comparison_data$Beta[comparison_data$Variable == var & comparison_data$Gruppe == "Mensch"]
  ki_p <- comparison_data$P_Wert[comparison_data$Variable == var & comparison_data$Gruppe == "KI"]
  mensch_p <- comparison_data$P_Wert[comparison_data$Variable == var & comparison_data$Gruppe == "Mensch"]
  
  cat("\n", var, ":\n")
  cat("  KI: β =", round(ki_beta, 3), "(p =", round(ki_p, 3), ")\n")
  cat("  Mensch: β =", round(mensch_beta, 3), "(p =", round(mensch_p, 3), ")\n")
  
  if (ki_p < 0.05 && mensch_p < 0.05) {
    if (abs(ki_beta) > abs(mensch_beta)) {
      cat("  → Vorwissen schwächt die Bedingung stärker in der KI-Gruppe\n")
    } else {
      cat("  → Vorwissen schwächt die Bedingung stärker in der Mensch-Gruppe\n")
    }
  } else if (ki_p < 0.05) {
    cat("  → Vorwissen schwächt die Bedingung nur in der KI-Gruppe\n")
  } else if (mensch_p < 0.05) {
    cat("  → Vorwissen schwächt die Bedingung nur in der Mensch-Gruppe\n")
  } else {
    cat("  → Kein signifikanter Effekt von Vorwissen\n")
  }
}

cat("\n=== MODERATIONSANALYSE ERFOLGREICH ERSTELLT ===\n") 