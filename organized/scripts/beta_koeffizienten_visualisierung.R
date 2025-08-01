#!/usr/bin/env Rscript

# Visualisierung der Beta-Koeffizienten aus der Regressionsanalyse
# Basierend auf Datensatz_simuliert.csv, gefiltert für FINISHED=1

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)

cat("=== VISUALISIERUNG: BETA-KOEFFIZIENTEN REGRESSIONSANALYSE ===\n")

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

# Funktion für Regressionsanalyse
perform_regression_analysis <- function(data_group, target_vars, demo_vars) {
  cat("\n=== REGRESSIONSANALYSE ===\n")
  
  results <- list()
  
  for (target in target_vars) {
    cat("\n--- Analyse für", target, "---\n")
    
    # Skalenmittelwerte für Zielvariable berechnen
    if (target == "VS") {
      target_cols <- sapply(c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08"), 
                            function(x) as.numeric(as.character(data_group[[x]])))
    } else if (target == "MN") {
      target_cols <- sapply(c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07"), 
                            function(x) as.numeric(as.character(data_group[[x]])))
    } else if (target == "ID") {
      target_cols <- sapply(c("ID01_01", "ID01_02", "ID01_03", "ID01_04"), 
                            function(x) as.numeric(as.character(data_group[[x]])))
    } else if (target == "EA") {
      target_cols <- sapply(c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05"), 
                            function(x) as.numeric(as.character(data_group[[x]])))
    }
    
    data_group[[paste0(target, "_mean")]] <- rowMeans(target_cols, na.rm=TRUE)
    
    # Demographische Variablen berechnen
    # SE01 (SE01_01 bis SE01_06)
    se01_cols <- sapply(c("SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06"), 
                        function(x) as.numeric(as.character(data_group[[x]])))
    data_group$SE01_mean <- rowMeans(se01_cols, na.rm=TRUE)
    
    # SE02 (SE02_01)
    data_group$SE02_mean <- as.numeric(as.character(data_group$SE02_01))
    
    # SO01 (SO01_01)
    data_group$SO01_mean <- as.numeric(as.character(data_group$SO01_01))
    
    # SO02 (SO02)
    data_group$SO02_mean <- as.numeric(as.character(data_group$SO02))
    
    # Modell erstellen
    formula_str <- paste0(target, "_mean ~ SE01_mean + SE02_mean + SO01_mean + SO02_mean")
    model <- lm(as.formula(formula_str), data = data_group)
    
    # Ergebnisse speichern
    results[[target]] <- list(
      model = model,
      summary = summary(model),
      coefficients = coef(model),
      p_values = summary(model)$coefficients[, 4]
    )
    
    cat("Modell für", target, "erstellt\n")
    cat("R² =", round(summary(model)$r.squared, 3), "\n")
  }
  
  return(results)
}

# Regressionsanalyse durchführen
target_vars <- c("VS", "MN", "ID", "EA")
demo_vars <- c("SE01", "SE02", "SO01", "SO02")

regression_results <- perform_regression_analysis(data_valid, target_vars, demo_vars)

# Beta-Koeffizienten extrahieren
beta_data <- data.frame()

for (target in target_vars) {
  coef_summary <- summary(regression_results[[target]]$model)$coefficients
  
  for (i in 2:nrow(coef_summary)) {  # Skip intercept
    predictor <- rownames(coef_summary)[i]
    beta <- coef_summary[i, 1]
    p_value <- coef_summary[i, 4]
    
    # Prädiktor-Namen vereinfachen
    predictor_clean <- gsub("_mean", "", predictor)
    
    beta_data <- rbind(beta_data, data.frame(
      Target = target,
      Predictor = predictor_clean,
      Beta = beta,
      P_Value = p_value,
      Significant = p_value < 0.05
    ))
  }
}

cat("\n=== BETA-KOEFFIZIENTEN ===\n")
print(beta_data)

# Balkendiagramm erstellen
p_beta <- ggplot(beta_data, aes(x = Predictor, y = Beta, fill = Target)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), 
           color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Beta)), 
            position = position_dodge(width = 0.9),
            vjust = -0.5, 
            size = 3, 
            fontface = "bold") +
  scale_fill_manual(values = c("VS" = "#f1c683", 
                               "MN" = "#b84d5c", 
                               "ID" = "#aacd9b",
                               "EA" = "#8dd3c7"),
                    labels = c("VS" = "Vertrauen in Systeme",
                              "MN" = "Menschliche Nähe",
                              "ID" = "Identifikation",
                              "EA" = "Emotionale Akzeptanz")) +
  labs(x = "Prädiktor",
       y = "Beta-Koeffizient",
       fill = "Zielvariable") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  scale_x_discrete(labels = c("SE01" = "SE01",
                             "SE02" = "SE02", 
                             "SO01" = "SO01",
                             "SO02" = "SO02"))

# PNG speichern
ggsave("../output/images/beta_koeffizienten_visualisierung.png", p_beta, 
       width = 12, height = 8, dpi = 300, bg = "white")

cat("\nPNG gespeichert: ../output/images/beta_koeffizienten_visualisierung.png\n")

# Zusätzliche Informationen
cat("\n=== ZUSÄTZLICHE INFORMATIONEN ===\n")

cat("\nSignifikante Beta-Koeffizienten (p < 0.05):\n")
significant_betas <- beta_data[beta_data$Significant, ]
if (nrow(significant_betas) > 0) {
  print(significant_betas[, c("Target", "Predictor", "Beta", "P_Value")])
} else {
  cat("Keine signifikanten Beta-Koeffizienten gefunden.\n")
}

cat("\nR²-Werte der Modelle:\n")
for (target in target_vars) {
  r2 <- summary(regression_results[[target]]$model)$r.squared
  cat(target, ":", round(r2, 3), "\n")
}

cat("\n=== VISUALISIERUNG ERFOLGREICH ERSTELLT ===\n") 