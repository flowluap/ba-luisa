#!/usr/bin/env Rscript

# Scatterplot: F-Wert vs. p-Wert für MANOVA-Statistiken
# Basierend auf Datensatz_simuliert.csv, gefiltert für FINISHED=1

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)

cat("=== SCATTERPLOT: MANOVA F-WERT VS. P-WERT ===\n")

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

# Funktion für MANOVA-Analyse
perform_manova_analysis <- function(data_group) {
  cat("\n=== MANOVA-ANALYSE ===\n")
  
  # Skalenmittelwerte berechnen
  vs_cols <- sapply(c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08"), 
                    function(x) as.numeric(as.character(data_group[[x]])))
  mn_cols <- sapply(c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07"), 
                    function(x) as.numeric(as.character(data_group[[x]])))
  id_cols <- sapply(c("ID01_01", "ID01_02", "ID01_03", "ID01_04"), 
                    function(x) as.numeric(as.character(data_group[[x]])))
  ea_cols <- sapply(c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05"), 
                    function(x) as.numeric(as.character(data_group[[x]])))
  
  # Daten für MANOVA vorbereiten
  manova_data <- data.frame(
    VS = rowMeans(vs_cols, na.rm=TRUE),
    MN = rowMeans(mn_cols, na.rm=TRUE),
    ID = rowMeans(id_cols, na.rm=TRUE),
    EA = rowMeans(ea_cols, na.rm=TRUE),
    Gruppe = factor(data_group$AB01, levels = c(1, 2), labels = c("KI", "Mensch"))
  )
  
  # MANOVA durchführen
  manova_result <- manova(cbind(VS, MN, ID, EA) ~ Gruppe, data = manova_data)
  
  # MANOVA-Statistiken extrahieren
  summary_manova <- summary(manova_result, test = "Pillai")
  
  # Alle Teststatistiken berechnen
  pillai <- summary(manova_result, test = "Pillai")
  wilks <- summary(manova_result, test = "Wilks")
  hotelling <- summary(manova_result, test = "Hotelling-Lawley")
  
  # Ergebnisse zusammenfassen
  results <- data.frame(
    Test = c("Pillai's Trace", "Wilks Lambda", "Hotelling's Trace"),
    Value = c(pillai$stats[1, 2], wilks$stats[1, 2], hotelling$stats[1, 2]),
    F_Value = c(pillai$stats[1, 3], wilks$stats[1, 3], hotelling$stats[1, 3]),
    p_Value = c(pillai$stats[1, 6], wilks$stats[1, 6], hotelling$stats[1, 6]),
    df1 = c(pillai$stats[1, 4], wilks$stats[1, 4], hotelling$stats[1, 4]),
    df2 = c(pillai$stats[1, 5], wilks$stats[1, 5], hotelling$stats[1, 5])
  )
  
  return(results)
}

# MANOVA-Analyse durchführen
manova_results <- perform_manova_analysis(data_valid)

cat("\n=== MANOVA-ERGEBNISSE ===\n")
print(manova_results)

# Balkendiagramm erstellen
p_bars <- ggplot(manova_results, aes(x = Test, y = Value, fill = Test)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", Value)), 
            vjust = -0.5, 
            size = 4, 
            fontface = "bold") +
  scale_fill_manual(values = c("Pillai's Trace" = "#f1c683", 
                               "Wilks Lambda" = "#b84d5c", 
                               "Hotelling's Trace" = "#aacd9b"),
                    labels = c("Pillai's Trace" = "Pillai's Trace",
                              "Wilks Lambda" = "Wilks Lambda",
                              "Hotelling's Trace" = "Hotelling's Trace")) +
  labs(x = "MANOVA-Test",
       y = "Value") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),

    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# PNG speichern
ggsave("../output/images/manova_balkendiagramm.png", p_bars, 
       width = 10, height = 6, dpi = 300, bg = "white")

cat("\nPNG gespeichert: ../output/images/manova_balkendiagramm.png\n")

# Zusätzliche Informationen
cat("\n=== ZUSÄTZLICHE INFORMATIONEN ===\n")

cat("\nSignifikanz-Niveaus:\n")
for (i in 1:nrow(manova_results)) {
  test_name <- manova_results$Test[i]
  p_val <- manova_results$p_Value[i]
  f_val <- manova_results$F_Value[i]
  
  cat(test_name, ":\n")
  cat("  F-Wert =", round(f_val, 2), "\n")
  cat("  p-Wert =", format(p_val, scientific = TRUE, digits = 3), "\n")
  
  if (p_val < 0.001) {
    cat("  → Extrem signifikant (p < 0.001)\n")
  } else if (p_val < 0.01) {
    cat("  → Sehr signifikant (p < 0.01)\n")
  } else if (p_val < 0.05) {
    cat("  → Signifikant (p < 0.05)\n")
  } else {
    cat("  → Nicht signifikant (p ≥ 0.05)\n")
  }
  cat("\n")
}

cat("\n=== SCATTERPLOT ERFOLGREICH ERSTELLT ===\n") 