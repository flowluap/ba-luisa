#!/usr/bin/env Rscript

# Korrelationsanalyse: SE02 vs. KI01
# Basierend auf Datensatz_simuliert.csv, gefiltert für FINISHED=1

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)

cat("=== KORRELATIONSANALYSE: SE02 VS. KI01 ===\n")

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

# Funktion für Korrelationsanalyse
perform_correlation_analysis <- function(data_group) {
  cat("\n=== KORRELATIONSANALYSE ===\n")
  
  # SE02 berechnen (SE02_01)
  se02_data <- as.numeric(as.character(data_group$SE02_01))
  
  # KI01 berechnen (KI01_01 bis KI01_04)
  ki01_cols <- data_group[, c("KI01_01", "KI01_02", "KI01_03", "KI01_04")]
  ki01_cols <- sapply(ki01_cols, function(x) as.numeric(as.character(x)))
  ki01_data <- rowMeans(ki01_cols, na.rm=TRUE)
  
  # Daten zusammenfassen
  cor_data <- data.frame(
    SE02 = se02_data,
    KI01 = ki01_data
  )
  
  # NA-Werte entfernen
  cor_data_clean <- na.omit(cor_data)
  cat("Anzahl gültiger Fälle für Korrelation:", nrow(cor_data_clean), "\n")
  
  # Korrelation berechnen
  correlation <- cor.test(cor_data_clean$SE02, cor_data_clean$KI01, method = "pearson")
  
  # Ergebnisse zusammenfassen
  results <- list(
    correlation = correlation,
    data = cor_data_clean,
    n = nrow(cor_data_clean)
  )
  
  return(results)
}

# Korrelationsanalyse durchführen
cor_results <- perform_correlation_analysis(data_valid)

cat("\n=== KORRELATIONSERGEBNISSE ===\n")
cat("Pearson-Korrelation zwischen SE02 und KI01:\n")
cat("r =", round(cor_results$correlation$estimate, 3), "\n")
cat("p-Wert =", format(cor_results$correlation$p.value, scientific = TRUE, digits = 3), "\n")
cat("95% Konfidenzintervall:", round(cor_results$correlation$conf.int[1], 3), "bis", round(cor_results$correlation$conf.int[2], 3), "\n")
cat("n =", cor_results$n, "\n")

# Signifikanz bestimmen
p_val <- cor_results$correlation$p.value
r_val <- cor_results$correlation$estimate

if (p_val < 0.001) {
  significance <- "*** (p < 0.001)"
} else if (p_val < 0.01) {
  significance <- "** (p < 0.01)"
} else if (p_val < 0.05) {
  significance <- "* (p < 0.05)"
} else {
  significance <- "n.s. (p ≥ 0.05)"
}

cat("Signifikanz:", significance, "\n")

# Effektstärke interpretieren
if (abs(r_val) >= 0.5) {
  effect_size <- "groß"
} else if (abs(r_val) >= 0.3) {
  effect_size <- "mittel"
} else if (abs(r_val) >= 0.1) {
  effect_size <- "klein"
} else {
  effect_size <- "sehr klein"
}

cat("Effektstärke:", effect_size, "\n")

# Scatterplot erstellen
p_scatter <- ggplot(cor_results$data, aes(x = SE02, y = KI01)) +
  geom_point(alpha = 0.6, color = "#b84d5c", size = 2) +
  geom_smooth(method = "lm", color = "#f1c683", linewidth = 1.5, se = TRUE) +
  labs(x = "SE02",
       y = "KI01",
       title = paste0("Korrelation SE02 vs. KI01\nr = ", round(r_val, 3), ", ", significance)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# PNG speichern
ggsave("../output/images/korrelation_se02_ki01_scatterplot.png", p_scatter, 
       width = 10, height = 8, dpi = 300, bg = "white")

cat("\nPNG gespeichert: ../output/images/korrelation_se02_ki01_scatterplot.png\n")

# APA-konforme Tabelle erstellen
apa_table_data <- data.frame(
  Variable1 = "SE02",
  Variable2 = "KI01", 
  r = round(r_val, 3),
  p = format(p_val, scientific = TRUE, digits = 3),
  n = cor_results$n,
  Signifikanz = significance
)

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
apa_tab$widths <- unit(c(0.2, 0.2, 0.2, 0.25, 0.15), "npc")

# Tabelle speichern
png("../output/images/korrelation_se02_ki01_tabelle.png", 
    width = 8, height = 3, units = "in", res = 300, bg = "white")
grid.draw(apa_tab)
dev.off()

cat("\nPNG gespeichert: ../output/images/korrelation_se02_ki01_tabelle.png\n")

# Zusätzliche Informationen
cat("\n=== ZUSÄTZLICHE INFORMATIONEN ===\n")

cat("\nDeskriptive Statistiken:\n")
cat("SE02 - Mittelwert:", round(mean(cor_results$data$SE02, na.rm=TRUE), 3), "\n")
cat("SE02 - SD:", round(sd(cor_results$data$SE02, na.rm=TRUE), 3), "\n")
cat("KI01 - Mittelwert:", round(mean(cor_results$data$KI01, na.rm=TRUE), 3), "\n")
cat("KI01 - SD:", round(sd(cor_results$data$KI01, na.rm=TRUE), 3), "\n")

cat("\n=== KORRELATIONSANALYSE ERFOLGREICH ERSTELLT ===\n") 