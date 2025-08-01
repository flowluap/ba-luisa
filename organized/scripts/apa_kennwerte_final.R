# Finale APA-Tabelle Kennwerte mit Datensatz_simuliert.csv
# Filtert ungültige Werte (FINISHED=0) heraus und entfernt Shapiro_p Spalte

library(dplyr)
library(e1071)
library(grid)
library(gridExtra)

# Lade Daten
cat("Lade Datensatz_simuliert.csv...\n")
data <- read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-16", check.names=FALSE)

# Filtere nur gültige Fälle (FINISHED=1)
cat("Filtere gültige Fälle (FINISHED=1)...\n")
data_valid <- data[data$FINISHED == 1, ]
cat("Anzahl gültiger Fälle:", nrow(data_valid), "\n")

# Erstelle die Kennwerte-Tabelle
apa_vars <- list(
  MN = rowMeans(select(data_valid, starts_with("MN01_")), na.rm = TRUE),
  VS = rowMeans(select(data_valid, starts_with("VS01_")), na.rm = TRUE),
  EA = rowMeans(select(data_valid, starts_with("EA01_")), na.rm = TRUE),
  ID = rowMeans(select(data_valid, starts_with("ID01_")), na.rm = TRUE),
  KI01 = rowMeans(select(data_valid, starts_with("KI01_")), na.rm = TRUE),
  KI02 = rowMeans(select(data_valid, starts_with("KI02_")), na.rm = TRUE)
)

# Erstelle die Tabelle Schritt für Schritt
apa_results <- list()
for (var_name in names(apa_vars)) {
  x <- apa_vars[[var_name]]
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    cat("Keine gültigen Werte für", var_name, "\n")
    next
  }
  
  # Shapiro-Wilk Test für Normalverteilung
  shapiro_test <- shapiro.test(x)
  
  apa_results[[var_name]] <- data.frame(
    Variable = var_name,
    SD = round(sd(x), 2),
    Schiefe = round(e1071::skewness(x, na.rm=TRUE, type=2), 2),
    Kurtosis = round(e1071::kurtosis(x, na.rm=TRUE, type=2), 2),
    Shapiro_W = round(shapiro_test$statistic, 3),
    stringsAsFactors = FALSE
  )
}

# Kombiniere alle Ergebnisse
apa_table <- do.call(rbind, apa_results)
rownames(apa_table) <- NULL

# Zeige Tabelle in Konsole
cat("\nAPA-Tabelle Kennwerte (n =", nrow(data_valid), "):\n")
print(apa_table)

# Speichere CSV
write.csv(apa_table, "apa_tabelle_kennwerte_final.csv", row.names = FALSE)
cat("\nCSV gespeichert: apa_tabelle_kennwerte_final.csv\n")

# Erstelle APA-Tabelle als PNG
apa_tab <- tableGrob(
  apa_table,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 14),
      bg_params = list(fill = NA)
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 15, fontface = 2),
      bg_params = list(fill = NA)
    ),
    padding = unit(c(8, 4), "mm")
  )
)

# Linien APA-Style: oben Kopfzeile, unter Kopfzeile, unter letzter Zeile
apa_tab <- gtable::gtable_add_grob(
  apa_tab,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 2)),
  t = 1, b = 1, l = 1, r = ncol(apa_tab)
)
apa_tab <- gtable::gtable_add_grob(
  apa_tab,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
  t = nrow(apa_tab), b = nrow(apa_tab), l = 1, r = ncol(apa_tab)
)

# Speichere PNG
png("../output/images/apa_tabelle_kennwerte.png", width = 800, height = 300, res = 120)
grid::grid.newpage()
grid::grid.draw(apa_tab)
dev.off()

cat("PNG gespeichert: ../output/images/apa_tabelle_kennwerte.png\n")
cat("\n=== APA-TABELLE KENNWERTE ERFOLGREICH ERSTELLT ===\n") 