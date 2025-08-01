# APA-Tabelle Min/Max-Werte getrennt nach Mensch und KI
# Filtert ungültige Werte (FINISHED=0) heraus und unterscheidet AB01

library(dplyr)
library(grid)
library(gridExtra)

# Lade Daten
cat("Lade Datensatz_simuliert.csv...\n")
data <- read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-16", check.names=FALSE)

# Filtere nur gültige Fälle (FINISHED=1)
cat("Filtere gültige Fälle (FINISHED=1)...\n")
data_valid <- data[data$FINISHED == 1, ]
cat("Anzahl gültiger Fälle:", nrow(data_valid), "\n")

# Trenne nach Gruppen
data_ki <- data_valid[data_valid$AB01 == 1, ]
data_mensch <- data_valid[data_valid$AB01 == 2, ]

cat("Anzahl KI (AB01=1):", nrow(data_ki), "\n")
cat("Anzahl Mensch (AB01=2):", nrow(data_mensch), "\n")

# Erstelle die Min/Max-Tabelle getrennt nach Gruppen
apa_vars <- list(
  MN = "MN01_",
  VS = "VS01_", 
  EA = "EA01_",
  ID = "ID01_",
  KI01 = "KI01_",
  KI02 = "KI02_"
)

# Erstelle die Tabelle Schritt für Schritt
apa_results <- list()
for (var_name in names(apa_vars)) {
  prefix <- apa_vars[[var_name]]
  
  # KI Gruppe
  ki_data <- rowMeans(select(data_ki, starts_with(prefix)), na.rm = TRUE)
  ki_data <- ki_data[!is.na(ki_data)]
  
  # Mensch Gruppe
  mensch_data <- rowMeans(select(data_mensch, starts_with(prefix)), na.rm = TRUE)
  mensch_data <- mensch_data[!is.na(mensch_data)]
  
  if (length(mensch_data) == 0 || length(ki_data) == 0) {
    cat("Keine gültigen Werte für", var_name, "\n")
    next
  }
  
  apa_results[[var_name]] <- data.frame(
    Variable = var_name,
    Mensch_Min = round(min(mensch_data), 2),
    KI_Min = round(min(ki_data), 2),
    Mensch_Max = round(max(mensch_data), 2),
    KI_Max = round(max(ki_data), 2),
    stringsAsFactors = FALSE
  )
}

# Kombiniere alle Ergebnisse
apa_table <- do.call(rbind, apa_results)
rownames(apa_table) <- NULL

# Zeige Tabelle in Konsole
cat("\nAPA-Tabelle Min/Max nach Gruppen:\n")
cat("KI (AB01=1): n =", nrow(data_ki), "\n")
cat("Mensch (AB01=2): n =", nrow(data_mensch), "\n\n")
print(apa_table)

# Speichere CSV
write.csv(apa_table, "apa_tabelle_min_max_gruppiert.csv", row.names = FALSE)
cat("\nCSV gespeichert: apa_tabelle_min_max_gruppiert.csv\n")

# Erstelle APA-Tabelle als PNG
apa_tab <- tableGrob(
  apa_table,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 12),
      bg_params = list(fill = NA)
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 13, fontface = 2),
      bg_params = list(fill = NA)
    ),
    padding = unit(c(6, 3), "mm")
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
png("../output/images/apa_tabelle_min_max_gruppiert.png", width = 800, height = 250, res = 120)
grid::grid.newpage()
grid::grid.draw(apa_tab)
dev.off()

cat("PNG gespeichert: ../output/images/apa_tabelle_min_max_gruppiert.png\n")
cat("\n=== APA-TABELLE MIN/MAX GRUPPIERT ERFOLGREICH ERSTELLT ===\n") 