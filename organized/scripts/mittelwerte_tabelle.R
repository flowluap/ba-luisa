# Tabelle mit Mittelwerten der Zielvariablen (MC, MN, VS, EA, ID)
# Basierend auf target_variable_distribution_mc_mn_vs_ea_id.png
# Nur abgeschlossene Daten (FINISHED=1)

library(dplyr)
library(grid)
library(gridExtra)

# Lade Daten
cat("Lade Datensatz_simuliert.csv...\n")
data <- read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-16", check.names=FALSE)

# Filtere nur abgeschlossene Fälle (FINISHED=1)
cat("Filtere abgeschlossene Fälle (FINISHED=1)...\n")
data_valid <- data[data$FINISHED == 1, ]
cat("Anzahl abgeschlossener Fälle:", nrow(data_valid), "\n")

# Trenne nach Gruppen (korrekt)
data_ki <- data_valid[data_valid$AB01 == 1, ]
data_mensch <- data_valid[data_valid$AB01 == 2, ]

cat("Anzahl KI (AB01=1):", nrow(data_ki), "\n")
cat("Anzahl Mensch (AB01=2):", nrow(data_mensch), "\n")

# Erstelle die Mittelwert-Tabelle für alle Zielvariablen
apa_vars <- list(
  MC = "MC01",
  MN = "MN01_",
  VS = "VS01_", 
  EA = "EA01_",
  ID = "ID01_"
)

# Erstelle die Tabelle Schritt für Schritt
apa_results <- list()
for (var_name in names(apa_vars)) {
  prefix <- apa_vars[[var_name]]
  
  # KI Gruppe
  if (var_name == "MC") {
    # MC ist ein einzelnes Item
    ki_data <- data_ki[[prefix]]
  } else {
    # Andere sind Skalen mit mehreren Items
    ki_data <- rowMeans(select(data_ki, starts_with(prefix)), na.rm = TRUE)
  }
  ki_data <- ki_data[!is.na(ki_data)]
  
  # Mensch Gruppe
  if (var_name == "MC") {
    # MC ist ein einzelnes Item
    mensch_data <- data_mensch[[prefix]]
  } else {
    # Andere sind Skalen mit mehreren Items
    mensch_data <- rowMeans(select(data_mensch, starts_with(prefix)), na.rm = TRUE)
  }
  mensch_data <- mensch_data[!is.na(mensch_data)]
  
  if (length(ki_data) == 0 || length(mensch_data) == 0) {
    cat("Keine gültigen Werte für", var_name, "\n")
    next
  }
  
  apa_results[[var_name]] <- data.frame(
    Variable = var_name,
    KI_Mean = round(mean(ki_data), 2),
    Mensch_Mean = round(mean(mensch_data), 2),
    stringsAsFactors = FALSE
  )
}

# Kombiniere alle Ergebnisse
apa_table <- do.call(rbind, apa_results)
rownames(apa_table) <- NULL

# Zeige Tabelle in Konsole
cat("\nMittelwerte der Zielvariablen (n =", nrow(data_valid), "):\n")
print(apa_table)

# Speichere CSV
write.csv(apa_table, "mittelwerte_zielvariablen.csv", row.names = FALSE)
cat("\nCSV gespeichert: mittelwerte_zielvariablen.csv\n")

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
png("../output/images/mittelwerte_zielvariablen.png", width = 900, height = 250, res = 120)
grid::grid.newpage()
grid::grid.draw(apa_tab)
dev.off()

cat("PNG gespeichert: ../output/images/mittelwerte_zielvariablen.png\n")
cat("\n=== MITTELWERTE-TABELLE ERFOLGREICH ERSTELLT ===\n") 