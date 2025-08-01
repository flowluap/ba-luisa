# Tabelle mit Mittelwerten und Konfidenzintervallen der Zielvariablen (MC, MN, VS, EA, ID)
# Basierend auf mittelwerte_zielvariablen.png mit zusätzlichen Konfidenzintervallen
# Nur abgeschlossene Daten (FINISHED=1)

library(dplyr)
library(grid)
library(gridExtra)

# Lade Daten
cat("Lade Datensatz_simuliert.csv...\n")
data <- tryCatch({
  read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-16", check.names=FALSE)
}, error = function(e) {
  cat("Fehler beim Laden mit UTF-16, versuche UTF-8-BOM...\n")
  read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-8-BOM", check.names=FALSE)
})

# Filtere nur abgeschlossene Fälle (FINISHED=1)
cat("Filtere abgeschlossene Fälle (FINISHED=1)...\n")
data_valid <- data[data$FINISHED == 1, ]
cat("Anzahl abgeschlossener Fälle:", nrow(data_valid), "\n")

# Trenne nach Gruppen (korrekt)
data_ki <- data_valid[data_valid$AB01 == 1, ]
data_mensch <- data_valid[data_valid$AB01 == 2, ]

cat("Anzahl KI (AB01=1):", nrow(data_ki), "\n")
cat("Anzahl Mensch (AB01=2):", nrow(data_mensch), "\n")

# Funktion für Konfidenzintervall
ci_function <- function(x, conf.level = 0.95) {
  if (length(x) < 2) return(c(NA, NA))
  t_result <- t.test(x, conf.level = conf.level)
  return(c(t_result$conf.int[1], t_result$conf.int[2]))
}

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
  
  # Berechne Konfidenzintervalle
  ki_ci <- ci_function(ki_data)
  mensch_ci <- ci_function(mensch_data)
  
  apa_results[[var_name]] <- data.frame(
    Variable = var_name,
    KI_Mean = round(mean(ki_data), 2),
    KI_CI = paste0("[", round(ki_ci[1], 2), ", ", round(ki_ci[2], 2), "]"),
    Mensch_Mean = round(mean(mensch_data), 2),
    Mensch_CI = paste0("[", round(mensch_ci[1], 2), ", ", round(mensch_ci[2], 2), "]"),
    stringsAsFactors = FALSE
  )
}

# Kombiniere alle Ergebnisse
apa_table <- do.call(rbind, apa_results)
rownames(apa_table) <- NULL

# Zeige Tabelle in Konsole
cat("\nMittelwerte und 95% Konfidenzintervalle der Zielvariablen (n =", nrow(data_valid), "):\n")
print(apa_table)

# Speichere CSV
write.csv(apa_table, "mittelwerte_zielvariablen_ci.csv", row.names = FALSE)
cat("\nCSV gespeichert: mittelwerte_zielvariablen_ci.csv\n")

# Erstelle APA-Tabelle als PNG
apa_tab <- tableGrob(
  apa_table,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 11),
      bg_params = list(fill = NA)
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 12, fontface = 2),
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
png("../output/images/mittelwerte_zielvariablen_ci.png", width = 1200, height = 300, res = 120)
grid::grid.newpage()
grid::grid.draw(apa_tab)
dev.off()

cat("PNG gespeichert: ../output/images/mittelwerte_zielvariablen_ci.png\n")
cat("\n=== MITTELWERTE-TABELLE MIT KONFIDENZINTERVALLEN ERFOLGREICH ERSTELLT ===\n") 