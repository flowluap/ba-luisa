#!/usr/bin/env Rscript

# APA-Tabelle: Cronbach's Alpha, p-Wert, Cohen's d
# Basierend auf Datensatz_simuliert.csv, gefiltert für FINISHED=1

library(dplyr)
library(psych)
library(gridExtra)
library(grid)
library(gtable)

cat("=== APA-TABELLE: CRONBACH'S ALPHA, P-WERT, COHEN'S D ===\n")

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

# Variablen definieren
variables <- list(
  MN = c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07"),
  VS = c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08"),
  EA = c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05"),
  ID = c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
)

# Ergebnisse sammeln
results <- list()

for (var_name in names(variables)) {
  cat("Analysiere", var_name, "...\n")
  
  # Spalten auswählen
  cols <- variables[[var_name]]
  var_data <- data_valid[, cols]
  
  # Zu numerisch konvertieren
  var_data <- sapply(var_data, function(x) as.numeric(as.character(x)))
  
  # Cronbach's Alpha berechnen
  alpha_result <- psych::alpha(var_data, check.keys=FALSE)
  cronbach_alpha <- alpha_result$total$raw_alpha
  
  # Skalenmittelwert berechnen
  scale_mean <- rowMeans(var_data, na.rm=TRUE)
  
  # Gruppen aufteilen
  ki_group <- scale_mean[data_valid$AB01 == 1]
  mensch_group <- scale_mean[data_valid$AB01 == 2]
  
  # t-Test
  t_test <- t.test(ki_group, mensch_group)
  p_value <- t_test$p.value
  
  # Cohen's d berechnen
  pooled_sd <- sqrt(((length(ki_group) - 1) * var(ki_group) + (length(mensch_group) - 1) * var(mensch_group)) / 
                    (length(ki_group) + length(mensch_group) - 2))
  cohens_d <- (mean(ki_group) - mean(mensch_group)) / pooled_sd
  
  # Ergebnisse speichern
  results[[var_name]] <- data.frame(
    Variable = var_name,
    Cronbachs_Alpha = round(cronbach_alpha, 2),
    p_Wert = signif(p_value, 3),
    Cohens_d = round(cohens_d, 2),
    stringsAsFactors = FALSE
  )
}

# Alle Ergebnisse zusammenfassen
apa_table <- do.call(rbind, results)
rownames(apa_table) <- NULL

# Tabelle anzeigen
cat("\nAPA-Tabelle - Cronbach's Alpha, p-Wert, Cohen's d:\n")
print(apa_table)

# CSV speichern
write.csv(apa_table, "apa_alpha_p_d.csv", row.names=FALSE)
cat("CSV gespeichert: apa_alpha_p_d.csv\n")

# APA-konforme Tabelle erstellen
apa_tab <- tableGrob(apa_table, 
                     rows = NULL,
                     theme = ttheme_default(
                       base_size = 12,
                       base_family = "Times",
                       core = list(
                         fg_params = list(hjust = 0.5, x = 0.5, fontsize = 11),
                         bg_params = list(fill = "white")
                       ),
                       colhead = list(
                         fg_params = list(fontface = "bold", hjust = 0.5, x = 0.5, fontsize = 12),
                         bg_params = list(fill = "white")
                       )
                     ))

# Spaltenbreiten anpassen - mehr Platz für Cronbach's Alpha
apa_tab$widths <- unit(c(0.18, 0.32, 0.25, 0.25), "npc")

# Linien APA-Style: oben Kopfzeile, unter letzter Zeile
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

# PNG speichern
png("../output/images/apa_tabelle_alpha_p_d_reduced.png", 
    width = 1200, height = 400, 
    units = "px", res = 300,
    type = "cairo")
grid.draw(apa_tab)
dev.off()

cat("PNG gespeichert: ../output/images/apa_tabelle_alpha_p_d_reduced.png\n")
cat("\n=== APA-TABELLE ALPHA/P/D ERFOLGREICH ERSTELLT ===\n") 