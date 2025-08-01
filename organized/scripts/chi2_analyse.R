#!/usr/bin/env Rscript

# Chi²-Analyse: Alter, Geschlecht, SM-Plattform (SE03)
# Basierend auf Datensatz_simuliert.csv, gefiltert für FINISHED=1

library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

cat("=== CHI²-ANALYSE: ALTER, GESCHLECHT, SM-PLATTFORM ===\n")

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

# Variablen vorbereiten
cat("Bereite Variablen vor...\n")

# Alter kategorisieren (SE03: 1=18-25, 2=26-35, 3=36-45, 4=46-55, 5=56+)
data_valid$Alter_Kat <- factor(data_valid$SE03, 
                               levels = 1:5,
                               labels = c("18-25", "26-35", "36-45", "46-55", "56+"))

# Geschlecht (SE02: 1=Männlich, 2=Weiblich, 3=Divers, 4=Andere, 5=Keine Angabe)
data_valid$Geschlecht <- factor(data_valid$SE02,
                                levels = 1:5,
                                labels = c("Männlich", "Weiblich", "Divers", "Andere", "Keine Angabe"))

# SM-Plattform (SE03_01 bis SE03_07 sind verschiedene Plattformen)
sm_cols <- c("SE03_01", "SE03_02", "SE03_03", "SE03_04", "SE03_05", "SE03_06", "SE03_07")
data_valid$SM_Plattform <- "Keine"

# Prüfe welche Plattformen genutzt werden
for (i in 1:nrow(data_valid)) {
  sm_usage <- sapply(sm_cols, function(col) {
    val <- as.numeric(as.character(data_valid[i, col]))
    return(!is.na(val) && val > 0)
  })
  
  if (sum(sm_usage) > 0) {
    used_platforms <- names(sm_usage)[sm_usage]
    if (length(used_platforms) == 1) {
      data_valid$SM_Plattform[i] <- used_platforms[1]
    } else {
      data_valid$SM_Plattform[i] <- "Mehrere"
    }
  }
}

# Chi²-Tests durchführen
cat("Führe Chi²-Tests durch...\n")

# 1. Alter vs Geschlecht
chi2_alter_geschlecht <- chisq.test(table(data_valid$Alter_Kat, data_valid$Geschlecht))

# 2. Alter vs SM-Plattform
chi2_alter_sm <- chisq.test(table(data_valid$Alter_Kat, data_valid$SM_Plattform))

# 3. Geschlecht vs SM-Plattform
chi2_geschlecht_sm <- chisq.test(table(data_valid$Geschlecht, data_valid$SM_Plattform))

# Ergebnisse sammeln
results <- data.frame(
  Vergleich = c("Alter vs Geschlecht", "Alter vs SM-Plattform", "Geschlecht vs SM-Plattform"),
  Chi2_Wert = c(chi2_alter_geschlecht$statistic, chi2_alter_sm$statistic, chi2_geschlecht_sm$statistic),
  df = c(chi2_alter_geschlecht$parameter, chi2_alter_sm$parameter, chi2_geschlecht_sm$parameter),
  p_Wert = c(chi2_alter_geschlecht$p.value, chi2_alter_sm$p.value, chi2_geschlecht_sm$p.value),
  stringsAsFactors = FALSE
)

# Werte runden
results$Chi2_Wert <- round(results$Chi2_Wert, 3)
results$p_Wert <- signif(results$p_Wert, 3)

# Tabelle anzeigen
cat("\nChi²-Analyse Ergebnisse:\n")
print(results)

# CSV speichern
write.csv(results, "chi2_analyse.csv", row.names=FALSE)
cat("CSV gespeichert: chi2_analyse.csv\n")

# APA-konforme Tabelle erstellen
apa_tab <- tableGrob(results, 
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

# Spaltenbreiten anpassen
apa_tab$widths <- unit(c(0.35, 0.2, 0.15, 0.3), "npc")

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
png("../output/images/chi2_analyse_tabelle.png", 
    width = 1200, height = 400, 
    units = "px", res = 300,
    type = "cairo")
grid.draw(apa_tab)
dev.off()

cat("PNG gespeichert: ../output/images/chi2_analyse_tabelle.png\n")

# Zusätzliche Informationen
cat("\n=== ZUSÄTZLICHE INFORMATIONEN ===\n")
cat("Alter Verteilung:\n")
print(table(data_valid$Alter_Kat))

cat("\nGeschlecht Verteilung:\n")
print(table(data_valid$Geschlecht))

cat("\nSM-Plattform Verteilung:\n")
print(table(data_valid$SM_Plattform))

cat("\n=== CHI²-ANALYSE ERFOLGREICH ERSTELLT ===\n") 