#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 6: HTML ZU PNG KONVERTIEREN
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 6: HTML ZU PNG KONVERTIEREN\n")
cat("================================================================================\n")

# Pakete laden
library(webshot)

# HTML-Datei zu PNG konvertieren
html_file <- "organized/images/clustering/tabelle6_effektstaerke_ea_id.html"
png_file <- "organized/images/clustering/tabelle6_effektstaerke_ea_id.png"

cat("Konvertiere:", html_file, "\n")
cat("Zu:", png_file, "\n")

# Konvertierung durchführen
webshot(html_file, png_file, 
         vwidth = 800, 
         vheight = 500, 
         zoom = 2)

cat("✓ Konvertierung erfolgreich!\n")
cat("PNG-Datei erstellt:", png_file, "\n")

# Dateigröße anzeigen
if(file.exists(png_file)) {
  file_size <- file.size(png_file) / 1024
  cat("Dateigröße:", round(file_size, 1), "KB\n")
}

cat("\n================================================================================\n")
cat("TABELLE 6 ERFOLGREICH ZU PNG KONVERTIERT\n")
cat("================================================================================\n") 