#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 9 KORRIGIERT: HTML ZU PNG KONVERTIEREN
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 9 KORRIGIERT: HTML ZU PNG KONVERTIEREN\n")
cat("================================================================================\n")

# Pakete laden
library(webshot)

# HTML-Datei zu PNG konvertieren
html_file <- "organized/images/clustering/tabelle9_korrelationsmatrix_korrigiert.html"
png_file <- "organized/images/clustering/tabelle9_korrelationsmatrix_korrigiert.png"

cat("Konvertiere:", html_file, "\n")
cat("Zu:", png_file, "\n")

# Konvertierung durchführen
webshot(html_file, png_file,
         vwidth = 800,
         vheight = 600,
         zoom = 2)

cat("✓ Konvertierung erfolgreich!\n")
cat("PNG-Datei erstellt:", png_file, "\n")

# Dateigröße anzeigen
if(file.exists(png_file)) {
  file_size <- file.size(png_file) / 1024
  cat("Dateigröße:", round(file_size, 1), "KB\n")
}

cat("\n================================================================================\n")
cat("TABELLE 9 KORRIGIERT ERFOLGREICH ZU PNG KONVERTIERT\n")
cat("================================================================================\n") 