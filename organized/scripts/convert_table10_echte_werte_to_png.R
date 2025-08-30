#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 10: HTML ZU PNG KONVERTIEREN (ECHTE WERTE)
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 10: HTML ZU PNG KONVERTIEREN (ECHTE WERTE)\n")
cat("================================================================================\n")

# Pakete laden
library(webshot)

# HTML-Datei zu PNG konvertieren
html_file <- "organized/images/clustering/tabelle10_korrelationsmatrix_soziodem_echte_werte.html"
png_file <- "organized/images/clustering/tabelle10_korrelationsmatrix_soziodem_echte_werte.png"

cat("Konvertiere:", html_file, "\n")
cat("Zu:", png_file, "\n")

# Konvertierung durchführen
webshot(html_file, png_file,
         vwidth = 900,
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
cat("TABELLE 10 MIT ECHTEN WERTEN ERFOLGREICH ZU PNG KONVERTIERT\n")
cat("================================================================================\n") 