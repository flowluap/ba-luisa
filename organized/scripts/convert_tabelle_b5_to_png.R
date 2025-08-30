#!/usr/bin/env Rscript

# ================================================================================
# TABELLE B5: HTML ZU PNG KONVERTIEREN
# ================================================================================

cat("================================================================================\n")
cat("TABELLE B5: HTML ZU PNG KONVERTIEREN\n")
cat("================================================================================\n")

# Pakete laden
library(webshot)

# HTML-Datei zu PNG konvertieren
html_file <- "organized/images/clustering/tabelle_b5_id_item_ebene.html"
png_file <- "organized/images/clustering/tabelle_b5_id_item_ebene.png"

cat("Konvertiere:", html_file, "\n")
cat("Zu:", png_file, "\n")

# Konvertierung durchführen
webshot(html_file, png_file,
         vwidth = 500,
         vheight = 400,
         zoom = 2)

cat("✓ Konvertierung erfolgreich!\n")
cat("PNG-Datei erstellt:", png_file, "\n")

# Dateigröße anzeigen
if(file.exists(png_file)) {
  file_size <- file.size(png_file) / 1024
  cat("Dateigröße:", round(file_size, 1), "KB\n")
}

cat("\n================================================================================\n")
cat("TABELLE B5 ERFOLGREICH ZU PNG KONVERTIERT\n")
cat("================================================================================\n") 