# =============================================================================
# TABELLE 4 ZU PNG KONVERTIEREN
# =============================================================================

library(webshot)

cat("================================================================================\n")
cat("TABELLE 4: HTML ZU PNG KONVERTIEREN\n")
cat("================================================================================\n")

# HTML-Datei zu PNG konvertieren
html_file <- "organized/images/clustering/tabelle4_effektstaerke_mn.html"
png_file <- "organized/images/clustering/tabelle4_effektstaerke_mn.png"

cat("Konvertiere:", html_file, "\n")
cat("Zu:", png_file, "\n")

# Konvertierung durchführen
webshot(html_file, png_file, 
         vwidth = 900, 
         vheight = 500,
         zoom = 2)

cat("✓ Konvertierung erfolgreich!\n")
cat("PNG-Datei erstellt:", png_file, "\n")

# Dateigröße anzeigen
if (file.exists(png_file)) {
  file_size <- file.size(png_file)
  cat("Dateigröße:", round(file_size / 1024, 1), "KB\n")
}

cat("\n================================================================================\n")
cat("TABELLE 4 ERFOLGREICH ZU PNG KONVERTIERT\n")
cat("================================================================================\n") 