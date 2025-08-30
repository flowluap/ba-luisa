# =============================================================================
# TABELLE 3 ZU PNG KONVERTIEREN
# =============================================================================

library(webshot)

cat("================================================================================\n")
cat("TABELLE 3: HTML ZU PNG KONVERTIEREN\n")
cat("================================================================================\n")

# HTML-Datei zu PNG konvertieren
html_file <- "organized/images/clustering/tabelle3_reliabilitaet_corrected.html"
png_file <- "organized/images/clustering/tabelle3_reliabilitaet_corrected.png"

cat("Konvertiere:", html_file, "\n")
cat("Zu:", png_file, "\n")

# Konvertierung durchführen
webshot(html_file, png_file, 
         vwidth = 600, 
         vheight = 400,
         zoom = 2)

cat("✓ Konvertierung erfolgreich!\n")
cat("PNG-Datei erstellt:", png_file, "\n")

# Dateigröße anzeigen
if (file.exists(png_file)) {
  file_size <- file.size(png_file)
  cat("Dateigröße:", round(file_size / 1024, 1), "KB\n")
}

cat("\n================================================================================\n")
cat("TABELLE 3 ERFOLGREICH ZU PNG KONVERTIERT\n")
cat("================================================================================\n") 