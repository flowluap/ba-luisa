# ================================================================================
# TABELLE 11: PNG-KONVERTIERUNG
# Interaktionseffekte von Vorwissen auf die Zielvariablen
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 11: PNG-KONVERTIERUNG STARTET\n")
cat("================================================================================\n")

# Benötigte Pakete laden
if (!require(webshot)) {
  install.packages("webshot")
  library(webshot)
}

cat("✓ Pakete geladen\n")

# HTML-Datei einlesen
html_file <- "organized/images/clustering/tabelle11_interaktionseffekte_vorwissen_groessere_schrift.html"

if (!file.exists(html_file)) {
  stop("❌ HTML-Datei nicht gefunden:", html_file)
}

cat("✓ HTML-Datei gefunden:", html_file, "\n")

# PNG-Datei konvertieren
png_file <- "organized/images/clustering/tabelle11_interaktionseffekte_vorwissen_groessere_schrift.png"

cat("\n=== PNG-KONVERTIERUNG ===\n")

tryCatch({
  webshot(
    html_file,
    png_file,
    vwidth = 1000,    # Breite für optimale Darstellung
    vheight = 800,    # Höhe für optimale Darstellung
    zoom = 2,         # Zoom für bessere Qualität
    delay = 1.0       # Verzögerung für korrekte Darstellung
  )
  
  cat("✓ PNG erfolgreich erstellt:", png_file, "\n")
  
  # Datei-Informationen anzeigen
  if (file.exists(png_file)) {
    file_info <- file.info(png_file)
    cat("✓ Dateigröße:", round(file_info$size / 1024, 2), "KB\n")
    cat("✓ Erstellt:", file_info$mtime, "\n")
  }
  
}, error = function(e) {
  cat("❌ Fehler bei der PNG-Konvertierung:", e$message, "\n")
})

cat("\n================================================================================\n")
cat("PNG-KONVERTIERUNG ABGESCHLOSSEN!\n")
cat("================================================================================\n")

cat("\n📊 ZUSAMMENFASSUNG:\n")
cat("• HTML zu PNG konvertiert\n")
cat("• Optimale Einstellungen für Lesbarkeit:\n")
cat("  - Breite: 1000px\n")
cat("  - Höhe: 800px\n")
cat("  - Zoom: 2x\n")
cat("  - Verzögerung: 1.0s\n")
cat("• PNG-Datei gespeichert in:", png_file, "\n")
cat("• Tabelle 11 ist jetzt als Bild verfügbar!\n") 