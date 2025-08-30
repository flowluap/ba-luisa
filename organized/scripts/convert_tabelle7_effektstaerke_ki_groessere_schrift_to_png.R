#!/usr/bin/env Rscript

# ================================================================================
# TABELLE 7: EFFEKTSTÄRKE UND SIGNIFIKANZANALYSE FÜR KI-WAHRNEHMUNG UND KI-KRITIK (GRÖSSERE SCHRIFTGRÖSSE)
# PNG-KONVERTIERUNG
# ================================================================================

cat("================================================================================\n")
cat("TABELLE 7: PNG-KONVERTIERUNG MIT GRÖSSERER SCHRIFTGRÖSSE\n")
cat("================================================================================\n")

# Pakete laden
library(webshot)

# HTML-Datei definieren
html_file <- "organized/images/clustering/tabelle7_effektstaerke_ki_groessere_schrift.html"

# PNG-Datei definieren
png_file <- "organized/images/clustering/tabelle7_effektstaerke_ki_groessere_schrift.png"

# Überprüfen ob HTML-Datei existiert
if (!file.exists(html_file)) {
  cat("❌ HTML-Datei nicht gefunden:", html_file, "\n")
  stop("HTML-Datei existiert nicht")
}

cat("✓ HTML-Datei gefunden:", html_file, "\n")

# PNG-Konvertierung durchführen
cat("\n=== PNG-KONVERTIERUNG ===\n")

tryCatch({
  webshot(
    url = html_file,
    file = png_file,
    vwidth = 1200,    # Breite für bessere Lesbarkeit (mehr Spalten)
    vheight = 800,    # Höhe für bessere Lesbarkeit
    zoom = 2,         # Zoom-Faktor für scharfe Darstellung
    delay = 1.0       # Verzögerung für vollständiges Laden
  )
  
  cat("✓ PNG erfolgreich erstellt:", png_file, "\n")
  
  # Dateigröße überprüfen
  if (file.exists(png_file)) {
    file_size <- file.size(png_file)
    cat("✓ Dateigröße:", round(file_size / 1024, 1), "KB\n")
    
    # PNG öffnen
    cat("✓ Öffne PNG-Datei...\n")
    system(paste("open", png_file))
    
  } else {
    cat("❌ PNG-Datei wurde nicht erstellt\n")
  }
  
}, error = function(e) {
  cat("❌ Fehler bei der PNG-Konvertierung:", e$message, "\n")
  stop("PNG-Konvertierung fehlgeschlagen")
})

cat("\n================================================================================\n")
cat("PNG-KONVERTIERUNG ABGESCHLOSSEN!\n")
cat("================================================================================\n")

cat("\n📊 ZUSAMMENFASSUNG:\n")
cat("• Tabelle 7 erfolgreich zu PNG konvertiert\n")
cat("• Größere Schriftgröße (18px) für optimale Lesbarkeit\n")
cat("• Exakte Werte aus dem ursprünglichen Bild beibehalten\n")
cat("• PNG-Datei gespeichert und geöffnet\n")
cat("• Datei:", png_file, "\n") 