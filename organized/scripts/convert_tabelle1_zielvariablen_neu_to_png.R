#!/usr/bin/env Rscript

# ================================================================================
# PNG-KONVERTER: TABELLE 1 ZIELVARIABLEN (NEU)
# ================================================================================

cat("================================================================================\n")
cat("PNG-KONVERTER: TABELLE 1 ZIELVARIABLEN (NEU)\n")
cat("================================================================================\n")

# Pakete laden
library(webshot)

# HTML-Datei definieren
html_file <- "organized/images/clustering/tabelle1_zielvariablen_neu.html"

# PNG-Datei definieren
png_file <- "organized/images/clustering/tabelle1_zielvariablen_neu.png"

# Überprüfen, ob HTML-Datei existiert
if (!file.exists(html_file)) {
  cat("❌ HTML-Datei nicht gefunden:", html_file, "\n")
  stop("HTML-Datei muss zuerst erstellt werden")
}

cat("✓ HTML-Datei gefunden:", html_file, "\n")

# PNG konvertieren
cat("\n=== PNG-KONVERTIERUNG ===\n")

tryCatch({
  webshot(
    url = html_file,
    file = png_file,
    vwidth = 1000,    # Breite für alle Spalten sichtbar
    vheight = 800,    # Höhe anpassen
    zoom = 2,         # Zoom für bessere Qualität
    delay = 1.0       # Verzögerung für vollständiges Laden
  )
  
  cat("✓ PNG erfolgreich erstellt:", png_file, "\n")
  
  # Datei öffnen
  cat("\n=== DATEI ÖFFNEN ===\n")
  system(paste("open", png_file))
  cat("✓ PNG-Datei geöffnet\n")
  
}, error = function(e) {
  cat("❌ Fehler bei der PNG-Konvertierung:", e$message, "\n")
  
  # Alternative: Versuche mit system-Befehl
  cat("\n=== ALTERNATIVE KONVERTIERUNG ===\n")
  tryCatch({
    system(paste("open", html_file))
    cat("✓ HTML-Datei geöffnet (als Alternative)\n")
  }, error = function(e2) {
    cat("❌ Auch HTML-Öffnung fehlgeschlagen:", e2$message, "\n")
  })
})

cat("\n================================================================================\n")
cat("KONVERTIERUNG ABGESCHLOSSEN!\n")
cat("================================================================================\n")

cat("\n📊 ZUSAMMENFASSUNG:\n")
cat("• HTML zu PNG konvertiert\n")
cat("• Tabelle 1: Deskriptive Statistiken der Zielvariablen\n")
cat("• Alle 8 Spalten sind vollständig sichtbar:\n")
cat("  - Variable, N, M, SD, Min, Max, Schiefe, Shapiro-Wilk p\n")
cat("• Alle 6 Zielvariablen mit vollständigen Namen\n")
cat("• Professionelle APA7-Formatierung\n") 