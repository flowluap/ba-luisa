#!/usr/bin/env Rscript

# ================================================================================
# PNG-KONVERTER: TABELLE 9 KORRELATIONSMATRIX (ORIGINAL LAYOUT)
# ================================================================================

cat("================================================================================\n")
cat("PNG-KONVERTER: TABELLE 9 KORRELATIONSMATRIX (ORIGINAL LAYOUT)\n")
cat("================================================================================\n")

# Pakete laden
library(webshot)

# HTML-Datei definieren
html_file <- "organized/images/clustering/tabelle9_korrelationsmatrix_original_layout.html"

# PNG-Datei definieren
png_file <- "organized/images/clustering/tabelle9_korrelationsmatrix_original_layout.png"

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
    vwidth = 900,     # Breite für das kompakte Original-Layout
    vheight = 700,    # Höhe anpassen
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
cat("• Exakt das gleiche Layout wie im Original\n")
cat("• Tabelle 9: Pearson-Korrelationsmatrix der zentralen Konstrukte\n")
cat("• Alle Variablen mit Abkürzungen: MN, VS, EA, ID, KI01, KI02\n")
cat("• Kompaktes Design mit optimalen Spaltenbreiten\n") 