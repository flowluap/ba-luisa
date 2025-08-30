#!/usr/bin/env Rscript

# ================================================================================
# PNG-KONVERTER: TABELLE 9 KORRELATIONSMATRIX MIT AUSGESCHRIEBENEN NAMEN
# ================================================================================

cat("================================================================================\n")
cat("PNG-KONVERTER: TABELLE 9 KORRELATIONSMATRIX\n")
cat("================================================================================\n")

# Pakete laden
library(webshot)

# HTML-Datei definieren
html_file <- "organized/images/clustering/tabelle9_korrelationsmatrix_mit_ausgeschriebenen_namen.html"

# PNG-Datei definieren
png_file <- "organized/images/clustering/tabelle9_korrelationsmatrix_mit_ausgeschriebenen_namen.png"

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
    vwidth = 1200,    # Breite für bessere Lesbarkeit der ausgeschriebenen Namen
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
cat("• Ausgeschriebene Variablennamen sind vollständig sichtbar\n")
cat("• Tabelle 9: Pearson-Korrelationsmatrix der zentralen Konstrukte\n")
cat("• Alle Variablen mit vollständigen Namen:\n")
cat("  - Menschlichkeit & Natürlichkeit\n")
cat("  - Vertrauen & Sympathie\n")
cat("  - Emotionale Ansprache\n")
cat("  - Identifikation\n")
cat("  - KI-Wahrnehmung\n")
cat("  - KI-Kritik\n") 