#!/usr/bin/env Rscript

# ================================================================================
# PNG-KONVERTER FÜR TABELLE B9: LINEARE REGRESSIONSKOEFFIZIENTEN
# ================================================================================

cat("================================================================================\n")
cat("PNG-KONVERTER: TABELLE B9 (LINEARE REGRESSIONSKOEFFIZIENTEN)\n")
cat("================================================================================\n\n")

# Bibliotheken laden
library(webshot)

# Dateipfade definieren
html_file <- "organized/images/clustering/tabelle_b9_regression_koeffizienten.html"
png_file <- "organized/images/clustering/tabelle_b9_regression_koeffizienten.png"

cat("=== DATEIEN ÜBERPRÜFEN ===\n")
cat("HTML-Datei:", html_file, "\n")
cat("Ziel-PNG:", png_file, "\n")

# Überprüfen, ob HTML-Datei existiert
if (!file.exists(html_file)) {
  cat("❌ FEHLER: HTML-Datei nicht gefunden!\n")
  cat("Pfad:", normalizePath(html_file), "\n")
  quit(status = 1)
}

cat("✓ HTML-Datei gefunden\n")

# PNG erstellen
cat("\n=== PNG ERSTELLEN ===\n")
tryCatch({
  # Lösche alte PNG falls vorhanden
  if (file.exists(png_file)) {
    file.remove(png_file)
    cat("✓ Alte PNG-Datei gelöscht\n")
  }
  
  # Erstelle neue PNG mit optimierten Einstellungen
  webshot(html_file, png_file,
           vwidth = 1000,  # Erhöht für bessere Darstellung
           vheight = 1000,  # Angepasst an Tabellenhöhe
           zoom = 2,
           delay = 1.0)  # Längere Verzögerung für bessere Qualität
  
  cat("✓ PNG erfolgreich erstellt\n")
  cat("Datei:", normalizePath(png_file), "\n")
  
  # Dateigröße anzeigen
  file_size <- file.size(png_file)
  cat("Größe:", round(file_size / 1024, 1), "KB\n")
  
  # Überprüfe, ob PNG-Datei lesbar ist
  if (file.exists(png_file) && file.size(png_file) > 0) {
    cat("✓ PNG-Datei ist lesbar und hat Inhalt\n")
    
    # Versuche die Datei zu öffnen
    cat("Öffne PNG-Datei...\n")
    system(paste("open", png_file))
    
    # Warte kurz und überprüfe nochmal
    Sys.sleep(2)
    if (file.exists(png_file)) {
      cat("✓ PNG-Datei erfolgreich erstellt und geöffnet\n")
    }
    
  } else {
    cat("❌ PNG-Datei ist leer oder nicht lesbar\n")
  }
  
}, error = function(e) {
  cat("❌ FEHLER beim Erstellen der PNG:\n")
  cat(e$message, "\n")
  quit(status = 1)
})

cat("\n================================================================================\n")
cat("PNG-KONVERTER ABGESCHLOSSEN\n")
cat("================================================================================\n") 