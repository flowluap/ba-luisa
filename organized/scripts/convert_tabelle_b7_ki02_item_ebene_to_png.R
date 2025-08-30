#!/usr/bin/env Rscript

# ================================================================================
# PNG-KONVERTER FÜR TABELLE B7: KI02 ITEM-EBENE
# ================================================================================

cat("================================================================================\n")
cat("PNG-KONVERTER: TABELLE B7 (KI02 ITEM-EBENE)\n")
cat("================================================================================\n\n")

# Bibliotheken laden
library(webshot)

# Dateipfade definieren
html_file <- "organized/images/clustering/tabelle_b7_ki02_item_ebene.html"
png_file <- "organized/images/clustering/tabelle_b7_ki02_item_ebene.png"

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
  webshot(html_file, png_file,
           vwidth = 1500,  # Increased significantly to ensure all columns are visible in PNG
           vheight = 400,
           zoom = 2)
  
  cat("✓ PNG erfolgreich erstellt\n")
  cat("Datei:", normalizePath(png_file), "\n")
  
  # Dateigröße anzeigen
  file_size <- file.size(png_file)
  cat("Größe:", round(file_size / 1024, 1), "KB\n")
  
}, error = function(e) {
  cat("❌ FEHLER beim Erstellen der PNG:\n")
  cat(e$message, "\n")
  quit(status = 1)
})

cat("\n================================================================================\n")
cat("PNG-KONVERTER ABGESCHLOSSEN\n")
cat("================================================================================\n") 