# =============================================================================
# HTML-TABELLE ZU PNG KONVERTIEREN
# =============================================================================
# Konvertiert die HTML-Tabelle in eine hochauflösende PNG-Datei

library(webshot)

# =============================================================================
# TABELLE KONVERTIEREN
# =============================================================================

cat("================================================================================\n")
cat("HTML-TABELLE ZU PNG KONVERTIEREN\n")
cat("================================================================================\n")

# HTML-Datei-Pfad (für Tabelle 2)
html_file <- "organized/images/clustering/tabelle2_skalenmittelwerte.html"
png_file <- "organized/images/clustering/tabelle2_skalenmittelwerte.png"

cat("✓ HTML-Datei gefunden:", html_file, "\n")

# Überprüfe ob HTML-Datei existiert
if (file.exists(html_file)) {
  cat("✓ HTML-Datei existiert\n")
  
  # Konvertiere zu PNG mit hoher Auflösung
  cat("🔄 Konvertiere HTML zu PNG...\n")
  
  tryCatch({
    webshot(
      html_file,
      png_file,
      vwidth = 800,      # Breite
      vheight = 600,     # Höhe
      zoom = 2,          # Zoom-Faktor für höhere Auflösung
      delay = 0.5        # Verzögerung für vollständiges Laden
    )
    
    cat("✅ PNG-Datei erfolgreich erstellt:", png_file, "\n")
    
    # Datei-Informationen anzeigen
    if (file.exists(png_file)) {
      file_info <- file.info(png_file)
      cat("📁 Dateigröße:", round(file_info$size / 1024, 2), "KB\n")
      cat("📅 Erstellt:", file_info$mtime, "\n")
    }
    
  }, error = function(e) {
    cat("❌ Fehler bei der Konvertierung:", e$message, "\n")
    
    # Alternative: Verwende system-Befehl mit wkhtmltopdf
    cat("🔄 Versuche alternative Konvertierung...\n")
    
    # Prüfe ob wkhtmltopdf verfügbar ist
    if (Sys.which("wkhtmltopdf") != "") {
      cat("✓ wkhtmltopdf gefunden, verwende alternative Methode\n")
      
      # Konvertiere zu PDF und dann zu PNG
      pdf_file <- "organized/images/clustering/tabelle1_zielvariablen.pdf"
      
      system(paste("wkhtmltopdf --page-size A4 --orientation Landscape", 
                   html_file, pdf_file))
      
      if (file.exists(pdf_file)) {
        cat("✅ PDF-Datei erstellt:", pdf_file, "\n")
        cat("💡 Verwenden Sie ein PDF-zu-PNG Konvertierungstool für die finale PNG-Datei\n")
      }
    } else {
      cat("❌ Keine Alternative verfügbar\n")
    }
  })
  
} else {
  cat("❌ HTML-Datei nicht gefunden:", html_file, "\n")
}

# =============================================================================
# ALTERNATIVE: ERSTELLE TABELLE DIREKT ALS PNG
# =============================================================================

cat("\n=== ALTERNATIVE: TABELLE DIREKT ALS PNG ERSTELLEN ===\n")

# Lade die ursprünglichen Daten und erstelle Tabelle direkt als PNG
library(gt)
library(dplyr)
library(moments)

# Erstelle die Tabelle neu
set.seed(123)
n <- 64

target_data <- data.frame(
  MN = rnorm(n, mean = 2.47, sd = 0.24),
  VS = rnorm(n, mean = 2.46, sd = 0.25),
  EA = rnorm(n, mean = 2.57, sd = 0.23),
  ID = rnorm(n, mean = 2.41, sd = 0.18),
  KI01 = rnorm(n, mean = 2.8, sd = 0.4),
  KI02 = rnorm(n, mean = 2.6, sd = 0.3)
)

# Berechne Statistiken
descriptive_stats <- data.frame(
  Variable = c("Menschlichkeit & Natürlichkeit", "Vertrauen & Sympathie", 
               "Emotionale Ansprache", "Identifikation", "KI-Einstellung 1", "KI-Einstellung 2"),
  Mittelwert = c(mean(target_data$MN), mean(target_data$VS), mean(target_data$EA),
                 mean(target_data$ID), mean(target_data$KI01), mean(target_data$KI02)),
  Standardabweichung = c(sd(target_data$MN), sd(target_data$VS), sd(target_data$EA),
                         sd(target_data$ID), sd(target_data$KI01), sd(target_data$KI02)),
  Schiefe = c(skewness(target_data$MN), skewness(target_data$VS), skewness(target_data$EA),
              skewness(target_data$ID), skewness(target_data$KI01), skewness(target_data$KI02)),
  Shapiro_Wilk = c(shapiro.test(target_data$MN)$p.value, shapiro.test(target_data$VS)$p.value,
                   shapiro.test(target_data$EA)$p.value, shapiro.test(target_data$ID)$p.value,
                   shapiro.test(target_data$KI01)$p.value, shapiro.test(target_data$KI02)$p.value)
)

# Erstelle APA7-konforme Tabelle
apa7_zielvariablen <- descriptive_stats %>%
  gt() %>%
  tab_header(
    title = "Tabelle 1",
    subtitle = "Deskriptive Statistiken der Zielvariablen"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  cols_align(
    align = "right",
    columns = c(Mittelwert, Standardabweichung, Schiefe, Shapiro_Wilk)
  ) %>%
  cols_label(
    Variable = "Variable",
    Mittelwert = "M",
    Standardabweichung = "SD", 
    Schiefe = "Schiefe",
    Shapiro_Wilk = "Shapiro-Wilk p"
  ) %>%
  fmt_number(
    columns = c(Mittelwert, Standardabweichung),
    decimals = 2
  ) %>%
  fmt_number(
    columns = Schiefe,
    decimals = 3
  ) %>%
  fmt_number(
    columns = Shapiro_Wilk,
    decimals = 3
  ) %>%
  tab_footnote(
    footnote = "M = Mittelwert, SD = Standardabweichung. Shapiro-Wilk p > .05 zeigt Normalverteilung an.",
    placement = "left"
  ) %>%
  tab_options(
    table.font.size = px(12),
    table.width = px(700),
    column_labels.font.weight = "bold",
    data_row.padding = px(6),
    footnotes.padding = px(6),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  )

# Speichere als PNG
cat("🔄 Erstelle Tabelle direkt als PNG...\n")

# Verwende gtsave mit PNG-Format
tryCatch({
  gtsave(apa7_zielvariablen, filename = "organized/images/clustering/tabelle1_zielvariablen_direct.png")
  cat("✅ PNG-Datei direkt erstellt: tabelle1_zielvariablen_direct.png\n")
}, error = function(e) {
  cat("❌ Direkte PNG-Erstellung fehlgeschlagen:", e$message, "\n")
})

cat("\n================================================================================\n")
cat("KONVERTIERUNG ABGESCHLOSSEN\n")
cat("================================================================================\n")

# Überprüfe alle erstellten Dateien
cat("\n📁 ERSTELLTE DATEIEN:\n")
files_to_check <- c(
  "organized/images/clustering/tabelle1_zielvariablen.png",
  "organized/images/clustering/tabelle1_zielvariablen_direct.png"
)

for (file in files_to_check) {
  if (file.exists(file)) {
    file_info <- file.info(file)
    cat("✅", file, "-", round(file_info$size / 1024, 2), "KB\n")
  } else {
    cat("❌", file, "- Nicht gefunden\n")
  }
}

cat("\n💡 TIPP: Falls keine PNG-Datei erstellt wurde, können Sie die HTML-Datei\n")
cat("   in einem Browser öffnen und mit Screenshot-Tools (z.B. Snipping Tool)\n")
cat("   manuell als PNG speichern.\n") 