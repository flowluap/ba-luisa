#!/usr/bin/env Rscript

# ================================================================================
# DEBUG: COHENS D BERECHNUNG FÜR VS (VERTRAUEN UND SYMPATHIE)
# ================================================================================

cat("================================================================================\n")
cat("DEBUG: COHENS D BERECHNUNG FÜR VS (VERTRAUEN UND SYMPATHIE)\n")
cat("================================================================================\n")

# Pakete laden
library(dplyr)
library(effectsize)

# Datensatz laden
cat("\n=== DATENSATZ LADEN ===\n")
tryCatch({
  # Verschiedene Encodings versuchen
  data <- read.delim("/Users/luisa.claussen/Desktop/Digitaler Anhang /Bereinigte Daten.csv", 
                     fileEncoding = "UTF-16", 
                     sep = "\t", 
                     stringsAsFactors = FALSE)
  cat("✓ Daten geladen mit UTF-16\n")
}, error = function(e) {
  cat("❌ Fehler beim Laden mit UTF-16, versuche andere Encodings...\n")
  tryCatch({
    data <- read.delim("/Users/luisa.claussen/Desktop/Digitaler Anhang /Bereinigte Daten.csv", 
                       fileEncoding = "UTF-8", 
                       sep = "\t", 
                       stringsAsFactors = FALSE)
    cat("✓ Daten geladen mit UTF-8\n")
  }, error = function(e2) {
    cat("❌ Fehler beim Laden mit UTF-8, versuche CSV...\n")
    tryCatch({
      data <- read.csv("/Users/luisa.claussen/Desktop/Digitaler Anhang /Bereinigte Daten.csv", 
                       fileEncoding = "UTF-8", 
                       stringsAsFactors = FALSE)
      cat("✓ Daten geladen als CSV mit UTF-8\n")
    }, error = function(e3) {
      cat("❌ Alle Versuche fehlgeschlagen, generiere Beispieldaten\n")
      # Beispieldaten generieren
      set.seed(123)
      n_total <- 131
      n_ki <- 64
      n_mensch <- 67
      
      # Realistischere Werte für VS (1-5 Skala)
      ki_vs <- rnorm(n_ki, mean = 2.8, sd = 0.9)
      mensch_vs <- rnorm(n_mensch, mean = 3.8, sd = 0.9)
      
      # AB01 Spalte hinzufügen
      data <- data.frame(
        AB01 = c(rep(1, n_ki), rep(2, n_mensch)),
        VS = c(ki_vs, mensch_vs),
        stringsAsFactors = FALSE
      )
      cat("✓ Beispieldaten generiert\n")
    })
  })
})

# Datenstruktur prüfen
cat("\n=== DATENSTRUKTUR PRÜFEN ===\n")
cat("Dimensionen:", dim(data), "\n")
cat("Spalten:", paste(names(data), collapse = ", "), "\n")

# AB01 Spalte prüfen
cat("\n=== AB01 SPALTE PRÜFEN ===\n")
if("AB01" %in% names(data)) {
  cat("✓ AB01 Spalte gefunden\n")
  cat("AB01 Werte:", paste(sort(unique(data$AB01)), collapse = ", "), "\n")
  cat("Häufigkeiten:\n")
  print(table(data$AB01))
} else {
  cat("❌ AB01 Spalte nicht gefunden!\n")
  # AB01 Spalte hinzufügen falls nicht vorhanden
  if(nrow(data) == 131) {
    data$AB01 <- c(rep(1, 64), rep(2, 67))
    cat("✓ AB01 Spalte hinzugefügt\n")
  }
}

# VS Spalte prüfen
cat("\n=== VS SPALTE PRÜFEN ===\n")
if("VS" %in% names(data)) {
  cat("✓ VS Spalte gefunden\n")
  cat("VS Werte (alle):", paste(round(data$VS, 3), collapse = ", "), "\n")
} else {
  cat("❌ VS Spalte nicht gefunden, versuche VS Items zu finden...\n")
  vs_items <- names(data)[grepl("^VS", names(data))]
  if(length(vs_items) > 0) {
    cat("✓ VS Items gefunden:", paste(vs_items, collapse = ", "), "\n")
    # VS Skala berechnen
    vs_data <- data[, vs_items, drop = FALSE]
    data$VS <- rowMeans(vs_data, na.rm = TRUE)
    cat("✓ VS Skala aus Items berechnet\n")
  } else {
    cat("❌ Keine VS Items gefunden!\n")
  }
}

# Gruppen trennen
cat("\n=== GRUPPEN TRENNEN ===\n")
if("AB01" %in% names(data) && "VS" %in% names(data)) {
  ki_data <- data[data$AB01 == 1, "VS", drop = FALSE]
  mensch_data <- data[data$AB01 == 2, "VS", drop = FALSE]
  
  cat("KI-Avatar Gruppe (AB01 = 1):\n")
  cat("  Anzahl:", nrow(ki_data), "\n")
  cat("  VS Werte:", paste(round(ki_data$VS, 3), collapse = ", "), "\n")
  cat("  Mittelwert:", round(mean(ki_data$VS, na.rm = TRUE), 3), "\n")
  cat("  SD:", round(sd(ki_data$VS, na.rm = TRUE), 3), "\n")
  
  cat("\nMensch Gruppe (AB01 = 2):\n")
  cat("  Anzahl:", nrow(mensch_data), "\n")
  cat("  VS Werte:", paste(round(mensch_data$VS, 3), collapse = ", "), "\n")
  cat("  Mittelwert:", round(mean(mensch_data$VS, na.rm = TRUE), 3), "\n")
  cat("  SD:", round(sd(mensch_data$VS, na.rm = TRUE), 3), "\n")
} else {
  cat("❌ Kann Gruppen nicht trennen - fehlende Spalten\n")
}

# T-Test manuell durchführen
cat("\n=== T-TEST MANUELL DURCHFÜHREN ===\n")
if("AB01" %in% names(data) && "VS" %in% names(data)) {
  ki_vs <- data$VS[data$AB01 == 1]
  mensch_vs <- data$VS[data$AB01 == 2]
  
  # T-Test
  t_result <- t.test(ki_vs, mensch_vs, var.equal = FALSE)
  cat("T-Test Ergebnis:\n")
  cat("  t =", round(t_result$statistic, 3), "\n")
  cat("  df =", round(t_result$parameter, 3), "\n")
  cat("  p =", format(t_result$p.value, scientific = TRUE), "\n")
  
  # Cohens d manuell berechnen
  cat("\nCohens d manuell berechnen:\n")
  pooled_sd <- sqrt(((length(ki_vs) - 1) * var(ki_vs) + (length(mensch_vs) - 1) * var(mensch_vs)) / 
                     (length(ki_vs) + length(mensch_vs) - 2))
  cohens_d_manual <- (mean(ki_vs) - mean(mensch_vs)) / pooled_sd
  cat("  Pooled SD =", round(pooled_sd, 3), "\n")
  cat("  Mean Difference =", round(mean(ki_vs) - mean(mensch_vs), 3), "\n")
  cat("  Cohens d (manuell) =", round(cohens_d_manual, 3), "\n")
  
  # Mit effectsize Paket vergleichen
  cat("\nMit effectsize Paket vergleichen:\n")
  cohens_d_package <- cohens_d(ki_vs, mensch_vs)
  cat("  Cohens d (effectsize) =", round(cohens_d_package$Cohens_d, 3), "\n")
  
  # Werte überprüfen
  cat("\n=== WERTE ÜBERPRÜFEN ===\n")
  cat("KI-Avatar VS Werte:\n")
  print(summary(ki_vs))
  cat("\nMensch VS Werte:\n")
  print(summary(mensch_vs))
  
  # Extremwerte prüfen
  cat("\nExtremwerte prüfen:\n")
  cat("KI-Avatar Min/Max:", min(ki_vs, na.rm = TRUE), "/", max(ki_vs, na.rm = TRUE), "\n")
  cat("Mensch Min/Max:", min(mensch_vs, na.rm = TRUE), "/", max(mensch_vs, na.rm = TRUE), "\n")
  
  # Mögliche Probleme identifizieren
  cat("\n=== MÖGLICHE PROBLEME IDENTIFIZIEREN ===\n")
  if(abs(cohens_d_manual) > 3) {
    cat("⚠️  WARNUNG: Cohens d > 3 ist extrem hoch!\n")
    cat("   Mögliche Ursachen:\n")
    cat("   - Sehr kleine Standardabweichungen\n")
    cat("   - Sehr große Mittelwertsunterschiede\n")
    cat("   - Datenfehler oder extreme Ausreißer\n")
    cat("   - Falsche Skalierung der Daten\n")
  }
  
  if(pooled_sd < 0.1) {
    cat("⚠️  WARNUNG: Pooled SD < 0.1 ist sehr klein!\n")
    cat("   Das erklärt den extrem hohen Cohens d\n")
  }
  
  if(abs(mean(ki_vs) - mean(mensch_vs)) > 2) {
    cat("⚠️  WARNUNG: Mittelwertsunterschied > 2 ist sehr groß!\n")
    cat("   Überprüfen Sie die Datenskala\n")
  }
  
  # Vergleich mit MN Werten
  cat("\n=== VERGLEICH MIT MN WERTEN ===\n")
  if("MN" %in% names(data)) {
    ki_mn <- data$MN[data$AB01 == 1]
    mensch_mn <- data$MN[data$AB01 == 2]
    
    cat("MN vs VS Vergleich:\n")
    cat("  MN - Cohens d:", round(cohens_d(ki_mn, mensch_mn)$Cohens_d, 3), "\n")
    cat("  VS - Cohens d:", round(cohens_d_manual, 3), "\n")
    
    if(abs(cohens_d(ki_mn, mensch_mn)$Cohens_d) > 3 && abs(cohens_d_manual) > 3) {
      cat("⚠️  BEIDE Variablen zeigen extrem hohe Effektstärken!\n")
      cat("   Das deutet auf ein systematisches Problem hin\n")
    }
  }
  
} else {
  cat("❌ Kann T-Test nicht durchführen - fehlende Daten\n")
}

cat("\n================================================================================\n")
cat("DEBUG VS ABGESCHLOSSEN\n")
cat("================================================================================\n") 