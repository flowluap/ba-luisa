# =============================================================================
# VERIFIKATION DER BERECHNUNGEN
# =============================================================================
# Überprüft die Berechnungen Schritt für Schritt

library(dplyr)

cat("================================================================================\n")
cat("VERIFIKATION DER BERECHNUNGEN\n")
cat("================================================================================\n")

# =============================================================================
# DATEN LADEN
# =============================================================================

data_file <- "/Users/luisa.claussen/Desktop/Digitaler Anhang /Bereinigte Daten.csv"
cat("Lade Datensatz:", data_file, "\n")

data <- read.delim(data_file, header = TRUE, fileEncoding = "UTF-16")
cat("✓ Daten geladen. Zeilen:", nrow(data), "Spalten:", ncol(data), "\n")

# Gruppierung
ki_avatar_data <- data[data$AB01 == 1, ]
mensch_data <- data[data$AB01 == 2, ]

cat("KI-Avatar Gruppe: n =", nrow(ki_avatar_data), "\n")
cat("Mensch Gruppe: n =", nrow(mensch_data), "\n")

# =============================================================================
# DETAILLIERTE ÜBERPRÜFUNG JEDER SKALA
# =============================================================================

cat("\n=== DETAILLIERTE ÜBERPRÜFUNG ===\n")

# Funktion für detaillierte Berechnung
detailed_calculation <- function(data, item_prefix, n_items, scale_name) {
  cat("\n---", scale_name, "---\n")
  
  # Alle Items finden
  items <- paste0(item_prefix, sprintf("%02d", 1:n_items))
  available_items <- items[items %in% colnames(data)]
  
  cat("Verfügbare Items:", paste(available_items, collapse = ", "), "\n")
  
  if (length(available_items) > 0) {
    # Daten extrahieren
    numeric_data <- data[, available_items, drop = FALSE]
    
    # Zeige erste paar Zeilen
    cat("Erste 3 Zeilen der Items:\n")
    print(head(numeric_data, 3))
    
    # Konvertiere zu numerisch
    numeric_data <- apply(numeric_data, 2, function(x) as.numeric(as.character(x)))
    
    # Zeige Zusammenfassung
    cat("Zusammenfassung der numerischen Daten:\n")
    print(summary(numeric_data))
    
    # Zeilenmittelwerte berechnen
    row_means <- rowMeans(numeric_data, na.rm = TRUE)
    
    # Zeige Zusammenfassung der Skalenmittelwerte
    cat("Skalenmittelwerte Zusammenfassung:\n")
    print(summary(row_means))
    
    # Finale Statistiken
    final_mean <- mean(row_means, na.rm = TRUE)
    final_sd <- sd(row_means, na.rm = TRUE)
    
    cat("FINAL:", scale_name, "- M =", round(final_mean, 3), "SD =", round(final_sd, 3), "\n")
    
    return(list(mean = final_mean, sd = final_sd, data = row_means))
  } else {
    cat("❌ Keine Items gefunden!\n")
    return(NULL)
  }
}

# Alle Skalen detailliert berechnen
cat("\n=== KI-AVATAR GRUPPE ===\n")

# MN (Menschlichkeit & Natürlichkeit) - 7 Items
mn_ki_result <- detailed_calculation(ki_avatar_data, "MN01_", 7, "Menschlichkeit & Natürlichkeit (KI-Avatar)")

# VS (Vertrauen & Sympathie) - 8 Items  
vs_ki_result <- detailed_calculation(ki_avatar_data, "VS01_", 8, "Vertrauen & Sympathie (KI-Avatar)")

# EA (Emotionale Ansprache) - 5 Items
ea_ki_result <- detailed_calculation(ki_avatar_data, "EA01_", 5, "Emotionale Ansprache (KI-Avatar)")

# ID (Identifikation) - 4 Items
id_ki_result <- detailed_calculation(ki_avatar_data, "ID01_", 4, "Identifikation (KI-Avatar)")

# KI01 (KI Wahrnehmung) - 4 Items
ki01_ki_result <- detailed_calculation(ki_avatar_data, "KI01_", 4, "KI Wahrnehmung (KI-Avatar)")

# KI02 (KI Kritik) - 8 Items
ki02_ki_result <- detailed_calculation(ki_avatar_data, "KI02_", 8, "KI Kritik (KI-Avatar)")

cat("\n=== MENSCH GRUPPE ===\n")

# MN (Menschlichkeit & Natürlichkeit) - 7 Items
mn_mensch_result <- detailed_calculation(mensch_data, "MN01_", 7, "Menschlichkeit & Natürlichkeit (Mensch)")

# VS (Vertrauen & Sympathie) - 8 Items  
vs_mensch_result <- detailed_calculation(mensch_data, "VS01_", 8, "Vertrauen & Sympathie (Mensch)")

# EA (Emotionale Ansprache) - 5 Items
ea_mensch_result <- detailed_calculation(mensch_data, "EA01_", 5, "Emotionale Ansprache (Mensch)")

# ID (Identifikation) - 4 Items
id_mensch_result <- detailed_calculation(mensch_data, "ID01_", 4, "Identifikation (Mensch)")

# KI01 (KI Wahrnehmung) - 4 Items
ki01_mensch_result <- detailed_calculation(mensch_data, "KI01_", 4, "KI Wahrnehmung (Mensch)")

# KI02 (KI Kritik) - 8 Items
ki02_mensch_result <- detailed_calculation(mensch_data, "KI02_", 8, "KI Kritik (Mensch)")

# =============================================================================
# ZUSAMMENFASSUNG ALLER WERTE
# =============================================================================

cat("\n================================================================================\n")
cat("ZUSAMMENFASSUNG ALLER BERECHNETEN WERTE\n")
cat("================================================================================\n")

cat("\n🤖 KI-AVATAR GRUPPE (n =", nrow(ki_avatar_data), "):\n")
if (!is.null(mn_ki_result)) cat("• MN: M =", round(mn_ki_result$mean, 3), "SD =", round(mn_ki_result$sd, 3), "\n")
if (!is.null(vs_ki_result)) cat("• VS: M =", round(vs_ki_result$mean, 3), "SD =", round(vs_ki_result$sd, 3), "\n")
if (!is.null(ea_ki_result)) cat("• EA: M =", round(ea_ki_result$mean, 3), "SD =", round(ea_ki_result$sd, 3), "\n")
if (!is.null(id_ki_result)) cat("• ID: M =", round(id_ki_result$mean, 3), "SD =", round(id_ki_result$sd, 3), "\n")
if (!is.null(ki01_ki_result)) cat("• KI01: M =", round(ki01_ki_result$mean, 3), "SD =", round(ki01_ki_result$sd, 3), "\n")
if (!is.null(ki02_ki_result)) cat("• KI02: M =", round(ki02_ki_result$mean, 3), "SD =", round(ki02_ki_result$sd, 3), "\n")

cat("\n👤 MENSCH GRUPPE (n =", nrow(mensch_data), "):\n")
if (!is.null(mn_mensch_result)) cat("• MN: M =", round(mn_mensch_result$mean, 3), "SD =", round(mn_mensch_result$mean, 3), "\n")
if (!is.null(vs_mensch_result)) cat("• VS: M =", round(vs_mensch_result$mean, 3), "SD =", round(vs_mensch_result$sd, 3), "\n")
if (!is.null(ea_mensch_result)) cat("• EA: M =", round(ea_mensch_result$mean, 3), "SD =", round(ea_mensch_result$sd, 3), "\n")
if (!is.null(id_mensch_result)) cat("• ID: M =", round(id_mensch_result$mean, 3), "SD =", round(id_mensch_result$sd, 3), "\n")
if (!is.null(ki01_mensch_result)) cat("• KI01: M =", round(ki01_mensch_result$mean, 3), "SD =", round(ki01_mensch_result$sd, 3), "\n")
if (!is.null(ki02_mensch_result)) cat("• KI02: M =", round(ki02_mensch_result$mean, 3), "SD =", round(ki02_mensch_result$sd, 3), "\n")

cat("\n================================================================================\n")
cat("VERIFIKATION ABGESCHLOSSEN\n")
cat("================================================================================\n") 