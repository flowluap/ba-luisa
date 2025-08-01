#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-
# Cronbach's Alpha Analyse für alle Variablengruppen
# Autor: [Ihr Name]
# Datum: [Aktuelles Datum]

# CRAN Mirror setzen
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Benötigte Pakete laden
if (!require(psych)) {
  install.packages("psych")
  library(psych)
}

# Funktion zur Berechnung und Ausgabe von Cronbach's Alpha
calculate_cronbach <- function(data, variables, group_name) {
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("CRONBACH'S ALPHA ANALYSE FÜR:", group_name, "\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  
  # Überprüfen ob alle Variablen existieren
  missing_vars <- variables[!variables %in% colnames(data)]
  if (length(missing_vars) > 0) {
    cat("WARNUNG: Folgende Variablen fehlen im Datensatz:", paste(missing_vars, collapse = ", "), "\n")
    variables <- variables[variables %in% colnames(data)]
  }
  
  if (length(variables) < 2) {
    cat("FEHLER: Weniger als 2 Variablen verfügbar für", group_name, "\n")
    return(NULL)
  }
  
  # Subset der Daten für die Variablengruppe
  subset_data <- data[, variables, drop = FALSE]
  
  # Konvertiere zu numerisch (falls nötig)
  subset_data[] <- lapply(subset_data, function(x) {
    if (is.character(x)) {
      x <- gsub("^\\s+|\\s+$", "", x)  # Leerzeichen entfernen
      x[x == ""] <- NA  # Leere Strings zu NA
      return(as.numeric(x))
    }
    return(as.numeric(x))
  })
  
  # Informationen über die Daten
  cat("Anzahl Variablen:", length(variables), "\n")
  cat("Anzahl Fälle (gesamt):", nrow(subset_data), "\n")
  
  # Berechne komplette Fälle
  complete_cases <- complete.cases(subset_data)
  complete_data <- subset_data[complete_cases, ]
  cat("Anzahl komplette Fälle:", nrow(complete_data), "\n")
  
  # Missing Values Information
  missing_count <- sum(is.na(subset_data))
  total_values <- nrow(subset_data) * ncol(subset_data)
  missing_percent <- round((missing_count / total_values) * 100, 2)
  cat("Missing Values:", missing_count, "von", total_values, "(", missing_percent, "%)\n")
  
  # Deskriptive Statistiken
  cat("\nDeskriptive Statistiken:\n")
  print(round(describe(subset_data), 3))
  
  # Cronbach's Alpha berechnen
  if (nrow(complete_data) < 10) {
    cat("\nWARNUNG: Sehr wenige komplette Fälle (< 10). Ergebnisse können unzuverlässig sein!\n")
  }
  
  if (nrow(complete_data) >= 2) {
    cat("\n--- CRONBACH'S ALPHA ERGEBNISSE ---\n")
    
    # Mit kompletten Fällen
    alpha_result <- alpha(complete_data)
    cat("Cronbach's Alpha (komplette Fälle):", round(alpha_result$total$raw_alpha, 4), "\n")
    cat("Standardisiertes Alpha:", round(alpha_result$total$std.alpha, 4), "\n")
    cat("Anzahl Items:", alpha_result$nvar, "\n")
    
    # Interpretation
    alpha_value <- alpha_result$total$raw_alpha
    interpretation <- if (alpha_value >= 0.9) {
      "Exzellent"
    } else if (alpha_value >= 0.8) {
      "Gut"
    } else if (alpha_value >= 0.7) {
      "Akzeptabel"
    } else if (alpha_value >= 0.6) {
      "Fragwürdig"
    } else {
      "Schlecht"
    }
    cat("Interpretation:", interpretation, "\n")
    
    # Item-Statistiken
    cat("\nItem-Statistiken:\n")
    item_stats <- data.frame(
      Variable = rownames(alpha_result$item.stats),
      Alpha_if_deleted = round(alpha_result$alpha.drop$raw_alpha, 4),
      Item_Total_Correlation = round(alpha_result$item.stats$r.drop, 4)
    )
    print(item_stats)
    
    # Mit pairwise deletion (alle verfügbaren Daten)
    if (missing_count > 0) {
      cat("\n--- MIT PAIRWISE DELETION ---\n")
      alpha_pairwise <- alpha(subset_data, use = "pairwise.complete.obs")
      cat("Cronbach's Alpha (pairwise):", round(alpha_pairwise$total$raw_alpha, 4), "\n")
    }
    
    return(list(
      group = group_name,
      alpha = alpha_result$total$raw_alpha,
      std_alpha = alpha_result$total$std.alpha,
      n_items = alpha_result$nvar,
      n_cases = nrow(complete_data),
      interpretation = interpretation
    ))
    
  } else {
    cat("\nFEHLER: Nicht genügend komplette Fälle für Cronbach's Alpha Berechnung!\n")
    return(NULL)
  }
}

# Hauptfunktion
main <- function() {
  cat("CRONBACH'S ALPHA ANALYSE - BACHELOR ARBEIT\n")
  cat("==========================================\n")
  cat("Startzeit:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Datensatz laden
  file_path <- "Datensatz_neu.csv"  # Einziger Datensatz (generiert und analysiert)
  
  if (!file.exists(file_path)) {
    cat("FEHLER: Datei", file_path, "nicht gefunden!\n")
    return()
  }
  
  cat("Lade Datensatz:", file_path, "\n")
  
  # Lade CSV - Check if it's our generated dataset format
      if (file_path == "Datensatz_neu.csv") {
    # Standard CSV format (comma-separated, single header row)
    data <- read.csv(file_path, stringsAsFactors = FALSE)
    cat("Standard CSV Format erkannt\n")
    
    # Filtere nur vollständige Sitzungen (exkludiere SKIP cases) für Analyse
    if ("CASE" %in% colnames(data)) {
      original_rows <- nrow(data)
      data <- data[!grepl("SKIP", data$CASE, fixed = TRUE), ]
      completed_rows <- nrow(data)
      skipped_rows <- original_rows - completed_rows
      cat("Filtere vollständige Sitzungen:", completed_rows, "von", original_rows, 
          "(", skipped_rows, "unvollständige übersprungen)\n")
    }
  } else {
    # Original TSV format (tab-separated, multiple header rows)
    data <- read.csv(file_path, sep = "\t", header = FALSE, 
                     stringsAsFactors = FALSE)
    
    # Setze Spaltennamen aus der ersten Zeile und entferne Anführungszeichen
    colnames(data) <- gsub('"', '', as.character(data[1, ]))
    
    # Entferne Header-Zeilen (erste 2 Zeilen)
    data <- data[3:nrow(data), ]
    cat("TSV Format erkannt\n")
  }
  
  cat("Datensatz geladen. Dimensionen:", nrow(data), "x", ncol(data), "\n")
  
  # Definiere Variablengruppen
  variable_groups <- list(
    "Emotionale Ansprache" = c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05"),
    
    "Identifikation" = c("ID01_01", "ID01_02", "ID01_03", "ID01_04"),
    
    "KI-Wahrnehmung & Akzeptanz" = c("KI01_01", "KI01_02", "KI01_03", "KI01_04"),
    
    "Unterschiede von KI-Avataren" = c("KI02_01", "KI02_02", "KI02_03", "KI02_04", 
                                       "KI02_05", "KI02_06", "KI02_07", "KI02_08"),
    
    "Menschlichkeit & Natürlichkeit" = c("MN01_01", "MN01_02", "MN01_03", "MN01_04", 
                                         "MN01_05", "MN01_06", "MN01_07"),
    
    "Mediennutzung & Vorwissen" = c("SE01_02", "SE01_06"),
    
    "Vertrauens- und Sympathiewerte" = c("VS01_01", "VS01_02", "VS01_03", "VS01_04",
                                         "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  )
  
  # Sammle alle Ergebnisse
  all_results <- list()
  
  # Berechne Cronbach's Alpha für jede Gruppe
  for (group_name in names(variable_groups)) {
    variables <- variable_groups[[group_name]]
    result <- calculate_cronbach(data, variables, group_name)
    if (!is.null(result)) {
      all_results[[group_name]] <- result
    }
  }
  
  # Zusammenfassung aller Ergebnisse
  cat("\n", paste(rep("=", 80), collapse=""), "\n")
  cat("ZUSAMMENFASSUNG ALLER CRONBACH'S ALPHA WERTE\n")
  cat(paste(rep("=", 80), collapse=""), "\n")
  
  if (length(all_results) > 0) {
    summary_df <- data.frame(
      Variablengruppe = names(all_results),
      Alpha = sapply(all_results, function(x) round(x$alpha, 4)),
      Std_Alpha = sapply(all_results, function(x) round(x$std_alpha, 4)),
      Anzahl_Items = sapply(all_results, function(x) x$n_items),
      Anzahl_Fälle = sapply(all_results, function(x) x$n_cases),
      Interpretation = sapply(all_results, function(x) x$interpretation),
      stringsAsFactors = FALSE
    )
    
    print(summary_df)
    
    # Speichere Ergebnisse in CSV
    write.csv(summary_df, "cronbach_alpha_ergebnisse.csv", row.names = FALSE, fileEncoding = "UTF-8")
    cat("\nErgebnisse gespeichert in: cronbach_alpha_ergebnisse.csv\n")
    
    # Beste und schlechteste Skalen
    best_alpha <- max(summary_df$Alpha)
    worst_alpha <- min(summary_df$Alpha)
    best_scale <- summary_df$Variablengruppe[which.max(summary_df$Alpha)]
    worst_scale <- summary_df$Variablengruppe[which.min(summary_df$Alpha)]
    
    cat("\nBeste interne Konsistenz:", best_scale, "(α =", best_alpha, ")\n")
    cat("Schlechteste interne Konsistenz:", worst_scale, "(α =", worst_alpha, ")\n")
    
  } else {
    cat("Keine gültigen Cronbach's Alpha Werte berechnet!\n")
  }
  
  cat("\nAnalyse abgeschlossen:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
}

# Script ausführen
if (!interactive()) {
  main()
} else {
  cat("Script geladen. Führen Sie main() aus, um die Analyse zu starten.\n")
} 