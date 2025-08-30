#!/usr/bin/env Rscript

# ================================================================================
# TABELLE B7: MITTELWERTE DER SKALA "KI-KRITIK" (KI02) AUF ITEM-EBENE
# ================================================================================

cat("================================================================================\n")
cat("TABELLE B7: MITTELWERTE DER SKALA \"KI-KRITIK\" (KI02) AUF ITEM-EBENE\n")
cat("================================================================================\n")

# Pakete laden
library(dplyr)
library(gt)

# Datensatz laden
cat("\n=== DATENSATZ LADEN ===\n")
tryCatch({
  data <- read.delim("/Users/luisa.claussen/Desktop/Digitaler Anhang /Bereinigte Daten.csv",
                     fileEncoding = "UTF-16",
                     sep = "\t",
                     stringsAsFactors = FALSE)
  cat("✓ Daten geladen. Gesamtstichprobe: n =", nrow(data), "\n")
}, error = function(e) {
  cat("❌ Fehler beim Laden der Daten:", e$message, "\n")
  stop("Kann nicht fortfahren ohne echte Daten")
})

# AB01 (Gruppe) identifizieren
cat("\n=== GRUPPEN IDENTIFIZIEREN ===\n")
if("AB01" %in% names(data)) {
  data$AB01 <- data$AB01
  cat("✓ AB01 (Gruppe) übernommen\n")
} else {
  cat("❌ AB01 nicht gefunden\n")
  stop("AB01 Spalte nicht gefunden")
}

# Gruppen identifizieren
if(all(c(1, 2) %in% unique(data$AB01))) {
  cat("✓ Gruppen gefunden: 1 = KI-Avatar, 2 = Mensch\n")
  cat("  KI-Avatar (AB01 = 1): n =", sum(data$AB01 == 1, na.rm = TRUE), "\n")
  cat("  Mensch (AB01 = 2): n =", sum(data$AB01 == 2, na.rm = TRUE), "\n")
} else {
  cat("❌ Gruppen nicht korrekt kodiert\n")
  stop("Gruppen nicht korrekt kodiert")
}

# KI02 Items identifizieren
cat("\n=== KI02 ITEMS IDENTIFIZIEREN ===\n")
ki02_items <- c("KI02_01", "KI02_02", "KI02_03", "KI02_04", "KI02_05", "KI02_06", "KI02_07", "KI02_08")

# Verfügbare Items prüfen
available_ki02_items <- ki02_items[ki02_items %in% names(data)]
cat("✓ Verfügbare KI02 Items:", paste(available_ki02_items, collapse = ", "), "\n")

if(length(available_ki02_items) == 0) {
  cat("❌ Keine KI02 Items gefunden\n")
  stop("Keine KI02 Items gefunden")
}

# Gruppen aufteilen
ki_data <- data[data$AB01 == 1, ]
mensch_data <- data[data$AB01 == 2, ]

cat("\nKI-Avatar Gruppe: n =", nrow(ki_data), "\n")
cat("Mensch Gruppe: n =", nrow(mensch_data), "\n")

# Mittelwerte für jedes Item berechnen
cat("\n=== MITTELWERTE AUF ITEM-EBENE BERECHNEN ===\n")

# KI-Avatar Gruppe
cat("\nKI-Avatar Gruppe:\n")
ki_means <- list()
ki_n <- list()

for(item in available_ki02_items) {
  if(item %in% names(ki_data)) {
    # Zu numerisch konvertieren
    item_values <- as.numeric(as.character(ki_data[[item]]))
    item_values_clean <- item_values[!is.na(item_values)]
    
    if(length(item_values_clean) > 0) {
      mean_val <- mean(item_values_clean, na.rm = TRUE)
      n_val <- length(item_values_clean)
      ki_means[[item]] <- mean_val
      ki_n[[item]] <- n_val
      
      cat("  ", item, ": M =", round(mean_val, 3), ", n =", n_val, "\n")
    } else {
      ki_means[[item]] <- NA
      ki_n[[item]] <- 0
      cat("  ", item, ": Keine gültigen Daten\n")
    }
  } else {
    ki_means[[item]] <- NA
    ki_n[[item]] <- 0
    cat("  ", item, ": Item nicht gefunden\n")
  }
}

# Mensch Gruppe
cat("\nMensch Gruppe:\n")
mensch_means <- list()
mensch_n <- list()

for(item in available_ki02_items) {
  if(item %in% names(mensch_data)) {
    # Zu numerisch konvertieren
    item_values <- as.numeric(as.character(mensch_data[[item]]))
    item_values_clean <- item_values[!is.na(item_values)]
    
    if(length(item_values_clean) > 0) {
      mean_val <- mean(item_values_clean, na.rm = TRUE)
      n_val <- length(item_values_clean)
      mensch_means[[item]] <- mean_val
      mensch_n[[item]] <- n_val
      
      cat("  ", item, ": M =", round(mean_val, 3), ", n =", n_val, "\n")
    } else {
      mensch_means[[item]] <- NA
      mensch_n[[item]] <- 0
      cat("  ", item, ": Keine gültigen Daten\n")
    }
  } else {
    mensch_means[[item]] <- NA
    mensch_n[[item]] <- 0
    cat("  ", item, ": Item nicht gefunden\n")
  }
}

# Daten für GT-Tabelle vorbereiten
cat("\n=== DATEN FÜR GT-TABELLE VORBEREITEN ===\n")

# Item-Namen für die Spalten
item_names <- c("KI02_01", "KI02_02", "KI02_03", "KI02_04", "KI02_05", "KI02_06", "KI02_07", "KI02_08")

# Tabelle erstellen
table_data <- data.frame(
  Gruppe = c("Gruppe Mensch", "Gruppe KI"),
  n = c(
    ifelse(length(mensch_n) > 0, mensch_n[[1]], 0),
    ifelse(length(ki_n) > 0, ki_n[[1]], 0)
  ),
  KI02_01 = c(
    ifelse(!is.na(mensch_means[[1]]), sprintf("%.2f", mensch_means[[1]]), "NA"),
    ifelse(!is.na(ki_means[[1]]), sprintf("%.2f", ki_means[[1]]), "NA")
  ),
  KI02_02 = c(
    ifelse(!is.na(mensch_means[[2]]), sprintf("%.2f", mensch_means[[2]]), "NA"),
    ifelse(!is.na(ki_means[[2]]), sprintf("%.2f", ki_means[[2]]), "NA")
  ),
  KI02_03 = c(
    ifelse(!is.na(mensch_means[[3]]), sprintf("%.2f", mensch_means[[3]]), "NA"),
    ifelse(!is.na(ki_means[[3]]), sprintf("%.2f", ki_means[[3]]), "NA")
  ),
  KI02_04 = c(
    ifelse(!is.na(mensch_means[[4]]), sprintf("%.2f", mensch_means[[4]]), "NA"),
    ifelse(!is.na(ki_means[[4]]), sprintf("%.2f", ki_means[[4]]), "NA")
  ),
  KI02_05 = c(
    ifelse(!is.na(mensch_means[[5]]), sprintf("%.2f", mensch_means[[5]]), "NA"),
    ifelse(!is.na(ki_means[[5]]), sprintf("%.2f", ki_means[[5]]), "NA")
  ),
  KI02_06 = c(
    ifelse(!is.na(mensch_means[[6]]), sprintf("%.2f", mensch_means[[6]]), "NA"),
    ifelse(!is.na(ki_means[[6]]), sprintf("%.2f", ki_means[[6]]), "NA")
  ),
  KI02_07 = c(
    ifelse(!is.na(mensch_means[[7]]), sprintf("%.2f", mensch_means[[7]]), "NA"),
    ifelse(!is.na(ki_means[[7]]), sprintf("%.2f", ki_means[[7]]), "NA")
  ),
  KI02_08 = c(
    ifelse(!is.na(mensch_means[[8]]), sprintf("%.2f", mensch_means[[8]]), "NA"),
    ifelse(!is.na(ki_means[[8]]), sprintf("%.2f", ki_means[[8]]), "NA")
  ),
  stringsAsFactors = FALSE
)

# GT Tabelle erstellen
cat("\n=== GT-TABELLE ERSTELLEN ===\n")

tabelle_b7 <- gt(table_data) %>%
  # Titel und Untertitel
  tab_header(
    title = "Tabelle B7",
    subtitle = "Mittelwerte der Skala \"KI-Kritik\" (KI02) auf Item-Ebene"
  ) %>%
  # Spaltenlabels
  cols_label(
    Gruppe = "Gruppe",
    n = "n",
    KI02_01 = "KI02_01",
    KI02_02 = "KI02_02",
    KI02_03 = "KI02_03",
    KI02_04 = "KI02_04",
    KI02_05 = "KI02_05",
    KI02_06 = "KI02_06",
    KI02_07 = "KI02_07",
    KI02_08 = "KI02_08"
  ) %>%
  # Spaltenausrichtung
  cols_align(
    align = "left",
    columns = Gruppe
  ) %>%
  cols_align(
    align = "center",
    columns = c("n", "KI02_01", "KI02_02", "KI02_03", "KI02_04", "KI02_05", "KI02_06", "KI02_07", "KI02_08")
  ) %>%
  # APA7: Tabellenformat
  tab_options(
    table.font.size = px(10),
    table.width = px(600), # Reduced from 750px to match new ultra-compact column widths
    column_labels.font.weight = "bold",
    data_row.padding = px(2),
    footnotes.padding = px(2),
    table_body.hlines.style = "none",
    table_body.hlines.color = "transparent",
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table.border.left.style = "none",
    table.border.right.style = "none"
  ) %>%
  # Spaltenbreiten
  cols_width(
    Gruppe ~ px(60), # Reduced from 70px to make it even more compact
    n ~ px(12), # Reduced from 15px to make it even more compact
    KI02_01 ~ px(25), # Reduced from 30px
    KI02_02 ~ px(25), # Reduced from 30px
    KI02_03 ~ px(25), # Reduced from 30px
    KI02_04 ~ px(25), # Reduced from 30px
    KI02_05 ~ px(25), # Reduced from 30px
    KI02_06 ~ px(25), # Reduced from 30px
    KI02_07 ~ px(25), # Reduced from 30px
    KI02_08 ~ px(25)  # Reduced from 30px
  )

# Tabelle anzeigen
print(tabelle_b7)

# HTML exportieren
cat("\n=== TABELLE EXPORTIEREN ===\n")
html_file <- "organized/images/clustering/tabelle_b7_ki02_item_ebene.html"
tabelle_b7 %>% gtsave(html_file)
cat("✓ HTML-Export:", html_file, "\n")

cat("\n=== INTERPRETATION DER ERGEBNISSE ===\n")

cat("\n📊 INTERPRETATION DER ITEM-MITTELWERTE:\n")
cat("• Jede Zeile zeigt die Mittelwerte für eine Gruppe\n")
cat("• Jede Spalte zeigt ein einzelnes Item der KI02-Skala\n")
cat("• n = Anzahl der gültigen Antworten pro Gruppe\n")

cat("\n🔍 IHRE ERGEBNISSE:\n")
cat("Skala: KI-Kritik (KI02)\n")
cat("Items: KI02_01 bis KI02_08\n")
cat("Gruppen: KI-Avatar vs. Mensch\n")

# Wichtige Befunde hervorheben
cat("\n📈 WICHTIGE BEFUNDE:\n")
for(i in 1:length(available_ki02_items)) {
  item <- available_ki02_items[i]
  item_short <- paste0("KI02_0", i)
  
  ki_mean <- ki_means[[i]]
  mensch_mean <- mensch_means[[i]]
  
  if(!is.na(ki_mean) && !is.na(mensch_mean)) {
    diff <- mensch_mean - ki_mean
    cat("•", item_short, ":", sprintf("Differenz = %.3f", diff), "(Mensch - KI)\n")
  }
}

cat("\n================================================================================\n")
cat("TABELLE B7: MITTELWERTE AUF ITEM-EBENE ERSTELLT\n")
cat("================================================================================\n")

cat("\n📋 ZUSAMMENFASSUNG:\n")
cat("• Tabelle B7 mit Item-Mittelwerten der KI02-Skala erstellt\n")
cat("• Separate Werte für KI-Avatar und Mensch Gruppe\n")
cat("• Alle 8 Items der KI02-Skala berücksichtigt\n")
cat("• HTML-Export für weitere Verwendung verfügbar\n") 