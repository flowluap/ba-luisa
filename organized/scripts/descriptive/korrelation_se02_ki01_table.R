# Load required libraries
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

# Function to create APA-style correlation table (same style as mean_table_EA.png)
create_apa_correlation_table <- function(table_data, title) {
  table_grob <- tableGrob(
    table_data,
    rows = NULL,
    theme = ttheme_default(
      base_family = "Times",
      base_size = 12,
      core = list(
        fg_params = list(hjust = 0, x = 0.05),
        bg_params = list(fill = NA),
        padding = unit(c(4, 1), "mm")
      ),
      colhead = list(
        fg_params = list(fontface = "bold", hjust = 0, x = 0.05),
        bg_params = list(fill = NA),
        padding = unit(c(2, 1), "mm")  # Minimal padding for headers
      ),
      rowhead = list(
        fg_params = list(hjust = 0, x = 0.05),
        bg_params = list(fill = NA),
        padding = unit(c(4, 1), "mm")
      )
    )
  )
  
  # Add border under column headers
  table_grob <- gtable_add_grob(
    table_grob,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 2)
    ),
    t = 1, b = 1, l = 1, r = ncol(table_data)
  )
  
  # Add bottom border below last row
  table_grob <- gtable_add_grob(
    table_grob,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 2)
    ),
    t = nrow(table_data) + 1, b = nrow(table_data) + 1, l = 1, r = ncol(table_data)
  )
  
  # Make header row taller by directly manipulating gtable heights
  table_grob$heights[1] <- unit(6, "mm")  # Make header row slightly taller
  
  return(table_grob)
}

# Load data
cat("Lade Daten...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

cat("Daten erfolgreich geladen\n")
cat("Anzahl Zeilen:", nrow(data), "\n")
cat("Anzahl Spalten:", ncol(data), "\n")

# Filter data (FINISHED=1)
data_processed <- data %>%
  filter(FINISHED == 1)

cat("Daten gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data_processed), "\n")

# Check available columns
cat("Verfügbare Spalten mit SE02:\n")
se02_cols <- colnames(data_processed)[grepl("SE02", colnames(data_processed))]
print(se02_cols)

cat("Verfügbare Spalten mit KI01:\n")
ki01_cols <- colnames(data_processed)[grepl("KI01", colnames(data_processed))]
print(ki01_cols)

# Calculate KI01 composite score
ki01_score <- rowMeans(data_processed[, grepl("^KI01_", colnames(data_processed))], na.rm = TRUE)

# Get SE02 variable (should be a single column)
se02_score <- data_processed$SE02_01

# Check if variables exist
if(is.null(se02_score)) {
  cat("SE02_01 nicht gefunden, verwende SE02...\n")
  se02_score <- data_processed$SE02
}

cat("SE02 Werte (erste 10):\n")
print(head(se02_score, 10))
cat("KI01 Werte (erste 10):\n")
print(head(ki01_score, 10))

# Remove NA values for correlation
valid_cases <- !is.na(se02_score) & !is.na(ki01_score)
se02_clean <- se02_score[valid_cases]
ki01_clean <- ki01_score[valid_cases]

cat("Gültige Fälle für Korrelation:", length(se02_clean), "\n")

# Calculate Pearson correlation
if(length(se02_clean) > 2 && length(ki01_clean) > 2) {
  cor_result <- cor.test(se02_clean, ki01_clean, method = "pearson")
  
  # Format p-value
  p_formatted <- if(cor_result$p.value < 0.001) {
    "< 0.001"
  } else if(cor_result$p.value < 0.01) {
    sprintf("%.3f", cor_result$p.value)
  } else {
    sprintf("%.3f", cor_result$p.value)
  }
  
  # Create results table
  results <- data.frame(
    `Variable 1` = "SE02",
    `Variable 2` = "KI01", 
    r = sprintf("%.3f", cor_result$estimate),
    `p-Wert` = p_formatted,
    n = length(se02_clean),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  cat("\nKorrelations-Ergebnisse:\n")
  print(results)
  
  cat("\nDetaillierte Korrelationsstatistik:\n")
  cat("Pearson Korrelation r =", round(cor_result$estimate, 3), "\n")
  cat("p-Wert =", cor_result$p.value, "\n")
  cat("95% Konfidenzintervall: [", round(cor_result$conf.int[1], 3), ", ", round(cor_result$conf.int[2], 3), "]\n")
  cat("N =", length(se02_clean), "\n")
  
} else {
  cat("Nicht genügend gültige Fälle für Korrelationsanalyse\n")
  results <- data.frame(
    `Variable 1` = "SE02",
    `Variable 2` = "KI01", 
    r = "NA",
    `p-Wert` = "NA",
    n = length(se02_clean),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Create APA table
correlation_apa_table <- create_apa_correlation_table(results, "Korrelation SE02 - KI01")

# Create directory if it doesn't exist
dir.create("organized/images/descriptive", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with optimal dimensions
png("organized/images/descriptive/korrelation_se02_ki01_tabelle.png", 
    width = 1400, height = 200, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with optimal margins
vp <- viewport(x = 0.5, y = 0.5, width = 0.98, height = 0.98, just = c("center", "center"))
pushViewport(vp)
grid::grid.draw(correlation_apa_table)
popViewport()

dev.off()

cat("Korrelations-Tabelle erstellt: korrelation_se02_ki01_tabelle.png\n")

cat("\n================================================================================\n")
cat("KORRELATIONS-TABELLE ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• korrelation_se02_ki01_tabelle.png (SE02 vs KI01)\n")
cat("• Tabelle im APA-Stil mit gleichem Look wie mean_table_EA.png\n")
cat("• Pearson Korrelation mit p-Wert und n\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• Minimale Whitespace, vollständig sichtbar\n")
cat("================================================================================\n") 