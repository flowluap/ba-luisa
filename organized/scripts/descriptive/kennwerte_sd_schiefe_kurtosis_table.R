# Load required libraries
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)
library(e1071) # für skewness, kurtosis

# Function to create APA-style kennwerte table (same style as mean_table_EA.png)
create_apa_kennwerte_table <- function(table_data, title) {
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

# Check group sizes
group_sizes <- table(data_processed$AB01)
cat("Gruppengrößen:\n")
cat("KI (AB01=1):", group_sizes["1"], "\n")
cat("Mensch (AB01=2):", group_sizes["2"], "\n")

# Calculate composite scores (same as in previous scripts)
apa_vars <- list(
  MN = rowMeans(data_processed[, grepl("^MN01_", colnames(data_processed))], na.rm = TRUE),
  VS = rowMeans(data_processed[, grepl("^VS01_", colnames(data_processed))], na.rm = TRUE),
  EA = rowMeans(data_processed[, grepl("^EA01_", colnames(data_processed))], na.rm = TRUE),
  ID = rowMeans(data_processed[, grepl("^ID01_", colnames(data_processed))], na.rm = TRUE),
  KI01 = rowMeans(data_processed[, grepl("^KI01_", colnames(data_processed))], na.rm = TRUE),
  KI02 = rowMeans(data_processed[, grepl("^KI02_", colnames(data_processed))], na.rm = TRUE)
)

# Calculate kennwerte for each variable
apa_results <- list()
for (var_name in names(apa_vars)) {
  cat("Berechne Kennwerte für:", var_name, "\n")
  
  x <- apa_vars[[var_name]]
  x <- x[!is.na(x)]
  
  # Shapiro-Wilk Test für Normalverteilung
  shapiro_test <- shapiro.test(x)
  
  apa_results[[var_name]] <- data.frame(
    Variable = var_name,
    SD = sprintf("%.2f", sd(x)),
    Schiefe = sprintf("%.2f", e1071::skewness(x, na.rm=TRUE, type=2)),
    Kurtosis = sprintf("%.2f", e1071::kurtosis(x, na.rm=TRUE, type=2)),
    Shapiro_W = sprintf("%.3f", shapiro_test$statistic),
    stringsAsFactors = FALSE
  )
}

# Kombiniere alle Ergebnisse
results <- do.call(rbind, apa_results)
rownames(results) <- NULL

cat("\nKennwerte-Ergebnisse:\n")
print(results)

# Create APA table
kennwerte_apa_table <- create_apa_kennwerte_table(results, "Kennwerte: SD, Schiefe, Kurtosis, Shapiro-W")

# Create directory if it doesn't exist
dir.create("organized/images/descriptive", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with expanded dimensions (much more space at bottom)
png("organized/images/descriptive/kennwerte_sd_schiefe_kurtosis_table.png", 
    width = 1600, height = 350, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with much more space at bottom for bottom line visibility
vp <- viewport(x = 0.5, y = 0.55, width = 0.98, height = 0.82, just = c("center", "center"))
pushViewport(vp)
grid::grid.draw(kennwerte_apa_table)
popViewport()

dev.off()

cat("Kennwerte-Tabelle erstellt: kennwerte_sd_schiefe_kurtosis_table.png\n")

cat("\n================================================================================\n")
cat("KENNWERTE-TABELLE ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• kennwerte_sd_schiefe_kurtosis_table.png (MN, VS, EA, ID, KI01, KI02)\n")
cat("• Tabelle im APA-Stil mit gleichem Look wie mean_table_EA.png\n")
cat("• SD, Schiefe, Kurtosis, Shapiro-W für jede Variable\n")
cat("• N-Werte: KI =", group_sizes["1"], ", Mensch =", group_sizes["2"], "\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• Minimale Whitespace, vollständig sichtbar\n")
cat("================================================================================\n") 