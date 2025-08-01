# ================================================================================
# CHI-QUADRAT-ANALYSE TABELLE
# ================================================================================
# Script zur Erstellung einer APA-formatierten Tabelle mit Chi-Quadrat-Ergebnissen
# für demografische Vergleiche: Alter vs Geschlecht, Alter vs SM-Plattform, Geschlecht vs SM-Plattform
# Verwendet "Bereinigte Daten von WhatsApp Business.csv"
# SO01 = Alter, SO02 = Geschlecht, SE03 = SM-Plattform
# ================================================================================

# Load required libraries
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

# ================================================================================
# ECHTE DATEN LADEN
# ================================================================================

# Load the real data file with proper encoding
data_path <- "organized/data/Bereinigte Daten von WhatsApp Business.csv"
data <- read.delim(data_path, fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter for FINISHED == 1
data <- data %>% filter(FINISHED == 1)

cat("Daten geladen und gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data), "\n")

# Check if we have the expected columns
expected_cols <- c("FINISHED", "SO01_01", "SO02", "SE03")
missing_cols <- expected_cols[!expected_cols %in% colnames(data)]
if(length(missing_cols) > 0) {
  cat("Warnung: Fehlende Spalten:", missing_cols, "\n")
}

# ================================================================================
# CHI-QUADRAT-TESTS DURCHFÜHREN
# ================================================================================

# Function to perform chi-squared test and extract results
perform_chi2_test <- function(var1, var2, data_df) {
  # Convert variables to factors for chi-squared test
  data_df[[var1]] <- as.factor(data_df[[var1]])
  data_df[[var2]] <- as.factor(data_df[[var2]])
  
  contingency_table <- table(data_df[[var1]], data_df[[var2]])
  chi2_result <- chisq.test(contingency_table)
  
  return(list(
    chi2_wert = sprintf("%.3f", chi2_result$statistic),
    df = chi2_result$parameter,
    p_wert = sprintf("%.3f", chi2_result$p.value)
  ))
}

# Perform tests for the requested comparisons
# SO01_01 = Alter, SO02 = Geschlecht, SE03 = SM-Plattform
results_alter_geschlecht <- perform_chi2_test("SO01_01", "SO02", data)
results_alter_sm_plattform <- perform_chi2_test("SO01_01", "SE03", data)
results_geschlecht_sm_plattform <- perform_chi2_test("SO02", "SE03", data)

# ================================================================================
# ERGEBNISSE FÜR DIE TABELLE VORBEREITEN
# ================================================================================

table_data <- data.frame(
  Vergleich = c(
    "Alter vs Geschlecht",
    "Alter vs SM-Plattform",
    "Geschlecht vs SM-Plattform"
  ),
  Chi2_Wert = c(
    results_alter_geschlecht$chi2_wert,
    results_alter_sm_plattform$chi2_wert,
    results_geschlecht_sm_plattform$chi2_wert
  ),
  df = c(
    results_alter_geschlecht$df,
    results_alter_sm_plattform$df,
    results_geschlecht_sm_plattform$df
  ),
  p_Wert = c(
    results_alter_geschlecht$p_wert,
    results_alter_sm_plattform$p_wert,
    results_geschlecht_sm_plattform$p_wert
  ),
  stringsAsFactors = FALSE
)

cat("\nChi-Quadrat-Ergebnisse:\n")
print(table_data)

# ================================================================================
# APA-STIL TABELLE ERSTELLEN
# ================================================================================

# Function to create APA-style table (same as mean_table_EA.png)
create_apa_table <- function(table_data) {
  # Create table grob with APA styling
  table_grob <- tableGrob(
    table_data,
    rows = NULL,  # Remove row numbers
    theme = ttheme_default(
      base_family = "Times",
      base_size = 12,
      core = list(
        fg_params = list(hjust = 0, x = 0.05),
        bg_params = list(fill = NA)
      ),
      colhead = list(
        fg_params = list(fontface = "bold", hjust = 0, x = 0.05),
        bg_params = list(fill = NA)
      ),
      rowhead = list(
        fg_params = list(hjust = 0, x = 0.05),
        bg_params = list(fill = NA)
      ),
      padding = unit(c(4, 1), "mm") # Increased padding for line height
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
  
  return(table_grob)
}

# Create APA-style table
chi2_apa_table <- create_apa_table(table_data)

# ================================================================================
# TABELLE ALS PNG SPEICHERN
# ================================================================================

# Create output directory if it doesn't exist
dir.create("organized/images/correlations", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with minimal whitespace (same dimensions as EA table)
png("organized/images/correlations/chi2_analyse_tabelle.png", 
    width = 1200, height = 200, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with minimal margins for full table visibility (same as EA table)
vp <- viewport(x = 0.5, y = 0.5, width = 0.98, height = 0.98, just = c("center", "center"))
pushViewport(vp)

# Draw table directly without extra spacing
grid::grid.draw(chi2_apa_table)
popViewport()
dev.off()

cat("\n================================================================================\n")
cat("CHI-QUADRAT-TABELLE ERSTELLT:\n")
cat("================================================================================\n")
cat("• chi2_analyse_tabelle.png\n")
cat("• Tabelle im APA-Stil mit gleichem Look wie mean_table_EA.png\n")
cat("• Vergleiche: Alter vs Geschlecht, Alter vs SM-Plattform, Geschlecht vs SM-Plattform\n")
cat("• Verwendet echte Daten aus CSV-Datei (SO01, SO02, SE03)\n")
cat("• Spalten: Vergleich, Chi2_Wert, df, p_Wert\n")
cat("================================================================================\n") 