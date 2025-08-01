# ================================================================================
# MODERATIONSANALYSE TABELLE
# ================================================================================
# Script zur Erstellung einer APA-formatierten Tabelle mit Moderationsanalyse-Ergebnissen
# für die Variablen VS, MN, ID, EA nach Gruppen (KI vs. Mensch).
# Verwendet "Bereinigte Daten von WhatsApp Business.csv"
# AB01=1 = KI, AB01=2 = Mensch
# Gleicher Stil wie mean_table_EA.png
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
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv",
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter for FINISHED = 1
data <- data %>% filter(FINISHED == 1)

cat("Daten geladen und gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data), "\n")

# ================================================================================
# BERECHNUNG DER MODERATIONSANALYSE
# ================================================================================

# Calculate composite scores for each participant
data_processed <- data %>%
  rowwise() %>%
  mutate(
    # VS: Vertrauen & Sympathie (VS01_01 to VS01_08)
    VS_Score = mean(c(VS01_01, VS01_02, VS01_03, VS01_04, VS01_05, VS01_06, VS01_07, VS01_08), na.rm = TRUE),
    
    # MN: Menschlichkeit & Natürlichkeit (MN01_01 to MN01_07)
    MN_Score = mean(c(MN01_01, MN01_02, MN01_03, MN01_04, MN01_05, MN01_06, MN01_07), na.rm = TRUE),
    
    # ID: Identifikation (ID01_01 to ID01_04)
    ID_Score = mean(c(ID01_01, ID01_02, ID01_03, ID01_04), na.rm = TRUE),
    
    # EA: Emotionale Ansprache (EA01_01 to EA01_05)
    EA_Score = mean(c(EA01_01, EA01_02, EA01_03, EA01_04, EA01_05), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(AB01, VS_Score, MN_Score, ID_Score, EA_Score)

# Create group factor
data_processed$Group <- factor(data_processed$AB01, levels = c(1, 2), labels = c("KI", "Mensch"))

# Function to perform regression analysis and extract results
perform_moderation_analysis <- function(dependent_var, data_df) {
  # Create formula for regression with group as predictor
  formula_str <- paste(dependent_var, "~ Group")
  
  # Fit linear model
  model <- lm(as.formula(formula_str), data = data_df)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Get R-squared
  r_squared <- summary(model)$r.squared
  
  # Extract results for both groups
  results <- list()
  
  # KI group (intercept)
  results[["KI"]] <- list(
    beta = sprintf("%.3f", coef_summary[1, 1]),
    p_value = ifelse(coef_summary[1, 4] < 0.001, "< 0.001", sprintf("%.3f", coef_summary[1, 4])),
    r_squared = sprintf("%.3f", r_squared)
  )
  
  # Mensch group (intercept + group effect)
  if(nrow(coef_summary) > 1) {
    results[["Mensch"]] <- list(
      beta = sprintf("%.3f", coef_summary[2, 1]),
      p_value = ifelse(coef_summary[2, 4] < 0.001, "< 0.001", sprintf("%.3f", coef_summary[2, 4])),
      r_squared = sprintf("%.3f", r_squared)
    )
  } else {
    results[["Mensch"]] <- list(
      beta = "0.000",
      p_value = "1.000",
      r_squared = sprintf("%.3f", r_squared)
    )
  }
  
  return(results)
}

# Perform moderation analysis for each variable
vs_results <- perform_moderation_analysis("VS_Score", data_processed)
mn_results <- perform_moderation_analysis("MN_Score", data_processed)
id_results <- perform_moderation_analysis("ID_Score", data_processed)
ea_results <- perform_moderation_analysis("EA_Score", data_processed)

# ================================================================================
# TABELLENDATEN VORBEREITEN
# ================================================================================

# Create table data in the required format
table_data <- data.frame(
  Variable = c("VS", "MN", "ID", "EA", "VS", "MN", "ID", "EA"),
  Gruppe = c("KI", "KI", "KI", "KI", "Mensch", "Mensch", "Mensch", "Mensch"),
  Beta = c(
    vs_results$KI$beta,
    mn_results$KI$beta,
    id_results$KI$beta,
    ea_results$KI$beta,
    vs_results$Mensch$beta,
    mn_results$Mensch$beta,
    id_results$Mensch$beta,
    ea_results$Mensch$beta
  ),
  p = c(
    vs_results$KI$p_value,
    mn_results$KI$p_value,
    id_results$KI$p_value,
    ea_results$KI$p_value,
    vs_results$Mensch$p_value,
    mn_results$Mensch$p_value,
    id_results$Mensch$p_value,
    ea_results$Mensch$p_value
  ),
  R2 = c(
    vs_results$KI$r_squared,
    mn_results$KI$r_squared,
    id_results$KI$r_squared,
    ea_results$KI$r_squared,
    vs_results$Mensch$r_squared,
    mn_results$Mensch$r_squared,
    id_results$Mensch$r_squared,
    ea_results$Mensch$r_squared
  ),
  stringsAsFactors = FALSE
)

# Rename columns to match expected format
colnames(table_data) <- c("Variable", "Gruppe", "β", "p", "R²")

cat("\nModerationsanalyse-Ergebnisse:\n")
print(table_data)

# ================================================================================
# APA-STIL TABELLE ERSTELLEN (EXAKT GLEICHER STIL WIE mean_table_EA.png)
# ================================================================================

# Function to create APA-style table (exactly like mean_table_EA.png)
create_apa_moderation_table <- function(table_data, title) {
  # Create table grob with APA styling
  table_grob <- tableGrob(
    table_data,
    rows = NULL,  # Remove row numbers
    theme = ttheme_default(
      base_family = "Times",
      base_size = 12,
      core = list(
        fg_params = list(hjust = 0, x = 0.05),
        bg_params = list(fill = NA),
        padding = unit(c(6, 1), "mm")  # Increased general row height
      ),
      colhead = list(
        fg_params = list(fontface = "bold", hjust = 0, x = 0.05),
        bg_params = list(fill = NA),
        padding = unit(c(2, 1), "mm")  # Minimal padding for headers
      ),
      rowhead = list(
        fg_params = list(hjust = 0, x = 0.05),
        bg_params = list(fill = NA),
        padding = unit(c(6, 1), "mm")  # Same as other rows for consistency
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
  
  return(table_grob)
}

# Create APA-style table
moderation_apa_table <- create_apa_moderation_table(table_data, "Moderationsanalyse")

# Make header row taller by directly manipulating gtable heights
moderation_apa_table$heights[1] <- unit(6, "mm")  # Make header row slightly taller

# ================================================================================
# TABELLE ALS PNG SPEICHERN
# ================================================================================

# Create output directory if it doesn't exist
dir.create("organized/images/descriptive", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with optimal dimensions (more space at bottom)
png("organized/images/descriptive/moderationsanalyse_tabelle.png", 
    width = 1600, height = 500, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with optimal margins (more space at bottom)
vp <- viewport(x = 0.5, y = 0.5, width = 0.95, height = 0.85, just = c("center", "center"))
pushViewport(vp)

# Draw table directly without extra spacing
grid::grid.draw(moderation_apa_table)
popViewport()
dev.off()

cat("Tabelle für Moderationsanalyse erstellt: moderationsanalyse_tabelle.png\n")

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("\n================================================================================\n")
cat("MODERATIONSANALYSE-TABELLE ERSTELLT:\n")
cat("================================================================================\n")
cat("• moderationsanalyse_tabelle.png\n")
cat("• Tabelle im APA-Stil mit gleichem Look wie mean_table_EA.png\n")
cat("• Variablen: VS, MN, ID, EA\n")
cat("• Gruppierung: KI vs. Mensch\n")
cat("• Spalten: Variable, Gruppe, β, p, R²\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• AB01=1 = KI, AB01=2 = Mensch\n")
cat("================================================================================\n") 