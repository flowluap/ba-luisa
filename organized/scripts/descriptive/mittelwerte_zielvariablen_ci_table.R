# ================================================================================
# MITTELWERTE ZIELVARIABLEN MIT KONFIDENZINTERVALLEN TABELLE
# ================================================================================
# Script zur Erstellung einer APA-formatierten Tabelle mit Mittelwerten und CIs
# für Zielvariablen (MC, MN, VS, EA, ID) pro Gruppe (KI vs. Mensch).
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

cat("Daten erfolgreich geladen\n")
cat("Anzahl Zeilen:", nrow(data), "\n")
cat("Anzahl Spalten:", ncol(data), "\n")

# Filter for FINISHED = 1
data <- data %>% filter(FINISHED == 1)

cat("Daten gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data), "\n")

# ================================================================================
# COMPOSITE SCORES BERECHNEN
# ================================================================================

# Calculate composite scores for each participant
data_processed <- data %>%
  rowwise() %>%
  mutate(
    # MC: Multiple Choice (MC01)
    MC_Score = MC01,
    
    # MN: Menschlichkeit & Natürlichkeit (MN01_01 to MN01_07)
    MN_Score = mean(c(MN01_01, MN01_02, MN01_03, MN01_04, MN01_05, MN01_06, MN01_07), na.rm = TRUE),
    
    # VS: Vertrauen & Sympathie (VS01_01 to VS01_08)
    VS_Score = mean(c(VS01_01, VS01_02, VS01_03, VS01_04, VS01_05, VS01_06, VS01_07, VS01_08), na.rm = TRUE),
    
    # EA: Emotionale Ansprache (EA01_01 to EA01_05)
    EA_Score = mean(c(EA01_01, EA01_02, EA01_03, EA01_04, EA01_05), na.rm = TRUE),
    
    # ID: Identifikation (ID01_01 to ID01_04)
    ID_Score = mean(c(ID01_01, ID01_02, ID01_03, ID01_04), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(AB01, MC_Score, MN_Score, VS_Score, EA_Score, ID_Score)

# Create group factor
data_processed$Group <- factor(data_processed$AB01, levels = c(1, 2), labels = c("KI", "Mensch"))

cat("Gruppengrößen:\n")
cat("KI (AB01=1):", sum(data_processed$AB01 == 1), "\n")
cat("Mensch (AB01=2):", sum(data_processed$AB01 == 2), "\n")

# ================================================================================
# MITTELWERTE UND KONFIDENZINTERVALLE BERECHNEN
# ================================================================================

# Function to calculate mean and 95% CI
calculate_mean_ci <- function(x, conf_level = 0.95) {
  n <- length(x[!is.na(x)])
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  se_val <- sd_val / sqrt(n)
  
  # 95% CI using t-distribution
  t_val <- qt((1 + conf_level) / 2, df = n - 1)
  ci_lower <- mean_val - t_val * se_val
  ci_upper <- mean_val + t_val * se_val
  
  return(list(
    mean = mean_val,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}

# Calculate means and CIs for each variable by group
variables <- c("MC_Score", "MN_Score", "VS_Score", "EA_Score", "ID_Score")
var_labels <- c("MC", "MN", "VS", "EA", "ID")

# Initialize result data frame
results <- data.frame()

for (i in 1:length(variables)) {
  var <- variables[i]
  label <- var_labels[i]
  
  # KI group
  ki_data <- data_processed[data_processed$AB01 == 1, var][[1]]
  ki_stats <- calculate_mean_ci(ki_data)
  
  # Mensch group  
  mensch_data <- data_processed[data_processed$AB01 == 2, var][[1]]
  mensch_stats <- calculate_mean_ci(mensch_data)
  
  # Add to results
  result_row <- data.frame(
    Variable = label,
    KI_Mean = round(ki_stats$mean, 2),
    KI_CI = paste0("[", round(ki_stats$ci_lower, 2), ", ", round(ki_stats$ci_upper, 2), "]"),
    Mensch_Mean = round(mensch_stats$mean, 2),
    Mensch_CI = paste0("[", round(mensch_stats$ci_lower, 2), ", ", round(mensch_stats$ci_upper, 2), "]")
  )
  
  results <- rbind(results, result_row)
}

cat("\nMittelwerte und Konfidenzintervalle pro Gruppe:\n")
print(results)

# Create final table data
table_data <- results
colnames(table_data) <- c("Variable", "KI_Mean", "KI_CI", "Mensch_Mean", "Mensch_CI")

cat("\nFinale Tabellendaten:\n")
print(table_data)

# ================================================================================
# APA-STIL TABELLE ERSTELLEN (EXAKT GLEICHER STIL WIE mean_table_EA.png)
# ================================================================================

# Function to create APA-style table (exactly like mean_table_EA.png)
create_apa_ci_table <- function(table_data, title) {
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
  
  # Add bottom border below last row (further down, same thickness)
  table_grob <- gtable_add_grob(
    table_grob,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(-1, "mm"),  # Further down to avoid cutting brackets
      x1 = unit(1, "npc"),
      y1 = unit(-1, "mm"),  # Further down to avoid cutting brackets
      gp = gpar(lwd = 2)  # Same thickness as before
    ),
    t = nrow(table_data) + 1, b = nrow(table_data) + 1, l = 1, r = ncol(table_data)
  )
  
  # Make header row taller by directly manipulating gtable heights
  table_grob$heights[1] <- unit(6, "mm")  # Make header row slightly taller
  
  return(table_grob)
}

# Create APA-style table
ci_apa_table <- create_apa_ci_table(table_data, "Zielvariablen Mittelwerte mit CI")

# ================================================================================
# TABELLE ALS PNG SPEICHERN
# ================================================================================

# Create output directory if it doesn't exist
dir.create("organized/images/descriptive", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with optimal dimensions (more height for lower line)
png("organized/images/descriptive/mittelwerte_zielvariablen_ci.png", 
    width = 1800, height = 320, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with optimal margins (more space at bottom)
vp <- viewport(x = 0.5, y = 0.52, width = 0.98, height = 0.88, just = c("center", "center"))
pushViewport(vp)

# Draw table directly without extra spacing
grid::grid.draw(ci_apa_table)
popViewport()
dev.off()

cat("Zielvariablen-Mittelwerte-CI-Tabelle erstellt: mittelwerte_zielvariablen_ci.png\n")

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("\n================================================================================\n")
cat("ZIELVARIABLEN-MITTELWERTE-CI-TABELLE ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• mittelwerte_zielvariablen_ci.png (MC, MN, VS, EA, ID mit 95% CIs)\n")
cat("• Tabelle im APA-Stil mit gleichem Look wie mean_table_EA.png\n")
cat("• Gruppierung: KI vs. Mensch\n")
cat("• N-Werte: KI =", sum(data_processed$AB01 == 1), ", Mensch =", sum(data_processed$AB01 == 2), "\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• AB01=1 = KI, AB01=2 = Mensch\n")
cat("• 95% Konfidenzintervalle für alle Mittelwerte\n")
cat("• Minimale Whitespace, vollständig sichtbar\n")
cat("================================================================================\n") 