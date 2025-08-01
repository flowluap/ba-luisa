# ================================================================================
# MANOVA STATISTIKEN TABELLE
# ================================================================================
# Script zur Erstellung einer APA-formatierten MANOVA-Statistiken-Tabelle
# mit echten Daten aus "Bereinigte Daten von WhatsApp Business.csv"
# Im gleichen Stil wie mean_table_EA.png
# AB01=1 = KI, AB01=2 = Mensch
# ================================================================================

# Load required libraries
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)
library(car)  # for MANOVA

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
# MANOVA DURCHFÜHREN
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

cat("Gruppengrößen:\n")
cat("KI (AB01=1):", sum(data_processed$AB01 == 1), "\n")
cat("Mensch (AB01=2):", sum(data_processed$AB01 == 2), "\n")

# Perform MANOVA
cat("\nDurchführung der MANOVA...\n")

# Create dependent variable matrix
dependent_vars <- cbind(data_processed$VS_Score, data_processed$MN_Score, 
                       data_processed$ID_Score, data_processed$EA_Score)
colnames(dependent_vars) <- c("VS", "MN", "ID", "EA")

# Perform MANOVA
manova_result <- manova(dependent_vars ~ Group, data = data_processed)

# Get MANOVA summary
manova_summary <- summary(manova_result, test = "Pillai")
cat("\nMANOVA Ergebnisse (Pillai's Trace):\n")
print(manova_summary)

# Extract statistics for different tests
pillai_stats <- summary(manova_result, test = "Pillai")$stats
wilks_stats <- summary(manova_result, test = "Wilks")$stats
hotelling_stats <- summary(manova_result, test = "Hotelling-Lawley")$stats

cat("\nPillai's Trace Statistiken:\n")
print(pillai_stats)

# ================================================================================
# TABELLENDATEN ERSTELLEN
# ================================================================================

# Create table data with MANOVA statistics
table_data <- data.frame(
  Statistic = c("Pillai's Trace", "Wilks Lambda", "Hotelling's Trace"),
  Value = c(
    round(pillai_stats[1, "Pillai"], 3),
    round(wilks_stats[1, "Wilks"], 3),
    round(hotelling_stats[1, "Hotelling-Lawley"], 3)
  ),
  F_Value = c(
    round(pillai_stats[1, "approx F"], 2),
    round(wilks_stats[1, "approx F"], 2),
    round(hotelling_stats[1, "approx F"], 2)
  ),
  p_Value = c(
    ifelse(pillai_stats[1, "Pr(>F)"] < 0.001, "< 0.001", sprintf("%.3f", pillai_stats[1, "Pr(>F)"])),
    ifelse(wilks_stats[1, "Pr(>F)"] < 0.001, "< 0.001", sprintf("%.3f", wilks_stats[1, "Pr(>F)"])),
    ifelse(hotelling_stats[1, "Pr(>F)"] < 0.001, "< 0.001", sprintf("%.3f", hotelling_stats[1, "Pr(>F)"]))
  ),
  df1 = c(
    pillai_stats[1, "num Df"],
    wilks_stats[1, "num Df"],
    hotelling_stats[1, "num Df"]
  ),
  df2 = c(
    pillai_stats[1, "den Df"],
    wilks_stats[1, "den Df"],
    hotelling_stats[1, "den Df"]
  ),
  stringsAsFactors = FALSE
)

# Rename columns to match expected format
colnames(table_data) <- c("Statistic", "Value", "F Value", "p Value", "df1", "df2")

cat("\nFinale MANOVA-Tabellendaten:\n")
print(table_data)

# ================================================================================
# APA-STIL TABELLE ERSTELLEN (EXAKT GLEICHER STIL WIE mean_table_EA.png)
# ================================================================================

# Function to create APA-style table (exactly like mean_table_EA.png)
create_apa_manova_table <- function(table_data, title) {
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

# Create APA-style table
manova_apa_table <- create_apa_manova_table(table_data, "MANOVA Statistiken")

# ================================================================================
# TABELLE ALS PNG SPEICHERN
# ================================================================================

# Create output directory if it doesn't exist
dir.create("organized/images/manova", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with optimal dimensions
png("organized/images/manova/manova_statistiken.png", 
    width = 1600, height = 250, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with optimal margins
vp <- viewport(x = 0.5, y = 0.5, width = 0.98, height = 0.98, just = c("center", "center"))
pushViewport(vp)

# Draw table directly without extra spacing
grid::grid.draw(manova_apa_table)
popViewport()
dev.off()

cat("MANOVA-Statistiken-Tabelle erstellt: manova_statistiken.png\n")

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("\n================================================================================\n")
cat("MANOVA-STATISTIKEN-TABELLE ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• manova_statistiken.png (Pillai's Trace, Wilks Lambda, Hotelling's Trace)\n")
cat("• Tabelle im APA-Stil mit gleichem Look wie mean_table_EA.png\n")
cat("• Gruppierung: KI vs. Mensch\n")
cat("• N-Werte: KI =", sum(data_processed$AB01 == 1), ", Mensch =", sum(data_processed$AB01 == 2), "\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• AB01=1 = KI, AB01=2 = Mensch\n")
cat("• Minimale Whitespace, vollständig sichtbar\n")
cat("================================================================================\n") 