# ================================================================================
# KI01 MITTELWERTTABELLE
# ================================================================================
# Script zur Erstellung einer APA-formatierten Tabelle mit Mittelwerten
# für KI01 Items (KI01_01, KI01_02, KI01_03, KI01_04) pro Gruppe (KI vs. Mensch).
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
cat("Spaltennamen:\n")
print(colnames(data))

# Filter for FINISHED = 1
data <- data %>% filter(FINISHED == 1)

cat("Daten gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data), "\n")

# ================================================================================
# MITTELWERTE BERECHNEN
# ================================================================================

# Calculate means for KI01 items by group
table_data_calculated <- data %>%
  group_by(AB01) %>%
  summarise(
    n = n(),
    KI01_01_mean = round(mean(KI01_01, na.rm = TRUE), 2),
    KI01_02_mean = round(mean(KI01_02, na.rm = TRUE), 2), 
    KI01_03_mean = round(mean(KI01_03, na.rm = TRUE), 2),
    KI01_04_mean = round(mean(KI01_04, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  mutate(Group = ifelse(AB01 == 1, "KI", "Mensch"))

cat("Gruppengrößen:\n")
cat("KI (AB01=1):", sum(data$AB01 == 1), "\n")
cat("Mensch (AB01=2):", sum(data$AB01 == 2), "\n")

cat("\nMittelwerte pro Gruppe:\n")
print(table_data_calculated)

# Create final table data
table_data <- data.frame(
  Gruppe = c("KI", "Mensch"),
  n = c(
    sum(data$AB01 == 1),
    sum(data$AB01 == 2)
  ),
  KI01_01 = c(
    round(mean(data$KI01_01[data$AB01 == 1], na.rm = TRUE), 2),
    round(mean(data$KI01_01[data$AB01 == 2], na.rm = TRUE), 2)
  ),
  KI01_02 = c(
    round(mean(data$KI01_02[data$AB01 == 1], na.rm = TRUE), 2),
    round(mean(data$KI01_02[data$AB01 == 2], na.rm = TRUE), 2)
  ),
  KI01_03 = c(
    round(mean(data$KI01_03[data$AB01 == 1], na.rm = TRUE), 2),
    round(mean(data$KI01_03[data$AB01 == 2], na.rm = TRUE), 2)
  ),
  KI01_04 = c(
    round(mean(data$KI01_04[data$AB01 == 1], na.rm = TRUE), 2),
    round(mean(data$KI01_04[data$AB01 == 2], na.rm = TRUE), 2)
  ),
  stringsAsFactors = FALSE
)

cat("\nFinale Tabellendaten:\n")
print(table_data)

# ================================================================================
# APA-STIL TABELLE ERSTELLEN (EXAKT GLEICHER STIL WIE mean_table_EA.png)
# ================================================================================

# Function to create APA-style table (exactly like mean_table_EA.png)
create_apa_mean_table <- function(table_data, title) {
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
ki01_apa_table <- create_apa_mean_table(table_data, "KI01 Mittelwerte")

# ================================================================================
# TABELLE ALS PNG SPEICHERN
# ================================================================================

# Create output directory if it doesn't exist
dir.create("organized/images/descriptive", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with optimal dimensions
png("organized/images/descriptive/mean_table_KI01.png", 
    width = 1400, height = 200, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with optimal margins
vp <- viewport(x = 0.5, y = 0.5, width = 0.98, height = 0.98, just = c("center", "center"))
pushViewport(vp)

# Draw table directly without extra spacing
grid::grid.draw(ki01_apa_table)
popViewport()
dev.off()

cat("Tabelle für KI01 erstellt: mean_table_KI01.png\n")

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("\n================================================================================\n")
cat("KI01-MITTELWERTTABELLE ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• mean_table_KI01.png (KI01: KI01_01, KI01_02, KI01_03, KI01_04)\n")
cat("• Tabelle im APA-Stil mit gleichem Look\n")
cat("• Gruppierung: KI vs. Mensch\n")
cat("• N-Werte: KI =", sum(data$AB01 == 1), ", Mensch =", sum(data$AB01 == 2), "\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• AB01=1 = KI, AB01=2 = Mensch\n")
cat("• Minimale Whitespace, vollständig sichtbar\n")
cat("================================================================================\n") 