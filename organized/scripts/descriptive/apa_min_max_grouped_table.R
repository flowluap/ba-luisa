# ================================================================================
# APA-STIL MIN/MAX TABELLE GRUPPIERT
# ================================================================================
# Script zur Erstellung einer APA-formatierten Tabelle mit Min/Max-Werten
# für ausgewählte Variablen, gruppiert nach KI und Mensch.
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
# BERECHNUNG DER MIN/MAX WERTE
# ================================================================================

# Calculate mean scores for composite variables for each participant
data_processed <- data %>%
  rowwise() %>%
  mutate(
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
  select(AB01, MN_Score, VS_Score, EA_Score, ID_Score, KI01_01, KI02_01)

# Summarize min and max for each variable by group
min_max_data <- data_processed %>%
  group_by(AB01) %>%
  summarise(
    MN_Min = min(MN_Score, na.rm = TRUE),
    MN_Max = max(MN_Score, na.rm = TRUE),
    VS_Min = min(VS_Score, na.rm = TRUE),
    VS_Max = max(VS_Score, na.rm = TRUE),
    EA_Min = min(EA_Score, na.rm = TRUE),
    EA_Max = max(EA_Score, na.rm = TRUE),
    ID_Min = min(ID_Score, na.rm = TRUE),
    ID_Max = max(ID_Score, na.rm = TRUE),
    KI01_Min = min(KI01_01, na.rm = TRUE),
    KI01_Max = max(KI01_01, na.rm = TRUE),
    KI02_Min = min(KI02_01, na.rm = TRUE),
    KI02_Max = max(KI02_01, na.rm = TRUE)
  ) %>%
  ungroup()

cat("\nMin/Max-Werte pro Gruppe:\n")
print(min_max_data)

# ================================================================================
# TABELLENDATEN VORBEREITEN
# ================================================================================

# Create table data in the required format
table_data <- data.frame(
  Variable = c("MN", "VS", "EA", "ID", "KI01", "KI02"),
  Mensch_Min = c(
    sprintf("%.2f", min_max_data$MN_Min[min_max_data$AB01 == 2]),
    sprintf("%.2f", min_max_data$VS_Min[min_max_data$AB01 == 2]),
    sprintf("%.2f", min_max_data$EA_Min[min_max_data$AB01 == 2]),
    sprintf("%.2f", min_max_data$ID_Min[min_max_data$AB01 == 2]),
    sprintf("%.2f", min_max_data$KI01_Min[min_max_data$AB01 == 2]),
    sprintf("%.2f", min_max_data$KI02_Min[min_max_data$AB01 == 2])
  ),
  KI_Min = c(
    sprintf("%.2f", min_max_data$MN_Min[min_max_data$AB01 == 1]),
    sprintf("%.2f", min_max_data$VS_Min[min_max_data$AB01 == 1]),
    sprintf("%.2f", min_max_data$EA_Min[min_max_data$AB01 == 1]),
    sprintf("%.2f", min_max_data$ID_Min[min_max_data$AB01 == 1]),
    sprintf("%.2f", min_max_data$KI01_Min[min_max_data$AB01 == 1]),
    sprintf("%.2f", min_max_data$KI02_Min[min_max_data$AB01 == 1])
  ),
  Mensch_Max = c(
    sprintf("%.2f", min_max_data$MN_Max[min_max_data$AB01 == 2]),
    sprintf("%.2f", min_max_data$VS_Max[min_max_data$AB01 == 2]),
    sprintf("%.2f", min_max_data$EA_Max[min_max_data$AB01 == 2]),
    sprintf("%.2f", min_max_data$ID_Max[min_max_data$AB01 == 2]),
    sprintf("%.2f", min_max_data$KI01_Max[min_max_data$AB01 == 2]),
    sprintf("%.2f", min_max_data$KI02_Max[min_max_data$AB01 == 2])
  ),
  KI_Max = c(
    sprintf("%.2f", min_max_data$MN_Max[min_max_data$AB01 == 1]),
    sprintf("%.2f", min_max_data$VS_Max[min_max_data$AB01 == 1]),
    sprintf("%.2f", min_max_data$EA_Max[min_max_data$AB01 == 1]),
    sprintf("%.2f", min_max_data$ID_Max[min_max_data$AB01 == 1]),
    sprintf("%.2f", min_max_data$KI01_Max[min_max_data$AB01 == 1]),
    sprintf("%.2f", min_max_data$KI02_Max[min_max_data$AB01 == 1])
  ),
  stringsAsFactors = FALSE
)

# ================================================================================
# APA-STIL TABELLE ERSTELLEN (EXAKT GLEICHER STIL WIE mean_table_EA.png)
# ================================================================================

# Function to create APA-style table (exactly like mean_table_EA.png)
create_apa_min_max_table <- function(table_data, title) {
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
min_max_apa_table <- create_apa_min_max_table(table_data, "Min/Max Werte")

# ================================================================================
# TABELLE ALS PNG SPEICHERN
# ================================================================================

# Create output directory if it doesn't exist
dir.create("organized/images/descriptive", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with optimal dimensions (more height, less width)
png("organized/images/descriptive/apa_tabelle_min_max_gruppiert.png", 
    width = 2000, height = 350, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with optimal margins
vp <- viewport(x = 0.5, y = 0.5, width = 0.90, height = 0.90, just = c("center", "center"))
pushViewport(vp)

# Draw table directly without extra spacing
grid::grid.draw(min_max_apa_table)
popViewport()
dev.off()

cat("Tabelle für Min/Max-Werte erstellt: apa_tabelle_min_max_gruppiert.png\n")

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("\n================================================================================\n")
cat("MIN/MAX-TABELLE ERSTELLT:\n")
cat("================================================================================\n")
cat("• apa_tabelle_min_max_gruppiert.png\n")
cat("• Tabelle im APA-Stil mit gleichem Look wie mean_table_EA.png\n")
cat("• Variablen: MN, VS, EA, ID, KI01, KI02\n")
cat("• Gruppierung: KI vs. Mensch\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• AB01=1 = KI, AB01=2 = Mensch\n")
cat("================================================================================\n") 