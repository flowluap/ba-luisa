# ================================================================================
# MITTELWERTTABELLE FÜR MN ITEMS - ECHTE DATEN
# ================================================================================
# Script zur Erstellung der MN-Mittelwerttabelle mit echten Daten
# Verwendet "Bereinigte Daten von WhatsApp Business.csv"
# AB01=1 = KI, AB01=2 = Mensch
# ================================================================================

# Load required libraries
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

# ================================================================================
# ECHTE DATEN LADEN
# ================================================================================

# Load the real data file with proper encoding handling
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

cat("Daten erfolgreich geladen\n")
cat("Anzahl Zeilen:", nrow(data), "\n")
cat("Anzahl Spalten:", ncol(data), "\n")

# Show column names to verify structure
cat("Spaltennamen:\n")
print(colnames(data))

# Check if we have the expected columns
expected_cols <- c("FINISHED", "AB01", "MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
missing_cols <- expected_cols[!expected_cols %in% colnames(data)]
if(length(missing_cols) > 0) {
  cat("Warnung: Fehlende Spalten:", missing_cols, "\n")
}

# Filter for valid cases only (FINISHED = 1)
data_filtered <- data[data$FINISHED == 1, ]

cat("Daten gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data_filtered), "\n")

# Check group sizes
group_sizes <- table(data_filtered$AB01)
cat("Gruppengrößen:\n")
cat("KI (AB01=1):", group_sizes["1"], "\n")
cat("Mensch (AB01=2):", group_sizes["2"], "\n")

# ================================================================================
# MITTELWERTE BERECHNEN
# ================================================================================

# Calculate means by group for MN items (using the actual column names)
means_by_group <- data_filtered %>%
  group_by(AB01) %>%
  summarise(
    n = n(),
    # MN items (using actual column names from CSV)
    MN01_mean = round(mean(MN01_01, na.rm = TRUE), 2),
    MN02_mean = round(mean(MN01_02, na.rm = TRUE), 2),
    MN03_mean = round(mean(MN01_03, na.rm = TRUE), 2),
    MN04_mean = round(mean(MN01_04, na.rm = TRUE), 2),
    MN05_mean = round(mean(MN01_05, na.rm = TRUE), 2),
    MN06_mean = round(mean(MN01_06, na.rm = TRUE), 2),
    MN07_mean = round(mean(MN01_07, na.rm = TRUE), 2)
  ) %>%
  mutate(
    Group = ifelse(AB01 == 1, "KI", "Mensch")
  )

cat("\nMittelwerte pro Gruppe:\n")
print(means_by_group)

# ================================================================================
# APA-STIL TABELLE ERSTELLEN
# ================================================================================

# Function to create APA-style table
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
        bg_params = list(fill = NA)
      ),
      colhead = list(
        fg_params = list(fontface = "bold", hjust = 0, x = 0.05),
        bg_params = list(fill = NA),
        padding = unit(c(2, 1), "mm")  # Minimal padding for headers
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
  
  # Make header row taller by directly manipulating gtable heights
  table_grob$heights[1] <- unit(6, "mm")  # Make header row slightly taller
  
  return(table_grob)
}

# ================================================================================
# MN-TABELLE ERSTELLEN
# ================================================================================

# Prepare table data for MN
mn_table_data <- means_by_group %>%
  select(Group, n, MN01_mean, MN02_mean, MN03_mean, MN04_mean, MN05_mean, MN06_mean, MN07_mean) %>%
  rename(
    "Gruppe" = Group,
    "MN01" = MN01_mean,
    "MN02" = MN02_mean,
    "MN03" = MN03_mean,
    "MN04" = MN04_mean,
    "MN05" = MN05_mean,
    "MN06" = MN06_mean,
    "MN07" = MN07_mean
  )

# Create APA-style table
mn_apa_table <- create_apa_mean_table(mn_table_data, "Menschlichkeit & Natürlichkeit")

# Create output directory if it doesn't exist
dir.create("organized/images/descriptive", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with minimal whitespace
png("organized/images/descriptive/mean_table_MN.png", 
    width = 1800, height = 200, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with minimal margins for full table visibility
vp <- viewport(x = 0.5, y = 0.5, width = 0.98, height = 0.98, just = c("center", "center"))
pushViewport(vp)

# Draw table directly without extra spacing
grid::grid.draw(mn_apa_table)
popViewport()
dev.off()

cat("Tabelle für Menschlichkeit & Natürlichkeit erstellt: mean_table_MN.png\n")

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("\n================================================================================\n")
cat("MN-MITTELWERTTABELLE ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• mean_table_MN.png (Menschlichkeit & Natürlichkeit: MN01, MN02, MN03, MN04, MN05, MN06, MN07)\n")
cat("• Tabelle im APA-Stil mit gleichem Look\n")
cat("• Gruppierung: KI vs. Mensch\n")
cat("• N-Werte: KI =", group_sizes["1"], ", Mensch =", group_sizes["2"], "\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• AB01=1 = KI, AB01=2 = Mensch\n")
cat("• Minimale Whitespace, vollständig sichtbar\n")
cat("================================================================================\n") 