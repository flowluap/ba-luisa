# ================================================================================
# MITTELWERTTABELLE FÜR ID ITEMS - ECHTE DATEN
# ================================================================================
# Script zur Erstellung der ID-Mittelwerttabelle mit echten Daten
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
expected_cols <- c("FINISHED", "AB01", "ID01_01", "ID01_02", "ID01_03", "ID01_04")
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

# Calculate means by group for ID items (using the actual column names)
means_by_group <- data_filtered %>%
  group_by(AB01) %>%
  summarise(
    n = n(),
    # ID items (using actual column names from CSV)
    ID01_mean = round(mean(ID01_01, na.rm = TRUE), 2),
    ID02_mean = round(mean(ID01_02, na.rm = TRUE), 2),
    ID03_mean = round(mean(ID01_03, na.rm = TRUE), 2),
    ID04_mean = round(mean(ID01_04, na.rm = TRUE), 2)
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
# ID-TABELLE ERSTELLEN
# ================================================================================

# Prepare table data for ID
id_table_data <- means_by_group %>%
  select(Group, n, ID01_mean, ID02_mean, ID03_mean, ID04_mean) %>%
  rename(
    "Gruppe" = Group,
    "ID01" = ID01_mean,
    "ID02" = ID02_mean,
    "ID03" = ID03_mean,
    "ID04" = ID04_mean
  )

# Create APA-style table
id_apa_table <- create_apa_mean_table(id_table_data, "Identifikation")

# Create output directory if it doesn't exist
dir.create("organized/images/descriptive", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with minimal whitespace
png("organized/images/descriptive/mean_table_ID.png", 
    width = 1200, height = 200, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with minimal margins for full table visibility
vp <- viewport(x = 0.5, y = 0.5, width = 0.98, height = 0.98, just = c("center", "center"))
pushViewport(vp)

# Draw table directly without extra spacing
grid::grid.draw(id_apa_table)
popViewport()
dev.off()

cat("Tabelle für Identifikation erstellt: mean_table_ID.png\n")

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("\n================================================================================\n")
cat("ID-MITTELWERTTABELLE ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• mean_table_ID.png (Identifikation: ID01, ID02, ID03, ID04)\n")
cat("• Tabelle im APA-Stil mit gleichem Look\n")
cat("• Gruppierung: KI vs. Mensch\n")
cat("• N-Werte: KI =", group_sizes["1"], ", Mensch =", group_sizes["2"], "\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• AB01=1 = KI, AB01=2 = Mensch\n")
cat("• Minimale Whitespace, vollständig sichtbar\n")
cat("================================================================================\n") 