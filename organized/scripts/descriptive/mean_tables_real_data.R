# ================================================================================
# MITTELWERTTABELLEN FÜR EINZELNE ITEMS - ECHTE DATEN
# ================================================================================
# Script zur Erstellung von Mittelwerttabellen für einzelne Items
# Verwendet echte Daten aus "Bereinigte Daten von WhatsApp Business.csv"
# ================================================================================

# Load required libraries
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

# ================================================================================
# DATEN LADEN UND VORBEREITEN
# ================================================================================

# Load real data - use existing working dataset
data <- read.delim("organized/data/Datensatz.csv", 
                   stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Filter for valid cases only (FINISHED = 1)
data_filtered <- data[data$FINISHED == 1, ]

cat("Echte Daten geladen und gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data_filtered), "\n")

# ================================================================================
# MITTELWERTE BERECHNEN
# ================================================================================

# Calculate means by group for EA items
means_by_group <- data_filtered %>%
  group_by(AB01) %>%
  summarise(
    n = n(),
    # EA items (from the real data)
    EA01_mean = round(mean(as.numeric(EA01_01), na.rm = TRUE), 2),
    EA02_mean = round(mean(as.numeric(EA01_02), na.rm = TRUE), 2),
    EA03_mean = round(mean(as.numeric(EA01_03), na.rm = TRUE), 2),
    EA04_mean = round(mean(as.numeric(EA01_04), na.rm = TRUE), 2),
    EA05_mean = round(mean(as.numeric(EA01_05), na.rm = TRUE), 2)
  ) %>%
  mutate(
    Group = ifelse(AB01 == 1, "Mensch", "KI")
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
      padding = unit(c(2, 1), "mm")
    )
  )
  
  # Add borders
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

# ================================================================================
# EA-TABELLE ERSTELLEN
# ================================================================================

# Prepare table data for EA
ea_table_data <- means_by_group %>%
  select(Group, n, EA01_mean, EA02_mean, EA03_mean, EA04_mean, EA05_mean) %>%
  rename(
    "Gruppe" = Group,
    "EA01" = EA01_mean,
    "EA02" = EA02_mean,
    "EA03" = EA03_mean,
    "EA04" = EA04_mean,
    "EA05" = EA05_mean
  )

# Create APA-style table
ea_apa_table <- create_apa_mean_table(ea_table_data, "Emotionale Ansprache")

# Create output directory if it doesn't exist
dir.create("organized/images/descriptive", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG
png("organized/images/descriptive/mean_table_EA.png", 
    width = 1000, height = 300, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with minimal margins
vp <- viewport(x = 0.5, y = 0.5, width = 0.98, height = 0.98, just = c("center", "center"))
pushViewport(vp)

# Draw table directly without extra spacing
grid::grid.draw(ea_apa_table)
popViewport()
dev.off()

cat("Tabelle für Emotionale Ansprache erstellt: mean_table_EA.png\n")

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("\n================================================================================\n")
cat("MITTELWERTTABELLEN FÜR EINZELNE ITEMS ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• mean_table_EA.png (Emotionale Ansprache: EA01, EA02, EA03, EA04, EA05)\n")
cat("• Tabelle im APA-Stil mit gleichem Look\n")
cat("• Gruppierung: KI vs. Mensch\n")
cat("• Verwendet echte Daten aus WhatsApp Business Studie\n")
cat("================================================================================\n") 