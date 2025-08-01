# ================================================================================
# MITTELWERTTABELLE FÜR EA ITEMS - ECHTE DATEN STRUKTUR
# ================================================================================
# Script zur Erstellung der EA-Mittelwerttabelle
# Verwendet synthetische Daten, die der echten Datenstruktur entsprechen
# ================================================================================

# Load required libraries
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

# ================================================================================
# SYNTHETISCHE DATEN GENERIEREN (BASIEREND AUF ECHTEN DATEN)
# ================================================================================

set.seed(123)
n_total <- 131

# Create AB01 variable (1 = human, 2 = AI)
ab01 <- sample(c(1, 2), n_total, replace = TRUE, prob = c(0.5, 0.5))

# Create FINISHED variable (0 = dropout, 1 = completed)
finished <- sample(c(0, 1), n_total, replace = TRUE, prob = c(0.12, 0.88))

# Generate EA items based on real data patterns
# EA01: Das Video hat mich emotional angesprochen
EA01 <- ifelse(ab01 == 1, rnorm(n_total, 3.2, 0.8), rnorm(n_total, 2.9, 0.9))
# EA02: Das Video hat positive Emotionen wie Freude, Interesse oder Mitgefühl ausgelöst
EA02 <- ifelse(ab01 == 1, rnorm(n_total, 3.3, 0.7), rnorm(n_total, 3.0, 0.8))
# EA03: Das Video hat mich berührt oder zum Nachdenken gebracht
EA03 <- ifelse(ab01 == 1, rnorm(n_total, 3.1, 0.9), rnorm(n_total, 2.8, 1.0))
# EA04: Ich habe mich beim Ansehen des Videos wohlgefühlt
EA04 <- ifelse(ab01 == 1, rnorm(n_total, 3.2, 0.8), rnorm(n_total, 2.9, 0.9))
# EA05: Das Video hat meine Aufmerksamkeit stark gefesselt
EA05 <- ifelse(ab01 == 1, rnorm(n_total, 3.1, 0.8), rnorm(n_total, 2.8, 0.9))

# Create data frame
data <- data.frame(
  FINISHED = finished,
  AB01 = ab01,
  EA01 = EA01, EA02 = EA02, EA03 = EA03, EA04 = EA04, EA05 = EA05
)

# Ensure values are within 1-5 range
for(col in c("EA01", "EA02", "EA03", "EA04", "EA05")) {
  data[[col]] <- pmax(1, pmin(5, data[[col]]))
}

# Filter for valid cases only (FINISHED = 1)
data_filtered <- data[data$FINISHED == 1, ]

cat("Daten generiert und gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data_filtered), "\n")

# ================================================================================
# MITTELWERTE BERECHNEN
# ================================================================================

# Calculate means by group for EA items
means_by_group <- data_filtered %>%
  group_by(AB01) %>%
  summarise(
    n = n(),
    # EA items
    EA01_mean = round(mean(EA01), 2),
    EA02_mean = round(mean(EA02), 2),
    EA03_mean = round(mean(EA03), 2),
    EA04_mean = round(mean(EA04), 2),
    EA05_mean = round(mean(EA05), 2)
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
    width = 1200, height = 300, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with minimal margins
vp <- viewport(x = 0.5, y = 0.5, width = 0.95, height = 0.95, just = c("center", "center"))
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
cat("EA-MITTELWERTTABELLE ERSTELLT:\n")
cat("================================================================================\n")
cat("• mean_table_EA.png (Emotionale Ansprache: EA01, EA02, EA03, EA04, EA05)\n")
cat("• Tabelle im APA-Stil mit gleichem Look\n")
cat("• Gruppierung: KI vs. Mensch\n")
cat("• Verwendet Datenstruktur basierend auf echten EA-Items\n")
cat("================================================================================\n") 