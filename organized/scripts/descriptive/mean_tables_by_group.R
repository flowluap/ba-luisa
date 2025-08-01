# ================================================================================
# MITTELWERTTABELLEN FÜR EINZELVARIABLEN PRO GRUPPE (KI vs MENSCH)
# ================================================================================
# 
# Ziel: Erstellung von APA-konformen Mittelwerttabellen für MN, VS, EA, ID
# Gruppierung: KI-Bedingung vs. Mensch-Bedingung
# Styling: Gleicher Look wie die vorherigen Tabellen
#
# Autor: Paul
# Datum: 2024
# ================================================================================

# Load required libraries
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

# ================================================================================
# SYNTHETISCHE DATEN GENERIEREN
# ================================================================================

set.seed(123)
n_total <- 131

# Create AB01 variable (1 = human, 2 = AI)
ab01 <- sample(c(1, 2), n_total, replace = TRUE, prob = c(0.5, 0.5))

# Create FINISHED variable (0 = dropout, 1 = completed)
finished <- sample(c(0, 1), n_total, replace = TRUE, prob = c(0.12, 0.88))

# Generate individual items data
# MN items (typically 3-7 items)
MN01 <- ifelse(ab01 == 1, rnorm(n_total, 3.3, 0.7), rnorm(n_total, 3.0, 0.8))
MN02 <- ifelse(ab01 == 1, rnorm(n_total, 3.4, 0.6), rnorm(n_total, 3.1, 0.7))
MN03 <- ifelse(ab01 == 1, rnorm(n_total, 3.2, 0.8), rnorm(n_total, 2.9, 0.9))
MN04 <- ifelse(ab01 == 1, rnorm(n_total, 3.3, 0.7), rnorm(n_total, 3.0, 0.8))
MN05 <- ifelse(ab01 == 1, rnorm(n_total, 3.2, 0.8), rnorm(n_total, 2.9, 0.9))
MN06 <- ifelse(ab01 == 1, rnorm(n_total, 3.3, 0.7), rnorm(n_total, 3.0, 0.8))
MN07 <- ifelse(ab01 == 1, rnorm(n_total, 3.1, 0.8), rnorm(n_total, 2.8, 0.9))

# VS items (typically 3-8 items)
VS01 <- ifelse(ab01 == 1, rnorm(n_total, 3.5, 0.8), rnorm(n_total, 3.2, 0.9))
VS02 <- ifelse(ab01 == 1, rnorm(n_total, 3.6, 0.7), rnorm(n_total, 3.3, 0.8))
VS03 <- ifelse(ab01 == 1, rnorm(n_total, 3.4, 0.9), rnorm(n_total, 3.1, 1.0))
VS04 <- ifelse(ab01 == 1, rnorm(n_total, 3.5, 0.8), rnorm(n_total, 3.2, 0.9))
VS05 <- ifelse(ab01 == 1, rnorm(n_total, 3.6, 0.7), rnorm(n_total, 3.1, 0.8))
VS06 <- ifelse(ab01 == 1, rnorm(n_total, 3.5, 0.8), rnorm(n_total, 3.3, 0.9))
VS07 <- ifelse(ab01 == 1, rnorm(n_total, 3.7, 0.6), rnorm(n_total, 3.2, 0.7))
VS08 <- ifelse(ab01 == 1, rnorm(n_total, 3.4, 0.8), rnorm(n_total, 3.0, 0.9))

# EA items (typically 3-5 items)
EA01 <- ifelse(ab01 == 1, rnorm(n_total, 3.2, 0.8), rnorm(n_total, 2.9, 0.9))
EA02 <- ifelse(ab01 == 1, rnorm(n_total, 3.3, 0.7), rnorm(n_total, 3.0, 0.8))
EA03 <- ifelse(ab01 == 1, rnorm(n_total, 3.1, 0.9), rnorm(n_total, 2.8, 1.0))
EA04 <- ifelse(ab01 == 1, rnorm(n_total, 3.2, 0.8), rnorm(n_total, 2.9, 0.9))
EA05 <- ifelse(ab01 == 1, rnorm(n_total, 3.1, 0.8), rnorm(n_total, 2.8, 0.9))

# ID items (typically 3-5 items)
ID01 <- ifelse(ab01 == 1, rnorm(n_total, 3.4, 0.7), rnorm(n_total, 3.1, 0.8))
ID02 <- ifelse(ab01 == 1, rnorm(n_total, 3.5, 0.6), rnorm(n_total, 3.2, 0.7))
ID03 <- ifelse(ab01 == 1, rnorm(n_total, 3.3, 0.8), rnorm(n_total, 3.0, 0.9))
ID04 <- ifelse(ab01 == 1, rnorm(n_total, 3.4, 0.7), rnorm(n_total, 3.1, 0.8))

# Create data frame with all items
data <- data.frame(
  FINISHED = finished,
  AB01 = ab01,
  # MN items
  MN01 = MN01, MN02 = MN02, MN03 = MN03, MN04 = MN04, MN05 = MN05, MN06 = MN06, MN07 = MN07,
  # VS items
  VS01 = VS01, VS02 = VS02, VS03 = VS03, VS04 = VS04, VS05 = VS05, VS06 = VS06, VS07 = VS07, VS08 = VS08,
  # EA items
  EA01 = EA01, EA02 = EA02, EA03 = EA03, EA04 = EA04, EA05 = EA05,
  # ID items
  ID01 = ID01, ID02 = ID02, ID03 = ID03, ID04 = ID04
)

# Ensure values are within 1-5 range
for(col in c("MN01", "MN02", "MN03", "MN04", "MN05", "MN06", "MN07", "VS01", "VS02", "VS03", "VS04", "VS05", "VS06", "VS07", "VS08",
             "EA01", "EA02", "EA03", "EA04", "EA05", "ID01", "ID02", "ID03", "ID04")) {
  data[[col]] <- pmax(1, pmin(5, data[[col]]))
}

# Filter for valid cases only (FINISHED = 1)
data_filtered <- data[data$FINISHED == 1, ]

cat("Daten generiert und gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data_filtered), "\n")

# ================================================================================
# MITTELWERTE BERECHNEN
# ================================================================================

# Calculate means by group for all items
means_by_group <- data_filtered %>%
  group_by(AB01) %>%
  summarise(
    n = n(),
    # MN items
    MN01_mean = round(mean(MN01), 2),
    MN02_mean = round(mean(MN02), 2),
    MN03_mean = round(mean(MN03), 2),
    MN04_mean = round(mean(MN04), 2),
    MN05_mean = round(mean(MN05), 2),
    MN06_mean = round(mean(MN06), 2),
    MN07_mean = round(mean(MN07), 2),
    # VS items
    VS01_mean = round(mean(VS01), 2),
    VS02_mean = round(mean(VS02), 2),
    VS03_mean = round(mean(VS03), 2),
    VS04_mean = round(mean(VS04), 2),
    VS05_mean = round(mean(VS05), 2),
    VS06_mean = round(mean(VS06), 2),
    VS07_mean = round(mean(VS07), 2),
    VS08_mean = round(mean(VS08), 2),
    # EA items
    EA01_mean = round(mean(EA01), 2),
    EA02_mean = round(mean(EA02), 2),
    EA03_mean = round(mean(EA03), 2),
    EA04_mean = round(mean(EA04), 2),
    EA05_mean = round(mean(EA05), 2),
    # ID items
    ID01_mean = round(mean(ID01), 2),
    ID02_mean = round(mean(ID02), 2),
    ID03_mean = round(mean(ID03), 2),
    ID04_mean = round(mean(ID04), 2)
  ) %>%
  mutate(
    Group = ifelse(AB01 == 1, "Mensch", "KI")
  )

cat("\nMittelwerte pro Gruppe:\n")
print(means_by_group)

# ================================================================================
# FUNKTION FÜR APA-TABELLEN
# ================================================================================

create_apa_mean_table <- function(variable_name, means_data) {
  
  # Create table data
  table_data <- data.frame(
    Gruppe = means_data$Group,
    n = means_data$n,
    Mittelwert = means_data[[paste0(variable_name, "_mean")]]
  )
  
  # Create APA-style table
  apa_table <- tableGrob(
    table_data,
    rows = NULL,
    theme = ttheme_default(
      core = list(
        fg_params = list(fontfamily = "Times New Roman", fontsize = 9),
        bg_params = list(fill = NA)
      ),
      colhead = list(
        fg_params = list(fontfamily = "Times New Roman", fontsize = 10, fontface = "bold"),
        bg_params = list(fill = NA)
      ),
      padding = unit(c(2, 1), "mm")
    )
  )
  
  # Add borders to match demographic correlations style
  apa_table <- gtable_add_grob(
    apa_table,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 2)
    ),
    t = 1, b = 1, l = 1, r = ncol(table_data)
  )
  
  apa_table <- gtable_add_grob(
    apa_table,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 2)
    ),
    t = nrow(table_data) + 1, b = nrow(table_data) + 1, l = 1, r = ncol(table_data)
  )
  
  return(apa_table)
}

# ================================================================================
# TABELLEN ERSTELLEN UND SPEICHERN
# ================================================================================

# Create output directory if it doesn't exist
if (!dir.exists("organized/images/descriptive")) {
  dir.create("organized/images/descriptive", recursive = TRUE)
}

# Create tables for each variable group with all items
variable_groups <- list(
  MN = c("MN01", "MN02", "MN03", "MN04", "MN05", "MN06", "MN07"),
  VS = c("VS01", "VS02", "VS03", "VS04", "VS05", "VS06", "VS07", "VS08"),
  EA = c("EA01", "EA02", "EA03", "EA04", "EA05"),
  ID = c("ID01", "ID02", "ID03", "ID04")
)

variable_names <- c("Menschlichkeit & Natürlichkeit", "Vertrauen & Sympathie", 
                   "Emotionale Ansprache", "Identifikation")

for(i in 1:length(variable_groups)) {
  var_group <- names(variable_groups)[i]
  var_name <- variable_names[i]
  items <- variable_groups[[i]]
  
  # Create table data for this variable group
  table_data <- data.frame(
    Gruppe = means_by_group$Group,
    n = means_by_group$n
  )
  
  # Add columns for each item
  for(item in items) {
    table_data[[item]] <- means_by_group[[paste0(item, "_mean")]]
  }
  
  # Create APA-style table
  apa_table <- tableGrob(
    table_data,
    rows = NULL,
    theme = ttheme_default(
      core = list(
        fg_params = list(fontfamily = "Times New Roman", fontsize = 9),
        bg_params = list(fill = NA)
      ),
      colhead = list(
        fg_params = list(fontfamily = "Times New Roman", fontsize = 10, fontface = "bold"),
        bg_params = list(fill = NA)
      ),
      padding = unit(c(2, 1), "mm")
    )
  )
  
  # Add borders to match demographic correlations style
  apa_table <- gtable_add_grob(
    apa_table,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 2)
    ),
    t = 1, b = 1, l = 1, r = ncol(table_data)
  )
  
  apa_table <- gtable_add_grob(
    apa_table,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 2)
    ),
    t = nrow(table_data) + 1, b = nrow(table_data) + 1, l = 1, r = ncol(table_data)
  )
  
  # Save table - adjust width based on number of columns
  table_width <- ifelse(var_group == "VS", 1600, 
                       ifelse(var_group == "MN", 1400, 1200))
  png(paste0("organized/images/descriptive/mean_table_", var_group, ".png"), 
      width = table_width, height = 300, res = 300, bg = "white")
  grid::grid.newpage()
  
  # Create viewport with minimal margins
  vp <- viewport(x = 0.5, y = 0.5, width = 0.95, height = 0.95, just = c("center", "center"))
  pushViewport(vp)
  
  # Draw table directly without extra spacing
  grid::grid.draw(apa_table)
  
  popViewport()
  dev.off()
  
  cat("Tabelle für", var_name, "erstellt: mean_table_", var_group, ".png\n")
}

# ================================================================================
# AUSGABE
# ================================================================================

cat("\n")
cat("================================================================================\n")
cat("MITTELWERTTABELLEN FÜR EINZELNE ITEMS ERSTELLT:\n")
cat("================================================================================\n")
cat("• mean_table_MN.png (Menschlichkeit & Natürlichkeit: MN01, MN02, MN03, MN04, MN05, MN06, MN07)\n")
cat("• mean_table_VS.png (Vertrauen & Sympathie: VS01, VS02, VS03, VS04, VS05, VS06, VS07, VS08)\n")
cat("• mean_table_EA.png (Emotionale Ansprache: EA01, EA02, EA03, EA04, EA05)\n")
cat("• mean_table_ID.png (Identifikation: ID01, ID02, ID03, ID04)\n")
cat("• Alle Tabellen im APA-Stil mit gleichem Look\n")
cat("• Gruppierung: KI vs. Mensch\n")
cat("• Jede Tabelle zeigt alle einzelnen Items der jeweiligen Variable\n")
cat("================================================================================\n") 