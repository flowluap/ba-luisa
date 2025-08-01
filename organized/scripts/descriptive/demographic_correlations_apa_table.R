# Load required libraries
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

# Function to create APA-style demographic correlation table (same style as mean_table_EA.png)
create_apa_demographic_table <- function(table_data, title) {
  table_grob <- tableGrob(
    table_data,
    rows = NULL,
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

# Load data
cat("Lade Daten...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

cat("Daten erfolgreich geladen\n")
cat("Anzahl Zeilen:", nrow(data), "\n")
cat("Anzahl Spalten:", ncol(data), "\n")

# Filter data (FINISHED=1)
data_processed <- data %>%
  filter(FINISHED == 1)

cat("Daten gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data_processed), "\n")

# Check available demographic columns
cat("Verfügbare demografische Spalten:\n")
demo_cols <- colnames(data_processed)[grepl("^SE|^SO", colnames(data_processed))]
print(demo_cols)

# Calculate composite scores for target variables
target_vars <- data.frame(
  MN = rowMeans(data_processed[, grepl("^MN01_", colnames(data_processed))], na.rm = TRUE),
  VS = rowMeans(data_processed[, grepl("^VS01_", colnames(data_processed))], na.rm = TRUE),
  EA = rowMeans(data_processed[, grepl("^EA01_", colnames(data_processed))], na.rm = TRUE),
  ID = rowMeans(data_processed[, grepl("^ID01_", colnames(data_processed))], na.rm = TRUE)
)

cat("Target Variablen berechnet\n")

# Extract demographic variables (excluding SE04 as it's text-based)
demographic_vars <- data.frame(
  SE01 = as.numeric(data_processed$SE01_01),  # SE01
  SE02 = as.numeric(data_processed$SE02_01),  # SE02  
  SE03 = as.numeric(data_processed$SE03),     # SE03
  SO01 = as.numeric(data_processed$SO01_01),  # SO01 (Alter)
  SO02 = as.numeric(data_processed$SO02)      # SO02 (Geschlecht)
)

cat("Demografische Variablen extrahiert\n")
cat("Erste 5 Zeilen demografische Daten:\n")
print(head(demographic_vars, 5))

cat("Erste 5 Zeilen Zielvariablen:\n")
print(head(target_vars, 5))

# Function to calculate correlation without significance stars
calculate_correlation_simple <- function(x, y) {
  # Remove cases where either variable is NA
  valid_cases <- !is.na(x) & !is.na(y)
  if(sum(valid_cases) < 3) {
    return("NA")
  }
  
  x_clean <- x[valid_cases]
  y_clean <- y[valid_cases]
  
  cor_test <- cor.test(x_clean, y_clean, method = "pearson")
  cor_value <- cor_test$estimate
  
  # Format correlation value without stars
  cor_formatted <- sprintf("%.3f", cor_value)
  return(cor_formatted)
}

# Calculate correlations between target variables and demographic variables
correlation_results <- data.frame(
  Variable = c("MN", "VS", "EA", "ID"),
  stringsAsFactors = FALSE
)

# Calculate correlations for each demographic variable
for(demo_var in colnames(demographic_vars)) {
  demo_values <- demographic_vars[[demo_var]]
  
  correlation_results[[demo_var]] <- sapply(target_vars, function(target_values) {
    calculate_correlation_simple(target_values, demo_values)
  })
}

cat("\nKorrelationsergebnisse:\n")
print(correlation_results)

# Create APA table
demographic_apa_table <- create_apa_demographic_table(correlation_results, "Demografische Korrelationen")

# Create directory if it doesn't exist
dir.create("organized/images/correlations", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with optimal dimensions (more space at bottom)
png("organized/images/correlations/demographic_correlations_apa.png", 
    width = 1400, height = 250, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with more space at bottom
vp <- viewport(x = 0.5, y = 0.52, width = 0.98, height = 0.88, just = c("center", "center"))
pushViewport(vp)
grid::grid.draw(demographic_apa_table)
popViewport()

dev.off()

cat("Demografische Korrelations-Tabelle erstellt: demographic_correlations_apa.png\n")

cat("\n================================================================================\n")
cat("DEMOGRAFISCHE KORRELATIONS-TABELLE ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• demographic_correlations_apa.png (MN, VS, EA, ID vs SE01, SE02, SE03, SO01, SO02)\n")
cat("• SE04 ausgeschlossen da text-basiert (Discord, Pinterest, etc.)\n")
cat("• Tabelle im APA-Stil mit gleichem Look wie mean_table_EA.png\n")
cat("• Pearson Korrelationen ohne Signifikanz-Sterne\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• N =", nrow(data_processed), "\n")
cat("• Minimale Whitespace, vollständig sichtbar\n")
cat("================================================================================\n") 