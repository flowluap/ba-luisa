# Load required libraries
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)

# Function to create APA-style correlation matrix table (same style as mean_table_EA.png)
create_apa_correlation_matrix_table <- function(cor_matrix, p_matrix, title) {
  # Format correlation matrix without significance stars
  cor_formatted <- cor_matrix
  for(i in 1:nrow(cor_matrix)) {
    for(j in 1:ncol(cor_matrix)) {
      if(i == j) {
        cor_formatted[i,j] <- "1.000"
      } else {
        cor_value <- sprintf("%.3f", cor_matrix[i,j])
        cor_formatted[i,j] <- cor_value
      }
    }
  }
  
  # Convert to data frame with variable names as first column
  cor_table_df <- data.frame(
    Variable = rownames(cor_formatted),
    cor_formatted,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  table_grob <- tableGrob(
    cor_table_df,
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
    t = 1, b = 1, l = 1, r = ncol(cor_table_df)
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
    t = nrow(cor_table_df) + 1, b = nrow(cor_table_df) + 1, l = 1, r = ncol(cor_table_df)
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

# Calculate composite scores (same as in previous scripts)
correlation_data <- data.frame(
  MN = rowMeans(data_processed[, grepl("^MN01_", colnames(data_processed))], na.rm = TRUE),
  VS = rowMeans(data_processed[, grepl("^VS01_", colnames(data_processed))], na.rm = TRUE),
  EA = rowMeans(data_processed[, grepl("^EA01_", colnames(data_processed))], na.rm = TRUE),
  ID = rowMeans(data_processed[, grepl("^ID01_", colnames(data_processed))], na.rm = TRUE),
  KI01 = rowMeans(data_processed[, grepl("^KI01_", colnames(data_processed))], na.rm = TRUE),
  KI02 = rowMeans(data_processed[, grepl("^KI02_", colnames(data_processed))], na.rm = TRUE)
)

cat("Composite Scores berechnet\n")
cat("Erste 5 Zeilen:\n")
print(head(correlation_data, 5))

# Calculate correlation matrix
cor_matrix <- cor(correlation_data, method = "pearson", use = "complete.obs")

cat("\nKorrelationsmatrix:\n")
print(round(cor_matrix, 3))

# Function to calculate p-values for correlations
calculate_cor_pvalues <- function(data) {
  n <- nrow(data)
  cor_mat <- cor(data, use = "complete.obs")
  p_values <- matrix(NA, nrow = ncol(data), ncol = ncol(data))
  
  for(i in 1:ncol(data)) {
    for(j in 1:ncol(data)) {
      if(i != j) {
        cor_test_result <- cor.test(data[,i], data[,j], method = "pearson")
        p_values[i,j] <- cor_test_result$p.value
      } else {
        p_values[i,j] <- 0  # Diagonal correlations are always significant (r=1)
      }
    }
  }
  
  rownames(p_values) <- colnames(data)
  colnames(p_values) <- colnames(data)
  return(p_values)
}

p_matrix <- calculate_cor_pvalues(correlation_data)

cat("\nP-Werte Matrix:\n")
print(round(p_matrix, 3))

# Create APA correlation matrix table
correlation_apa_table <- create_apa_correlation_matrix_table(cor_matrix, p_matrix, "Korrelationsmatrix")

# Create directory if it doesn't exist
dir.create("organized/images", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with optimal dimensions
png("organized/images/korrelationsmatrix_apa.png", 
    width = 1800, height = 350, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with optimal margins
vp <- viewport(x = 0.5, y = 0.5, width = 0.98, height = 0.95, just = c("center", "center"))
pushViewport(vp)
grid::grid.draw(correlation_apa_table)
popViewport()

dev.off()

cat("Korrelationsmatrix-Tabelle erstellt: korrelationsmatrix_apa.png\n")

cat("\n================================================================================\n")
cat("KORRELATIONSMATRIX-TABELLE ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• korrelationsmatrix_apa.png (MN, VS, EA, ID, KI01, KI02)\n")
cat("• Tabelle im APA-Stil mit gleichem Look wie mean_table_EA.png\n")
cat("• Pearson Korrelationen ohne Signifikanz-Sterne\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• N =", nrow(correlation_data), "\n")
cat("• Minimale Whitespace, vollständig sichtbar\n")
cat("================================================================================\n") 