# Load required libraries
library(dplyr)
library(gridExtra)
library(grid)
library(gtable)
library(psych)

# Function to create APA-style alpha table (same style as mean_table_EA.png)
create_apa_alpha_table <- function(table_data, title) {
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

# Check group sizes
group_sizes <- table(data_processed$AB01)
cat("Gruppengrößen:\n")
cat("KI (AB01=1):", group_sizes["1"], "\n")
cat("Mensch (AB01=2):", group_sizes["2"], "\n")

# Calculate composite scores
data_processed <- data_processed %>%
  rowwise() %>%
  mutate(
    MN = mean(c(MN01_01, MN01_02, MN01_03, MN01_04, MN01_05, MN01_06, MN01_07), na.rm = TRUE),
    VS = mean(c(VS01_01, VS01_02, VS01_03, VS01_04, VS01_05, VS01_06, VS01_07, VS01_08), na.rm = TRUE),
    EA = mean(c(EA01_01, EA01_02, EA01_03, EA01_04, EA01_05), na.rm = TRUE),
    ID = mean(c(ID01_01, ID01_02, ID01_03, ID01_04), na.rm = TRUE)
  ) %>%
  ungroup()

# Function to calculate Cronbach's Alpha for a scale
calculate_cronbach_alpha <- function(data, items) {
  scale_data <- data[, items, drop = FALSE]
  scale_data <- scale_data[complete.cases(scale_data), ]
  if(nrow(scale_data) < 2) return(NA)
  alpha_result <- psych::alpha(scale_data, check.keys = TRUE)
  return(alpha_result$total$raw_alpha)
}

# Function to perform t-test and calculate Cohen's d
perform_t_test_cohens_d <- function(data, variable, group_var = "AB01") {
  group1 <- data[data[[group_var]] == 1, variable]
  group2 <- data[data[[group_var]] == 2, variable]
  
  # Remove NA values
  group1 <- group1[!is.na(group1)]
  group2 <- group2[!is.na(group2)]
  
  if(length(group1) < 2 || length(group2) < 2) {
    return(list(p_value = NA, cohens_d = NA))
  }
  
  # Perform t-test
  t_result <- t.test(group1, group2)
  
  # Calculate Cohen's d
  pooled_sd <- sqrt(((length(group1) - 1) * var(group1) + (length(group2) - 1) * var(group2)) / 
                    (length(group1) + length(group2) - 2))
  cohens_d <- (mean(group1) - mean(group2)) / pooled_sd
  
  return(list(p_value = t_result$p.value, cohens_d = cohens_d))
}

# Calculate results for each scale
scales <- list(
  MN = c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07"),
  VS = c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08"),
  EA = c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05"),
  ID = c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
)

results <- data.frame(
  Variable = character(),
  Cronbachs_Alpha = numeric(),
  p_Wert = character(),
  Cohens_d = numeric(),
  stringsAsFactors = FALSE
)

for(scale_name in names(scales)) {
  cat("Berechne für Skala:", scale_name, "\n")
  
  # Calculate Cronbach's Alpha
  alpha_value <- calculate_cronbach_alpha(data_processed, scales[[scale_name]])
  
  # Perform t-test and calculate Cohen's d
  test_results <- perform_t_test_cohens_d(data_processed, scale_name)
  
  # Format p-value
  p_formatted <- if(is.na(test_results$p_value)) {
    "NA"
  } else if(test_results$p_value < 0.001) {
    "< 0.001"
  } else {
    sprintf("%.4f", test_results$p_value)
  }
  
  # Add to results
  results <- rbind(results, data.frame(
    Variable = scale_name,
    Cronbachs_Alpha = ifelse(is.na(alpha_value), "NA", sprintf("%.2f", alpha_value)),
    p_Wert = p_formatted,
    Cohens_d = ifelse(is.na(test_results$cohens_d), "NA", sprintf("%.2f", test_results$cohens_d)),
    stringsAsFactors = FALSE
  ))
}

cat("\nErgebnisse:\n")
print(results)

# Rename columns for APA style
colnames(results) <- c("Variable", "Cronbachs Alpha", "p-Wert", "Cohen's d")

cat("\nFinale Tabellendaten:\n")
print(results)

# Create APA table
alpha_apa_table <- create_apa_alpha_table(results, "Cronbach's Alpha, p-Werte und Cohen's d")

# Create directory if it doesn't exist
dir.create("organized/images", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with expanded dimensions (more space top and bottom)
png("organized/images/apa_tabelle_alpha_p_d_reduced.png", 
    width = 1400, height = 250, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with more margins (especially bottom)
vp <- viewport(x = 0.5, y = 0.52, width = 0.98, height = 0.88, just = c("center", "center"))
pushViewport(vp)
grid::grid.draw(alpha_apa_table)
popViewport()

dev.off()

cat("Alpha-p-d-Tabelle erstellt: apa_tabelle_alpha_p_d_reduced.png\n")

cat("\n================================================================================\n")
cat("APA ALPHA-P-D TABELLE ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• apa_tabelle_alpha_p_d_reduced.png (MN, VS, EA, ID)\n")
cat("• Tabelle im APA-Stil mit gleichem Look wie mean_table_EA.png\n")
cat("• Cronbach's Alpha für jede Skala\n")
cat("• p-Werte aus t-Tests (KI vs. Mensch)\n")
cat("• Cohen's d Effektstärken\n")
cat("• N-Werte: KI =", group_sizes["1"], ", Mensch =", group_sizes["2"], "\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• AB01=1 = KI, AB01=2 = Mensch\n")
cat("• Minimale Whitespace, vollständig sichtbar\n")
cat("================================================================================\n") 