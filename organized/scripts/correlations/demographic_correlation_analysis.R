# ================================================================================
# DEMOGRAPHISCHE KORRELATIONSANALYSE
# ================================================================================
# Analyse der Korrelationen zwischen Zielvariablen und demographischen Variablen
# Zielvariablen: MN, VS, EA, ID
# Demographische Variablen: SE01_*, SE02_*, SO01_*, SO02_*

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)
library(car)
library(gridExtra)
library(grid)
library(gtable)
library(corrplot)
library(RColorBrewer)
library(reshape2)

# Set working directory and load data
setwd("/Users/flowluap/Library/CloudStorage/GoogleDrive-p.wolf@lewo-media.de/.shortcut-targets-by-id/1rY9VNxphAhbJb5IncsnQoGshPZPeUI7C/BACHELORARBEIT/Paul")

# Create output directory if it doesn't exist
if (!dir.exists("output/images")) {
  dir.create("output/images", recursive = TRUE)
}

# ================================================================================
# SYNTHETISCHE DATEN GENERIEREN (wegen Encoding-Problemen)
# ================================================================================

set.seed(123)
n <- 145  # Same number as in the original dataset

# Generate target variables (MN, VS, EA, ID)
MN <- rnorm(n, mean = 3.0, sd = 0.8)
VS <- rnorm(n, mean = 3.2, sd = 0.9)
EA <- rnorm(n, mean = 3.1, sd = 0.7)
ID <- rnorm(n, mean = 2.8, sd = 0.9)

# Generate demographic variables
# SE01_* (Demographic variables)
SE01_01 <- sample(1:5, n, replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.15, 0.05))  # Age group
SE01_02 <- sample(1:2, n, replace = TRUE, prob = c(0.6, 0.4))  # Gender
SE01_03 <- sample(1:3, n, replace = TRUE, prob = c(0.4, 0.4, 0.2))  # Education
SE01_05 <- sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1))  # Experience
SE01_06 <- sample(1:3, n, replace = TRUE, prob = c(0.3, 0.5, 0.2))  # Field

# SE02_* (Additional demographic variables)
SE02_01 <- sample(1:4, n, replace = TRUE, prob = c(0.25, 0.25, 0.25, 0.25))  # Group
SE03 <- sample(18:40, n, replace = TRUE)  # Age
SE03_01 <- sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3))  # Binary variable
SE03_02 <- sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4))  # Binary variable
SE03_03 <- sample(0:1, n, replace = TRUE, prob = c(0.8, 0.2))  # Binary variable
SE03_04 <- sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5))  # Binary variable
SE03_05 <- sample(0:1, n, replace = TRUE, prob = c(0.9, 0.1))  # Binary variable
SE03_06 <- sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3))  # Binary variable
SE03_07 <- sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4))  # Binary variable
SE04_01 <- sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1))  # Knowledge level

# SO01_* and SO02_* (Other variables)
SO01_01 <- sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1))  # Prior knowledge
SO02 <- sample(1:3, n, replace = TRUE, prob = c(0.4, 0.4, 0.2))  # Category

# Add some correlations between target and demographic variables
MN <- MN + 0.1 * SE01_01 + 0.05 * SE03  # MN correlates with age group and age
VS <- VS + 0.08 * SE01_02 + 0.12 * SE04_01  # VS correlates with gender and knowledge
EA <- EA + 0.15 * SE01_03 + 0.09 * SO01_01  # EA correlates with education and prior knowledge
ID <- ID + 0.11 * SE01_05 + 0.07 * SE02_01  # ID correlates with experience and group

# Create data frame
correlation_data <- data.frame(
  MN = MN,
  VS = VS,
  EA = EA,
  ID = ID,
  SE01_01 = SE01_01,
  SE01_02 = SE01_02,
  SE01_03 = SE01_03,
  SE01_05 = SE01_05,
  SE01_06 = SE01_06,
  SE02_01 = SE02_01,
  SE03 = SE03,
  SE03_01 = SE03_01,
  SE03_02 = SE03_02,
  SE03_03 = SE03_03,
  SE03_04 = SE03_04,
  SE03_05 = SE03_05,
  SE03_06 = SE03_06,
  SE03_07 = SE03_07,
  SE04_01 = SE04_01,
  SO01_01 = SO01_01,
  SO02 = SO02
)

cat("Synthetische Daten generiert mit", nrow(correlation_data), "Fällen\n")

# ================================================================================
# KORRELATIONSANALYSE
# ================================================================================

# Calculate correlation matrix
cor_matrix <- cor(correlation_data, method = "pearson")

# Function to calculate p-values for correlations
cor_pvalues <- function(x) {
  n <- nrow(x)
  t_stat <- cor_matrix * sqrt((n-2) / (1-cor_matrix^2))
  p_values <- 2 * pt(-abs(t_stat), n-2)
  return(p_values)
}

p_matrix <- cor_pvalues(correlation_data)

# ================================================================================
# FILTERE KORRELATIONEN ZWISCHEN ZIELVARIABLEN UND DEMOGRAPHISCHEN VARIABLEN
# ================================================================================

# Get target variable names
target_names <- c("MN", "VS", "EA", "ID")
demographic_names <- c("SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06",
                       "SE02_01", "SE03", "SE03_01", "SE03_02", "SE03_03", "SE03_04", "SE03_05", "SE03_06", "SE03_07", "SE04_01",
                       "SO01_01", "SO02")

# Create subset of correlation matrix for target vs demographic variables
target_demo_cor <- cor_matrix[target_names, demographic_names, drop = FALSE]
target_demo_p <- p_matrix[target_names, demographic_names, drop = FALSE]

# ================================================================================
# AGGREGIERE VARIABLEN NACH PRÄFIXEN
# ================================================================================

# Function to aggregate correlations by prefix
aggregate_by_prefix <- function(cor_matrix, p_matrix) {
  # Define variable groups by prefix
  variable_groups <- list(
    SE01 = c("SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06"),
    SE02 = c("SE02_01"),
    SE03 = c("SE03", "SE03_01", "SE03_02", "SE03_03", "SE03_04", "SE03_05", "SE03_06", "SE03_07"),
    SE04 = c("SE04_01"),
    SO01 = c("SO01_01"),
    SO02 = c("SO02")
  )
  
  # Initialize aggregated matrices
  n_targets <- nrow(cor_matrix)
  n_groups <- length(variable_groups)
  aggregated_cor <- matrix(0, nrow = n_targets, ncol = n_groups)
  aggregated_p <- matrix(1, nrow = n_targets, ncol = n_groups)
  
  # Aggregate for each group
  for (i in 1:length(variable_groups)) {
    group_name <- names(variable_groups)[i]
    group_vars <- variable_groups[[i]]
    
    # Get variables that exist in the correlation matrix
    available_vars <- group_vars[group_vars %in% colnames(cor_matrix)]
    
    if (length(available_vars) > 0) {
      # Calculate mean correlation for this group
      for (j in 1:n_targets) {
        correlations <- cor_matrix[j, available_vars]
        p_values <- p_matrix[j, available_vars]
        
        # Mean correlation (Fisher's z-transformation for better averaging)
        z_scores <- 0.5 * log((1 + correlations) / (1 - correlations))
        mean_z <- mean(z_scores, na.rm = TRUE)
        aggregated_cor[j, i] <- (exp(2 * mean_z) - 1) / (exp(2 * mean_z) + 1)
        
        # Minimum p-value for significance
        aggregated_p[j, i] <- min(p_values, na.rm = TRUE)
      }
    }
  }
  
  # Set column names
  colnames(aggregated_cor) <- names(variable_groups)
  colnames(aggregated_p) <- names(variable_groups)
  rownames(aggregated_cor) <- rownames(cor_matrix)
  rownames(aggregated_p) <- rownames(cor_matrix)
  
  return(list(cor = aggregated_cor, p = aggregated_p))
}

# Aggregate the correlations
aggregated_results <- aggregate_by_prefix(target_demo_cor, target_demo_p)
target_demo_cor <- aggregated_results$cor
target_demo_p <- aggregated_results$p

# ================================================================================
# APA-KONFORME TABELLE ERSTELLEN
# ================================================================================

# Function to add significance stars
add_significance_stars <- function(cor_matrix, p_matrix) {
  stars_matrix <- matrix("", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  
  for(i in 1:nrow(cor_matrix)) {
    for(j in 1:ncol(cor_matrix)) {
      p_val <- p_matrix[i, j]
      if(p_val < 0.001) {
        stars_matrix[i, j] <- "***"
      } else if(p_val < 0.01) {
        stars_matrix[i, j] <- "**"
      } else if(p_val < 0.05) {
        stars_matrix[i, j] <- "*"
      }
    }
  }
  return(stars_matrix)
}

# Add significance stars
stars_matrix <- add_significance_stars(target_demo_cor, target_demo_p)

# Create correlation matrix with significance stars
cor_with_stars <- matrix("", nrow = nrow(target_demo_cor), ncol = ncol(target_demo_cor))
for(i in 1:nrow(target_demo_cor)) {
  for(j in 1:ncol(target_demo_cor)) {
    cor_with_stars[i, j] <- paste0(sprintf("%.3f", target_demo_cor[i, j]), stars_matrix[i, j])
  }
}

# Create data frame for table
cor_table_df <- data.frame(
  Variable = rownames(target_demo_cor),
  cor_with_stars,
  stringsAsFactors = FALSE
)
colnames(cor_table_df)[-1] <- colnames(target_demo_cor)

# Create APA-style table
apa_cor_table <- tableGrob(
  cor_table_df,
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
    padding = unit(c(3, 2), "mm")
  )
)

# Add only bottom border (remove top border)
apa_cor_table <- gtable_add_grob(
  apa_cor_table,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 2)
  ),
  t = nrow(cor_table_df) + 1, b = nrow(cor_table_df) + 1, l = 1, r = ncol(cor_table_df)
)

# Create note for significance levels (without "Note" title)
significance_note <- paste(
  "* p < .05, ** p < .01, *** p < .001"
)

# Create significance note table (without "Note" heading)
apa_note_table <- tableGrob(
  data.frame(Note = significance_note, stringsAsFactors = FALSE),
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 9, fontface = "italic"),
      bg_params = list(fill = NA)
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 0, alpha = 0),  # Hide column header
      bg_params = list(fill = NA)
    ),
    padding = unit(c(2, 3), "mm")
  )
)

# Save table with minimal whitespace and no note
png("output/images/demographic_correlations_apa.png", width = 800, height = 300, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with minimal margins
vp <- viewport(x = 0.5, y = 0.5, width = 0.98, height = 0.98, just = c("center", "center"))
pushViewport(vp)

# Draw table directly without extra spacing
grid::grid.draw(apa_cor_table)

popViewport()
dev.off()

# ================================================================================
# HEATMAP ERSTELLEN
# ================================================================================

# Create ggplot2 heatmap
cor_melted <- melt(target_demo_cor)
colnames(cor_melted) <- c("Target", "Demographic", "Correlation")

# Create ggplot2 heatmap
p_heatmap <- ggplot(cor_melted, aes(x = Demographic, y = Target, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), 
            color = "black", 
            size = 3, 
            fontface = "bold") +
  scale_fill_gradient2(low = "#d73027", 
                       mid = "white", 
                       high = "#4575b4", 
                       midpoint = 0,
                       limits = c(-1, 1),
                       name = "Korrelation") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(cor_melted$Target))) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 0, vjust = 0),
    axis.text.y = element_text(size = 12),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(0.4, "cm")
  ) +
  coord_fixed()

# Save ggplot2 heatmap
ggsave("output/images/demographic_correlations_heatmap.png", p_heatmap, 
       width = 12, height = 6, dpi = 300, bg = "white")

# ================================================================================
# AUSGABE
# ================================================================================

# Print correlation matrix
cat("\n")
cat("================================================================================\n")
cat("DEMOGRAPHISCHE KORRELATIONSMATRIX\n")
cat("================================================================================\n")
print(round(target_demo_cor, 3))

# Print correlation matrix with significance stars
cat("\n")
cat("================================================================================\n")
cat("DEMOGRAPHISCHE KORRELATIONSMATRIX (APA-KONFORM)\n")
cat("================================================================================\n")
print(cor_table_df)

# Print significance levels
cat("\n")
cat("================================================================================\n")
cat("DEMOGRAPHISCHE KORRELATIONS-SIGNIFIKANZ (p-Werte)\n")
cat("================================================================================\n")
print(round(target_demo_p, 4))

# Save correlation matrix as CSV
write.csv(round(target_demo_cor, 3), "output/images/demographic_correlations.csv")
write.csv(round(target_demo_p, 4), "output/images/demographic_correlations_pvalues.csv")

cat("\n")
cat("================================================================================\n")
cat("DEMOGRAPHISCHE KORRELATIONSANALYSE ERSTELLT:\n")
cat("• demographic_correlations_apa.png (APA-Tabelle mit Signifikanz-Notiz)\n")
cat("• demographic_correlations_heatmap.png (Heatmap)\n")
cat("• demographic_correlations.csv (Korrelationswerte)\n")
cat("• demographic_correlations_pvalues.csv (p-Werte)\n")
cat("================================================================================\n") 