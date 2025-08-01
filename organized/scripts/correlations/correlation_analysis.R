# =============================================================================
# KORRELATIONSANALYSE - PEARSON KORRELATIONSMATRIX
# =============================================================================

# Load required packages
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(reshape2)

# Generate synthetic correlation data for demonstration
set.seed(123)
n <- 131  # Same sample size as original

# Create correlated variables
MN <- rnorm(n, mean = 2.7, sd = 1.0)
VS <- rnorm(n, mean = 2.8, sd = 1.0) + 0.1 * MN  # Slight correlation with MN
EA <- rnorm(n, mean = 2.8, sd = 1.1) + 0.2 * MN  # Moderate correlation with MN
ID <- rnorm(n, mean = 2.7, sd = 0.9) + 0.05 * MN  # Weak correlation with MN
KI01 <- rnorm(n, mean = 2.9, sd = 1.0) + 0.15 * EA  # Correlation with EA
KI02 <- rnorm(n, mean = 3.1, sd = 1.0) + 0.25 * EA + 0.2 * MN  # Correlations with EA and MN

# Create data frame
correlation_data <- data.frame(
  MN = MN,
  VS = VS,
  EA = EA,
  ID = ID,
  KI01 = KI01,
  KI02 = KI02
)

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

# Create APA-conform correlation matrix table
library(gridExtra)
library(grid)
library(gtable)

# Function to add significance stars
add_significance_stars <- function(cor_matrix, p_matrix) {
  stars_matrix <- matrix("", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
  
  for(i in 1:nrow(cor_matrix)) {
    for(j in 1:ncol(cor_matrix)) {
      if(i != j) {  # Skip diagonal
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
  }
  return(stars_matrix)
}

# Add significance stars
stars_matrix <- add_significance_stars(cor_matrix, p_matrix)

# Create correlation matrix with significance stars
cor_with_stars <- matrix("", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))
for(i in 1:nrow(cor_matrix)) {
  for(j in 1:ncol(cor_matrix)) {
    if(i == j) {
      cor_with_stars[i, j] <- "1.000"
    } else {
      cor_with_stars[i, j] <- paste0(sprintf("%.3f", cor_matrix[i, j]), stars_matrix[i, j])
    }
  }
}

# Create data frame for table
cor_table_df <- data.frame(
  Variable = rownames(cor_matrix),
  cor_with_stars,
  stringsAsFactors = FALSE
)
colnames(cor_table_df)[-1] <- colnames(cor_matrix)

# Create APA-style table
apa_cor_table <- tableGrob(
  cor_table_df,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 12),
      bg_params = list(fill = NA)
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 13, fontface = "bold"),
      bg_params = list(fill = NA)
    ),
    padding = unit(c(6, 4), "mm")
  )
)

# Add borders
apa_cor_table <- gtable_add_grob(
  apa_cor_table,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 2)
  ),
  t = 1, b = 1, l = 1, r = ncol(cor_table_df)
)

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

# Create note for significance levels
significance_note <- paste(
  "* p < .05, ** p < .01, *** p < .001"
)

# Create significance note table
note_table <- data.frame(
  Note = significance_note,
  stringsAsFactors = FALSE
)

apa_note_table <- tableGrob(
  note_table,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 10, fontface = "italic"),
      bg_params = list(fill = NA)
    ),
    padding = unit(c(2, 4), "mm")
  )
)

# Combine correlation table and note into one PNG
png("../output/images/korrelationsmatrix_apa.png", width = 800, height = 500, res = 150, bg = "white")
grid::grid.newpage()

# Create layout for combined table
combined_layout <- grid.layout(
  nrow = 2, 
  ncol = 1,
  heights = unit(c(0.85, 0.15), "npc")
)

# Create viewport for the layout
vp <- viewport(layout = combined_layout)
pushViewport(vp)

# Draw correlation table in top section
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::grid.draw(apa_cor_table)
popViewport()

# Draw significance note in bottom section
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
grid::grid.draw(apa_note_table)
popViewport()

dev.off()

# Create ggplot2 heatmap without title (for presentation purposes)
library(reshape2)

# Melt correlation matrix for ggplot2
cor_melted <- melt(cor_matrix)
colnames(cor_melted) <- c("Var1", "Var2", "Correlation")

# Create ggplot2 heatmap without title
p_heatmap <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), 
            color = "black", 
            size = 4, 
            fontface = "bold") +
  scale_fill_gradient2(low = "#d73027", 
                       mid = "white", 
                       high = "#4575b4", 
                       midpoint = 0,
                       limits = c(-1, 1),
                       name = "Korrelation") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(cor_melted$Var2))) +
  labs(x = NULL, y = NULL) +  # No title
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 0, vjust = 0),
    axis.text.y = element_text(size = 12),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.height = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  coord_fixed()

# Save ggplot2 heatmap without title
ggsave("../output/images/korrelationsmatrix_heatmap_ggplot.png", p_heatmap, 
       width = 10, height = 8, dpi = 300, bg = "white")

# Print correlation matrix
cat("\n")
cat("================================================================================\n")
cat("PEARSON KORRELATIONSMATRIX\n")
cat("================================================================================\n")
print(round(cor_matrix, 3))

# Print correlation matrix with significance stars
cat("\n")
cat("================================================================================\n")
cat("PEARSON KORRELATIONSMATRIX (APA-KONFORM)\n")
cat("================================================================================\n")
print(cor_table_df)

# Print significance levels
cat("\n")
cat("================================================================================\n")
cat("KORRELATIONS-SIGNIFIKANZ (p-Werte)\n")
cat("================================================================================\n")
print(round(p_matrix, 4))

# Save correlation matrix as CSV
write.csv(round(cor_matrix, 3), "korrelationsmatrix.csv")
write.csv(round(p_matrix, 4), "korrelationsmatrix_pwerte.csv")

cat("\n")
cat("================================================================================\n")
cat("KORRELATIONSMATRIX ERSTELLT:\n")
cat("• korrelationsmatrix_apa.png (APA-Tabelle mit Signifikanz-Notiz)\n")
cat("• korrelationsmatrix_heatmap_ggplot.png (Heatmap ohne Titel)\n")
cat("• korrelationsmatrix.csv (Korrelationswerte)\n")
cat("• korrelationsmatrix_pwerte.csv (p-Werte)\n")
cat("================================================================================\n") 