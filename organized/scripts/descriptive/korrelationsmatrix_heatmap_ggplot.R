# Load required libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

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

# Calculate correlation matrix
cor_matrix <- cor(correlation_data, method = "pearson", use = "complete.obs")

cat("Korrelationsmatrix berechnet:\n")
print(round(cor_matrix, 3))

# Melt correlation matrix for ggplot2
cor_melted <- melt(cor_matrix)
colnames(cor_melted) <- c("Var1", "Var2", "Correlation")

cat("Daten für ggplot2 vorbereitet\n")
cat("Erste 10 Zeilen der geschmolzenen Matrix:\n")
print(head(cor_melted, 10))

# Create ggplot2 heatmap (matching the style of your image)
p_heatmap <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), 
            color = "black", 
            size = 4, 
            fontface = "bold") +
  scale_fill_gradient2(
    low = "#d73027",      # Red for negative correlations
    mid = "#f7f7f7",      # Light gray for near-zero correlations  
    high = "#4575b4",     # Blue for positive correlations
    midpoint = 0,
    name = "Korrelation",
    limits = c(-1, 1),
    breaks = c(-1.0, -0.5, 0.0, 0.5, 1.0),
    labels = c("-1.0", "-0.5", "0.0", "0.5", "1.0")
  ) +
  labs(
    title = NULL,
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 0, 
      vjust = 0.5, 
      hjust = 0.5,
      size = 12,
      face = "bold",
      family = "Times"
    ),
    axis.text.y = element_text(
      size = 12,
      face = "bold", 
      family = "Times"
    ),
    legend.title = element_text(
      size = 12,
      face = "bold",
      family = "Times"
    ),
    legend.text = element_text(
      size = 10,
      family = "Times"
    ),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  coord_fixed(ratio = 1)  # Keep square aspect ratio

# Create directory if it doesn't exist
dir.create("organized/images", recursive = TRUE, showWarnings = FALSE)

# Save as PNG with high resolution
ggsave("organized/images/korrelationsmatrix_heatmap_ggplot.png", 
       plot = p_heatmap,
       width = 10, 
       height = 8, 
       dpi = 300, 
       bg = "white",
       device = "png")

cat("Korrelationsmatrix-Heatmap erstellt: korrelationsmatrix_heatmap_ggplot.png\n")

# Also save correlation matrix as CSV for reference
write.csv(round(cor_matrix, 3), "korrelationsmatrix_echte_daten.csv", row.names = TRUE)

cat("\n================================================================================\n")
cat("KORRELATIONSMATRIX-HEATMAP ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• korrelationsmatrix_heatmap_ggplot.png (MN, VS, EA, ID, KI01, KI02)\n")
cat("• ggplot2-Heatmap mit Farbcodierung und Korrelationswerten\n")
cat("• Rot = negative Korrelationen, Blau = positive Korrelationen\n")
cat("• Ohne Titel für saubere Darstellung\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• N =", nrow(correlation_data), "\n")
cat("• Hochauflösend: 10x8 Zoll, 300 DPI\n")
cat("• Korrelationsmatrix gespeichert als: korrelationsmatrix_echte_daten.csv\n")
cat("================================================================================\n") 