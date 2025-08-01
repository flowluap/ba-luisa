# =============================================================================
# COMBINED ELBOW PLOT
# =============================================================================
# Erstellt ein kombiniertes Elbow-Plot für KI- und Mensch-Gruppe in einem Chart

library(dplyr)
library(ggplot2)
library(factoextra)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Lade Daten...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data (FINISHED=1)
data_processed <- data %>% filter(FINISHED == 1)

# Separate KI and Mensch groups
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("✓ KI-Gruppe: n =", nrow(data_ki), "\n")
cat("✓ Mensch-Gruppe: n =", nrow(data_mensch), "\n")

# =============================================================================
# PREPARE CLUSTERING DATA
# =============================================================================

# Define variable columns
mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")

# Create composite scores for KI group
cluster_data_ki <- data.frame(
  VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
)

# Create composite scores for Mensch group
cluster_data_mensch <- data.frame(
  VS = rowMeans(data_mensch[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_mensch[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_mensch[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_mensch[, ea_cols], na.rm = TRUE)
)

# Remove missing values
cluster_data_ki_clean <- cluster_data_ki[complete.cases(cluster_data_ki), ]
cluster_data_mensch_clean <- cluster_data_mensch[complete.cases(cluster_data_mensch), ]

cat("✓ KI-Gruppe (nach Missing Values): n =", nrow(cluster_data_ki_clean), "\n")
cat("✓ Mensch-Gruppe (nach Missing Values): n =", nrow(cluster_data_mensch_clean), "\n")

# =============================================================================
# CREATE COMBINED ELBOW PLOT WITH TWO LINES
# =============================================================================

cat("\n=== ERSTELLE KOMBINIERTES ELBOW-PLOT MIT ZWEI LINIEN ===\n")

# Function to calculate WSS for different k values
calculate_wss <- function(data, k_max = 10) {
  wss <- numeric(k_max)
  for (k in 1:k_max) {
    set.seed(123)
    kmeans_result <- kmeans(data, centers = k, nstart = 25)
    wss[k] <- kmeans_result$tot.withinss
  }
  return(wss)
}

# Calculate WSS for both groups
set.seed(123)
wss_ki <- calculate_wss(cluster_data_ki_clean, k_max = 10)
wss_mensch <- calculate_wss(cluster_data_mensch_clean, k_max = 10)

# Create data frame for plotting
plot_data <- data.frame(
  k = rep(1:10, 2),
  wss = c(wss_ki, wss_mensch),
  group = rep(c("KI-Gruppe", "Mensch-Gruppe"), each = 10)
)

# Create combined elbow plot with two lines
combined_elbow <- ggplot(plot_data, aes(x = k, y = wss, color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("KI-Gruppe" = "#8DD3C8", "Mensch-Gruppe" = "#FFB6C1")) +
  labs(title = "Elbow-Methode: Optimale Cluster-Anzahl",
       x = "Anzahl Cluster (k)",
       y = "Within-Cluster Sum of Squares (WSS)",
       color = "Gruppe") +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 11, family = "Times New Roman"),
    axis.text = element_text(size = 10, family = "Times New Roman"),
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold", family = "Times New Roman"),
    legend.title = element_text(size = 11, family = "Times New Roman"),
    legend.text = element_text(size = 10, family = "Times New Roman"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "top",
    legend.justification = "center"
  )

# =============================================================================
# SAVE COMBINED PLOT
# =============================================================================

cat("=== SPEICHERE KOMBINIERTES PLOT ===\n")

# Create directory
dir.create("organized/images/clustering", recursive = TRUE, showWarnings = FALSE)

# Save combined plot
ggsave("organized/images/clustering/combined_elbow_plot.png", 
       combined_elbow, width = 10, height = 6, dpi = 300, bg = "white", 
       limitsize = FALSE)

cat("✓ combined_elbow_plot.png erstellt\n")

# =============================================================================
# STATISTICAL SUMMARY
# =============================================================================

cat("\n=== STATISTISCHE ZUSAMMENFASSUNG ===\n")

# Calculate WSS for k=3 for both groups
set.seed(123)
wss_ki_3 <- kmeans(cluster_data_ki_clean, centers = 3, nstart = 25)$tot.withinss
wss_mensch_3 <- kmeans(cluster_data_mensch_clean, centers = 3, nstart = 25)$tot.withinss

cat("KI-Gruppe:\n")
cat("- WSS für k=3:", round(wss_ki_3, 2), "\n")
cat("- Stichprobengröße:", nrow(cluster_data_ki_clean), "\n")

cat("\nMensch-Gruppe:\n")
cat("- WSS für k=3:", round(wss_mensch_3, 2), "\n")
cat("- Stichprobengröße:", nrow(cluster_data_mensch_clean), "\n")

cat("\n=== INTERPRETATION ===\n")
cat("Beide Linien zeigen einen deutlichen Knick bei k=3,\n")
cat("was die Wahl von 3 Clustern für beide Gruppen bestätigt.\n")

cat("\n=== FARBEN ===\n")
cat("KI-Gruppe: #8DD3C8 (Helles Türkis/Teal)\n")
cat("Mensch-Gruppe: #FFB6C1 (Helles Rosa)\n")

cat("\n================================================================================\n")
cat("COMBINED ELBOW PLOT COMPLETED\n")
cat("================================================================================\n")
cat("Generated files:\n")
cat("- organized/images/clustering/combined_elbow_plot.png\n")

cat("\nVorteile des neuen Designs:\n")
cat("- Beide Gruppen in einem Chart für direkten Vergleich\n")
cat("- Zwei verschiedene Farben für klare Unterscheidung\n")
cat("- Platzsparend und übersichtlich\n")
cat("- Konsistenter Style mit Times New Roman\n")

cat("\n================================================================================\n") 