# =============================================================================
# GENERATE ALL CLUSTER ANALYSES - MASTER SCRIPT
# =============================================================================
# Generiert alle wichtigen Cluster-Analysen in einer Datei:
# 1. ki_specific_apa.png
# 2. human_specific_apa.png  
# 3. cluster_prozent_tabelle.png
# 4. cluster_mittelwerte_ki_style.png

library(dplyr)
library(gridExtra)
library(grid)
library(gtable)
library(ggplot2)

cat("================================================================================\n")
cat("GENERATE ALL CLUSTER ANALYSES - MASTER SCRIPT\n")
cat("================================================================================\n")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Function to create APA-style table
create_apa_table <- function(table_data, title) {
  table_grob <- tableGrob(
    table_data,
    rows = NULL,
    theme = ttheme_default(
      base_family = "Times New Roman",
      base_size = 12,
      core = list(
        fg_params = list(hjust = 0, x = 0.1),
        bg_params = list(fill = NA),
        padding = unit(c(6, 1), "mm")
      ),
      colhead = list(
        fg_params = list(fontface = "bold", hjust = 0, x = 0.1),
        bg_params = list(fill = NA),
        padding = unit(c(0.1, 1), "mm")
      ),
      rowhead = list(
        fg_params = list(hjust = 0, x = 0.1),
        bg_params = list(fill = NA),
        padding = unit(c(6, 1), "mm")
      )
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
  
  # Adjust header height
  table_grob$heights[1] <- unit(1.5, "mm")
  
  # Add bottom border
  table_grob <- gtable_add_grob(
    table_grob,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 3, col = "black")
    ),
    t = nrow(table_data) + 1, b = nrow(table_data) + 1, l = 1, r = ncol(table_data)
  )
  
  return(table_grob)
}

# Function to perform clustering with logical naming
perform_clustering_logical <- function(data, group_name) {
  # Calculate composite scores
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  
  # Create composite scores
  cluster_data <- data.frame(
    VS = rowMeans(data[, vs_cols], na.rm = TRUE),
    MN = rowMeans(data[, mn_cols], na.rm = TRUE),
    ID = rowMeans(data[, id_cols], na.rm = TRUE),
    EA = rowMeans(data[, ea_cols], na.rm = TRUE)
  )
  
  # Remove missing values
  cluster_data_clean <- cluster_data[complete.cases(cluster_data), ]
  
  # Perform k-means clustering
  set.seed(123)
  kmeans_result <- kmeans(cluster_data_clean, centers = 3, nstart = 25)
  clusters <- kmeans_result$cluster
  
  # Calculate cluster means
  cluster_means <- data.frame(
    cluster_number = 1:3,
    n = as.numeric(table(clusters)),
    VS = round(tapply(cluster_data_clean$VS, clusters, mean), 3),
    MN = round(tapply(cluster_data_clean$MN, clusters, mean), 3),
    ID = round(tapply(cluster_data_clean$ID, clusters, mean), 3),
    EA = round(tapply(cluster_data_clean$EA, clusters, mean), 3)
  )
  
  # Calculate overall mean for each cluster
  cluster_means$overall_mean <- (cluster_means$VS + cluster_means$MN + 
                                cluster_means$ID + cluster_means$EA) / 4
  
  # Sort by overall mean (highest to lowest)
  cluster_means_sorted <- cluster_means[order(cluster_means$overall_mean, decreasing = TRUE), ]
  
  # Assign names based on ranking
  if(group_name == "KI") {
    cluster_means_sorted$cluster_name <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")
  } else {
    cluster_means_sorted$cluster_name <- c("Emotional Offen", "Ambivalent", "Emotional Distanziert")
  }
  
  return(list(
    clusters = clusters,
    cluster_means = cluster_means_sorted,
    total_n = nrow(cluster_data_clean)
  ))
}

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Lade Daten...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data (FINISHED=1)
data_processed <- data %>% filter(FINISHED == 1)

# Split by group
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("✓ KI-Gruppe: n =", nrow(data_ki), "\n")
cat("✓ Mensch-Gruppe: n =", nrow(data_mensch), "\n")

# =============================================================================
# 1. GENERATE KI-SPECIFIC APA TABLE
# =============================================================================

cat("\n=== 1. GENERATE KI-SPECIFIC APA TABLE ===\n")

ki_result <- perform_clustering_logical(data_ki, "KI")

# Create KI-specific table
ki_table_data <- data.frame(
  Cluster = ki_result$cluster_means$cluster_name,
  n = ki_result$cluster_means$n,
  VS = ki_result$cluster_means$VS,
  MN = ki_result$cluster_means$MN,
  ID = ki_result$cluster_means$ID,
  EA = ki_result$cluster_means$EA,
  stringsAsFactors = FALSE
)

ki_apa_table <- create_apa_table(ki_table_data, "KI-Specific Cluster Analysis")

# Create directory and save
dir.create("organized/images/clustering", recursive = TRUE, showWarnings = FALSE)
png("organized/images/clustering/ki_specific_apa.png", 
    width = 1200, height = 300, res = 300, bg = "white")
grid.newpage()
grid.draw(ki_apa_table)
dev.off()

cat("✓ ki_specific_apa.png erstellt\n")

# =============================================================================
# 2. GENERATE HUMAN-SPECIFIC APA TABLE
# =============================================================================

cat("\n=== 2. GENERATE HUMAN-SPECIFIC APA TABLE ===\n")

mensch_result <- perform_clustering_logical(data_mensch, "MENSCH")

# Create Mensch-specific table
mensch_table_data <- data.frame(
  Cluster = mensch_result$cluster_means$cluster_name,
  n = mensch_result$cluster_means$n,
  VS = mensch_result$cluster_means$VS,
  MN = mensch_result$cluster_means$MN,
  ID = mensch_result$cluster_means$ID,
  EA = mensch_result$cluster_means$EA,
  stringsAsFactors = FALSE
)

mensch_apa_table <- create_apa_table(mensch_table_data, "Human-Specific Cluster Analysis")

# Save
png("organized/images/clustering/human_specific_apa.png", 
    width = 1200, height = 300, res = 300, bg = "white")
grid.newpage()
grid.draw(mensch_apa_table)
dev.off()

cat("✓ human_specific_apa.png erstellt\n")

# =============================================================================
# 3. GENERATE CLUSTER PROZENT TABLE
# =============================================================================

cat("\n=== 3. GENERATE CLUSTER PROZENT TABLE ===\n")

# Create result table
results <- data.frame(
  Cluster = c(ki_result$cluster_means$cluster_name, mensch_result$cluster_means$cluster_name),
  Gruppe = c(rep("KI", 3), rep("Mensch", 3)),
  N = c(ki_result$cluster_means$n, mensch_result$cluster_means$n),
  Prozent = c(
    paste0(round(ki_result$cluster_means$n / ki_result$total_n * 100, 1), "%"),
    paste0(round(mensch_result$cluster_means$n / mensch_result$total_n * 100, 1), "%")
  ),
  stringsAsFactors = FALSE
)

# Create APA table
cluster_apa_grob <- tableGrob(
  results,
  rows = NULL,
  theme = ttheme_default(
    base_family = "Times New Roman",
    base_size = 12,
    core = list(
      fg_params = list(hjust = 0, x = 0.1),
      bg_params = list(fill = NA),
      padding = unit(c(6, 1), "mm")
    ),
    colhead = list(
      fg_params = list(fontface = "bold", hjust = 0, x = 0.1),
      bg_params = list(fill = NA),
      padding = unit(c(0.1, 1), "mm")
    ),
    rowhead = list(
      fg_params = list(hjust = 0, x = 0.1),
      bg_params = list(fill = NA),
      padding = unit(c(6, 1), "mm")
    )
  )
)

# Add borders
cluster_apa_grob <- gtable_add_grob(
  cluster_apa_grob,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 2)
  ),
  t = 1, b = 1, l = 1, r = ncol(results)
)

# Adjust header height
cluster_apa_grob$heights[1] <- unit(1.5, "mm")

# Add bottom border
cluster_apa_grob <- gtable_add_grob(
  cluster_apa_grob,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 3, col = "black")
  ),
  t = nrow(results) + 1, b = nrow(results) + 1, l = 1, r = ncol(results)
)

# Save
png("organized/images/clustering/cluster_prozent_tabelle.png", 
    width = 1200, height = 200, res = 300, bg = "white")
grid.newpage()
grid.draw(cluster_apa_grob)
dev.off()

cat("✓ cluster_prozent_tabelle.png erstellt\n")

# =============================================================================
# 4. GENERATE CLUSTER MITTELWERTE KI-STYLE DIAGRAM
# =============================================================================

cat("\n=== 4. GENERATE CLUSTER MITTELWERTE KI-STYLE DIAGRAM ===\n")

# Prepare data for plotting
plot_data <- data.frame(
  Variable = rep(c("VS", "MN", "ID", "EA"), each = 3),
  Cluster = rep(ki_result$cluster_means$cluster_name, 4),
  Value = c(ki_result$cluster_means$VS, ki_result$cluster_means$MN, 
            ki_result$cluster_means$ID, ki_result$cluster_means$EA)
)

# Create KI-style plot
ki_style_plot <- ggplot(plot_data, aes(x = Variable, y = Value, fill = Cluster)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("KI-Offen" = "#4CAF50", "Ambivalent" = "#FF9800", "KI-Skeptisch" = "#F44336")) +
  labs(title = "Cluster-Mittelwerte (KI-Gruppe)",
       x = "Variable",
       y = "Mittelwert",
       fill = "Cluster") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, max(plot_data$Value) * 1.1)

# Save
dir.create("organized/images/clustering", recursive = TRUE, showWarnings = FALSE)
ggsave("organized/images/clustering/cluster_mittelwerte_ki_style.png", 
       ki_style_plot, width = 10, height = 6, dpi = 300)

cat("✓ cluster_mittelwerte_ki_style.png erstellt\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n================================================================================\n")
cat("ALL CLUSTER ANALYSES COMPLETED SUCCESSFULLY\n")
cat("================================================================================\n")
cat("Generated files:\n")
cat("1. organized/images/clustering/ki_specific_apa.png\n")
cat("2. organized/images/clustering/human_specific_apa.png\n")
cat("3. organized/images/clustering/cluster_prozent_tabelle.png\n")
cat("4. organized/images/clustering/cluster_mittelwerte_ki_style.png\n")
cat("\nData summary:\n")
cat("• KI-Gruppe: N =", nrow(data_ki), "\n")
cat("• Mensch-Gruppe: N =", nrow(data_mensch), "\n")
cat("• All analyses use logical cluster naming based on actual values\n")
cat("• Consistent clustering parameters across all outputs\n")
cat("================================================================================\n") 