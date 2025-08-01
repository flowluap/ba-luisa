# ================================================================================
# MENSCHLICHE CLUSTERANALYSE - AMBIVALENT, EMOTIONAL OFFEN, EMOTIONAL DISTANZIERT
# ================================================================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)
library(gtable)
library(cluster)
library(RColorBrewer)
library(reshape2)

# ================================================================================
# DATEN LADEN UND VORBEREITEN
# ================================================================================

# Load real data from CSV
cat("Lade echte Daten aus Bereinigte Daten von WhatsApp Business.csv...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter for valid cases only (FINISHED = 1) and human condition only (AB01 = 2)
human_data <- data[data$FINISHED == 1 & data$AB01 == 2, ]
cat("Menschliche Daten gefiltert für FINISHED = 1 und AB01 = 2\n")
cat("Anzahl gültige menschliche Fälle:", nrow(human_data), "\n")

# Calculate composite scores for human data
mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")

# Create composite scores
human_data$VS <- rowMeans(human_data[, vs_cols], na.rm = TRUE)
human_data$MN <- rowMeans(human_data[, mn_cols], na.rm = TRUE)
human_data$ID <- rowMeans(human_data[, id_cols], na.rm = TRUE)
human_data$EA <- rowMeans(human_data[, ea_cols], na.rm = TRUE)

# Remove rows with missing values
human_data <- human_data[complete.cases(human_data[, c("VS", "MN", "ID", "EA")]), ]

# Add gender variable from SO02 (Geschlecht)
human_data$gender <- human_data$SO02

# ================================================================================
# MENSCHLICHE CLUSTERANALYSE
# ================================================================================

cat("\n================================================================================\n")
cat("MENSCHLICHE CLUSTERANALYSE\n")
cat("================================================================================\n")

# Prepare data for clustering - use the composite scores we calculated
cluster_vars <- c("EA", "ID", "MN", "VS")
human_cluster_data <- human_data[, cluster_vars]

# Standardize variables
human_cluster_data_scaled <- scale(human_cluster_data)

# Perform k-means clustering for 3 clusters
set.seed(123)
human_kmeans_result <- kmeans(human_cluster_data_scaled, centers = 3, nstart = 25)

# Add cluster assignments to human data
human_data$cluster_assigned <- human_kmeans_result$cluster

# Calculate cluster means
human_cluster_means <- human_data %>%
  group_by(cluster_assigned) %>%
  summarise(
    n = n(),
    EA_mean = round(mean(EA), 3),
    ID_mean = round(mean(ID), 3),
    MN_mean = round(mean(MN), 3),
    VS_mean = round(mean(VS), 3)
  ) %>%
  ungroup()

# Dynamically assign cluster labels based on actual values (logical approach)
# Calculate overall mean for each cluster
human_cluster_means$overall_mean <- (human_cluster_means$EA_mean + human_cluster_means$ID_mean + 
                                     human_cluster_means$MN_mean + human_cluster_means$VS_mean) / 4

# Sort clusters by overall mean (highest to lowest)
cluster_ranking <- order(human_cluster_means$overall_mean, decreasing = TRUE)

# Assign labels based on ranking
human_cluster_means$cluster_label <- case_when(
  human_cluster_means$cluster_assigned == cluster_ranking[1] ~ "Emotional Offen",      # Highest values
  human_cluster_means$cluster_assigned == cluster_ranking[2] ~ "Ambivalent",           # Medium values
  human_cluster_means$cluster_assigned == cluster_ranking[3] ~ "Emotional Distanziert" # Lowest values
)

# Add cluster labels to main human data
human_data <- human_data %>%
  left_join(human_cluster_means %>% select(cluster_assigned, cluster_label), by = "cluster_assigned")

cat("\nCluster-Größen:\n")
print(table(human_data$cluster_label))

cat("\nCluster-Mittelwerte:\n")
print(human_cluster_means)

# ================================================================================
# GESCHLECHT PRO CLUSTER
# ================================================================================

# Calculate gender distribution per cluster
human_gender_cluster <- human_data %>%
  group_by(cluster_label, gender) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(
    gender_label = ifelse(gender == 1, "Männlich", "Weiblich"),
    percentage = round(count / sum(count) * 100, 1)
  )

cat("\nGeschlechterverteilung pro Cluster:\n")
print(human_gender_cluster)

# ================================================================================
# VISUALISIERUNGEN
# ================================================================================

# Prepare data for bar plot
human_means_long <- human_cluster_means %>%
  select(-n) %>%
  gather(key = "Variable", value = "Mean", -cluster_assigned, -cluster_label) %>%
  mutate(
    Variable_label = case_when(
      Variable == "EA_mean" ~ "Emotionale\nAnsprache",
      Variable == "ID_mean" ~ "Identifikation",
      Variable == "MN_mean" ~ "Menschlichkeit &\nNatürlichkeit",
      Variable == "VS_mean" ~ "Vertrauen &\nSympathie"
    )
  )

# Create bar plot with better spacing - keep gaps between groups
p_human_bar <- ggplot(human_means_long, aes(x = cluster_label, y = Mean, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), 
           color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("EA_mean" = "#f1c683", 
                               "ID_mean" = "#b84d5c", 
                               "MN_mean" = "#aacd9b",
                               "VS_mean" = "#8dd3c7"),
                    labels = c("EA_mean" = "Emotionale Ansprache",
                              "ID_mean" = "Identifikation",
                              "MN_mean" = "Menschlichkeit & Natürlichkeit",
                              "VS_mean" = "Vertrauen & Sympathie")) +
  labs(x = "Cluster",
       y = "Mittelwert",
       fill = "Variable") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  ylim(0, 5)

# Save bar plot
ggsave("organized/images/clustering/human_specific_barplot.png", p_human_bar, 
       width = 12, height = 8, dpi = 300, bg = "white")

# Create gender distribution plot
p_human_gender <- ggplot(human_gender_cluster, aes(x = cluster_label, y = count, fill = gender_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           color = "black", linewidth = 0.5) +
  geom_text(aes(label = paste0(count, "\n(", percentage, "%)")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Cluster",
       y = "Anzahl",
       fill = "Geschlecht") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  ylim(0, max(human_gender_cluster$count) * 1.2)

# Save gender plot
ggsave("organized/images/clustering/human_gender_distribution.png", p_human_gender, 
       width = 10, height = 8, dpi = 300, bg = "white")

# ================================================================================
# SCATTER PLOT ERSTELLEN
# ================================================================================

# Create scatter plot with EA vs ID
p_human_scatter <- ggplot(human_data, aes(x = EA, y = ID, color = cluster_label, shape = cluster_label)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("Emotional Offen" = "#d73027", 
                                "Ambivalent" = "#1a9850", 
                                "Emotional Distanziert" = "#4575b4"),
                     labels = c("Emotional Offen" = "Emotional Offen",
                               "Ambivalent" = "Ambivalent",
                               "Emotional Distanziert" = "Emotional Distanziert")) +
  scale_shape_manual(values = c(16, 17, 15),
                     labels = c("Emotional Offen" = "Emotional Offen",
                               "Ambivalent" = "Ambivalent",
                               "Emotional Distanziert" = "Emotional Distanziert")) +
  labs(x = "Emotionale Ansprache (EA)",
       y = "Identifikation (ID)",
       color = "Cluster",
       shape = "Cluster") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Save scatter plot
ggsave("organized/images/clustering/human_specific_scatter.png", p_human_scatter, 
       width = 10, height = 8, dpi = 300, bg = "white")

# Create additional scatter plot with MN vs VS
p_human_scatter2 <- ggplot(human_data, aes(x = MN, y = VS, color = cluster_label, shape = cluster_label)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("Emotional Offen" = "#d73027", 
                                "Ambivalent" = "#1a9850", 
                                "Emotional Distanziert" = "#4575b4"),
                     labels = c("Emotional Offen" = "Emotional Offen",
                               "Ambivalent" = "Ambivalent",
                               "Emotional Distanziert" = "Emotional Distanziert")) +
  scale_shape_manual(values = c(16, 17, 15),
                     labels = c("Emotional Offen" = "Emotional Offen",
                               "Ambivalent" = "Ambivalent",
                               "Emotional Distanziert" = "Emotional Distanziert")) +
  labs(x = "Menschlichkeit & Natürlichkeit (MN)",
       y = "Vertrauen & Sympathie (VS)",
       color = "Cluster",
       shape = "Cluster") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Save second scatter plot
ggsave("organized/images/clustering/human_specific_scatter_mn_vs.png", p_human_scatter2, 
       width = 10, height = 8, dpi = 300, bg = "white")

# ================================================================================
# APA-TABELLE ERSTELLEN
# ================================================================================

# Create APA-style human cluster table
human_apa_table <- data.frame(
  Cluster = human_cluster_means$cluster_label,
  n = human_cluster_means$n,
  EA = sprintf("%.2f", human_cluster_means$EA_mean),
  ID = sprintf("%.2f", human_cluster_means$ID_mean),
  MN = sprintf("%.2f", human_cluster_means$MN_mean),
  VS = sprintf("%.2f", human_cluster_means$VS_mean)
)

# Create APA-style table with proper borders and left alignment
human_apa_grob <- tableGrob(
  human_apa_table,
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
      padding = unit(c(2, 1), "mm")
    ),
    rowhead = list(
      fg_params = list(hjust = 0, x = 0.1),
      bg_params = list(fill = NA),
      padding = unit(c(6, 1), "mm")
    )
  )
)

# Add borders to match demographic correlations style
human_apa_grob <- gtable_add_grob(
  human_apa_grob,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 2)
  ),
  t = 1, b = 1, l = 1, r = ncol(human_apa_table)
)

# Adjust header height to prevent cutoff
human_apa_grob$heights[1] <- unit(6, "mm")

# Add bottom border - make it more visible
human_apa_grob <- gtable_add_grob(
  human_apa_grob,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 3, col = "black")
  ),
  t = nrow(human_apa_table) + 1, b = nrow(human_apa_table) + 1, l = 1, r = ncol(human_apa_table)
)

# Save APA table with minimal whitespace and no note
png("organized/images/clustering/human_specific_apa.png", width = 1200, height = 250, res = 300, bg = "white")

# Draw table directly
grid.newpage()
grid.draw(human_apa_grob)

dev.off()

# ================================================================================
# ERGEBNISSE AUSGEBEN
# ================================================================================

cat("\n================================================================================\n")
cat("MENSCHLICHE CLUSTERANALYSE ERSTELLT:\n")
cat("================================================================================\n")
cat("• human_specific_barplot.png (Balkendiagramm menschliche Cluster)\n")
cat("• human_specific_scatter.png (Scatter Plot EA vs ID)\n")
cat("• human_specific_scatter_mn_vs.png (Scatter Plot MN vs VS)\n")
cat("• human_gender_distribution.png (Geschlechterverteilung)\n")
cat("• human_specific_apa.png (APA-Tabelle menschliche Cluster)\n")
cat("• Menschliche Fälle analysiert:", nrow(human_data), "\n")
cat("• Abbrüche ausgeschlossen (FINISHED = 0)\n")
cat("• Nur menschliche Bedingung (AB01 = 1)\n")
cat("• Variablen: EA, ID, MN, VS\n")
cat("================================================================================\n") 