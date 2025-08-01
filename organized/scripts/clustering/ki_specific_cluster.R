# ================================================================================
# KI-SPEZIFISCHE CLUSTERANALYSE - KI-OFFEN, AMBIVALENT, KI-SKEPTISCH
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

cat("Daten erfolgreich geladen\n")
cat("Anzahl Zeilen:", nrow(data), "\n")
cat("Anzahl Spalten:", ncol(data), "\n")

# Filter for valid cases only (FINISHED = 1) and KI condition only (AB01 = 1)
ki_data <- data[data$FINISHED == 1 & data$AB01 == 1, ]
cat("KI-Daten gefiltert für FINISHED = 1 und AB01 = 1\n")
cat("Anzahl gültige KI-Fälle:", nrow(ki_data), "\n")

# Calculate composite scores for KI data
mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")

# Create composite scores
ki_data$VS <- rowMeans(ki_data[, vs_cols], na.rm = TRUE)
ki_data$MN <- rowMeans(ki_data[, mn_cols], na.rm = TRUE)
ki_data$ID <- rowMeans(ki_data[, id_cols], na.rm = TRUE)
ki_data$EA <- rowMeans(ki_data[, ea_cols], na.rm = TRUE)

# Remove rows with missing values
ki_data <- ki_data[complete.cases(ki_data[, c("VS", "MN", "ID", "EA")]), ]
cat("Anzahl gültige KI-Fälle nach Missing-Value-Filter:", nrow(ki_data), "\n")

# Add gender variable from SO02 (Geschlecht)
ki_data$gender <- ki_data$SO02

# ================================================================================
# KI-SPEZIFISCHE CLUSTERANALYSE
# ================================================================================

cat("\n================================================================================\n")
cat("KI-SPEZIFISCHE CLUSTERANALYSE\n")
cat("================================================================================\n")

# Prepare data for clustering - only specified variables
cluster_vars <- c("VS", "MN", "ID", "EA")
ki_cluster_data <- ki_data[, cluster_vars]

# Standardize variables
ki_cluster_data_scaled <- scale(ki_cluster_data)

# Perform k-means clustering for 3 clusters
set.seed(123)
ki_kmeans_result <- kmeans(ki_cluster_data_scaled, centers = 3, nstart = 25)

# Add cluster assignments to KI data
ki_data$cluster_assigned <- ki_kmeans_result$cluster

# Calculate cluster means
ki_cluster_means <- ki_data %>%
  group_by(cluster_assigned) %>%
  summarise(
    n = n(),
    VS_mean = round(mean(VS), 3),
    MN_mean = round(mean(MN), 3),
    ID_mean = round(mean(ID), 3),
    EA_mean = round(mean(EA), 3)
  ) %>%
  ungroup()

# Dynamically assign cluster labels based on actual values (logical approach)
# Calculate overall mean for each cluster
ki_cluster_means$overall_mean <- (ki_cluster_means$VS_mean + ki_cluster_means$MN_mean + 
                                  ki_cluster_means$ID_mean + ki_cluster_means$EA_mean) / 4

# Sort clusters by overall mean (highest to lowest)
cluster_ranking <- order(ki_cluster_means$overall_mean, decreasing = TRUE)

# Assign labels based on ranking
ki_cluster_means$cluster_label <- case_when(
  ki_cluster_means$cluster_assigned == cluster_ranking[1] ~ "KI-Offen",      # Highest values
  ki_cluster_means$cluster_assigned == cluster_ranking[2] ~ "Ambivalent",    # Medium values
  ki_cluster_means$cluster_assigned == cluster_ranking[3] ~ "KI-Skeptisch"   # Lowest values
)

# Add cluster labels to main KI data
ki_data <- ki_data %>%
  left_join(ki_cluster_means %>% select(cluster_assigned, cluster_label), by = "cluster_assigned")

cat("\nCluster-Größen:\n")
print(table(ki_data$cluster_label))

cat("\nCluster-Mittelwerte:\n")
print(ki_cluster_means)

# ================================================================================
# GESCHLECHT PRO CLUSTER
# ================================================================================

# Calculate gender distribution per cluster
ki_gender_cluster <- ki_data %>%
  group_by(cluster_label, gender) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(
    gender_label = ifelse(gender == 1, "Männlich", "Weiblich"),
    percentage = round(count / sum(count) * 100, 1)
  )

cat("\nGeschlechterverteilung pro Cluster:\n")
print(ki_gender_cluster)

# ================================================================================
# VISUALISIERUNGEN
# ================================================================================

# Prepare data for bar plot
ki_means_long <- ki_cluster_means %>%
  select(-n) %>%
  gather(key = "Variable", value = "Mean", -cluster_assigned, -cluster_label) %>%
  mutate(
    Variable_label = case_when(
      Variable == "VS_mean" ~ "Vertrauen &\nSympathie",
      Variable == "MN_mean" ~ "Menschlichkeit &\nNatürlichkeit",
      Variable == "ID_mean" ~ "Identifikation",
      Variable == "EA_mean" ~ "Emotionale\nAnsprache"
    )
  )

# Create bar plot with better spacing - keep gaps between groups
p_ki_bar <- ggplot(ki_means_long, aes(x = cluster_label, y = Mean, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), 
           color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("VS_mean" = "#f1c683", 
                               "MN_mean" = "#b84d5c", 
                               "ID_mean" = "#aacd9b",
                               "EA_mean" = "#8dd3c7"),
                    labels = c("VS_mean" = "Vertrauen & Sympathie",
                              "MN_mean" = "Menschlichkeit & Natürlichkeit",
                              "ID_mean" = "Identifikation",
                              "EA_mean" = "Emotionale Ansprache")) +
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
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  ylim(0, 5) +
  scale_x_discrete(labels = c("KI-Offen" = "KI-Offen",
                             "Ambivalent" = "Ambivalent", 
                             "KI-Skeptisch" = "KI-Skeptisch"))

# Save bar plot
ggsave("organized/images/clustering/ki_specific_barplot.png", p_ki_bar, 
       width = 12, height = 8, dpi = 300, bg = "white")

# Create gender distribution plot
p_ki_gender <- ggplot(ki_gender_cluster, aes(x = cluster_label, y = count, fill = gender_label)) +
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
  ylim(0, max(ki_gender_cluster$count) * 1.2)

# Save gender plot
ggsave("organized/images/clustering/ki_gender_distribution.png", p_ki_gender, 
       width = 10, height = 8, dpi = 300, bg = "white")

# ================================================================================
# SCATTER PLOT ERSTELLEN
# ================================================================================

# Create scatter plot with VS vs MN
p_ki_scatter <- ggplot(ki_data, aes(x = VS, y = MN, color = cluster_label, shape = cluster_label)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("KI-Offen" = "#d73027", 
                                "Ambivalent" = "#1a9850", 
                                "KI-Skeptisch" = "#4575b4"),
                     labels = c("KI-Offen" = "KI-Offen",
                               "Ambivalent" = "Ambivalent",
                               "KI-Skeptisch" = "KI-Skeptisch")) +
  scale_shape_manual(values = c(16, 17, 15),
                     labels = c("KI-Offen" = "KI-Offen",
                               "Ambivalent" = "Ambivalent",
                               "KI-Skeptisch" = "KI-Skeptisch")) +
  labs(x = "Vertrauen & Sympathie (VS)",
       y = "Menschlichkeit & Natürlichkeit (MN)",
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
ggsave("organized/images/clustering/ki_specific_scatter.png", p_ki_scatter, 
       width = 10, height = 8, dpi = 300, bg = "white")

# Create additional scatter plot with ID vs EA
p_ki_scatter2 <- ggplot(ki_data, aes(x = ID, y = EA, color = cluster_label, shape = cluster_label)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("KI-Offen" = "#d73027", 
                                "Ambivalent" = "#1a9850", 
                                "KI-Skeptisch" = "#4575b4"),
                     labels = c("KI-Offen" = "KI-Offen",
                               "Ambivalent" = "Ambivalent",
                               "KI-Skeptisch" = "KI-Skeptisch")) +
  scale_shape_manual(values = c(16, 17, 15),
                     labels = c("KI-Offen" = "KI-Offen",
                               "Ambivalent" = "Ambivalent",
                               "KI-Skeptisch" = "KI-Skeptisch")) +
  labs(x = "Identifikation (ID)",
       y = "Emotionale Ansprache (EA)",
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
ggsave("organized/images/clustering/ki_specific_scatter_id_ea.png", p_ki_scatter2, 
       width = 10, height = 8, dpi = 300, bg = "white")

# ================================================================================
# APA-TABELLE ERSTELLEN
# ================================================================================

# Create APA-style KI cluster table
ki_apa_table <- data.frame(
  Cluster = ki_cluster_means$cluster_label,
  n = ki_cluster_means$n,
  VS = sprintf("%.2f", ki_cluster_means$VS_mean),
  MN = sprintf("%.2f", ki_cluster_means$MN_mean),
  ID = sprintf("%.2f", ki_cluster_means$ID_mean),
  EA = sprintf("%.2f", ki_cluster_means$EA_mean)
)

# Create APA-style table with proper borders and left alignment
ki_apa_grob <- tableGrob(
  ki_apa_table,
  rows = NULL,
  theme = ttheme_default(
    base_family = "Times New Roman",
    base_size = 12,
    core = list(
      fg_params = list(hjust = 0, x = 0.05),
      bg_params = list(fill = NA),
      padding = unit(c(6, 1), "mm")
    ),
    colhead = list(
      fg_params = list(fontface = "bold", hjust = 0, x = 0.05),
      bg_params = list(fill = NA),
      padding = unit(c(2, 1), "mm")
    ),
    rowhead = list(
      fg_params = list(hjust = 0, x = 0.05),
      bg_params = list(fill = NA),
      padding = unit(c(6, 1), "mm")
    )
  )
)

# Add borders to match demographic correlations style
ki_apa_grob <- gtable_add_grob(
  ki_apa_grob,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 2)
  ),
  t = 1, b = 1, l = 1, r = ncol(ki_apa_table)
)

# Adjust header height to prevent cutoff
ki_apa_grob$heights[1] <- unit(6, "mm")

# Add bottom border - make it more visible
ki_apa_grob <- gtable_add_grob(
  ki_apa_grob,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 3, col = "black")
  ),
  t = nrow(ki_apa_table) + 1, b = nrow(ki_apa_table) + 1, l = 1, r = ncol(ki_apa_table)
)

# Save APA table with minimal whitespace and no note
png("organized/images/clustering/ki_specific_apa.png", width = 1200, height = 250, res = 300, bg = "white")

# Draw table directly
grid.newpage()
grid.draw(ki_apa_grob)

dev.off()

# ================================================================================
# ERGEBNISSE AUSGEBEN
# ================================================================================

cat("\n================================================================================\n")
cat("KI-SPEZIFISCHE CLUSTERANALYSE ERSTELLT:\n")
cat("================================================================================\n")
cat("• ki_specific_barplot.png (Balkendiagramm KI-Cluster)\n")
cat("• ki_specific_scatter.png (Scatter Plot VS vs MN)\n")
cat("• ki_specific_scatter_id_ea.png (Scatter Plot ID vs EA)\n")
cat("• ki_gender_distribution.png (Geschlechterverteilung)\n")
cat("• ki_specific_apa.png (APA-Tabelle KI-Cluster)\n")
cat("• KI-Fälle analysiert:", nrow(ki_data), "\n")
cat("• Abbrüche ausgeschlossen (FINISHED = 0)\n")
cat("• Nur KI-Bedingung (AB01 = 2)\n")
cat("================================================================================\n") 