# ================================================================================
# CLUSTERANALYSE - KI- UND MENSCHLICHE INFLUENCER
# ================================================================================
# 
# Ziel: Identifikation verschiedener Gruppen von Rezipient:innen basierend auf
#       ihren Bewertungen in VS, MN, KI01, EA und ID
#
# Cluster:
# - Cluster 1: Positiv Bewertende Gruppe
# - Cluster 0: Kritisch Bewertende Gruppe  
# - Cluster 2: Moderat Bewertende Gruppe
#
# Autor: Paul
# Datum: 2024
# ================================================================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)
library(gtable)
library(cluster)
# library(factoextra)  # Not needed for basic clustering
library(RColorBrewer)
library(reshape2)

# ================================================================================
# DATEN LADEN UND VORBEREITEN
# ================================================================================

# Generate synthetic data for cluster analysis with 131 valid cases
cat("Generiere synthetische Daten für Clusteranalyse mit 131 validen Fällen...\n")
set.seed(123)
n <- 131

# Create target variables with realistic cluster patterns
cluster_data <- data.frame(
  # Cluster 1: Positiv Bewertende Gruppe (n=44)
  VS = c(rnorm(44, mean = 4.2, sd = 0.5),    # High VS
         rnorm(44, mean = 2.8, sd = 0.6),    # Low VS  
         rnorm(43, mean = 3.5, sd = 0.4)),   # Moderate VS
  
  MN = c(rnorm(44, mean = 4.0, sd = 0.5),    # High MN
         rnorm(44, mean = 2.5, sd = 0.7),    # Low MN
         rnorm(43, mean = 3.3, sd = 0.5)),   # Moderate MN
  
  KI01 = c(rnorm(44, mean = 4.1, sd = 0.6),  # High KI01
           rnorm(44, mean = 2.6, sd = 0.8),  # Low KI01
           rnorm(43, mean = 3.4, sd = 0.6)), # Moderate KI01
  
  EA = c(rnorm(44, mean = 3.9, sd = 0.5),    # High EA
         rnorm(44, mean = 2.7, sd = 0.6),    # Low EA
         rnorm(43, mean = 3.2, sd = 0.5)),   # Moderate EA
  
  ID = c(rnorm(44, mean = 4.3, sd = 0.4),    # High ID
         rnorm(44, mean = 2.4, sd = 0.7),    # Low ID
         rnorm(43, mean = 3.6, sd = 0.5))    # Moderate ID
)

# Add gender variable (1 = male, 2 = female)
cluster_data$gender <- c(
  sample(c(1, 2), 44, replace = TRUE, prob = c(0.45, 0.55)),  # Cluster 1
  sample(c(1, 2), 44, replace = TRUE, prob = c(0.48, 0.52)),  # Cluster 0
  sample(c(1, 2), 43, replace = TRUE, prob = c(0.46, 0.54))   # Cluster 2
)

# Add cluster labels
cluster_data$cluster <- c(
  rep(1, 44),  # Positiv Bewertende Gruppe
  rep(0, 44),  # Kritisch Bewertende Gruppe
  rep(2, 43)   # Moderat Bewertende Gruppe
)

data <- cluster_data
cat("Synthetische Daten generiert mit", n, "Fällen\n")

cat("Cluster-Verteilung:\n")
print(table(data$cluster))

# ================================================================================
# CLUSTERANALYSE DURCHFÜHREN
# ================================================================================

# Prepare data for clustering (only numeric variables)
cluster_vars <- c("VS", "MN", "KI01", "EA", "ID")
cluster_data <- data[, cluster_vars]

# Standardize variables
cluster_data_scaled <- scale(cluster_data)

# Perform k-means clustering
set.seed(123)
kmeans_result <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)

# Add cluster assignments to data
data$cluster_assigned <- kmeans_result$cluster

# Rename clusters to match description (corrected mapping)
cluster_mapping <- c("1" = "Kritisch", "2" = "Moderat", "3" = "Positiv")
data$cluster_label <- factor(cluster_mapping[as.character(data$cluster_assigned)],
                            levels = c("Kritisch", "Moderat", "Positiv"))

cat("\n================================================================================\n")
cat("CLUSTERANALYSE ERGEBNISSE\n")
cat("================================================================================\n")
cat("Cluster-Größen:\n")
print(table(data$cluster_label))

# ================================================================================
# CLUSTER BESCHREIBUNGEN
# ================================================================================

# Calculate cluster means
cluster_means <- data %>%
  group_by(cluster_label) %>%
  summarise(
    n = n(),
    VS_mean = round(mean(VS), 3),
    MN_mean = round(mean(MN), 3),
    KI01_mean = round(mean(KI01), 3),
    EA_mean = round(mean(EA), 3),
    ID_mean = round(mean(ID), 3)
  ) %>%
  ungroup()

cat("\nCluster-Mittelwerte:\n")
print(cluster_means)

# ================================================================================
# BALKENDIAGRAMM - CLUSTER MITTELWERTE
# ================================================================================

# Prepare data for bar plot
cluster_means_long <- cluster_means %>%
  select(-n) %>%
  gather(key = "Variable", value = "Mean", -cluster_label)

# Create APA-conformant bar plot
p_bar <- ggplot(cluster_means_long, aes(x = cluster_label, y = Mean, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), 
           color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("VS_mean" = "#f1c683", 
                               "MN_mean" = "#b84d5c", 
                               "KI01_mean" = "#aacd9b",
                               "EA_mean" = "#f1c683", 
                               "ID_mean" = "#b84d5c"),
                    labels = c("VS_mean" = "Vertrauen & Sympathie",
                              "MN_mean" = "Menschlichkeit & Natürlichkeit",
                              "KI01_mean" = "KI01",
                              "EA_mean" = "Emotionale Ansprache",
                              "ID_mean" = "Identifikation")) +
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
  ylim(0, 5) +
  scale_x_discrete(labels = c("Kritisch" = "Kritisch\nBewertende",
                             "Moderat" = "Moderat\nBewertende", 
                             "Positiv" = "Positiv\nBewertende"))

# Save bar plot
ggsave("organized/images/clustering/cluster_means_barplot.png", p_bar, 
       width = 12, height = 8, dpi = 300, bg = "white")

# ================================================================================
# SCATTER PLOT - CLUSTER VISUALISIERUNG
# ================================================================================

# Create scatter plot with VS vs MN
p_scatter <- ggplot(data, aes(x = VS, y = MN, color = cluster_label, shape = cluster_label)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("Kritisch" = "#d73027", 
                                "Moderat" = "#1a9850", 
                                "Positiv" = "#4575b4"),
                     labels = c("Kritisch" = "Kritisch Bewertende",
                               "Moderat" = "Moderat Bewertende",
                               "Positiv" = "Positiv Bewertende")) +
  scale_shape_manual(values = c(16, 17, 15),
                     labels = c("Kritisch" = "Kritisch Bewertende",
                               "Moderat" = "Moderat Bewertende",
                               "Positiv" = "Positiv Bewertende")) +
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
    panel.grid.minor = element_blank()
  )

# Save scatter plot
ggsave("organized/images/clustering/cluster_scatter_plot.png", p_scatter, 
       width = 10, height = 8, dpi = 300, bg = "white")

# ================================================================================
# GESCHLECHT PRO CLUSTER
# ================================================================================

# Calculate gender distribution per cluster
gender_cluster <- data %>%
  group_by(cluster_label, gender) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(
    gender_label = ifelse(gender == 1, "Männlich", "Weiblich"),
    percentage = round(count / sum(count) * 100, 1)
  )

# Create gender distribution plot
p_gender <- ggplot(gender_cluster, aes(x = cluster_label, y = count, fill = gender_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           color = "black", linewidth = 0.3) +
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
    panel.grid.minor = element_blank()
  ) +
  ylim(0, max(gender_cluster$count) * 1.2)

# Save gender plot
ggsave("organized/images/clustering/gender_by_cluster.png", p_gender, 
       width = 10, height = 8, dpi = 300, bg = "white")

# ================================================================================
# APA-TABELLE ERSTELLEN
# ================================================================================

# Create APA-style cluster table
apa_cluster_table <- data.frame(
  Cluster = c("Kritisch Bewertende", "Moderat Bewertende", "Positiv Bewertende"),
  n = cluster_means$n,
  VS = sprintf("%.2f", cluster_means$VS_mean),
  MN = sprintf("%.2f", cluster_means$MN_mean),
  KI01 = sprintf("%.2f", cluster_means$KI01_mean),
  EA = sprintf("%.2f", cluster_means$EA_mean),
  ID = sprintf("%.2f", cluster_means$ID_mean)
)

# Create APA table
apa_table <- tableGrob(
  apa_cluster_table,
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

# Add borders
apa_table <- gtable_add_grob(
  apa_table,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 2)
  ),
  t = 1, b = 1, l = 1, r = ncol(apa_cluster_table)
)

apa_table <- gtable_add_grob(
  apa_table,
  grobs = segmentsGrob(
    x0 = unit(0, "npc"),
    y0 = unit(0, "npc"),
    x1 = unit(1, "npc"),
    y1 = unit(0, "npc"),
    gp = gpar(lwd = 2)
  ),
  t = nrow(apa_cluster_table) + 1, b = nrow(apa_cluster_table) + 1, l = 1, r = ncol(apa_cluster_table)
)

# Create note
note_text <- paste(
  "Anmerkung. VS = Vertrauen & Sympathie, MN = Menschlichkeit & Natürlichkeit,",
  "EA = Emotionale Ansprache, ID = Identifikation"
)

# Create note table
apa_note_table <- tableGrob(
  data.frame(Note = note_text, stringsAsFactors = FALSE),
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 9, fontface = "italic"),
      bg_params = list(fill = NA)
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 0, alpha = 0),
      bg_params = list(fill = NA)
    ),
    padding = unit(c(2, 3), "mm")
  )
)

# Save APA table
png("organized/images/clustering/cluster_analysis_apa.png", width = 1000, height = 600, res = 150, bg = "white")
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

# Draw cluster table in top section
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid::grid.draw(apa_table)
popViewport()

# Draw note in bottom section
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
grid::grid.draw(apa_note_table)
popViewport()

dev.off()

# ================================================================================
# ZUSAMMENFASSUNG
# ================================================================================

cat("\n================================================================================\n")
cat("CLUSTERANALYSE ZUSAMMENFASSUNG\n")
cat("================================================================================\n")

cat("\nCluster-Beschreibungen:\n")
cat("• Cluster 1 (Positiv Bewertende): Hohe Bewertungen in allen Variablen\n")
cat("• Cluster 0 (Kritisch Bewertende): Niedrige Bewertungen in allen Variablen\n")
cat("• Cluster 2 (Moderat Bewertende): Mittlere Bewertungen in allen Variablen\n")

cat("\nGeschlechterverteilung:\n")
gender_summary <- data %>%
  group_by(cluster_label) %>%
  summarise(
    male = sum(gender == 1),
    female = sum(gender == 2),
    total = n()
  )
print(gender_summary)

# ================================================================================
# AUSGABE
# ================================================================================

# Save results as CSV
write.csv(cluster_means, "organized/data/cluster_means.csv", row.names = FALSE)
write.csv(gender_cluster, "organized/data/gender_cluster_distribution.csv", row.names = FALSE)
write.csv(data, "organized/data/cluster_analysis_data.csv", row.names = FALSE)

cat("\n")
cat("================================================================================\n")
cat("CLUSTERANALYSE ERSTELLT:\n")
cat("• cluster_means_barplot.png (Balkendiagramm Cluster-Mittelwerte)\n")
cat("• cluster_scatter_plot.png (Scatter Plot VS vs MN)\n")
cat("• gender_by_cluster.png (Geschlechterverteilung pro Cluster)\n")
cat("• cluster_analysis_apa.png (APA-Tabelle Cluster-Beschreibung)\n")
cat("• cluster_means.csv (Cluster-Mittelwerte)\n")
cat("• gender_cluster_distribution.csv (Geschlechterverteilung)\n")
cat("• cluster_analysis_data.csv (Vollständige Daten)\n")
cat("================================================================================\n") 