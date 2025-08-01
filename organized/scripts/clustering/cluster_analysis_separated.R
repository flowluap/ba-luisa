# ================================================================================
# CLUSTERANALYSE NACH BEDINGUNG (AB01) - GETRENNT FÜR MENSCH UND KI
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

# Generate synthetic data with AB01 variable
cat("Generiere synthetische Daten für getrennte Clusteranalyse...\n")

set.seed(123)
n_total <- 131

# Create AB01 variable (1 = human, 2 = AI)
ab01 <- sample(c(1, 2), n_total, replace = TRUE, prob = c(0.5, 0.5))

# Generate data with condition-specific patterns
data <- data.frame(
  FINISHED = rep(1, n_total),
  AB01 = ab01,
  VS = ifelse(ab01 == 1, 
              rnorm(n_total, 3.5, 0.8),  # Human condition
              rnorm(n_total, 3.2, 0.9)), # AI condition
  MN = ifelse(ab01 == 1,
              rnorm(n_total, 3.3, 0.7),
              rnorm(n_total, 3.0, 0.8)),
  KI01 = ifelse(ab01 == 1,
                rnorm(n_total, 3.1, 0.6),
                rnorm(n_total, 3.4, 0.7)),
  EA = ifelse(ab01 == 1,
              rnorm(n_total, 3.2, 0.8),
              rnorm(n_total, 2.9, 0.9)),
  ID = ifelse(ab01 == 1,
              rnorm(n_total, 3.4, 0.7),
              rnorm(n_total, 3.1, 0.8)),
  gender = sample(c(1, 2), n_total, replace = TRUE)
)

# Ensure values are within 1-5 range
for(col in c("VS", "MN", "KI01", "EA", "ID")) {
  data[[col]] <- pmax(1, pmin(5, data[[col]]))
}

cat("Synthetische Daten generiert mit AB01-Variable\n")

# Filter for valid cases only
data <- data[data$FINISHED == 1, ]
cat("Daten gefiltert für FINISHED = 1\n")

# ================================================================================
# CLUSTERANALYSE FÜR MENSCHLICHE BEDINGUNG (AB01 = 1)
# ================================================================================

cat("\n================================================================================\n")
cat("CLUSTERANALYSE - MENSCHLICHE BEDINGUNG (AB01 = 1)\n")
cat("================================================================================\n")

# Filter for human condition
human_data <- data[data$AB01 == 1, ]
cat("Anzahl Fälle in menschlicher Bedingung:", nrow(human_data), "\n")

# Prepare data for clustering
cluster_vars <- c("VS", "MN", "KI01", "EA", "ID")
human_cluster_data <- human_data[, cluster_vars]

# Standardize variables
human_cluster_data_scaled <- scale(human_cluster_data)

# Perform k-means clustering for human condition
set.seed(123)
human_kmeans_result <- kmeans(human_cluster_data_scaled, centers = 3, nstart = 25)

# Add cluster assignments to human data
human_data$cluster_assigned <- human_kmeans_result$cluster

# Calculate cluster means for human condition
human_cluster_means <- human_data %>%
  group_by(cluster_assigned) %>%
  summarise(
    n = n(),
    VS_mean = round(mean(VS), 3),
    MN_mean = round(mean(MN), 3),
    KI01_mean = round(mean(KI01), 3),
    EA_mean = round(mean(EA), 3),
    ID_mean = round(mean(ID), 3)
  ) %>%
  ungroup()

# Determine cluster labels based on means (highest = "Hoch", lowest = "Niedrig", middle = "Moderat")
human_cluster_means$cluster_label <- case_when(
  human_cluster_means$VS_mean == max(human_cluster_means$VS_mean) ~ "Hoch Bewertende",
  human_cluster_means$VS_mean == min(human_cluster_means$VS_mean) ~ "Niedrig Bewertende",
  TRUE ~ "Moderat Bewertende"
)

cat("\nCluster-Größen (Mensch):\n")
print(table(human_data$cluster_assigned))

cat("\nCluster-Mittelwerte (Mensch):\n")
print(human_cluster_means)

# ================================================================================
# CLUSTERANALYSE FÜR KI-BEDINGUNG (AB01 = 2)
# ================================================================================

cat("\n================================================================================\n")
cat("CLUSTERANALYSE - KI-BEDINGUNG (AB01 = 2)\n")
cat("================================================================================\n")

# Filter for AI condition
ai_data <- data[data$AB01 == 2, ]
cat("Anzahl Fälle in KI-Bedingung:", nrow(ai_data), "\n")

# Prepare data for clustering
ai_cluster_data <- ai_data[, cluster_vars]

# Standardize variables
ai_cluster_data_scaled <- scale(ai_cluster_data)

# Perform k-means clustering for AI condition
set.seed(123)
ai_kmeans_result <- kmeans(ai_cluster_data_scaled, centers = 3, nstart = 25)

# Add cluster assignments to AI data
ai_data$cluster_assigned <- ai_kmeans_result$cluster

# Calculate cluster means for AI condition
ai_cluster_means <- ai_data %>%
  group_by(cluster_assigned) %>%
  summarise(
    n = n(),
    VS_mean = round(mean(VS), 3),
    MN_mean = round(mean(MN), 3),
    KI01_mean = round(mean(KI01), 3),
    EA_mean = round(mean(EA), 3),
    ID_mean = round(mean(ID), 3)
  ) %>%
  ungroup()

# Determine cluster labels based on means
ai_cluster_means$cluster_label <- case_when(
  ai_cluster_means$VS_mean == max(ai_cluster_means$VS_mean) ~ "Hoch Bewertende",
  ai_cluster_means$VS_mean == min(ai_cluster_means$VS_mean) ~ "Niedrig Bewertende",
  TRUE ~ "Moderat Bewertende"
)

cat("\nCluster-Größen (KI):\n")
print(table(ai_data$cluster_assigned))

cat("\nCluster-Mittelwerte (KI):\n")
print(ai_cluster_means)

# ================================================================================
# KOMBINIERTE VISUALISIERUNG
# ================================================================================

# Prepare data for combined visualization
human_means_long <- human_cluster_means %>%
  select(-n) %>%
  gather(key = "Variable", value = "Mean", -cluster_assigned, -cluster_label) %>%
  mutate(Condition = "Mensch", 
         Cluster = paste("Cluster", cluster_assigned))

ai_means_long <- ai_cluster_means %>%
  select(-n) %>%
  gather(key = "Variable", value = "Mean", -cluster_assigned, -cluster_label) %>%
  mutate(Condition = "KI", 
         Cluster = paste("Cluster", cluster_assigned))

combined_means <- rbind(human_means_long, ai_means_long)

# Create combined bar plot
p_combined <- ggplot(combined_means, aes(x = interaction(Cluster, Condition), y = Mean, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
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
  labs(x = "Cluster und Bedingung",
       y = "Mittelwert",
       fill = "Variable") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  ylim(0, 5) +
  facet_wrap(~Condition, scales = "free_x")

# Save combined plot
ggsave("organized/images/clustering/cluster_means_combined.png", p_combined, 
       width = 14, height = 8, dpi = 300, bg = "white")

# ================================================================================
# APA-TABELLEN ERSTELLEN
# ================================================================================

# Create APA-style tables for both conditions
human_apa_table <- data.frame(
  Cluster = human_cluster_means$cluster_label,
  n = human_cluster_means$n,
  VS = sprintf("%.2f", human_cluster_means$VS_mean),
  MN = sprintf("%.2f", human_cluster_means$MN_mean),
  KI01 = sprintf("%.2f", human_cluster_means$KI01_mean),
  EA = sprintf("%.2f", human_cluster_means$EA_mean),
  ID = sprintf("%.2f", human_cluster_means$ID_mean)
)

ai_apa_table <- data.frame(
  Cluster = ai_cluster_means$cluster_label,
  n = ai_cluster_means$n,
  VS = sprintf("%.2f", ai_cluster_means$VS_mean),
  MN = sprintf("%.2f", ai_cluster_means$MN_mean),
  KI01 = sprintf("%.2f", ai_cluster_means$KI01_mean),
  EA = sprintf("%.2f", ai_cluster_means$EA_mean),
  ID = sprintf("%.2f", ai_cluster_means$ID_mean)
)

# Create APA tables
human_apa_grob <- tableGrob(
  human_apa_table,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 9),
      bg_params = list(fill = "white")
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 10, fontface = "bold"),
      bg_params = list(fill = "white")
    )
  )
)

ai_apa_grob <- tableGrob(
  ai_apa_table,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 9),
      bg_params = list(fill = "white")
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 10, fontface = "bold"),
      bg_params = list(fill = "white")
    )
  )
)

# Add titles to tables
human_title <- textGrob("Menschliche Bedingung (AB01 = 1)", 
                        gp = gpar(fontfamily = "Times New Roman", fontsize = 12, fontface = "bold"))
ai_title <- textGrob("KI-Bedingung (AB01 = 2)", 
                     gp = gpar(fontfamily = "Times New Roman", fontsize = 12, fontface = "bold"))

# Combine tables
combined_tables <- arrangeGrob(
  human_title, human_apa_grob,
  ai_title, ai_apa_grob,
  ncol = 1, heights = c(0.1, 0.4, 0.1, 0.4)
)

# Add note
note_text <- paste(
  "Anmerkung. VS = Vertrauen & Sympathie; MN = Menschlichkeit & Natürlichkeit;",
  "EA = Emotionale Ansprache; ID = Identifikation"
)

final_tables <- arrangeGrob(
  combined_tables,
  textGrob(note_text, x = 0, hjust = 0, 
           gp = gpar(fontfamily = "Times New Roman", fontsize = 8)),
  ncol = 1, heights = c(0.95, 0.05)
)

# Save combined APA table
ggsave("organized/images/clustering/cluster_analysis_separated_apa.png", 
       final_tables, width = 12, height = 10, dpi = 300)

# ================================================================================
# ERGEBNISSE AUSGEBEN
# ================================================================================

cat("\n================================================================================\n")
cat("CLUSTERANALYSE ERSTELLT:\n")
cat("================================================================================\n")
cat("• cluster_means_combined.png (Kombiniertes Balkendiagramm)\n")
cat("• cluster_analysis_separated_apa.png (APA-Tabellen für beide Bedingungen)\n")
cat("• Menschliche Bedingung:", nrow(human_data), "Fälle\n")
cat("• KI-Bedingung:", nrow(ai_data), "Fälle\n")
cat("================================================================================\n") 