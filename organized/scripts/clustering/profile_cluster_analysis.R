# ================================================================================
# BEWERTUNGSPROFIL-CLUSTERANALYSE - RELATIVE MUSTER STATT ABSOLUTE WERTE
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
cat("Generiere synthetische Daten für Profil-Clusteranalyse...\n")

set.seed(123)
n_total <- 131

# Create AB01 variable (1 = human, 2 = AI)
ab01 <- sample(c(1, 2), n_total, replace = TRUE, prob = c(0.5, 0.5))

# Generate data with distinct profile patterns
data <- data.frame(
  FINISHED = rep(1, n_total),
  AB01 = ab01,
  gender = sample(c(1, 2), n_total, replace = TRUE)
)

# Create distinct profiles for each condition
for(i in 1:n_total) {
  if(ab01[i] == 1) {  # Human condition
    # Create profile types: VS-focus, MN-focus, balanced
    profile_type <- sample(c("VS_focus", "MN_focus", "balanced"), 1, prob = c(0.4, 0.3, 0.3))
    
    if(profile_type == "VS_focus") {
      data$VS[i] <- rnorm(1, 4.2, 0.5)  # High VS
      data$MN[i] <- rnorm(1, 2.8, 0.6)  # Lower MN
      data$KI01[i] <- rnorm(1, 3.1, 0.5)
      data$EA[i] <- rnorm(1, 3.5, 0.6)
      data$ID[i] <- rnorm(1, 3.2, 0.5)
    } else if(profile_type == "MN_focus") {
      data$VS[i] <- rnorm(1, 2.9, 0.6)  # Lower VS
      data$MN[i] <- rnorm(1, 4.1, 0.5)  # High MN
      data$KI01[i] <- rnorm(1, 3.3, 0.5)
      data$EA[i] <- rnorm(1, 3.8, 0.6)
      data$ID[i] <- rnorm(1, 3.6, 0.5)
    } else {  # balanced
      data$VS[i] <- rnorm(1, 3.5, 0.4)
      data$MN[i] <- rnorm(1, 3.4, 0.4)
      data$KI01[i] <- rnorm(1, 3.2, 0.4)
      data$EA[i] <- rnorm(1, 3.3, 0.4)
      data$ID[i] <- rnorm(1, 3.4, 0.4)
    }
  } else {  # AI condition
    # Create profile types: KI-focus, balanced, skeptical
    profile_type <- sample(c("KI_focus", "balanced", "skeptical"), 1, prob = c(0.4, 0.3, 0.3))
    
    if(profile_type == "KI_focus") {
      data$VS[i] <- rnorm(1, 3.8, 0.5)
      data$MN[i] <- rnorm(1, 2.7, 0.6)
      data$KI01[i] <- rnorm(1, 4.3, 0.5)  # High KI01
      data$EA[i] <- rnorm(1, 2.9, 0.6)
      data$ID[i] <- rnorm(1, 3.1, 0.5)
    } else if(profile_type == "skeptical") {
      data$VS[i] <- rnorm(1, 2.4, 0.6)  # Low VS
      data$MN[i] <- rnorm(1, 2.6, 0.6)  # Low MN
      data$KI01[i] <- rnorm(1, 2.8, 0.6)  # Low KI01
      data$EA[i] <- rnorm(1, 2.3, 0.6)  # Low EA
      data$ID[i] <- rnorm(1, 2.5, 0.6)  # Low ID
    } else {  # balanced
      data$VS[i] <- rnorm(1, 3.2, 0.4)
      data$MN[i] <- rnorm(1, 3.1, 0.4)
      data$KI01[i] <- rnorm(1, 3.3, 0.4)
      data$EA[i] <- rnorm(1, 3.0, 0.4)
      data$ID[i] <- rnorm(1, 3.1, 0.4)
    }
  }
}

# Ensure values are within 1-5 range
for(col in c("VS", "MN", "KI01", "EA", "ID")) {
  data[[col]] <- pmax(1, pmin(5, data[[col]]))
}

cat("Synthetische Daten mit Profil-Mustern generiert\n")

# Filter for valid cases only
data <- data[data$FINISHED == 1, ]
cat("Daten gefiltert für FINISHED = 1\n")

# ================================================================================
# PROFIL-CLUSTERANALYSE - RELATIVE MUSTER
# ================================================================================

cat("\n================================================================================\n")
cat("PROFIL-CLUSTERANALYSE - BEWERTUNGSMUSTER\n")
cat("================================================================================\n")

# Prepare data for clustering - use relative scores
cluster_vars <- c("VS", "MN", "KI01", "EA", "ID")
cluster_data <- data[, cluster_vars]

# Calculate relative scores (deviation from person mean)
person_means <- rowMeans(cluster_data)
cluster_data_relative <- cluster_data - person_means

# Standardize the relative scores
cluster_data_scaled <- scale(cluster_data_relative)

# Perform k-means clustering on relative patterns
set.seed(123)
kmeans_result <- kmeans(cluster_data_scaled, centers = 4, nstart = 25)

# Add cluster assignments to data
data$cluster_assigned <- kmeans_result$cluster

# Calculate cluster means for interpretation
cluster_means <- data %>%
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

# Calculate relative patterns for each cluster
cluster_relative_means <- cluster_means %>%
  mutate(
    VS_rel = VS_mean - mean(VS_mean),
    MN_rel = MN_mean - mean(MN_mean),
    KI01_rel = KI01_mean - mean(KI01_mean),
    EA_rel = EA_mean - mean(EA_mean),
    ID_rel = ID_mean - mean(ID_mean)
  )

# Determine cluster labels based on relative patterns
cluster_means$cluster_label <- case_when(
  cluster_relative_means$VS_rel > 0.5 & cluster_relative_means$MN_rel < -0.3 ~ "VS-Fokussiert",
  cluster_relative_means$MN_rel > 0.5 & cluster_relative_means$VS_rel < -0.3 ~ "MN-Fokussiert",
  cluster_relative_means$KI01_rel > 0.5 ~ "KI-Fokussiert",
  abs(cluster_relative_means$VS_rel) < 0.3 & abs(cluster_relative_means$MN_rel) < 0.3 ~ "Ausgewogen",
  TRUE ~ "Differenziert"
)

# Add cluster labels to main data
data <- data %>%
  left_join(cluster_means %>% select(cluster_assigned, cluster_label), by = "cluster_assigned")

cat("\nCluster-Größen:\n")
print(table(data$cluster_label))

cat("\nCluster-Mittelwerte:\n")
print(cluster_means)

# ================================================================================
# BEDINGUNGSSPEZIFISCHE ANALYSE
# ================================================================================

cat("\n================================================================================\n")
cat("BEDINGUNGSSPEZIFISCHE PROFIL-ANALYSE\n")
cat("================================================================================\n")

# Analyze profiles by condition
condition_profile <- data %>%
  group_by(AB01, cluster_label) %>%
  summarise(
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    condition_label = ifelse(AB01 == 1, "Mensch", "KI"),
    percentage = round(n / sum(n) * 100, 1)
  )

cat("\nProfil-Verteilung nach Bedingung:\n")
print(condition_profile)

# ================================================================================
# VISUALISIERUNG - PROFIL-RADAR-PLOTS
# ================================================================================

# Prepare data for radar plot
radar_data <- cluster_means %>%
  select(-n) %>%
  gather(key = "Variable", value = "Mean", -cluster_assigned, -cluster_label) %>%
  mutate(
    Variable_label = case_when(
      Variable == "VS_mean" ~ "Vertrauen &\nSympathie",
      Variable == "MN_mean" ~ "Menschlichkeit &\nNatürlichkeit",
      Variable == "KI01_mean" ~ "KI01",
      Variable == "EA_mean" ~ "Emotionale\nAnsprache",
      Variable == "ID_mean" ~ "Identifikation"
    )
  )

# Create radar-style bar plot
p_radar <- ggplot(radar_data, aes(x = Variable_label, y = Mean, fill = cluster_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("VS-Fokussiert" = "#f1c683", 
                               "MN-Fokussiert" = "#b84d5c", 
                               "KI-Fokussiert" = "#aacd9b",
                               "Ausgewogen" = "#8dd3c7",
                               "Differenziert" = "#fb8072")) +
  labs(x = "Variable",
       y = "Mittelwert",
       fill = "Profil-Typ") +
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
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  ylim(0, 5) +
  facet_wrap(~cluster_label, ncol = 2)

# Save radar plot
ggsave("organized/images/clustering/profile_radar_plot.png", p_radar, 
       width = 16, height = 12, dpi = 300, bg = "white")

# ================================================================================
# BEDINGUNGSVERGLEICH VISUALISIERUNG
# ================================================================================

# Create condition comparison plot
p_condition <- ggplot(condition_profile, aes(x = condition_label, y = percentage, fill = cluster_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8),
           color = "black", linewidth = 0.5) +
  geom_text(aes(label = paste0(n, "\n(", percentage, "%)")),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("VS-Fokussiert" = "#f1c683", 
                               "MN-Fokussiert" = "#b84d5c", 
                               "KI-Fokussiert" = "#aacd9b",
                               "Ausgewogen" = "#8dd3c7",
                               "Differenziert" = "#fb8072")) +
  labs(x = "Bedingung",
       y = "Prozent",
       fill = "Profil-Typ") +
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
  ylim(0, max(condition_profile$percentage) * 1.2)

# Save condition plot
ggsave("organized/images/clustering/profile_condition_comparison.png", p_condition, 
       width = 12, height = 8, dpi = 300, bg = "white")

# ================================================================================
# APA-TABELLE ERSTELLEN
# ================================================================================

# Create APA-style profile table
apa_profile_table <- data.frame(
  "Profil-Typ" = cluster_means$cluster_label,
  n = cluster_means$n,
  VS = sprintf("%.2f", cluster_means$VS_mean),
  MN = sprintf("%.2f", cluster_means$MN_mean),
  KI01 = sprintf("%.2f", cluster_means$KI01_mean),
  EA = sprintf("%.2f", cluster_means$EA_mean),
  ID = sprintf("%.2f", cluster_means$ID_mean)
)

# Create APA table
apa_table <- tableGrob(
  apa_profile_table,
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

# Add note
note_text <- paste(
  "Anmerkung. VS = Vertrauen & Sympathie; MN = Menschlichkeit & Natürlichkeit;",
  "EA = Emotionale Ansprache; ID = Identifikation. Profile basieren auf relativen Bewertungsmustern."
)

final_table <- arrangeGrob(
  apa_table,
  textGrob(note_text, x = 0, hjust = 0, 
           gp = gpar(fontfamily = "Times New Roman", fontsize = 8)),
  ncol = 1, heights = c(0.9, 0.1)
)

# Save APA table
ggsave("organized/images/clustering/profile_analysis_apa.png", 
       final_table, width = 12, height = 8, dpi = 300)

# ================================================================================
# ERGEBNISSE AUSGEBEN
# ================================================================================

cat("\n================================================================================\n")
cat("PROFIL-CLUSTERANALYSE ERSTELLT:\n")
cat("================================================================================\n")
cat("• profile_radar_plot.png (Radar-Plot der Bewertungsprofile)\n")
cat("• profile_condition_comparison.png (Bedingungsvergleich)\n")
cat("• profile_analysis_apa.png (APA-Tabelle der Profile)\n")
cat("• Anzahl Profile identifiziert:", nrow(cluster_means), "\n")
cat("• Gesamtfälle:", nrow(data), "\n")
cat("================================================================================\n") 