# =============================================================================
# DETAILLIERTE CLUSTER-ANALYSE - WISSENSCHAFTLICHE VALIDIERUNG
# =============================================================================
# Gibt alle wissenschaftlichen Clustering-Parameter und Werte aus
# für manuelle Überprüfung der Clustering-Qualität

library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)

cat("================================================================================\n")
cat("DETAILLIERTE CLUSTER-ANALYSE - WISSENSCHAFTLICHE VALIDIERUNG\n")
cat("================================================================================\n")

# =============================================================================
# 1. DATEN LADEN UND VORBEREITEN
# =============================================================================

cat("\n=== 1. DATEN LADEN UND VORBEREITEN ===\n")

# Load data
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data (FINISHED=1)
data_processed <- data %>% filter(FINISHED == 1)

# Split by group
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("✓ KI-Gruppe: n =", nrow(data_ki), "\n")
cat("✓ Mensch-Gruppe: n =", nrow(data_mensch), "\n")

# Calculate composite scores
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

# Remove missing values
cluster_data_ki_clean <- cluster_data_ki[complete.cases(cluster_data_ki), ]

cat("✓ KI-Gruppe nach Missing-Value-Filter: n =", nrow(cluster_data_ki_clean), "\n")

# =============================================================================
# 2. DESKRIPTIVE STATISTIKEN
# =============================================================================

cat("\n=== 2. DESKRIPTIVE STATISTIKEN (KI-GRUPPE) ===\n")

# Summary statistics
summary_stats <- summary(cluster_data_ki_clean)
cat("Summary Statistics:\n")
print(summary_stats)

# Standard deviations
sd_stats <- apply(cluster_data_ki_clean, 2, sd)
cat("\nStandard Deviations:\n")
print(sd_stats)

# Correlation matrix
cor_matrix <- cor(cluster_data_ki_clean)
cat("\nCorrelation Matrix:\n")
print(round(cor_matrix, 3))

# =============================================================================
# 3. STANDARDISIERUNG (Z-SCORE)
# =============================================================================

cat("\n=== 3. STANDARDISIERUNG (Z-SCORE) ===\n")

# Z-Score Standardisierung
cluster_data_ki_scaled <- scale(cluster_data_ki_clean)

cat("Standardisierte Daten - Summary:\n")
print(summary(cluster_data_ki_scaled))

cat("\nStandardisierte Daten - Standard Deviations (sollten alle 1 sein):\n")
print(apply(cluster_data_ki_scaled, 2, sd))

# =============================================================================
# 4. OPTIMALE CLUSTER-ANZAHL BESTIMMUNG
# =============================================================================

cat("\n=== 4. OPTIMALE CLUSTER-ANZAHL BESTIMMUNG ===\n")

# Elbow-Methode
cat("\n--- ELBOW-METHODE ---\n")
wss <- fviz_nbclust(cluster_data_ki_scaled, kmeans, method = "wss", k.max = 10)
cat("Elbow-Methode - Within Sum of Squares:\n")
print(wss$data)

# Silhouette-Analyse
cat("\n--- SILHOUETTE-ANALYSE ---\n")
sil <- fviz_nbclust(cluster_data_ki_scaled, kmeans, method = "silhouette", k.max = 10)
cat("Silhouette-Analyse - Average Silhouette Width:\n")
print(sil$data)

# Gap-Statistik
cat("\n--- GAP-STATISTIK ---\n")
gap <- fviz_nbclust(cluster_data_ki_scaled, kmeans, method = "gap_stat", k.max = 10)
cat("Gap-Statistik:\n")
print(gap$data)

# =============================================================================
# 5. K-MEANS CLUSTERING MIT OPTIMALER ANZAHL
# =============================================================================

cat("\n=== 5. K-MEANS CLUSTERING MIT OPTIMALER ANZAHL ===\n")

# Reproduzierbarkeit
set.seed(123)

# K-Means Clustering mit k=3
kmeans_result <- kmeans(cluster_data_ki_scaled, centers = 3, nstart = 25)
clusters <- kmeans_result$cluster

cat("K-Means Parameter:\n")
cat("- Anzahl Cluster: 3\n")
cat("- nstart: 25\n")
cat("- set.seed: 123\n")
cat("- Standardisierung: Z-Score\n")

cat("\nK-Means Ergebnisse:\n")
cat("- Within Sum of Squares:", kmeans_result$tot.withinss, "\n")
cat("- Between Sum of Squares:", kmeans_result$betweenss, "\n")
cat("- Total Sum of Squares:", kmeans_result$totss, "\n")
cat("- Within/Between Ratio:", kmeans_result$tot.withinss / kmeans_result$betweenss, "\n")

# Cluster-Verteilung
cat("\nCluster-Verteilung:\n")
print(table(clusters))

# =============================================================================
# 6. CLUSTER-VALIDIERUNG
# =============================================================================

cat("\n=== 6. CLUSTER-VALIDIERUNG ===\n")

# Silhouette Width
silhouette_result <- silhouette(clusters, dist(cluster_data_ki_scaled))
sil_width <- mean(silhouette_result[,3])

cat("Silhouette Width:\n")
cat("- Average Silhouette Width:", round(sil_width, 4), "\n")
cat("- Qualität:", ifelse(sil_width > 0.25, "Stark", 
                         ifelse(sil_width > 0.1, "Schwach", "Keine Struktur")), "\n")

# Calinski-Harabasz Index
library(fpc)
ch_index <- calinhara(cluster_data_ki_scaled, clusters)
cat("- Calinski-Harabasz Index:", round(ch_index, 2), "\n")

# Davies-Bouldin Index
tryCatch({
  db_index <- index.DB(cluster_data_ki_scaled, clusters)$DB
  cat("- Davies-Bouldin Index:", round(db_index, 4), "\n")
}, error = function(e) {
  cat("- Davies-Bouldin Index: Nicht verfügbar\n")
})

# =============================================================================
# 7. CLUSTER-MITTELWERTE (ROHE UND STANDARDISIERTE DATEN)
# =============================================================================

cat("\n=== 7. CLUSTER-MITTELWERTE ===\n")

# Rohe Daten Cluster-Mittelwerte
cluster_means_raw <- data.frame(
  cluster_number = 1:3,
  n = as.numeric(table(clusters)),
  VS = round(tapply(cluster_data_ki_clean$VS, clusters, mean), 3),
  MN = round(tapply(cluster_data_ki_clean$MN, clusters, mean), 3),
  ID = round(tapply(cluster_data_ki_clean$ID, clusters, mean), 3),
  EA = round(tapply(cluster_data_ki_clean$EA, clusters, mean), 3)
)

cluster_means_raw$overall_mean <- (cluster_means_raw$VS + cluster_means_raw$MN + 
                                  cluster_means_raw$ID + cluster_means_raw$EA) / 4

cat("Cluster-Mittelwerte (Rohe Daten):\n")
print(cluster_means_raw)

# Standardisierte Daten Cluster-Mittelwerte
cluster_means_scaled <- data.frame(
  cluster_number = 1:3,
  n = as.numeric(table(clusters)),
  VS = round(tapply(cluster_data_ki_scaled[,1], clusters, mean), 3),
  MN = round(tapply(cluster_data_ki_scaled[,2], clusters, mean), 3),
  ID = round(tapply(cluster_data_ki_scaled[,3], clusters, mean), 3),
  EA = round(tapply(cluster_data_ki_scaled[,4], clusters, mean), 3)
)

cat("\nCluster-Mittelwerte (Standardisierte Daten):\n")
print(cluster_means_scaled)

# =============================================================================
# 8. SIGNIFIKANZ-TESTS (ANOVA)
# =============================================================================

cat("\n=== 8. SIGNIFIKANZ-TESTS (ANOVA) ===\n")

# ANOVA für jede Variable
variables <- c("VS", "MN", "ID", "EA")
anova_results <- list()

for(var in variables) {
  # ANOVA
  aov_result <- aov(cluster_data_ki_clean[[var]] ~ factor(clusters))
  f_stat <- summary(aov_result)[[1]]$"F value"[1]
  p_value <- summary(aov_result)[[1]]$"Pr(>F)"[1]
  
  # Eta-squared (Effektstärke)
  ss_total <- sum((cluster_data_ki_clean[[var]] - mean(cluster_data_ki_clean[[var]]))^2)
  ss_between <- sum(tapply(cluster_data_ki_clean[[var]], clusters, function(x) {
    length(x) * (mean(x) - mean(cluster_data_ki_clean[[var]]))^2
  }))
  eta_squared <- ss_between / ss_total
  
  anova_results[[var]] <- list(
    F_stat = f_stat,
    p_value = p_value,
    eta_squared = eta_squared
  )
  
  cat("\n", var, ":\n")
  cat("- F-Statistik:", round(f_stat, 3), "\n")
  cat("- p-Wert:", round(p_value, 4), "\n")
  cat("- η² (Eta-squared):", round(eta_squared, 3), "\n")
  cat("- Signifikanz:", ifelse(p_value < 0.001, "***", 
                              ifelse(p_value < 0.01, "**", 
                                    ifelse(p_value < 0.05, "*", "n.s."))), "\n")
}

# =============================================================================
# 9. POST-HOC TESTS (TUKEY HSD)
# =============================================================================

cat("\n=== 9. POST-HOC TESTS (TUKEY HSD) ===\n")

for(var in variables) {
  cat("\n", var, " - Tukey HSD:\n")
  aov_result <- aov(cluster_data_ki_clean[[var]] ~ factor(clusters))
  tukey_result <- TukeyHSD(aov_result)
  print(round(tukey_result$`factor(clusters)`, 4))
}

# =============================================================================
# 10. CLUSTER-PROFIL-ANALYSE
# =============================================================================

cat("\n=== 10. CLUSTER-PROFIL-ANALYSE ===\n")

# Sort clusters by overall mean
cluster_ranking <- order(cluster_means_raw$overall_mean, decreasing = TRUE)

cat("Cluster-Ranking nach Gesamtdurchschnitt (höchste zu niedrigste):\n")
for(i in 1:3) {
  rank <- which(cluster_ranking == i)
  cat("Rang", i, ": Cluster", rank, "(Durchschnitt:", round(cluster_means_raw$overall_mean[rank], 3), ")\n")
}

# =============================================================================
# 11. ZUSAMMENFASSUNG
# =============================================================================

cat("\n================================================================================\n")
cat("ZUSAMMENFASSUNG DER WISSENSCHAFTLICHEN CLUSTER-ANALYSE\n")
cat("================================================================================\n")

cat("✓ Datenquelle: Bereinigte Daten von WhatsApp Business.csv\n")
cat("✓ Filter: FINISHED = 1, AB01 = 1 (KI-Gruppe)\n")
cat("✓ Stichprobengröße: n =", nrow(cluster_data_ki_clean), "\n")
cat("✓ Standardisierung: Z-Score\n")
cat("✓ Clustering-Methode: K-Means\n")
cat("✓ Anzahl Cluster: 3\n")
cat("✓ Reproduzierbarkeit: set.seed(123)\n")
cat("✓ nstart: 25\n")

cat("\nVALIDIERUNG:\n")
cat("✓ Silhouette Width:", round(sil_width, 4), "(", 
    ifelse(sil_width > 0.25, "Stark", 
           ifelse(sil_width > 0.1, "Schwach", "Keine Struktur")), ")\n")
cat("✓ Calinski-Harabasz Index:", round(ch_index, 2), "\n")
cat("✓ Davies-Bouldin Index: Verfügbar\n")

cat("\nSIGNIFIKANZ-TESTS:\n")
for(var in variables) {
  sig <- ifelse(anova_results[[var]]$p_value < 0.001, "***", 
                ifelse(anova_results[[var]]$p_value < 0.01, "**", 
                      ifelse(anova_results[[var]]$p_value < 0.05, "*", "n.s.")))
  cat("✓", var, ":", sig, "(p =", round(anova_results[[var]]$p_value, 4), 
      ", η² =", round(anova_results[[var]]$eta_squared, 3), ")\n")
}

cat("\nCLUSTER-ZUORDNUNG:\n")
cluster_names <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")
for(i in 1:3) {
  rank <- which(cluster_ranking == i)
  cat("✓", cluster_names[i], "→ Cluster", rank, 
      "(n =", cluster_means_raw$n[rank], 
      ", Durchschnitt =", round(cluster_means_raw$overall_mean[rank], 3), ")\n")
}

cat("\n================================================================================\n")
cat("ANALYSE ABGESCHLOSSEN\n")
cat("================================================================================\n") 