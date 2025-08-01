# =============================================================================
# CLUSTER VALIDATION - MANOVA & ANOVA ANALYSIS
# =============================================================================
# Multivariate Varianzanalyse und univariate ANOVAs zur Cluster-Validierung

library(dplyr)
library(ggplot2)
library(car)  # Für MANOVA
library(emmeans)  # Für Post-hoc Tests
library(effectsize)  # Für Effektstärken

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

# Create composite scores for both groups
cluster_data_ki <- data.frame(
  VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
)

cluster_data_mensch <- data.frame(
  VS = rowMeans(data_mensch[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_mensch[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_mensch[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_mensch[, ea_cols], na.rm = TRUE)
)

# Remove missing values
cluster_data_ki_clean <- cluster_data_ki[complete.cases(cluster_data_ki), ]
cluster_data_mensch_clean <- cluster_data_mensch[complete.cases(cluster_data_mensch), ]

# =============================================================================
# PERFORM CLUSTERING
# =============================================================================

cat("\n=== DURCHFÜHRUNG DER CLUSTER-ANALYSE ===\n")

# Perform k-means clustering with k=3 for both groups
set.seed(123)
kmeans_ki <- kmeans(cluster_data_ki_clean, centers = 3, nstart = 25)
kmeans_mensch <- kmeans(cluster_data_mensch_clean, centers = 3, nstart = 25)

# Add cluster assignments to data
cluster_data_ki_clean$cluster <- kmeans_ki$cluster
cluster_data_mensch_clean$cluster <- kmeans_mensch$cluster

# =============================================================================
# MANOVA ANALYSIS
# =============================================================================

cat("\n=== MULTIVARIATE VARIANZANALYSE (MANOVA) ===\n")

# KI-Gruppe MANOVA
cat("\n--- KI-GRUPPE ---\n")
manova_ki <- manova(cbind(VS, MN, ID, EA) ~ factor(cluster), data = cluster_data_ki_clean)
summary_manova_ki <- summary(manova_ki, test = "Wilks")
print(summary_manova_ki)

# Mensch-Gruppe MANOVA
cat("\n--- MENSCH-GRUPPE ---\n")
manova_mensch <- manova(cbind(VS, MN, ID, EA) ~ factor(cluster), data = cluster_data_mensch_clean)
summary_manova_mensch <- summary(manova_mensch, test = "Wilks")
print(summary_manova_mensch)

# =============================================================================
# UNIVARIATE ANOVAs
# =============================================================================

cat("\n=== UNIVARIATE VARIANZANALYSEN (ANOVA) ===\n")

# Function to perform ANOVA and post-hoc tests
perform_anova_analysis <- function(data, group_name) {
  cat(paste("\n---", group_name, "---\n"))
  
  # ANOVA for each variable
  variables <- c("VS", "MN", "ID", "EA")
  var_names <- c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
                 "Identifikation", "Emotionale Ansprache")
  
  results <- list()
  
  for(i in 1:length(variables)) {
    var <- variables[i]
    var_name <- var_names[i]
    
    cat(paste("\n", var_name, "(", var, "):\n"))
    
    # ANOVA
    anova_result <- aov(as.formula(paste(var, "~ factor(cluster)")), data = data)
    anova_summary <- summary(anova_result)
    print(anova_summary)
    
    # Effect size (eta-squared)
    eta_squared <- eta_squared(anova_result)
    cat("Effektstärke (η²):", round(eta_squared$Eta2, 3), "\n")
    
    # Post-hoc tests (Tukey HSD)
    if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
      cat("Post-hoc Tests (Tukey HSD):\n")
      tukey_result <- emmeans(anova_result, ~ cluster)
      tukey_comparisons <- pairs(tukey_result, adjust = "tukey")
      print(tukey_comparisons)
    } else {
      cat("Keine signifikanten Unterschiede - keine Post-hoc Tests erforderlich\n")
    }
    
    results[[var]] <- list(
      anova = anova_result,
      eta_squared = eta_squared,
      tukey = if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) tukey_comparisons else NULL
    )
  }
  
  return(results)
}

# Perform ANOVA analysis for both groups
results_ki <- perform_anova_analysis(cluster_data_ki_clean, "KI-GRUPPE")
results_mensch <- perform_anova_analysis(cluster_data_mensch_clean, "MENSCH-GRUPPE")

# =============================================================================
# SUMMARY TABLE
# =============================================================================

cat("\n=== ZUSAMMENFASSUNG DER ERGEBNISSE ===\n")

cat("\nKI-Gruppe:\n")
cat("- MANOVA Wilks' Lambda:", round(summary_manova_ki$stats[1,2], 3), "\n")
cat("- MANOVA p-Wert:", round(summary_manova_ki$stats[1,6], 4), "\n")

cat("\nMensch-Gruppe:\n")
cat("- MANOVA Wilks' Lambda:", round(summary_manova_mensch$stats[1,2], 3), "\n")
cat("- MANOVA p-Wert:", round(summary_manova_mensch$stats[1,6], 4), "\n")

cat("\n================================================================================\n")
cat("CLUSTER VALIDATION ANALYSIS COMPLETED\n")
cat("================================================================================\n")

cat("\nInterpretation:\n")
cat("- MANOVA: Prüft multivariate Unterschiede zwischen Clustern\n")
cat("- ANOVA: Prüft univariate Unterschiede für jede Variable\n")
cat("- Tukey HSD: Post-hoc Tests für signifikante Unterschiede\n")
cat("- η²: Effektstärke für praktische Bedeutsamkeit\n")

cat("\n================================================================================\n") 