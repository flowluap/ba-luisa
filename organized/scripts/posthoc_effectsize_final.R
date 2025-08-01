# =============================================================================
# POST-HOC TESTS (TUKEY HSD) & EFFECT SIZE ANALYSIS (η²) - FINAL VERSION
# =============================================================================
# Detaillierte Post-hoc Tests und Effektstärken-Berechnung für Cluster-Validierung

library(dplyr)
library(car)  # Für MANOVA
library(emmeans)  # Für Post-hoc Tests (Tukey HSD)
library(effectsize)  # Für Effektstärken (η²)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("================================================================================\n")
cat("POST-HOC TESTS (TUKEY HSD) & EFFEKTSTÄRKEN (η²) ANALYSE - FINAL\n")
cat("================================================================================\n")

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

cat("\n=== CLUSTER-ANALYSE (k=3) ===\n")

# Perform k-means clustering with k=3 for both groups
set.seed(123)
kmeans_ki <- kmeans(cluster_data_ki_clean, centers = 3, nstart = 25)
kmeans_mensch <- kmeans(cluster_data_mensch_clean, centers = 3, nstart = 25)

# Add cluster assignments to data
cluster_data_ki_clean$cluster <- kmeans_ki$cluster
cluster_data_mensch_clean$cluster <- kmeans_mensch$cluster

# =============================================================================
# DETAILLIERTE POST-HOC TESTS (TUKEY HSD) & EFFEKTSTÄRKEN (η²)
# =============================================================================

cat("\n================================================================================\n")
cat("DETAILLIERTE POST-HOC TESTS (TUKEY HSD) & EFFEKTSTÄRKEN (η²)\n")
cat("================================================================================\n")

# Function for detailed analysis
detailed_posthoc_effectsize_analysis <- function(data, group_name) {
  cat(paste("\n", paste(rep("=", 80), collapse = ""), "\n", sep = ""))
  cat(paste("ANALYSE:", group_name, "\n"))
  cat(paste(paste(rep("=", 80), collapse = ""), "\n", sep = ""))
  
  # Variable definitions
  variables <- c("VS", "MN", "ID", "EA")
  var_names <- c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
                 "Identifikation", "Emotionale Ansprache")
  
  # Cluster labels based on group
  if (group_name == "KI-GRUPPE") {
    cluster_labels <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")
  } else {
    cluster_labels <- c("Emotional Offen", "Ambivalent", "Emotional Distanziert")
  }
  
  # Results storage
  all_results <- list()
  
  for(i in 1:length(variables)) {
    var <- variables[i]
    var_name <- var_names[i]
    
    cat(paste("\n", paste(rep("-", 60), collapse = ""), "\n", sep = ""))
    cat(paste("VARIABLE:", var_name, "(", var, ")\n"))
    cat(paste(paste(rep("-", 60), collapse = ""), "\n", sep = ""))
    
    # 1. ANOVA
    cat("\n1. UNIVARIATE VARIANZANALYSE (ANOVA):\n")
    cat("   ", paste(rep("-", 40), collapse = ""), "\n")
    
    anova_result <- aov(as.formula(paste(var, "~ factor(cluster)")), data = data)
    anova_summary <- summary(anova_result)
    
    # Print ANOVA results
    cat("   F-Wert:", round(anova_summary[[1]]$`F value`[1], 3), "\n")
    cat("   p-Wert:", round(anova_summary[[1]]$`Pr(>F)`[1], 4), "\n")
    cat("   Signifikant:", ifelse(anova_summary[[1]]$`Pr(>F)`[1] < 0.05, "JA", "NEIN"), "\n")
    
    # 2. Effect Size (η²)
    cat("\n2. EFFEKTSTÄRKE (η²):\n")
    cat("   ", paste(rep("-", 40), collapse = ""), "\n")
    
    eta_squared_result <- eta_squared(anova_result)
    eta_squared_value <- eta_squared_result$Eta2
    
    cat("   η² =", round(eta_squared_value, 3), "\n")
    
    # Interpret effect size
    if (eta_squared_value < 0.01) {
      effect_interpretation <- "klein"
    } else if (eta_squared_value < 0.06) {
      effect_interpretation <- "mittel"
    } else {
      effect_interpretation <- "groß"
    }
    cat("   Interpretation:", effect_interpretation, "\n")
    
    # 3. Post-hoc Tests (Tukey HSD) - only if ANOVA is significant
    if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
      cat("\n3. POST-HOC TESTS (TUKEY HSD):\n")
      cat("   ", paste(rep("-", 40), collapse = ""), "\n")
      
      # Calculate means for each cluster
      cluster_means <- aggregate(data[[var]], by = list(data$cluster), FUN = mean)
      names(cluster_means) <- c("Cluster", "Mittelwert")
      cluster_means$Cluster_Name <- cluster_labels[cluster_means$Cluster]
      
      cat("   Cluster-Mittelwerte:\n")
      for(j in 1:nrow(cluster_means)) {
        cat("   ", cluster_means$Cluster_Name[j], ":", round(cluster_means$Mittelwert[j], 2), "\n")
      }
      
      # Tukey HSD
      tukey_result <- emmeans(anova_result, ~ cluster)
      tukey_comparisons <- pairs(tukey_result, adjust = "tukey")
      
      cat("\n   Paarweise Vergleiche (Tukey HSD):\n")
      tukey_df <- as.data.frame(tukey_comparisons)
      
      # Format and print Tukey results with proper cluster names
      for(j in 1:nrow(tukey_df)) {
        contrast <- tukey_df$contrast[j]
        estimate <- tukey_df$estimate[j]
        p_value <- tukey_df$p.value[j]
        
        # Parse contrast to get cluster numbers (handle "cluster1", "cluster2", etc.)
        contrast_parts <- strsplit(contrast, " - ")[[1]]
        
        # Extract cluster numbers from "cluster1", "cluster2", etc.
        cluster1_num <- as.numeric(sub("cluster", "", contrast_parts[1]))
        cluster2_num <- as.numeric(sub("cluster", "", contrast_parts[2]))
        
        # Get cluster names
        cluster1_name <- cluster_labels[cluster1_num]
        cluster2_name <- cluster_labels[cluster2_num]
        
        # Significance levels
        significance <- ifelse(p_value < 0.001, "***", 
                              ifelse(p_value < 0.01, "**", 
                                    ifelse(p_value < 0.05, "*", "n.s.")))
        
        cat("   ", cluster1_name, " vs ", cluster2_name, ": ", 
            round(estimate, 3), " (p = ", round(p_value, 4), ") ", significance, "\n", sep = "")
      }
      
      # Summary of significant differences
      significant_comparisons <- tukey_df[tukey_df$p.value < 0.05, ]
      if(nrow(significant_comparisons) > 0) {
        cat("\n   Signifikante Unterschiede (p < 0.05):\n")
        for(j in 1:nrow(significant_comparisons)) {
          contrast <- significant_comparisons$contrast[j]
          contrast_parts <- strsplit(contrast, " - ")[[1]]
          
          cluster1_num <- as.numeric(sub("cluster", "", contrast_parts[1]))
          cluster2_num <- as.numeric(sub("cluster", "", contrast_parts[2]))
          
          cluster1_name <- cluster_labels[cluster1_num]
          cluster2_name <- cluster_labels[cluster2_num]
          
          cat("   ✓ ", cluster1_name, " ≠ ", cluster2_name, "\n", sep = "")
        }
      } else {
        cat("\n   Keine signifikanten Unterschiede zwischen Clustern\n")
      }
      
    } else {
      cat("\n3. POST-HOC TESTS (TUKEY HSD):\n")
      cat("   ", paste(rep("-", 40), collapse = ""), "\n")
      cat("   Nicht erforderlich - ANOVA nicht signifikant\n")
    }
    
    # Store results
    all_results[[var]] <- list(
      variable_name = var_name,
      anova_f = anova_summary[[1]]$`F value`[1],
      anova_p = anova_summary[[1]]$`Pr(>F)`[1],
      eta_squared = eta_squared_value,
      effect_size_interpretation = effect_interpretation,
      tukey_results = if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) tukey_comparisons else NULL,
      cluster_means = if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) cluster_means else NULL
    )
  }
  
  return(all_results)
}

# =============================================================================
# EXECUTE ANALYSES
# =============================================================================

# Perform detailed analysis for both groups
results_ki <- detailed_posthoc_effectsize_analysis(cluster_data_ki_clean, "KI-GRUPPE")
results_mensch <- detailed_posthoc_effectsize_analysis(cluster_data_mensch_clean, "MENSCH-GRUPPE")

# =============================================================================
# SUMMARY TABLE
# =============================================================================

cat("\n================================================================================\n")
cat("ZUSAMMENFASSUNG: POST-HOC TESTS & EFFEKTSTÄRKEN\n")
cat("================================================================================\n")

# Create summary table
create_summary_table <- function(results, group_name) {
  cat(paste("\n", group_name, ":\n", sep = ""))
  cat(paste(rep("-", 50), collapse = ""), "\n")
  cat("Variable                    | F-Wert | p-Wert | η²    | Effekt | Signif.\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  variables <- c("VS", "MN", "ID", "EA")
  var_names_short <- c("Vertrauen", "Menschlichkeit", "Identifikation", "Emotionale Ansprache")
  
  for(i in 1:length(variables)) {
    var <- variables[i]
    var_name_short <- var_names_short[i]
    
    result <- results[[var]]
    f_value <- round(result$anova_f, 2)
    p_value <- round(result$anova_p, 4)
    eta_sq <- round(result$eta_squared, 3)
    effect_size <- result$effect_size_interpretation
    significant <- ifelse(result$anova_p < 0.05, "JA", "NEIN")
    
    cat(sprintf("%-25s | %6.2f | %6.4f | %5.3f | %-6s | %s\n", 
                var_name_short, f_value, p_value, eta_sq, effect_size, significant))
  }
}

create_summary_table(results_ki, "KI-GRUPPE")
create_summary_table(results_mensch, "MENSCH-GRUPPE")

# =============================================================================
# INTERPRETATION GUIDE
# =============================================================================

cat("\n================================================================================\n")
cat("INTERPRETATIONS-LEITFADEN\n")
cat("================================================================================\n")

cat("\nEFFEKTSTÄRKEN (η²):\n")
cat("• η² < 0.01: Kleiner Effekt\n")
cat("• η² = 0.01 - 0.06: Mittlerer Effekt\n")
cat("• η² > 0.06: Großer Effekt\n")

cat("\nPOST-HOC TESTS (TUKEY HSD):\n")
cat("• p < 0.001: *** (hochsignifikant)\n")
cat("• p < 0.01: ** (sehr signifikant)\n")
cat("• p < 0.05: * (signifikant)\n")
cat("• p ≥ 0.05: n.s. (nicht signifikant)\n")

cat("\nWISSENSCHAFTLICHE BEDEUTUNG:\n")
cat("• MANOVA: Multivariate Unterschiede zwischen Clustern\n")
cat("• ANOVA: Univariate Unterschiede für jede Variable\n")
cat("• Tukey HSD: Paarweise Vergleiche zwischen Clustern\n")
cat("• η²: Praktische Bedeutsamkeit der Effekte\n")

cat("\n================================================================================\n")
cat("ANALYSE ERFOLGREICH ABGESCHLOSSEN\n")
cat("================================================================================\n") 