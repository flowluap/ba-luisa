# =============================================================================
# COHEN'S D TABLE WITH T-TEST MEANS, CONFIDENCE INTERVALS & STANDARD DEVIATIONS
# =============================================================================
# Erstellt eine umfassende Tabelle mit Cohen's d, t-Test-Mittelwerten, 
# Konfidenzintervallen und Standardabweichungen mit ggplot alpha theme

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(scales)
library(effectsize)

# =============================================================================
# LOAD DATA
# =============================================================================

cat("================================================================================\n")
cat("COHEN'S D TABELLE MIT T-TEST-MITTELWERTEN, KONFIDENZINTERVALLEN & SD\n")
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
# COMPREHENSIVE T-TEST ANALYSIS WITH COHEN'S D
# =============================================================================

cat("\n================================================================================\n")
cat("UMFASSENDE T-TEST-ANALYSE MIT COHEN'S D\n")
cat("================================================================================\n")

# Function for comprehensive t-test analysis
comprehensive_ttest_analysis <- function(data, group_name) {
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
    
    # Get data for each cluster
    cluster1_data <- data[data$cluster == 1, var]
    cluster2_data <- data[data$cluster == 2, var]
    cluster3_data <- data[data$cluster == 3, var]
    
    # Calculate descriptive statistics
    cluster1_mean <- mean(cluster1_data, na.rm = TRUE)
    cluster2_mean <- mean(cluster2_data, na.rm = TRUE)
    cluster3_mean <- mean(cluster3_data, na.rm = TRUE)
    
    cluster1_sd <- sd(cluster1_data, na.rm = TRUE)
    cluster2_sd <- sd(cluster2_data, na.rm = TRUE)
    cluster3_sd <- sd(cluster3_data, na.rm = TRUE)
    
    cluster1_n <- length(na.omit(cluster1_data))
    cluster2_n <- length(na.omit(cluster2_data))
    cluster3_n <- length(na.omit(cluster3_data))
    
    # Calculate confidence intervals (95%)
    cluster1_se <- cluster1_sd / sqrt(cluster1_n)
    cluster2_se <- cluster2_sd / sqrt(cluster2_n)
    cluster3_se <- cluster3_sd / sqrt(cluster3_n)
    
    cluster1_ci_lower <- cluster1_mean - qt(0.975, cluster1_n - 1) * cluster1_se
    cluster1_ci_upper <- cluster1_mean + qt(0.975, cluster1_n - 1) * cluster1_se
    cluster2_ci_lower <- cluster2_mean - qt(0.975, cluster2_n - 1) * cluster2_se
    cluster2_ci_upper <- cluster2_mean + qt(0.975, cluster2_n - 1) * cluster2_se
    cluster3_ci_lower <- cluster3_mean - qt(0.975, cluster3_n - 1) * cluster3_se
    cluster3_ci_upper <- cluster3_mean + qt(0.975, cluster3_n - 1) * cluster3_se
    
    # Perform t-tests between all pairs
    ttest_12 <- t.test(cluster1_data, cluster2_data, var.equal = FALSE)
    ttest_13 <- t.test(cluster1_data, cluster3_data, var.equal = FALSE)
    ttest_23 <- t.test(cluster2_data, cluster3_data, var.equal = FALSE)
    
    # Calculate Cohen's d for all pairs
    cohens_d_12 <- cohens_d(cluster1_data, cluster2_data)
    cohens_d_13 <- cohens_d(cluster1_data, cluster3_data)
    cohens_d_23 <- cohens_d(cluster2_data, cluster3_data)
    
    # Print results
    cat("\n1. DESKRIPTIVE STATISTIKEN:\n")
    cat("   ", paste(rep("-", 40), collapse = ""), "\n")
    cat("   Cluster 1 (", cluster_labels[1], "):\n")
    cat("     n =", cluster1_n, ", M =", round(cluster1_mean, 3), 
        ", SD =", round(cluster1_sd, 3), "\n")
    cat("     95% CI [", round(cluster1_ci_lower, 3), ", ", 
        round(cluster1_ci_upper, 3), "]\n", sep = "")
    
    cat("   Cluster 2 (", cluster_labels[2], "):\n")
    cat("     n =", cluster2_n, ", M =", round(cluster2_mean, 3), 
        ", SD =", round(cluster2_sd, 3), "\n")
    cat("     95% CI [", round(cluster2_ci_lower, 3), ", ", 
        round(cluster2_ci_upper, 3), "]\n", sep = "")
    
    cat("   Cluster 3 (", cluster_labels[3], "):\n")
    cat("     n =", cluster3_n, ", M =", round(cluster3_mean, 3), 
        ", SD =", round(cluster3_sd, 3), "\n")
    cat("     95% CI [", round(cluster3_ci_lower, 3), ", ", 
        round(cluster3_ci_upper, 3), "]\n", sep = "")
    
    cat("\n2. T-TEST VERGLEICHE:\n")
    cat("   ", paste(rep("-", 40), collapse = ""), "\n")
    
    # T-test 1 vs 2
    cat("   ", cluster_labels[1], " vs ", cluster_labels[2], ":\n")
    cat("     t(", round(ttest_12$parameter, 1), ") = ", round(ttest_12$statistic, 3), 
        ", p = ", round(ttest_12$p.value, 4), sep = "")
    if(ttest_12$p.value < 0.001) cat(" ***") else if(ttest_12$p.value < 0.01) cat(" **") 
    else if(ttest_12$p.value < 0.05) cat(" *") else cat(" n.s.")
    cat("\n")
    
    # T-test 1 vs 3
    cat("   ", cluster_labels[1], " vs ", cluster_labels[3], ":\n")
    cat("     t(", round(ttest_13$parameter, 1), ") = ", round(ttest_13$statistic, 3), 
        ", p = ", round(ttest_13$p.value, 4), sep = "")
    if(ttest_13$p.value < 0.001) cat(" ***") else if(ttest_13$p.value < 0.01) cat(" **") 
    else if(ttest_13$p.value < 0.05) cat(" *") else cat(" n.s.")
    cat("\n")
    
    # T-test 2 vs 3
    cat("   ", cluster_labels[2], " vs ", cluster_labels[3], ":\n")
    cat("     t(", round(ttest_23$parameter, 1), ") = ", round(ttest_23$statistic, 3), 
        ", p = ", round(ttest_23$p.value, 4), sep = "")
    if(ttest_23$p.value < 0.001) cat(" ***") else if(ttest_23$p.value < 0.01) cat(" **") 
    else if(ttest_23$p.value < 0.05) cat(" *") else cat(" n.s.")
    cat("\n")
    
    cat("\n3. COHEN'S D EFFEKTSTÄRKEN:\n")
    cat("   ", paste(rep("-", 40), collapse = ""), "\n")
    cat("   ", cluster_labels[1], " vs ", cluster_labels[2], ": d = ", 
        round(cohens_d_12$Cohens_d, 3), "\n", sep = "")
    cat("   ", cluster_labels[1], " vs ", cluster_labels[3], ": d = ", 
        round(cohens_d_13$Cohens_d, 3), "\n", sep = "")
    cat("   ", cluster_labels[2], " vs ", cluster_labels[3], ": d = ", 
        round(cohens_d_23$Cohens_d, 3), "\n", sep = "")
    
    # Store results
    all_results[[var]] <- list(
      variable_name = var_name,
      cluster_means = c(cluster1_mean, cluster2_mean, cluster3_mean),
      cluster_sds = c(cluster1_sd, cluster2_sd, cluster3_sd),
      cluster_ns = c(cluster1_n, cluster2_n, cluster3_n),
      cluster_cis = list(
        c(cluster1_ci_lower, cluster1_ci_upper),
        c(cluster2_ci_lower, cluster2_ci_upper),
        c(cluster3_ci_lower, cluster3_ci_upper)
      ),
      ttest_results = list(ttest_12, ttest_13, ttest_23),
      cohens_d = list(cohens_d_12, cohens_d_13, cohens_d_23),
      cluster_labels = cluster_labels
    )
  }
  
  return(all_results)
}

# =============================================================================
# EXECUTE ANALYSES
# =============================================================================

# Perform comprehensive analysis for both groups
results_ki <- comprehensive_ttest_analysis(cluster_data_ki_clean, "KI-GRUPPE")
results_mensch <- comprehensive_ttest_analysis(cluster_data_mensch_clean, "MENSCH-GRUPPE")

# =============================================================================
# CREATE COMPREHENSIVE TABLE WITH GGPLOT
# =============================================================================

cat("\n================================================================================\n")
cat("ERSTELLE UMFASSENDE TABELLE MIT GGPLOT\n")
cat("================================================================================\n")

# Function to create comprehensive table
create_comprehensive_table <- function(results, group_name) {
  # Prepare data for table
  table_data <- data.frame()
  
  variables <- c("VS", "MN", "ID", "EA")
  var_names <- c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
                 "Identifikation", "Emotionale Ansprache")
  
  for(i in 1:length(variables)) {
    var <- variables[i]
    var_name <- var_names[i]
    result <- results[[var]]
    
    for(j in 1:3) {
      cluster_label <- result$cluster_labels[j]
      mean_val <- result$cluster_means[j]
      sd_val <- result$cluster_sds[j]
      n_val <- result$cluster_ns[j]
      ci_lower <- result$cluster_cis[[j]][1]
      ci_upper <- result$cluster_cis[[j]][2]
      
      # Add row to table data
      table_data <- rbind(table_data, data.frame(
        Variable = var_name,
        Cluster = cluster_label,
        Mean = mean_val,
        SD = sd_val,
        N = n_val,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Create comparison data for t-tests and Cohen's d
  comparison_data <- data.frame()
  
  for(i in 1:length(variables)) {
    var <- variables[i]
    var_name <- var_names[i]
    result <- results[[var]]
    
    # Add t-test and Cohen's d results
    for(j in 1:3) {
      if(j == 1) {
        # 1 vs 2
        ttest <- result$ttest_results[[1]]
        cohens_d <- result$cohens_d[[1]]
        cluster1 <- result$cluster_labels[1]
        cluster2 <- result$cluster_labels[2]
      } else if(j == 2) {
        # 1 vs 3
        ttest <- result$ttest_results[[2]]
        cohens_d <- result$cohens_d[[2]]
        cluster1 <- result$cluster_labels[1]
        cluster2 <- result$cluster_labels[3]
      } else {
        # 2 vs 3
        ttest <- result$ttest_results[[3]]
        cohens_d <- result$cohens_d[[3]]
        cluster1 <- result$cluster_labels[2]
        cluster2 <- result$cluster_labels[3]
      }
      
      comparison_data <- rbind(comparison_data, data.frame(
        Variable = var_name,
        Comparison = paste(cluster1, "vs", cluster2),
        T_Value = ttest$statistic,
        P_Value = ttest$p.value,
        Cohens_D = cohens_d$Cohens_d,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Create comprehensive table plot
  p1 <- ggplot(table_data, aes(x = Cluster, y = Mean, fill = Variable)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                  position = position_dodge(width = 0.8), width = 0.25, alpha = 0.7) +
    geom_text(aes(label = sprintf("M=%.2f\nSD=%.2f\nn=%d", Mean, SD, N)), 
              position = position_dodge(width = 0.8), vjust = -0.5, size = 2.5, alpha = 0.9) +
    facet_wrap(~Variable, scales = "free_y", ncol = 2) +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(),
      axis.title = element_text(),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(),
      panel.grid.major = element_line(),
      panel.grid.minor = element_line(),
      strip.text = element_text(face = "bold")
    ) +
    labs(
      title = paste("Deskriptive Statistiken:", group_name),
      x = "Cluster",
      y = "Mittelwert (95% Konfidenzintervall)",
      fill = "Variable"
    )
  
  # Create comparison table plot
  p2 <- ggplot(comparison_data, aes(x = Comparison, y = Cohens_D, fill = Variable)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.8) +
    geom_text(aes(label = sprintf("t=%.2f\np=%.3f\nd=%.2f", T_Value, P_Value, Cohens_D)), 
              position = position_dodge(width = 0.8), vjust = -0.5, size = 2.5, alpha = 0.9) +
    facet_wrap(~Variable, scales = "free_y", ncol = 2) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(),
      axis.title = element_text(),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(),
      panel.grid.major = element_line(),
      panel.grid.minor = element_line(),
      strip.text = element_text(face = "bold")
    ) +
    labs(
      title = paste("T-Test Vergleiche & Cohen's d:", group_name),
      x = "Vergleich",
      y = "Cohen's d",
      fill = "Variable"
    )
  
  # Combine plots
  combined_plot <- grid.arrange(p1, p2, ncol = 1, heights = c(1, 1))
  
  return(list(
    table_data = table_data,
    comparison_data = comparison_data,
    plot1 = p1,
    plot2 = p2,
    combined_plot = combined_plot
  ))
}

# Create tables for both groups
ki_table <- create_comprehensive_table(results_ki, "KI-GRUPPE")
mensch_table <- create_comprehensive_table(results_mensch, "MENSCH-GRUPPE")

# =============================================================================
# SAVE PLOTS
# =============================================================================

cat("\nSpeichere Plots...\n")

# Save KI group plots
ggsave("organized/images/clustering/ki_comprehensive_table.png", 
       ki_table$combined_plot, width = 14, height = 12, dpi = 300)

# Save Mensch group plots
ggsave("organized/images/clustering/mensch_comprehensive_table.png", 
       mensch_table$combined_plot, width = 14, height = 12, dpi = 300)

# Save individual plots
ggsave("organized/images/clustering/ki_descriptive_stats.png", 
       ki_table$plot1, width = 12, height = 8, dpi = 300)
ggsave("organized/images/clustering/ki_comparisons.png", 
       ki_table$plot2, width = 12, height = 8, dpi = 300)

ggsave("organized/images/clustering/mensch_descriptive_stats.png", 
       mensch_table$plot1, width = 12, height = 8, dpi = 300)
ggsave("organized/images/clustering/mensch_comparisons.png", 
       mensch_table$plot2, width = 12, height = 8, dpi = 300)

cat("✓ Plots erfolgreich gespeichert!\n")

# =============================================================================
# PRINT SUMMARY TABLES
# =============================================================================

cat("\n================================================================================\n")
cat("ZUSAMMENFASSUNG: DESKRIPTIVE STATISTIKEN\n")
cat("================================================================================\n")

# Print KI group summary
cat("\nKI-GRUPPE:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
print(ki_table$table_data)

cat("\nKI-GRUPPE VERGLEICHE:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
print(ki_table$comparison_data)

# Print Mensch group summary
cat("\nMENSCH-GRUPPE:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
print(mensch_table$table_data)

cat("\nMENSCH-GRUPPE VERGLEICHE:\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
print(mensch_table$comparison_data)

# =============================================================================
# INTERPRETATION GUIDE
# =============================================================================

cat("\n================================================================================\n")
cat("INTERPRETATIONS-LEITFADEN\n")
cat("================================================================================\n")

cat("\nCOHEN'S D EFFEKTSTÄRKEN:\n")
cat("• |d| < 0.2: Kleiner Effekt\n")
cat("• |d| = 0.2 - 0.5: Kleiner bis mittlerer Effekt\n")
cat("• |d| = 0.5 - 0.8: Mittlerer Effekt\n")
cat("• |d| > 0.8: Großer Effekt\n")

cat("\nT-TEST SIGNIFIKANZ:\n")
cat("• p < 0.001: *** (hochsignifikant)\n")
cat("• p < 0.01: ** (sehr signifikant)\n")
cat("• p < 0.05: * (signifikant)\n")
cat("• p ≥ 0.05: n.s. (nicht signifikant)\n")

cat("\nKONFIDENZINTERVALLE:\n")
cat("• 95% Konfidenzintervall zeigt die Unsicherheit der Mittelwertschätzung\n")
cat("• Überlappende Konfidenzintervalle deuten auf nicht-signifikante Unterschiede hin\n")

cat("\nWISSENSCHAFTLICHE BEDEUTUNG:\n")
cat("• T-Tests: Prüfen auf statistische Signifikanz zwischen Clustern\n")
cat("• Cohen's d: Quantifiziert die praktische Bedeutsamkeit der Effekte\n")
cat("• Konfidenzintervalle: Zeigen die Präzision der Schätzungen\n")
cat("• Standardabweichungen: Messen die Variabilität innerhalb der Cluster\n")

cat("\n================================================================================\n")
cat("ANALYSE ERFOLGREICH ABGESCHLOSSEN\n")
cat("================================================================================\n") 