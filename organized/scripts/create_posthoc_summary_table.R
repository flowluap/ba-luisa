# =============================================================================
# CREATE POST-HOC SUMMARY TABLE (PNG)
# =============================================================================
# Erstellt eine übersichtliche Tabelle der wichtigsten Post-hoc Test Ergebnisse

library(dplyr)
library(car)
library(emmeans)
library(effectsize)
library(ggplot2)
library(gridExtra)
library(grid)

# =============================================================================
# LOAD DATA AND PERFORM ANALYSIS
# =============================================================================

cat("Lade Daten und führe Analysen durch...\n")

# Load data
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data
data_processed <- data %>% filter(FINISHED == 1)
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

# Prepare clustering data
mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")

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

cluster_data_ki_clean <- cluster_data_ki[complete.cases(cluster_data_ki), ]
cluster_data_mensch_clean <- cluster_data_mensch[complete.cases(cluster_data_mensch), ]

# Perform clustering
set.seed(123)
kmeans_ki <- kmeans(cluster_data_ki_clean, centers = 3, nstart = 25)
kmeans_mensch <- kmeans(cluster_data_mensch_clean, centers = 3, nstart = 25)

cluster_data_ki_clean$cluster <- kmeans_ki$cluster
cluster_data_mensch_clean$cluster <- kmeans_mensch$cluster

# =============================================================================
# EXTRACT POST-HOC RESULTS
# =============================================================================

extract_posthoc_results <- function(data, group_name) {
  variables <- c("VS", "MN", "ID", "EA")
  var_names <- c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
                 "Identifikation", "Emotionale Ansprache")
  
  if (group_name == "KI-Gruppe") {
    cluster_labels <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")
  } else {
    cluster_labels <- c("Emotional Offen", "Ambivalent", "Emotional Distanziert")
  }
  
  posthoc_data <- data.frame()
  
  for(i in 1:length(variables)) {
    var <- variables[i]
    var_name <- var_names[i]
    
    # ANOVA
    anova_result <- aov(as.formula(paste(var, "~ factor(cluster)")), data = data)
    anova_summary <- summary(anova_result)
    
    # Post-hoc tests if significant
    if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
      tukey_result <- emmeans(anova_result, ~ cluster)
      tukey_comparisons <- pairs(tukey_result, adjust = "tukey")
      tukey_df <- as.data.frame(tukey_comparisons)
      
      # Process Tukey results
      for(j in 1:nrow(tukey_df)) {
        contrast <- tukey_df$contrast[j]
        estimate <- tukey_df$estimate[j]
        p_value <- tukey_df$p.value[j]
        
        contrast_parts <- strsplit(contrast, " - ")[[1]]
        cluster1_num <- as.numeric(sub("cluster", "", contrast_parts[1]))
        cluster2_num <- as.numeric(sub("cluster", "", contrast_parts[2]))
        
        cluster1_name <- cluster_labels[cluster1_num]
        cluster2_name <- cluster_labels[cluster2_num]
        
        significance <- ifelse(p_value < 0.001, "***", 
                              ifelse(p_value < 0.01, "**", 
                                    ifelse(p_value < 0.05, "*", "n.s.")))
        
        posthoc_data <- rbind(posthoc_data, data.frame(
          Variable = var_name,
          Vergleich = paste(cluster1_name, "vs", cluster2_name),
          Differenz = round(estimate, 3),
          p_Wert = round(p_value, 4),
          Signifikanz = significance,
          Gruppe = group_name,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(posthoc_data)
}

# Extract results
posthoc_ki <- extract_posthoc_results(cluster_data_ki_clean, "KI-Gruppe")
posthoc_mensch <- extract_posthoc_results(cluster_data_mensch_clean, "Mensch-Gruppe")

# =============================================================================
# CREATE SUMMARY TABLE
# =============================================================================

cat("Erstelle Post-hoc Zusammenfassungstabelle...\n")

# Create summary table text
create_posthoc_summary_text <- function(posthoc_ki, posthoc_mensch) {
  # Header
  summary_text <- paste(
    "POST-HOC TESTS (TUKEY HSD) - ZUSAMMENFASSUNG",
    "",
    "Format: Variable | Vergleich | Differenz | p-Wert | Signifikanz",
    "",
    sep = "\n"
  )
  
  # KI-Gruppe
  if(nrow(posthoc_ki) > 0) {
    summary_text <- paste(summary_text, "KI-GRUPPE:", sep = "\n")
    for(i in 1:nrow(posthoc_ki)) {
      summary_text <- paste(summary_text, 
                          sprintf("%s | %s | %.3f | %.4f | %s", 
                                  posthoc_ki$Variable[i], posthoc_ki$Vergleich[i], 
                                  posthoc_ki$Differenz[i], posthoc_ki$p_Wert[i], 
                                  posthoc_ki$Signifikanz[i]), 
                          sep = "\n")
    }
  } else {
    summary_text <- paste(summary_text, "KI-GRUPPE: Keine signifikanten Unterschiede", sep = "\n")
  }
  
  # Mensch-Gruppe
  if(nrow(posthoc_mensch) > 0) {
    summary_text <- paste(summary_text, "", "MENSCH-GRUPPE:", sep = "\n")
    for(i in 1:nrow(posthoc_mensch)) {
      summary_text <- paste(summary_text, 
                          sprintf("%s | %s | %.3f | %.4f | %s", 
                                  posthoc_mensch$Variable[i], posthoc_mensch$Vergleich[i], 
                                  posthoc_mensch$Differenz[i], posthoc_mensch$p_Wert[i], 
                                  posthoc_mensch$Signifikanz[i]), 
                          sep = "\n")
    }
  } else {
    summary_text <- paste(summary_text, "", "MENSCH-GRUPPE: Keine signifikanten Unterschiede", sep = "\n")
  }
  
  # Legend
  summary_text <- paste(summary_text, "", 
                       "LEGENDE:",
                       "*** p < 0.001 (hochsignifikant)",
                       "** p < 0.01 (sehr signifikant)", 
                       "* p < 0.05 (signifikant)",
                       "n.s. p ≥ 0.05 (nicht signifikant)",
                       "",
                       "Interpretation:",
                       "Positive Differenz: Erster Cluster > Zweiter Cluster",
                       "Negative Differenz: Erster Cluster < Zweiter Cluster",
                       sep = "\n")
  
  return(summary_text)
}

# Create the summary text
posthoc_summary_text <- create_posthoc_summary_text(posthoc_ki, posthoc_mensch)

# Create plot with text
p <- ggplot() + 
  annotate("text", x = 0.05, y = 0.95, label = posthoc_summary_text, 
           hjust = 0, vjust = 1, size = 3.5, family = "Times New Roman") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

# Save the plot
ggsave("organized/images/clustering/posthoc_summary_table.png", 
       p, width = 12, height = 10, dpi = 300, bg = "white")

cat("✓ Post-hoc Zusammenfassungstabelle erstellt: posthoc_summary_table.png\n")

# =============================================================================
# CREATE COMPACT TABLE
# =============================================================================

# Create a more compact table with only significant results
create_compact_table <- function(posthoc_ki, posthoc_mensch) {
  # Filter only significant results
  significant_ki <- posthoc_ki[posthoc_ki$Signifikanz != "n.s.", ]
  significant_mensch <- posthoc_mensch[posthoc_mensch$Signifikanz != "n.s.", ]
  
  # Create compact text
  compact_text <- paste(
    "POST-HOC TESTS (TUKEY HSD) - SIGNIFIKANTE ERGEBNISSE",
    "",
    "KI-GRUPPE:",
    sep = "\n"
  )
  
  if(nrow(significant_ki) > 0) {
    for(i in 1:nrow(significant_ki)) {
      compact_text <- paste(compact_text, 
                          sprintf("• %s: %s (Differenz = %.3f, %s)", 
                                  significant_ki$Variable[i], significant_ki$Vergleich[i], 
                                  significant_ki$Differenz[i], significant_ki$Signifikanz[i]), 
                          sep = "\n")
    }
  } else {
    compact_text <- paste(compact_text, "Keine signifikanten Unterschiede", sep = "\n")
  }
  
  compact_text <- paste(compact_text, "", "MENSCH-GRUPPE:", sep = "\n")
  
  if(nrow(significant_mensch) > 0) {
    for(i in 1:nrow(significant_mensch)) {
      compact_text <- paste(compact_text, 
                          sprintf("• %s: %s (Differenz = %.3f, %s)", 
                                  significant_mensch$Variable[i], significant_mensch$Vergleich[i], 
                                  significant_mensch$Differenz[i], significant_mensch$Signifikanz[i]), 
                          sep = "\n")
    }
  } else {
    compact_text <- paste(compact_text, "Keine signifikanten Unterschiede", sep = "\n")
  }
  
  compact_text <- paste(compact_text, "", 
                       "LEGENDE: *** p<0.001, ** p<0.01, * p<0.05", sep = "\n")
  
  return(compact_text)
}

# Create compact table
compact_text <- create_compact_table(posthoc_ki, posthoc_mensch)

# Create plot with compact text
p_compact <- ggplot() + 
  annotate("text", x = 0.05, y = 0.95, label = compact_text, 
           hjust = 0, vjust = 1, size = 4, family = "Times New Roman") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

# Save the compact plot
ggsave("organized/images/clustering/posthoc_compact_table.png", 
       p_compact, width = 10, height = 8, dpi = 300, bg = "white")

cat("✓ Kompakte Post-hoc Tabelle erstellt: posthoc_compact_table.png\n")

cat("\n================================================================================\n")
cat("POST-HOC TABELLEN ERFOLGREICH ERSTELLT\n")
cat("================================================================================\n")
cat("✓ posthoc_summary_table.png - Vollständige Übersicht\n")
cat("✓ posthoc_compact_table.png - Kompakte Übersicht (nur signifikant)\n")
cat("================================================================================\n") 