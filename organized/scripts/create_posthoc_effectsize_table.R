# =============================================================================
# CREATE POST-HOC TESTS & EFFECT SIZE TABLE (PNG)
# =============================================================================
# Erstellt eine übersichtliche Tabelle der Post-hoc Tests und Effektstärken

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
# FUNCTION TO EXTRACT RESULTS
# =============================================================================

extract_results <- function(data, group_name) {
  variables <- c("VS", "MN", "ID", "EA")
  var_names <- c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
                 "Identifikation", "Emotionale Ansprache")
  
  if (group_name == "KI-Gruppe") {
    cluster_labels <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")
  } else {
    cluster_labels <- c("Emotional Offen", "Ambivalent", "Emotional Distanziert")
  }
  
  results_list <- list()
  
  for(i in 1:length(variables)) {
    var <- variables[i]
    var_name <- var_names[i]
    
    # ANOVA
    anova_result <- aov(as.formula(paste(var, "~ factor(cluster)")), data = data)
    anova_summary <- summary(anova_result)
    
    # Effect size
    eta_squared_result <- eta_squared(anova_result)
    eta_squared_value <- eta_squared_result$Eta2
    
    # Interpret effect size
    if (eta_squared_value < 0.01) {
      effect_interpretation <- "klein"
    } else if (eta_squared_value < 0.06) {
      effect_interpretation <- "mittel"
    } else {
      effect_interpretation <- "groß"
    }
    
    # Post-hoc tests if significant
    tukey_results <- NULL
    if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
      tukey_result <- emmeans(anova_result, ~ cluster)
      tukey_comparisons <- pairs(tukey_result, adjust = "tukey")
      tukey_df <- as.data.frame(tukey_comparisons)
      
      # Process Tukey results
      tukey_results <- data.frame()
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
        
        tukey_results <- rbind(tukey_results, data.frame(
          Variable = var_name,
          Vergleich = paste(cluster1_name, "vs", cluster2_name),
          Differenz = round(estimate, 3),
          p_Wert = round(p_value, 4),
          Signifikanz = significance,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    results_list[[var]] <- list(
      variable_name = var_name,
      f_value = anova_summary[[1]]$`F value`[1],
      p_value = anova_summary[[1]]$`Pr(>F)`[1],
      eta_squared = eta_squared_value,
      effect_size_interpretation = effect_interpretation,
      significant = anova_summary[[1]]$`Pr(>F)`[1] < 0.05,
      tukey_results = tukey_results
    )
  }
  
  return(results_list)
}

# =============================================================================
# EXTRACT RESULTS FOR BOTH GROUPS
# =============================================================================

cat("Extrahiere Ergebnisse...\n")

results_ki <- extract_results(cluster_data_ki_clean, "KI-Gruppe")
results_mensch <- extract_results(cluster_data_mensch_clean, "Mensch-Gruppe")

# =============================================================================
# CREATE SUMMARY TABLES
# =============================================================================

# Create ANOVA summary table
create_anova_table <- function(results, group_name) {
  variables <- c("VS", "MN", "ID", "EA")
  var_names_short <- c("Vertrauen", "Menschlichkeit", "Identifikation", "Emotionale Ansprache")
  
  anova_data <- data.frame()
  
  for(i in 1:length(variables)) {
    var <- variables[i]
    result <- results[[var]]
    
    anova_data <- rbind(anova_data, data.frame(
      Variable = var_names_short[i],
      F_Wert = round(result$f_value, 2),
      p_Wert = round(result$p_value, 4),
      Eta_Quadrat = round(result$eta_squared, 3),
      Effektgroesse = result$effect_size_interpretation,
      Signifikant = ifelse(result$significant, "JA", "NEIN"),
      stringsAsFactors = FALSE
    ))
  }
  
  return(anova_data)
}

# Create Tukey summary table
create_tukey_table <- function(results, group_name) {
  tukey_data <- data.frame()
  
  variables <- c("VS", "MN", "ID", "EA")
  
  for(var in variables) {
    result <- results[[var]]
    if(!is.null(result$tukey_results)) {
      tukey_data <- rbind(tukey_data, result$tukey_results)
    }
  }
  
  return(tukey_data)
}

anova_ki <- create_anova_table(results_ki, "KI-Gruppe")
anova_mensch <- create_anova_table(results_mensch, "Mensch-Gruppe")
tukey_ki <- create_tukey_table(results_ki, "KI-Gruppe")
tukey_mensch <- create_tukey_table(results_mensch, "Mensch-Gruppe")

# =============================================================================
# CREATE PNG TABLES
# =============================================================================

cat("Erstelle PNG-Tabellen...\n")

# Function to create table plot
create_table_plot <- function(data, title, subtitle = "") {
  if(nrow(data) == 0) {
    # Create empty plot with message
    p <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "Keine signifikanten Unterschiede", 
               size = 5, family = "Times New Roman") +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold", family = "Times New Roman"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, family = "Times New Roman")
      )
  } else {
    # Create table
    p <- ggplot(data, aes(x = 1, y = nrow(data):1)) +
      geom_text(aes(label = paste(Variable, "|", F_Wert, "|", p_Wert, "|", Eta_Quadrat, "|", Effektgroesse, "|", Signifikant)), 
                hjust = 0, size = 3.5, family = "Times New Roman") +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold", family = "Times New Roman"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, family = "Times New Roman"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")
      ) +
      scale_x_continuous(limits = c(0.5, 10)) +
      scale_y_continuous(limits = c(0, nrow(data) + 1))
  }
  
  return(p)
}

# Function to create Tukey table plot
create_tukey_table_plot <- function(data, title, subtitle = "") {
  if(nrow(data) == 0) {
    p <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "Keine signifikanten Unterschiede", 
               size = 5, family = "Times New Roman") +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold", family = "Times New Roman"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, family = "Times New Roman")
      )
  } else {
    p <- ggplot(data, aes(x = 1, y = nrow(data):1)) +
      geom_text(aes(label = paste(Variable, "|", Vergleich, "|", Differenz, "|", p_Wert, "|", Signifikanz)), 
                hjust = 0, size = 3, family = "Times New Roman") +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold", family = "Times New Roman"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, family = "Times New Roman"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")
      ) +
      scale_x_continuous(limits = c(0.5, 12)) +
      scale_y_continuous(limits = c(0, nrow(data) + 1))
  }
  
  return(p)
}

# Create combined table
create_combined_table <- function(anova_ki, anova_mensch, tukey_ki, tukey_mensch) {
  # Create header
  header_text <- paste(
    "POST-HOC TESTS (TUKEY HSD) & EFFEKTSTÄRKEN (η²) - ZUSAMMENFASSUNG",
    "",
    "ANOVA-ERGEBNISSE:",
    "Variable | F-Wert | p-Wert | η² | Effekt | Signif.",
    "",
    "KI-GRUPPE:",
    sep = "\n"
  )
  
  # Add KI ANOVA results
  for(i in 1:nrow(anova_ki)) {
    header_text <- paste(header_text, 
                        sprintf("%s | %.2f | %.4f | %.3f | %s | %s", 
                                anova_ki$Variable[i], anova_ki$F_Wert[i], 
                                anova_ki$p_Wert[i], anova_ki$Eta_Quadrat[i], 
                                anova_ki$Effektgroesse[i], anova_ki$Signifikant[i]), 
                        sep = "\n")
  }
  
  header_text <- paste(header_text, "", "MENSCH-GRUPPE:", sep = "\n")
  
  # Add Mensch ANOVA results
  for(i in 1:nrow(anova_mensch)) {
    header_text <- paste(header_text, 
                        sprintf("%s | %.2f | %.4f | %.3f | %s | %s", 
                                anova_mensch$Variable[i], anova_mensch$F_Wert[i], 
                                anova_mensch$p_Wert[i], anova_mensch$Eta_Quadrat[i], 
                                anova_mensch$Effektgroesse[i], anova_mensch$Signifikant[i]), 
                        sep = "\n")
  }
  
  header_text <- paste(header_text, "", "TUKEY HSD POST-HOC TESTS:", sep = "\n")
  
  # Add KI Tukey results
  if(nrow(tukey_ki) > 0) {
    header_text <- paste(header_text, "", "KI-GRUPPE:", sep = "\n")
    for(i in 1:nrow(tukey_ki)) {
      header_text <- paste(header_text, 
                          sprintf("%s | %s | %.3f | %.4f | %s", 
                                  tukey_ki$Variable[i], tukey_ki$Vergleich[i], 
                                  tukey_ki$Differenz[i], tukey_ki$p_Wert[i], 
                                  tukey_ki$Signifikanz[i]), 
                          sep = "\n")
    }
  }
  
  # Add Mensch Tukey results
  if(nrow(tukey_mensch) > 0) {
    header_text <- paste(header_text, "", "MENSCH-GRUPPE:", sep = "\n")
    for(i in 1:nrow(tukey_mensch)) {
      header_text <- paste(header_text, 
                          sprintf("%s | %s | %.3f | %.4f | %s", 
                                  tukey_mensch$Variable[i], tukey_mensch$Vergleich[i], 
                                  tukey_mensch$Differenz[i], tukey_mensch$p_Wert[i], 
                                  tukey_mensch$Signifikanz[i]), 
                          sep = "\n")
    }
  }
  
  # Add legend
  header_text <- paste(header_text, "", 
                      "LEGENDE:",
                      "*** p < 0.001 (hochsignifikant)",
                      "** p < 0.01 (sehr signifikant)", 
                      "* p < 0.05 (signifikant)",
                      "n.s. p ≥ 0.05 (nicht signifikant)",
                      "",
                      "η² Interpretation:",
                      "< 0.01: Kleiner Effekt",
                      "0.01 - 0.06: Mittlerer Effekt", 
                      "> 0.06: Großer Effekt",
                      sep = "\n")
  
  return(header_text)
}

# Create the combined table text
combined_table_text <- create_combined_table(anova_ki, anova_mensch, tukey_ki, tukey_mensch)

# Create plot with text
p <- ggplot() + 
  annotate("text", x = 0.05, y = 0.95, label = combined_table_text, 
           hjust = 0, vjust = 1, size = 3, family = "Times New Roman") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1))

# Save the plot
ggsave("organized/images/clustering/posthoc_effectsize_summary_table.png", 
       p, width = 12, height = 10, dpi = 300, bg = "white")

cat("✓ Tabelle erfolgreich erstellt: posthoc_effectsize_summary_table.png\n")

# =============================================================================
# CREATE SEPARATE TABLES
# =============================================================================

# Create separate ANOVA table
create_anova_summary_plot <- function(anova_ki, anova_mensch) {
  # Combine data
  anova_combined <- rbind(
    cbind(anova_ki, Gruppe = "KI-Gruppe"),
    cbind(anova_mensch, Gruppe = "Mensch-Gruppe")
  )
  
  # Create plot
  p <- ggplot(anova_combined, aes(x = Variable, y = Eta_Quadrat, fill = Gruppe)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), 
             color = "black", linewidth = 0.3) +
    geom_text(aes(label = sprintf("η² = %.3f\np < 0.001", Eta_Quadrat)), 
              position = position_dodge(width = 0.9), vjust = -0.5, 
              size = 2.5, family = "Times New Roman") +
    scale_fill_manual(values = c("KI-Gruppe" = "#8DD3C8", "Mensch-Gruppe" = "#FFB6C1")) +
    labs(title = "ANOVA-Ergebnisse: Effektstärken (η²) und p-Werte",
         x = "Variablen",
         y = "Effektstärke (η²)",
         fill = "Gruppe") +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12, family = "Times New Roman"),
      axis.text = element_text(size = 10, family = "Times New Roman"),
      axis.text.x = element_text(angle = 45, hjust = 1, family = "Times New Roman"),
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold", family = "Times New Roman"),
      legend.title = element_text(size = 12, family = "Times New Roman"),
      legend.text = element_text(size = 10, family = "Times New Roman"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    ylim(0, max(anova_combined$Eta_Quadrat) * 1.3)
  
  return(p)
}

# Create ANOVA summary plot
anova_plot <- create_anova_summary_plot(anova_ki, anova_mensch)
ggsave("organized/images/clustering/anova_effectsize_summary.png", 
       anova_plot, width = 10, height = 6, dpi = 300, bg = "white")

cat("✓ ANOVA-Zusammenfassung erstellt: anova_effectsize_summary.png\n")

cat("\n================================================================================\n")
cat("TABELLEN ERFOLGREICH ERSTELLT\n")
cat("================================================================================\n")
cat("✓ posthoc_effectsize_summary_table.png - Vollständige Übersicht\n")
cat("✓ anova_effectsize_summary.png - ANOVA-Zusammenfassung\n")
cat("================================================================================\n") 