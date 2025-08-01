# =============================================================================
# CREATE POST-HOC APA TABLES (KI & MENSCH)
# =============================================================================
# Erstellt separate APA-formatierte Tabellen für KI und Mensch Gruppen

library(dplyr)
library(car)
library(emmeans)
library(effectsize)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)

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
    
    # Always perform post-hoc tests (not just if significant)
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
      
      comparison <- paste(cluster1_name, "vs", cluster2_name)
      
      # Determine significance
      if(p_value < 0.001) {
        significance <- "***"
      } else if(p_value < 0.01) {
        significance <- "**"
      } else if(p_value < 0.05) {
        significance <- "*"
      } else {
        significance <- "n.s."
      }
      
      posthoc_data <- rbind(posthoc_data, data.frame(
        Variable = var_name,
        Vergleich = comparison,
        Differenz = estimate,
        p_Wert = p_value,
        Signifikanz = significance,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(posthoc_data)
}

# Extract post-hoc results
significant_ki <- extract_posthoc_results(cluster_data_ki_clean, "KI-Gruppe")
significant_mensch <- extract_posthoc_results(cluster_data_mensch_clean, "Mensch-Gruppe")

cat("Erstelle APA-formatierte Tabellen...\n")

# =============================================================================
# CREATE APA TABLE FUNCTION
# =============================================================================

create_apa_table <- function(table_data, left_align = FALSE) {
  # Create table using tableGrob with APA styling
  table_grob <- tableGrob(
    table_data,
    rows = NULL,
    theme = ttheme_default(
      base_family = "Times New Roman",
      base_size = 12,
      core = list(
        fg_params = list(hjust = if(left_align) 0 else 0.5, x = if(left_align) 0.1 else 0.5),
        bg_params = list(fill = NA),
        padding = unit(c(6, 1), "mm")
      ),
      colhead = list(
        fg_params = list(fontface = "bold", hjust = if(left_align) 0 else 0.5, x = if(left_align) 0.1 else 0.5),
        bg_params = list(fill = NA),
        padding = unit(c(3, 1), "mm")
      ),
      rowhead = list(
        fg_params = list(hjust = if(left_align) 0 else 0.5, x = if(left_align) 0.1 else 0.5),
        bg_params = list(fill = NA),
        padding = unit(c(6, 1), "mm")
      )
    )
  )
  
  # Add borders
  table_grob <- gtable_add_grob(
    table_grob,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 2)
    ),
    t = 1, b = 1, l = 1, r = ncol(table_data)
  )
  
  # Adjust header height
  table_grob$heights[1] <- unit(8, "mm")
  
  # Add bottom border
  table_grob <- gtable_add_grob(
    table_grob,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 3, col = "black")
    ),
    t = nrow(table_data) + 1, b = nrow(table_data) + 1, l = 1, r = ncol(table_data)
  )
  
  return(table_grob)
}

# =============================================================================
# CREATE SEPARATE TABLES
# =============================================================================

# Create separate table data
create_ki_table <- function(significant_ki) {
  table_rows <- list()
  
  # Process KI group results
  for(var in unique(significant_ki$Variable)) {
    ki_results <- significant_ki[significant_ki$Variable == var, ]
    if(nrow(ki_results) > 0) {
      # Add variable name as header row
      table_rows[[length(table_rows) + 1]] <- data.frame(
        Vergleich = var,
        Differenz = "",
        p_Tukey = "",
        stringsAsFactors = FALSE
      )
      # Add individual comparisons
      for(i in 1:nrow(ki_results)) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
          Vergleich = paste("  ", ki_results$Vergleich[i]),
          Differenz = sprintf("%.3f", ki_results$Differenz[i]),
          p_Tukey = sprintf("%.3f", ki_results$p_Wert[i]),
          stringsAsFactors = FALSE
        )
      }
    } else {
      table_rows[[length(table_rows) + 1]] <- data.frame(
        Vergleich = var,
        Differenz = "",
        p_Tukey = "",
        stringsAsFactors = FALSE
      )
      table_rows[[length(table_rows) + 1]] <- data.frame(
        Vergleich = "  Keine signifikanten Unterschiede",
        Differenz = "",
        p_Tukey = "",
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine all rows
  if(length(table_rows) > 0) {
    ki_data <- do.call(rbind, table_rows)
  } else {
    ki_data <- data.frame(
      Vergleich = character(),
      Differenz = character(),
      p_Tukey = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(ki_data)
}

create_mensch_table <- function(significant_mensch) {
  table_rows <- list()
  
  # Process Mensch group results
  for(var in unique(significant_mensch$Variable)) {
    mensch_results <- significant_mensch[significant_mensch$Variable == var, ]
    if(nrow(mensch_results) > 0) {
      # Add variable name as header row
      table_rows[[length(table_rows) + 1]] <- data.frame(
        Vergleich = var,
        Differenz = "",
        p_Tukey = "",
        stringsAsFactors = FALSE
      )
      # Add individual comparisons
      for(i in 1:nrow(mensch_results)) {
        table_rows[[length(table_rows) + 1]] <- data.frame(
          Vergleich = paste("  ", mensch_results$Vergleich[i]),
          Differenz = sprintf("%.3f", mensch_results$Differenz[i]),
          p_Tukey = sprintf("%.3f", mensch_results$p_Wert[i]),
          stringsAsFactors = FALSE
        )
      }
    } else {
      table_rows[[length(table_rows) + 1]] <- data.frame(
        Vergleich = var,
        Differenz = "",
        p_Tukey = "",
        stringsAsFactors = FALSE
      )
      table_rows[[length(table_rows) + 1]] <- data.frame(
        Vergleich = "  Keine signifikanten Unterschiede",
        Differenz = "",
        p_Tukey = "",
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine all rows
  if(length(table_rows) > 0) {
    mensch_data <- do.call(rbind, table_rows)
  } else {
    mensch_data <- data.frame(
      Vergleich = character(),
      Differenz = character(),
      p_Tukey = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(mensch_data)
}

# Create separate table data
ki_data <- create_ki_table(significant_ki)
mensch_data <- create_mensch_table(significant_mensch)

# Create separate APA tables with left alignment
ki_apa_table <- create_apa_table(ki_data, left_align = TRUE)
mensch_apa_table <- create_apa_table(mensch_data, left_align = TRUE)

# Save the KI table
png("organized/images/clustering/posthoc_ki_table.png", 
    width = 10, height = 4, units = "in", res = 300, bg = "white")
grid.draw(ki_apa_table)
dev.off()

# Save the Mensch table
png("organized/images/clustering/posthoc_mensch_table.png", 
    width = 10, height = 5, units = "in", res = 300, bg = "white")
grid.draw(mensch_apa_table)
dev.off()

cat("✓ KI-Gruppe APA-Tabelle erstellt: posthoc_ki_table.png\n")
cat("✓ Mensch-Gruppe APA-Tabelle erstellt: posthoc_mensch_table.png\n")

cat("\n================================================================================\n")
cat("POST-HOC APA-TABELLEN ERFOLGREICH ERSTELLT\n")
cat("================================================================================\n")
cat("✓ posthoc_ki_table.png - KI-Gruppe Post-hoc Ergebnisse\n")
cat("✓ posthoc_mensch_table.png - Mensch-Gruppe Post-hoc Ergebnisse\n")
cat("================================================================================\n") 