# =============================================================================
# CREATE MODERATION ANALYSIS VISUALIZATION
# =============================================================================
# Erstellt eine Visualisierung der Moderationsanalyse-Ergebnisse

library(dplyr)
library(ggplot2)

# =============================================================================
# LOAD DATA AND PERFORM ANALYSIS
# =============================================================================

cat("=== MODERATIONSANALYSE VISUALISIERUNG ===\n")

# Load data
cat("Lade Bereinigte Daten von WhatsApp Business.csv...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data
data_processed <- data %>% filter(FINISHED == 1)
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

# =============================================================================
# PERFORM MODERATION ANALYSIS
# =============================================================================

perform_moderation_analysis <- function(data_group, group_name, target_vars) {
  results <- list()
  
  for (var_name in target_vars) {
    # Skalenmittelwerte berechnen
    if (var_name == "VS") {
      var_cols <- sapply(c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08"), 
                         function(x) as.numeric(as.character(data_group[[x]])))
    } else if (var_name == "MN") {
      var_cols <- sapply(c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07"), 
                         function(x) as.numeric(as.character(data_group[[x]])))
    } else if (var_name == "ID") {
      var_cols <- sapply(c("ID01_01", "ID01_02", "ID01_03", "ID01_04"), 
                         function(x) as.numeric(as.character(data_group[[x]])))
    } else if (var_name == "EA") {
      var_cols <- sapply(c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05"), 
                         function(x) as.numeric(as.character(data_group[[x]])))
    }
    
    # Zielvariable berechnen
    data_group[[paste0(var_name, "_mean")]] <- rowMeans(var_cols, na.rm = TRUE)
    
    # Vorwissen (SE01 und SE02)
    se01_cols <- sapply(c("SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06"), 
                        function(x) as.numeric(as.character(data_group[[x]])))
    se02_cols <- sapply(c("SE02_01"), 
                        function(x) as.numeric(as.character(data_group[[x]])))
    
    # Mittelwerte für SE01 und SE02
    data_group$SE01_mean <- rowMeans(se01_cols, na.rm = TRUE)
    data_group$SE02_mean <- rowMeans(se02_cols, na.rm = TRUE)
    
    # Gesamtes Vorwissen (Mittelwert von SE01 und SE02)
    data_group$Vorwissen <- rowMeans(cbind(data_group$SE01_mean, data_group$SE02_mean), na.rm = TRUE)
    
    # Modell erstellen
    model_formula <- as.formula(paste0(var_name, "_mean ~ Vorwissen"))
    model <- lm(model_formula, data = data_group)
    
    # Ergebnisse speichern
    results[[var_name]] <- list(
      model = model,
      summary = summary(model),
      data = data_group
    )
  }
  
  return(results)
}

# Moderationsanalyse für beide Gruppen
target_vars <- c("VS", "MN", "ID", "EA")
var_names <- c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
               "Identifikation", "Emotionale Ansprache")

ki_results <- perform_moderation_analysis(data_ki, "KI", target_vars)
mensch_results <- perform_moderation_analysis(data_mensch, "MENSCH", target_vars)

# =============================================================================
# CREATE VISUALIZATION DATA
# =============================================================================

cat("Erstelle Visualisierungsdaten...\n")

# Create data frame for visualization
visualization_data <- data.frame(
  Variable = rep(var_names, 2),
  Gruppe = rep(c("KI-Gruppe", "Mensch-Gruppe"), each = length(target_vars)),
  Beta = c(
    sapply(ki_results, function(x) coef(x$model)[2]),
    sapply(mensch_results, function(x) coef(x$model)[2])
  ),
  P_Wert = c(
    sapply(ki_results, function(x) x$summary$coefficients[2,4]),
    sapply(mensch_results, function(x) x$summary$coefficients[2,4])
  ),
  R_Quadrat = c(
    sapply(ki_results, function(x) x$summary$r.squared),
    sapply(mensch_results, function(x) x$summary$r.squared)
  )
)

# Add significance indicator
visualization_data$Signifikant <- ifelse(visualization_data$P_Wert < 0.05, "Signifikant", "Nicht signifikant")
visualization_data$Signifikant_Level <- ifelse(visualization_data$P_Wert < 0.001, "p < 0.001",
                                              ifelse(visualization_data$P_Wert < 0.01, "p < 0.01",
                                                     ifelse(visualization_data$P_Wert < 0.05, "p < 0.05", 
                                                            sprintf("p = %.3f", visualization_data$P_Wert))))

# Round values for display
visualization_data$Beta_Rounded <- round(visualization_data$Beta, 3)
visualization_data$P_Rounded <- round(visualization_data$P_Wert, 3)
visualization_data$R2_Rounded <- round(visualization_data$R_Quadrat, 3)

cat("Visualisierungsdaten erstellt:\n")
print(visualization_data[, c("Variable", "Gruppe", "Beta_Rounded", "P_Rounded", "R2_Rounded", "Signifikant")])

# =============================================================================
# CREATE BAR CHART
# =============================================================================

cat("\nErstelle Balkendiagramm...\n")

# Create bar chart with manual x-positioning for labels
# Create manual x positions for labels
visualization_data$label_x <- as.numeric(factor(visualization_data$Variable)) + 
  ifelse(visualization_data$Gruppe == "KI-Gruppe", -0.2, 0.2)

moderation_plot <- ggplot(visualization_data, aes(x = Variable, y = Beta, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           color = "black", linewidth = 0.3, width = 0.6) +
  # Labels for positive values (above bars)
  geom_text(data = visualization_data[visualization_data$Beta >= 0, ], 
            aes(x = label_x, y = Beta, label = sprintf("β = %.3f\n%s", Beta_Rounded, Signifikant_Level)), 
            vjust = -0.5, hjust = 0.5, size = 3, family = "Times New Roman") +
  # Labels for negative values (below bars)
  geom_text(data = visualization_data[visualization_data$Beta < 0, ], 
            aes(x = label_x, y = Beta, label = sprintf("β = %.3f\n%s", Beta_Rounded, Signifikant_Level)), 
            vjust = 1.2, hjust = 0.5, size = 3, family = "Times New Roman") +
  scale_fill_manual(values = c("KI-Gruppe" = "#8DD3C8", "Mensch-Gruppe" = "#FFB6C1")) +
  labs(title = "Moderationsanalyse: Vorwissen-Effekte nach Gruppe",
       x = "Abhängige Variablen",
       y = "Beta-Koeffizient (Vorwissen-Effekt)",
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
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  ylim(min(visualization_data$Beta) - 0.1, max(visualization_data$Beta) + 0.1)

# Save the plot
ggsave("organized/images/clustering/moderation_analysis_visualization.png", 
       moderation_plot, width = 12, height = 8, dpi = 300, bg = "white")

cat("✓ Moderationsanalyse-Visualisierung erstellt: moderation_analysis_visualization.png\n")

# =============================================================================
# INTERPRETATION
# =============================================================================

cat("\n=== INTERPRETATION DER VISUALISIERUNG ===\n")

cat("\nWichtige Erkenntnisse:\n")
cat("1. Positive Beta-Werte: Vorwissen stärkt die Variable\n")
cat("2. Negative Beta-Werte: Vorwissen schwächt die Variable\n")
cat("3. Signifikante Effekte (p < 0.05) sind statistisch bedeutsam\n")

cat("\nSpezifische Ergebnisse:\n")

# Show significant effects
significant_effects <- visualization_data[visualization_data$P_Wert < 0.05, ]
if(nrow(significant_effects) > 0) {
  cat("\nSignifikante Vorwissen-Effekte:\n")
  for(i in 1:nrow(significant_effects)) {
    effect <- significant_effects[i, ]
    direction <- if(effect$Beta > 0) "stärkt" else "schwächt"
    cat("  ", effect$Variable, "(", effect$Gruppe, "): β =", effect$Beta_Rounded, 
        "→ Vorwissen", direction, "die Variable (", effect$Signifikant_Level, ")\n")
  }
} else {
  cat("\nKeine signifikanten Vorwissen-Effekte gefunden.\n")
}

cat("\n================================================================================\n")
cat("MODERATIONSANALYSE VISUALISIERUNG ERFOLGREICH ERSTELLT\n")
cat("================================================================================\n")
cat("✓ moderation_analysis_visualization.png - Balkendiagramm der Beta-Koeffizienten\n")
cat("================================================================================\n") 