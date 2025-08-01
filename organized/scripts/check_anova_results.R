# =============================================================================
# CHECK ANOVA RESULTS FOR KI GROUP
# =============================================================================
# Überprüft die ANOVA-Ergebnisse für die KI-Gruppe

library(dplyr)
library(car)

cat("================================================================================\n")
cat("ANOVA-ERGEBNISSE FÜR KI-GRUPPE\n")
cat("================================================================================\n")

# Load data
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data
data_processed <- data %>% filter(FINISHED == 1)
data_ki <- data_processed %>% filter(AB01 == 1)

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

cluster_data_ki_clean <- cluster_data_ki[complete.cases(cluster_data_ki), ]

# Perform clustering
set.seed(123)
kmeans_ki <- kmeans(cluster_data_ki_clean, centers = 3, nstart = 25)
cluster_data_ki_clean$cluster <- kmeans_ki$cluster

# Check ANOVA results for each variable
variables <- c("VS", "MN", "ID", "EA")
var_names <- c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
               "Identifikation", "Emotionale Ansprache")

cat("\nANOVA-ERGEBNISSE:\n")
cat("================\n")

for(i in 1:length(variables)) {
  var <- variables[i]
  var_name <- var_names[i]
  
  # ANOVA
  anova_result <- aov(as.formula(paste(var, "~ factor(cluster)")), data = cluster_data_ki_clean)
  anova_summary <- summary(anova_result)
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  
  cat(sprintf("\n%s (%s):\n", var_name, var))
  cat(sprintf("  p-Wert: %.6f\n", p_value))
  cat(sprintf("  Signifikant: %s\n", if(p_value < 0.05) "JA" else "NEIN"))
  
  if(p_value < 0.05) {
    cat("  → Post-hoc Tests werden durchgeführt\n")
  } else {
    cat("  → Keine Post-hoc Tests (nicht signifikant)\n")
  }
}

cat("\n================================================================================\n")
cat("ERKLÄRUNG:\n")
cat("================================================================================\n")
cat("Nur Variablen mit p < 0.05 in der ANOVA werden in der Post-hoc Tabelle angezeigt.\n")
cat("Vertrauen & Sympathie und Menschlichkeit & Natürlichkeit haben wahrscheinlich\n")
cat("p-Werte ≥ 0.05 und werden daher nicht in der Tabelle aufgeführt.\n")
cat("================================================================================\n") 