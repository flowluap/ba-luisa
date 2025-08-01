# =============================================================================
# VALIDATION: MODERATIONSANALYSE-TABELLE
# =============================================================================
# Umfangreiche Tests zur Validierung der Moderationsanalyse-Tabelle

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)

cat("================================================================================\n")
cat("VALIDATION: MODERATIONSANALYSE-TABELLE\n")
cat("================================================================================\n")

# =============================================================================
# 1. DATEN LADEN UND GRUNDLEGENDE VALIDIERUNG
# =============================================================================

cat("\n1. DATEN LADEN UND GRUNDLEGENDE VALIDIERUNG\n")
cat("============================================\n")

# Load data
cat("Lade Bereinigte Daten von WhatsApp Business.csv...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Basic data validation
cat("Grundlegende Datenvalidierung:\n")
cat("- Gesamtanzahl Zeilen:", nrow(data), "\n")
cat("- Anzahl Spalten:", ncol(data), "\n")
cat("- FINISHED = 1:", sum(data$FINISHED == 1, na.rm = TRUE), "\n")
cat("- AB01 = 1 (KI):", sum(data$AB01 == 1 & data$FINISHED == 1, na.rm = TRUE), "\n")
cat("- AB01 = 2 (Mensch):", sum(data$AB01 == 2 & data$FINISHED == 1, na.rm = TRUE), "\n")

# Filter data
data_processed <- data %>% filter(FINISHED == 1)
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("\nGefilterte Daten:\n")
cat("- Abgeschlossene Fälle:", nrow(data_processed), "\n")
cat("- KI-Gruppe:", nrow(data_ki), "\n")
cat("- Mensch-Gruppe:", nrow(data_mensch), "\n")

# =============================================================================
# 2. VARIABLEN-VALIDIERUNG
# =============================================================================

cat("\n2. VARIABLEN-VALIDIERUNG\n")
cat("========================\n")

# Check if all required variables exist
required_vars <- c(
  # VS variables
  "VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08",
  # MN variables
  "MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07",
  # ID variables
  "ID01_01", "ID01_02", "ID01_03", "ID01_04",
  # EA variables
  "EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05",
  # SE variables
  "SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06", "SE02_01"
)

missing_vars <- setdiff(required_vars, colnames(data))
if(length(missing_vars) > 0) {
  cat("❌ FEHLENDE VARIABLEN:", missing_vars, "\n")
} else {
  cat("✅ Alle erforderlichen Variablen vorhanden\n")
}

# =============================================================================
# 3. SKALEN-MITTELWERTE VALIDIERUNG
# =============================================================================

cat("\n3. SKALEN-MITTELWERTE VALIDIERUNG\n")
cat("==================================\n")

# Function to calculate scale means
calculate_scale_means <- function(data_group, group_name) {
  cat("\n---", group_name, "---\n")
  
  # VS scale
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  vs_data <- data_group[, vs_cols]
  vs_means <- rowMeans(vs_data, na.rm = TRUE)
  
  # MN scale
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  mn_data <- data_group[, mn_cols]
  mn_means <- rowMeans(mn_data, na.rm = TRUE)
  
  # ID scale
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  id_data <- data_group[, id_cols]
  id_means <- rowMeans(id_data, na.rm = TRUE)
  
  # EA scale
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  ea_data <- data_group[, ea_cols]
  ea_means <- rowMeans(ea_data, na.rm = TRUE)
  
  # SE scales
  se01_cols <- c("SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06")
  se01_data <- data_group[, se01_cols]
  se01_means <- rowMeans(se01_data, na.rm = TRUE)
  
  se02_cols <- c("SE02_01")
  se02_data <- data_group[, se02_cols, drop = FALSE]
  se02_means <- rowMeans(se02_data, na.rm = TRUE)
  
  # Overall Vorwissen
  vorwissen_means <- rowMeans(cbind(se01_means, se02_means), na.rm = TRUE)
  
  # Summary statistics
  cat("VS (Vertrauen & Sympathie):\n")
  cat("  Mittelwert:", round(mean(vs_means, na.rm = TRUE), 3), "\n")
  cat("  SD:", round(sd(vs_means, na.rm = TRUE), 3), "\n")
  cat("  Min:", round(min(vs_means, na.rm = TRUE), 3), "\n")
  cat("  Max:", round(max(vs_means, na.rm = TRUE), 3), "\n")
  
  cat("MN (Menschlichkeit & Natürlichkeit):\n")
  cat("  Mittelwert:", round(mean(mn_means, na.rm = TRUE), 3), "\n")
  cat("  SD:", round(sd(mn_means, na.rm = TRUE), 3), "\n")
  cat("  Min:", round(min(mn_means, na.rm = TRUE), 3), "\n")
  cat("  Max:", round(max(mn_means, na.rm = TRUE), 3), "\n")
  
  cat("ID (Identifikation):\n")
  cat("  Mittelwert:", round(mean(id_means, na.rm = TRUE), 3), "\n")
  cat("  SD:", round(sd(id_means, na.rm = TRUE), 3), "\n")
  cat("  Min:", round(min(id_means, na.rm = TRUE), 3), "\n")
  cat("  Max:", round(max(id_means, na.rm = TRUE), 3), "\n")
  
  cat("EA (Emotionale Ansprache):\n")
  cat("  Mittelwert:", round(mean(ea_means, na.rm = TRUE), 3), "\n")
  cat("  SD:", round(sd(ea_means, na.rm = TRUE), 3), "\n")
  cat("  Min:", round(min(ea_means, na.rm = TRUE), 3), "\n")
  cat("  Max:", round(max(ea_means, na.rm = TRUE), 3), "\n")
  
  cat("Vorwissen:\n")
  cat("  Mittelwert:", round(mean(vorwissen_means, na.rm = TRUE), 3), "\n")
  cat("  SD:", round(sd(vorwissen_means, na.rm = TRUE), 3), "\n")
  cat("  Min:", round(min(vorwissen_means, na.rm = TRUE), 3), "\n")
  cat("  Max:", round(max(vorwissen_means, na.rm = TRUE), 3), "\n")
  
  return(list(
    vs_means = vs_means,
    mn_means = mn_means,
    id_means = id_means,
    ea_means = ea_means,
    vorwissen_means = vorwissen_means
  ))
}

ki_scale_data <- calculate_scale_means(data_ki, "KI-Gruppe")
mensch_scale_data <- calculate_scale_means(data_mensch, "Mensch-Gruppe")

# =============================================================================
# 4. REGRESSIONSANALYSE VALIDIERUNG
# =============================================================================

cat("\n4. REGRESSIONSANALYSE VALIDIERUNG\n")
cat("==================================\n")

# Function to perform regression analysis
perform_regression_validation <- function(data_group, scale_data, group_name) {
  cat("\n---", group_name, "---\n")
  
  # Create data frame with all variables
  analysis_data <- data.frame(
    VS = scale_data$vs_means,
    MN = scale_data$mn_means,
    ID = scale_data$id_means,
    EA = scale_data$ea_means,
    Vorwissen = scale_data$vorwissen_means
  )
  
  # Remove rows with NA
  analysis_data <- analysis_data[complete.cases(analysis_data), ]
  cat("Anzahl gültige Fälle:", nrow(analysis_data), "\n")
  
  # Perform regressions
  variables <- c("VS", "MN", "ID", "EA")
  var_names <- c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
                 "Identifikation", "Emotionale Ansprache")
  
  results <- list()
  
  for(i in 1:length(variables)) {
    var <- variables[i]
    var_name <- var_names[i]
    
    cat("\n", var_name, "(", var, "):\n")
    
    # Regression
    model <- lm(as.formula(paste(var, "~ Vorwissen")), data = analysis_data)
    summary_model <- summary(model)
    
    # Extract coefficients
    beta <- coef(model)[2]
    p_value <- summary_model$coefficients[2, 4]
    r_squared <- summary_model$r.squared
    
    cat("  Beta:", round(beta, 6), "\n")
    cat("  p-Wert:", round(p_value, 6), "\n")
    cat("  R²:", round(r_squared, 6), "\n")
    cat("  Signifikant:", if(p_value < 0.05) "JA" else "NEIN", "\n")
    
    # Store results
    results[[var]] <- list(
      beta = beta,
      p_value = p_value,
      r_squared = r_squared,
      model = model,
      data = analysis_data
    )
  }
  
  return(results)
}

ki_regression_results <- perform_regression_validation(data_ki, ki_scale_data, "KI-Gruppe")
mensch_regression_results <- perform_regression_validation(data_mensch, mensch_scale_data, "Mensch-Gruppe")

# =============================================================================
# 5. VERGLEICH MIT MODERATIONSANALYSE-TABELLE
# =============================================================================

cat("\n5. VERGLEICH MIT MODERATIONSANALYSE-TABELLE\n")
cat("============================================\n")

# Expected values from moderation_analysis_table.png
expected_values <- data.frame(
  Variable = rep(c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
                   "Identifikation", "Emotionale Ansprache"), 2),
  Gruppe = rep(c("KI-Gruppe", "Mensch-Gruppe"), each = 4),
  Beta_Expected = c(0.052, 0.001, 0.123, -0.143, -0.001, -0.011, -0.050, 0.000),
  P_Expected = c(0.284, 0.985, 0.049, 0.013, 0.986, 0.797, 0.373, 0.994),
  R2_Expected = c(0.019, 0.000, 0.061, 0.095, 0.000, 0.001, 0.012, 0.000)
)

# Calculate actual values
actual_values <- data.frame(
  Variable = rep(c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
                   "Identifikation", "Emotionale Ansprache"), 2),
  Gruppe = rep(c("KI-Gruppe", "Mensch-Gruppe"), each = 4),
  Beta_Actual = c(
    ki_regression_results$VS$beta,
    ki_regression_results$MN$beta,
    ki_regression_results$ID$beta,
    ki_regression_results$EA$beta,
    mensch_regression_results$VS$beta,
    mensch_regression_results$MN$beta,
    mensch_regression_results$ID$beta,
    mensch_regression_results$EA$beta
  ),
  P_Actual = c(
    ki_regression_results$VS$p_value,
    ki_regression_results$MN$p_value,
    ki_regression_results$ID$p_value,
    ki_regression_results$EA$p_value,
    mensch_regression_results$VS$p_value,
    mensch_regression_results$MN$p_value,
    mensch_regression_results$ID$p_value,
    mensch_regression_results$EA$p_value
  ),
  R2_Actual = c(
    ki_regression_results$VS$r_squared,
    ki_regression_results$MN$r_squared,
    ki_regression_results$ID$r_squared,
    ki_regression_results$EA$r_squared,
    mensch_regression_results$VS$r_squared,
    mensch_regression_results$MN$r_squared,
    mensch_regression_results$ID$r_squared,
    mensch_regression_results$EA$r_squared
  )
)

# Merge expected and actual values
comparison_data <- merge(expected_values, actual_values, by = c("Variable", "Gruppe"))

# Calculate differences
comparison_data$Beta_Diff <- abs(comparison_data$Beta_Expected - comparison_data$Beta_Actual)
comparison_data$P_Diff <- abs(comparison_data$P_Expected - comparison_data$P_Actual)
comparison_data$R2_Diff <- abs(comparison_data$R2_Expected - comparison_data$R2_Actual)

# Check for significant differences
tolerance <- 0.001
comparison_data$Beta_Match <- comparison_data$Beta_Diff < tolerance
comparison_data$P_Match <- comparison_data$P_Diff < tolerance
comparison_data$R2_Match <- comparison_data$R2_Diff < tolerance

cat("\nVergleich mit Moderationsanalyse-Tabelle:\n")
print(comparison_data[, c("Variable", "Gruppe", "Beta_Expected", "Beta_Actual", "Beta_Diff", "Beta_Match")])

cat("\nZusammenfassung der Abweichungen:\n")
cat("Beta-Koeffizienten übereinstimmend:", sum(comparison_data$Beta_Match), "von", nrow(comparison_data), "\n")
cat("p-Werte übereinstimmend:", sum(comparison_data$P_Match), "von", nrow(comparison_data), "\n")
cat("R²-Werte übereinstimmend:", sum(comparison_data$R2_Match), "von", nrow(comparison_data), "\n")

# =============================================================================
# 6. KORRELATIONSANALYSE
# =============================================================================

cat("\n6. KORRELATIONSANALYSE\n")
cat("======================\n")

# Function to calculate correlations
calculate_correlations <- function(data_group, scale_data, group_name) {
  cat("\n---", group_name, "---\n")
  
  # Create data frame
  analysis_data <- data.frame(
    VS = scale_data$vs_means,
    MN = scale_data$mn_means,
    ID = scale_data$id_means,
    EA = scale_data$ea_means,
    Vorwissen = scale_data$vorwissen_means
  )
  
  # Remove NA
  analysis_data <- analysis_data[complete.cases(analysis_data), ]
  
  # Calculate correlations with Vorwissen
  variables <- c("VS", "MN", "ID", "EA")
  var_names <- c("Vertrauen & Sympathie", "Menschlichkeit & Natürlichkeit", 
                 "Identifikation", "Emotionale Ansprache")
  
  for(i in 1:length(variables)) {
    var <- variables[i]
    var_name <- var_names[i]
    
    cor_result <- cor.test(analysis_data[[var]], analysis_data$Vorwissen)
    
    cat(var_name, "mit Vorwissen:\n")
    cat("  r =", round(cor_result$estimate, 3), "\n")
    cat("  p =", round(cor_result$p.value, 3), "\n")
    cat("  Signifikant:", if(cor_result$p.value < 0.05) "JA" else "NEIN", "\n")
  }
}

calculate_correlations(data_ki, ki_scale_data, "KI-Gruppe")
calculate_correlations(data_mensch, mensch_scale_data, "Mensch-Gruppe")

# =============================================================================
# 7. DATENQUALITÄT UND AUSREISSER
# =============================================================================

cat("\n7. DATENQUALITÄT UND AUSREISSER\n")
cat("===============================\n")

# Function to check data quality
check_data_quality <- function(data_group, scale_data, group_name) {
  cat("\n---", group_name, "---\n")
  
  # Check for missing values
  vs_missing <- sum(is.na(scale_data$vs_means))
  mn_missing <- sum(is.na(scale_data$mn_means))
  id_missing <- sum(is.na(scale_data$id_means))
  ea_missing <- sum(is.na(scale_data$ea_means))
  vorwissen_missing <- sum(is.na(scale_data$vorwissen_means))
  
  cat("Fehlende Werte:\n")
  cat("  VS:", vs_missing, "\n")
  cat("  MN:", mn_missing, "\n")
  cat("  ID:", id_missing, "\n")
  cat("  EA:", ea_missing, "\n")
  cat("  Vorwissen:", vorwissen_missing, "\n")
  
  # Check for outliers (using IQR method)
  check_outliers <- function(x, var_name) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    outliers <- sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
    cat("  ", var_name, "Ausreißer:", outliers, "\n")
  }
  
  cat("Ausreißer (IQR-Methode):\n")
  check_outliers(scale_data$vs_means, "VS: ")
  check_outliers(scale_data$mn_means, "MN: ")
  check_outliers(scale_data$id_means, "ID: ")
  check_outliers(scale_data$ea_means, "EA: ")
  check_outliers(scale_data$vorwissen_means, "Vorwissen: ")
}

check_data_quality(data_ki, ki_scale_data, "KI-Gruppe")
check_data_quality(data_mensch, mensch_scale_data, "Mensch-Gruppe")

# =============================================================================
# 8. ZUSAMMENFASSUNG UND BEWERTUNG
# =============================================================================

cat("\n8. ZUSAMMENFASSUNG UND BEWERTUNG\n")
cat("==================================\n")

cat("\nVALIDIERUNGSERGEBNISSE:\n")
cat("======================\n")

# Overall assessment
beta_matches <- sum(comparison_data$Beta_Match)
p_matches <- sum(comparison_data$P_Match)
r2_matches <- sum(comparison_data$R2_Match)
total_tests <- nrow(comparison_data)

cat("✅ Übereinstimmung mit Moderationsanalyse-Tabelle:\n")
cat("  Beta-Koeffizienten:", beta_matches, "/", total_tests, "(", round(beta_matches/total_tests*100, 1), "%)\n")
cat("  p-Werte:", p_matches, "/", total_tests, "(", round(p_matches/total_tests*100, 1), "%)\n")
cat("  R²-Werte:", r2_matches, "/", total_tests, "(", round(r2_matches/total_tests*100, 1), "%)\n")

if(beta_matches == total_tests && p_matches == total_tests && r2_matches == total_tests) {
  cat("\n🎉 PERFEKTE ÜBEREINSTIMMUNG! Die Moderationsanalyse-Tabelle ist korrekt.\n")
} else {
  cat("\n⚠️  ABWEICHUNGEN GEFUNDEN! Die Moderationsanalyse-Tabelle weicht von den berechneten Werten ab.\n")
  
  # Show specific differences
  cat("\nSpezifische Abweichungen:\n")
  for(i in 1:nrow(comparison_data)) {
    if(!comparison_data$Beta_Match[i] || !comparison_data$P_Match[i] || !comparison_data$R2_Match[i]) {
      cat("  ", comparison_data$Variable[i], "(", comparison_data$Gruppe[i], "):\n")
      if(!comparison_data$Beta_Match[i]) {
        cat("    Beta: Erwartet", comparison_data$Beta_Expected[i], ", Berechnet", comparison_data$Beta_Actual[i], "\n")
      }
      if(!comparison_data$P_Match[i]) {
        cat("    p: Erwartet", comparison_data$P_Expected[i], ", Berechnet", comparison_data$P_Actual[i], "\n")
      }
      if(!comparison_data$R2_Match[i]) {
        cat("    R²: Erwartet", comparison_data$R2_Expected[i], ", Berechnet", comparison_data$R2_Actual[i], "\n")
      }
    }
  }
}

cat("\n================================================================================\n")
cat("VALIDIERUNG ABGESCHLOSSEN\n")
cat("================================================================================\n") 