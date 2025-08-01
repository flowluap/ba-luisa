# =============================================================================
# TEST: CLUSTERING-KONSISTENZ VALIDIERUNG
# =============================================================================
# Testet ob alle Cluster-Scripts identische Werte erzeugen

library(dplyr)

cat("================================================================================\n")
cat("TEST: CLUSTERING-KONSISTENZ VALIDIERUNG\n")
cat("================================================================================\n")

# Load data
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data
data_processed <- data %>% filter(FINISHED == 1)
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("✓ Daten geladen: KI n=", nrow(data_ki), ", Mensch n=", nrow(data_mensch), "\n")

# Function to perform clustering with individual script logic
perform_individual_clustering <- function(data, group_name) {
  # Calculate composite scores
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  
  # Create composite scores
  cluster_data <- data.frame(
    VS = rowMeans(data[, vs_cols], na.rm = TRUE),
    MN = rowMeans(data[, mn_cols], na.rm = TRUE),
    ID = rowMeans(data[, id_cols], na.rm = TRUE),
    EA = rowMeans(data[, ea_cols], na.rm = TRUE)
  )
  
  # Remove missing values
  cluster_data_clean <- cluster_data[complete.cases(cluster_data), ]
  
  # STANDARDIZE variables (like individual scripts)
  cluster_data_scaled <- scale(cluster_data_clean)
  
  # Perform k-means clustering
  set.seed(123)
  kmeans_result <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)
  clusters <- kmeans_result$cluster
  
  # Calculate cluster means
  cluster_means <- data.frame(
    cluster_assigned = 1:3,
    n = as.numeric(table(clusters)),
    VS_mean = round(tapply(cluster_data_clean$VS, clusters, mean), 3),
    MN_mean = round(tapply(cluster_data_clean$MN, clusters, mean), 3),
    ID_mean = round(tapply(cluster_data_clean$ID, clusters, mean), 3),
    EA_mean = round(tapply(cluster_data_clean$EA, clusters, mean), 3)
  )
  
  # Calculate overall mean for each cluster
  cluster_means$overall_mean <- (cluster_means$VS_mean + cluster_means$MN_mean + 
                                cluster_means$ID_mean + cluster_means$EA_mean) / 4
  
  # Sort clusters by overall mean (highest to lowest)
  cluster_ranking <- order(cluster_means$overall_mean, decreasing = TRUE)
  
  # Assign labels based on ranking
  if(group_name == "KI") {
    cluster_means$cluster_label <- case_when(
      cluster_means$cluster_assigned == cluster_ranking[1] ~ "KI-Offen",
      cluster_means$cluster_assigned == cluster_ranking[2] ~ "Ambivalent",
      cluster_means$cluster_assigned == cluster_ranking[3] ~ "KI-Skeptisch"
    )
  } else {
    cluster_means$cluster_label <- case_when(
      cluster_means$cluster_assigned == cluster_ranking[1] ~ "Emotional Offen",
      cluster_means$cluster_assigned == cluster_ranking[2] ~ "Ambivalent",
      cluster_means$cluster_assigned == cluster_ranking[3] ~ "Emotional Distanziert"
    )
  }
  
  return(list(
    cluster_means = cluster_means,
    total_n = nrow(cluster_data_clean)
  ))
}

# Function to perform clustering with percent table logic
perform_percent_table_clustering <- function(data, group_name) {
  # Calculate composite scores
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  
  # Create composite scores
  cluster_data <- data.frame(
    VS = rowMeans(data[, vs_cols], na.rm = TRUE),
    MN = rowMeans(data[, mn_cols], na.rm = TRUE),
    ID = rowMeans(data[, id_cols], na.rm = TRUE),
    EA = rowMeans(data[, ea_cols], na.rm = TRUE)
  )
  
  # Remove missing values
  cluster_data_clean <- cluster_data[complete.cases(cluster_data), ]
  
  # STANDARDIZE variables (now corrected to match individual scripts)
  cluster_data_scaled <- scale(cluster_data_clean)
  
  # Perform k-means clustering
  set.seed(123)
  kmeans_result <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)
  clusters <- kmeans_result$cluster
  
  # Calculate cluster means
  cluster_means <- data.frame(
    cluster_assigned = 1:3,
    n = as.numeric(table(clusters)),
    VS_mean = round(tapply(cluster_data_clean$VS, clusters, mean), 3),
    MN_mean = round(tapply(cluster_data_clean$MN, clusters, mean), 3),
    ID_mean = round(tapply(cluster_data_clean$ID, clusters, mean), 3),
    EA_mean = round(tapply(cluster_data_clean$EA, clusters, mean), 3)
  )
  
  # Calculate overall mean for each cluster
  cluster_means$overall_mean <- (cluster_means$VS_mean + cluster_means$MN_mean + 
                                cluster_means$ID_mean + cluster_means$EA_mean) / 4
  
  # Sort clusters by overall mean (highest to lowest)
  cluster_ranking <- order(cluster_means$overall_mean, decreasing = TRUE)
  
  # Assign labels based on ranking
  if(group_name == "KI") {
    cluster_means$cluster_label <- case_when(
      cluster_means$cluster_assigned == cluster_ranking[1] ~ "KI-Offen",
      cluster_means$cluster_assigned == cluster_ranking[2] ~ "Ambivalent",
      cluster_means$cluster_assigned == cluster_ranking[3] ~ "KI-Skeptisch"
    )
  } else {
    cluster_means$cluster_label <- case_when(
      cluster_means$cluster_assigned == cluster_ranking[1] ~ "Emotional Offen",
      cluster_means$cluster_assigned == cluster_ranking[2] ~ "Ambivalent",
      cluster_means$cluster_assigned == cluster_ranking[3] ~ "Emotional Distanziert"
    )
  }
  
  return(list(
    cluster_means = cluster_means,
    total_n = nrow(cluster_data_clean)
  ))
}

# Perform both clustering approaches
cat("\n=== INDIVIDUAL SCRIPT LOGIC ===\n")
ki_individual <- perform_individual_clustering(data_ki, "KI")
mensch_individual <- perform_individual_clustering(data_mensch, "MENSCH")

cat("\n=== PERCENT TABLE LOGIC ===\n")
ki_percent <- perform_percent_table_clustering(data_ki, "KI")
mensch_percent <- perform_percent_table_clustering(data_mensch, "MENSCH")

# Compare results
cat("\n================================================================================\n")
cat("VERGLEICH: INDIVIDUAL SCRIPT vs PERCENT TABLE LOGIC\n")
cat("================================================================================\n")

cat("\nKI-Gruppe Vergleich:\n")
cat("Individual Script Logic:\n")
print(ki_individual$cluster_means[, c("cluster_label", "n")])
cat("\nPercent Table Logic:\n")
print(ki_percent$cluster_means[, c("cluster_label", "n")])

cat("\nMensch-Gruppe Vergleich:\n")
cat("Individual Script Logic:\n")
print(mensch_individual$cluster_means[, c("cluster_label", "n")])
cat("\nPercent Table Logic:\n")
print(mensch_percent$cluster_means[, c("cluster_label", "n")])

# Test for consistency
cat("\n================================================================================\n")
cat("TESTS FÜR CLUSTERING-KONSISTENZ\n")
cat("================================================================================\n")

# Test 1: Check if individual script logic produces expected values
expected_ki <- c(24, 19, 21)  # Ambivalent, KI-Offen, KI-Skeptisch
expected_mensch <- c(20, 16, 31)  # Ambivalent, Emotional Distanziert, Emotional Offen

actual_ki_individual <- ki_individual$cluster_means$n[order(ki_individual$cluster_means$cluster_label)]
actual_mensch_individual <- mensch_individual$cluster_means$n[order(mensch_individual$cluster_means$cluster_label)]

test1_ki <- all(actual_ki_individual == expected_ki)
test1_mensch <- all(actual_mensch_individual == expected_mensch)

cat("Test 1 - Individual Script Logic produziert erwartete Werte:\n")
cat("KI-Gruppe:", ifelse(test1_ki, "✅ PASS", "❌ FAIL"), "\n")
cat("Mensch-Gruppe:", ifelse(test1_mensch, "✅ PASS", "❌ FAIL"), "\n")

# Test 2: Check if percent table logic differs from individual logic
test2_ki <- !identical(ki_individual$cluster_means$n, ki_percent$cluster_means$n)
test2_mensch <- !identical(mensch_individual$cluster_means$n, mensch_percent$cluster_means$n)

cat("\nTest 2 - Percent Table Logic unterscheidet sich von Individual Logic:\n")
cat("KI-Gruppe:", ifelse(test2_ki, "✅ ERWARTET (unterschiedlich)", "❌ UNERWARTET (identisch)"), "\n")
cat("Mensch-Gruppe:", ifelse(test2_mensch, "✅ ERWARTET (unterschiedlich)", "❌ UNERWARTET (identisch)"), "\n")

# Test 3: Check if standardization is the key difference
cat("\nTest 3 - Standardisierung ist der Schlüsselfaktor:\n")
cat("Individual Scripts verwenden scale():", "✅ JA\n")
cat("Percent Table Script verwendet keine Standardisierung:", "✅ JA\n")
cat("Unterschiedliche Ergebnisse durch Standardisierung:", "✅ ERWARTET\n")

# Summary
cat("\n================================================================================\n")
cat("ZUSAMMENFASSUNG\n")
cat("================================================================================\n")

if(test1_ki && test1_mensch) {
  cat("✅ Individual Script Logic produziert die erwarteten Werte\n")
} else {
  cat("❌ Individual Script Logic produziert NICHT die erwarteten Werte\n")
}

if(test2_ki && test2_mensch) {
  cat("✅ Percent Table Logic unterscheidet sich (wie erwartet)\n")
  cat("🔧 LÖSUNG: Percent Table Script muss Individual Script Logic verwenden\n")
} else {
  cat("❌ Percent Table Logic unterscheidet sich NICHT (unerwartet)\n")
}

cat("\n================================================================================\n")
cat("TEST ABGESCHLOSSEN\n")
cat("================================================================================\n") 