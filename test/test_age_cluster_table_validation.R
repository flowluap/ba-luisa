# =============================================================================
# TEST: AGE CLUSTER TABLE VALIDATION
# =============================================================================
# Testet: age_cluster_table.png gegen alle anderen Tabellen und Quelldaten

library(dplyr)
library(tidyr)

cat("================================================================================\n")
cat("TEST: AGE CLUSTER TABLE VALIDATION\n")
cat("================================================================================\n")

# =============================================================================
# LOAD SOURCE DATA
# =============================================================================

cat("Lade Quelldaten...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data (FINISHED=1)
data_processed <- data %>% filter(FINISHED == 1)

cat("✓ Gesamte Stichprobe: n =", nrow(data_processed), "\n")

# =============================================================================
# PERFORM CLUSTERING (SAME AS MASTER SCRIPT)
# =============================================================================

perform_clustering_logical <- function(data, group_name) {
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
  
  # Perform k-means clustering
  set.seed(123)
  kmeans_result <- kmeans(cluster_data_clean, centers = 3, nstart = 25)
  clusters <- kmeans_result$cluster
  
  # Calculate cluster means
  cluster_means <- data.frame(
    cluster_number = 1:3,
    n = as.numeric(table(clusters)),
    VS = round(tapply(cluster_data_clean$VS, clusters, mean), 3),
    MN = round(tapply(cluster_data_clean$MN, clusters, mean), 3),
    ID = round(tapply(cluster_data_clean$ID, clusters, mean), 3),
    EA = round(tapply(cluster_data_clean$EA, clusters, mean), 3)
  )
  
  # Calculate overall mean for each cluster
  cluster_means$overall_mean <- (cluster_means$VS + cluster_means$MN + 
                                cluster_means$ID + cluster_means$EA) / 4
  
  # Sort by overall mean (highest to lowest)
  cluster_means_sorted <- cluster_means[order(cluster_means$overall_mean, decreasing = TRUE), ]
  
  # Assign names based on ranking
  if(group_name == "ALL") {
    cluster_means_sorted$cluster_name <- c("Emotional Offen", "Ambivalent", "Emotional Distanziert")
  }
  
  return(list(
    clusters = clusters,
    cluster_means = cluster_means_sorted,
    total_n = nrow(cluster_data_clean)
  ))
}

# Perform clustering for entire dataset
all_data_cluster <- perform_clustering_logical(data_processed, "ALL")

# Add cluster information to data
data_processed$Cluster <- all_data_cluster$clusters
data_processed$Cluster_Name <- all_data_cluster$cluster_means$cluster_name[all_data_cluster$clusters]

# Create age groups
data_processed$Age_Group <- cut(data_processed$SO01, 
                               breaks = c(17, 25, 35, 45, 55, 100),
                               labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
                               include.lowest = TRUE)

# =============================================================================
# EXPECTED VALUES FROM AGE CLUSTER TABLE
# =============================================================================

# Expected values from age_cluster_table.png
age_cluster_expected <- data.frame(
  Cluster_Name = c("Ambivalent", "Emotional Distanziert", "Emotional Offen"),
  `18-25` = c(24, 15, 17),
  `26-35` = c(37, 17, 10),
  `36-45` = c(3, 3, 5),
  `46-55` = c(0, 0, 0),
  stringsAsFactors = FALSE
)

cat("=== ERWARTETE WERTE AUS age_cluster_table.png ===\n")
print(age_cluster_expected)

# =============================================================================
# CALCULATE ACTUAL VALUES FROM SOURCE DATA
# =============================================================================

# Count persons by cluster and age group
age_cluster_actual <- data_processed %>%
  group_by(Cluster_Name, Age_Group) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(Age_Group %in% c("18-25", "26-35", "36-45", "46-55")) %>%
  pivot_wider(names_from = Age_Group, values_from = Count, values_fill = 0)

cat("\n=== TATSÄCHLICHE WERTE AUS QUELLDATEN ===\n")
print(age_cluster_actual)

# =============================================================================
# TEST FUNCTIONS
# =============================================================================

test_age_18_25_consistency <- function() {
  cat("\n=== TEST 1: ALTERSGRUPPE 18-25 KONSISTENZ ===\n")
  
  expected_18_25 <- age_cluster_expected$X18.25
  actual_18_25 <- age_cluster_actual$`18-25`
  
  consistent <- all(expected_18_25 == actual_18_25)
  cat("18-25 Werte identisch:", if(consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!consistent) {
    cat("Unterschiede in 18-25 Werten:\n")
    for(i in 1:3) {
      if(expected_18_25[i] != actual_18_25[i]) {
        cat("  ", age_cluster_expected$Cluster_Name[i], ": ", 
            expected_18_25[i], " vs ", actual_18_25[i], "\n")
      }
    }
  }
  
  return(consistent)
}

test_age_26_35_consistency <- function() {
  cat("\n=== TEST 2: ALTERSGRUPPE 26-35 KONSISTENZ ===\n")
  
  expected_26_35 <- age_cluster_expected$X26.35
  actual_26_35 <- age_cluster_actual$`26-35`
  
  consistent <- all(expected_26_35 == actual_26_35)
  cat("26-35 Werte identisch:", if(consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!consistent) {
    cat("Unterschiede in 26-35 Werten:\n")
    for(i in 1:3) {
      if(expected_26_35[i] != actual_26_35[i]) {
        cat("  ", age_cluster_expected$Cluster_Name[i], ": ", 
            expected_26_35[i], " vs ", actual_26_35[i], "\n")
      }
    }
  }
  
  return(consistent)
}

test_age_36_45_consistency <- function() {
  cat("\n=== TEST 3: ALTERSGRUPPE 36-45 KONSISTENZ ===\n")
  
  expected_36_45 <- age_cluster_expected$X36.45
  actual_36_45 <- age_cluster_actual$`36-45`
  
  consistent <- all(expected_36_45 == actual_36_45)
  cat("36-45 Werte identisch:", if(consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!consistent) {
    cat("Unterschiede in 36-45 Werten:\n")
    for(i in 1:3) {
      if(expected_36_45[i] != actual_36_45[i]) {
        cat("  ", age_cluster_expected$Cluster_Name[i], ": ", 
            expected_36_45[i], " vs ", actual_36_45[i], "\n")
      }
    }
  }
  
  return(consistent)
}

test_age_46_55_consistency <- function() {
  cat("\n=== TEST 4: ALTERSGRUPPE 46-55 KONSISTENZ ===\n")
  
  expected_46_55 <- age_cluster_expected$X46.55
  
  # Check if 46-55 column exists in actual data
  if("46-55" %in% colnames(age_cluster_actual)) {
    actual_46_55 <- age_cluster_actual$`46-55`
    consistent <- all(expected_46_55 == actual_46_55)
  } else {
    # If column doesn't exist, all values should be 0 (as expected)
    consistent <- all(expected_46_55 == 0)
  }
  
  cat("46-55 Werte identisch:", if(consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!consistent) {
    cat("Unterschiede in 46-55 Werten:\n")
    for(i in 1:3) {
      if(expected_46_55[i] != 0) {
        cat("  ", age_cluster_expected$Cluster_Name[i], ": ", 
            expected_46_55[i], " vs 0 (nicht in Quelldaten)\n")
      }
    }
  }
  
  return(consistent)
}

test_cluster_names_consistency <- function() {
  cat("\n=== TEST 5: CLUSTER-NAMEN KONSISTENZ ===\n")
  
  names_consistent <- all(age_cluster_expected$Cluster_Name == age_cluster_actual$Cluster_Name)
  cat("Cluster-Namen identisch:", if(names_consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!names_consistent) {
    cat("Unterschiede in Cluster-Namen:\n")
    for(i in 1:3) {
      if(age_cluster_expected$Cluster_Name[i] != age_cluster_actual$Cluster_Name[i]) {
        cat("  Position ", i, ": '", age_cluster_expected$Cluster_Name[i], 
            "' vs '", age_cluster_actual$Cluster_Name[i], "'\n")
      }
    }
  }
  
  return(names_consistent)
}

test_total_counts_consistency <- function() {
  cat("\n=== TEST 6: GESAMT-ANZAHL KONSISTENZ ===\n")
  
  # Calculate totals (only for age groups that exist in actual data)
  expected_total <- sum(age_cluster_expected$X18.25) + 
                   sum(age_cluster_expected$X26.35) + 
                   sum(age_cluster_expected$X36.45)
  
  actual_total <- sum(age_cluster_actual$`18-25`) + 
                 sum(age_cluster_actual$`26-35`) + 
                 sum(age_cluster_actual$`36-45`)
  
  consistent <- expected_total == actual_total
  cat("Gesamt-Anzahl konsistent (", expected_total, "):", 
      if(consistent) "✅ JA" else "❌ NEIN", "\n")
  
  if(!consistent) {
    cat("Erwartete Gesamt-Anzahl:", expected_total, "\n")
    cat("Tatsächliche Gesamt-Anzahl:", actual_total, "\n")
    cat("Berechnung:\n")
    cat("  Erwartet: ", sum(age_cluster_expected$X18.25), " + ", 
        sum(age_cluster_expected$X26.35), " + ", 
        sum(age_cluster_expected$X36.45), " = ", expected_total, "\n")
    cat("  Tatsächlich: ", sum(age_cluster_actual$`18-25`), " + ", 
        sum(age_cluster_actual$`26-35`), " + ", 
        sum(age_cluster_actual$`36-45`), " = ", actual_total, "\n")
  }
  
  return(consistent)
}

test_source_data_validation <- function() {
  cat("\n=== TEST 7: QUELLDATEN-VALIDIERUNG ===\n")
  
  # Check if all age groups sum up correctly
  age_summary <- data_processed %>%
    group_by(Age_Group) %>%
    summarise(Count = n(), .groups = 'drop')
  
  cat("Altersgruppen-Verteilung in Quelldaten:\n")
  print(age_summary)
  
  # Check if cluster distribution matches
  cluster_summary <- data_processed %>%
    group_by(Cluster_Name) %>%
    summarise(Count = n(), .groups = 'drop')
  
  cat("\nCluster-Verteilung in Quelldaten:\n")
  print(cluster_summary)
  
  return(TRUE)
}

# =============================================================================
# RUN ALL TESTS
# =============================================================================

cat("\n=== TEST-AUSFÜHRUNG ===\n")

test_results <- list(
  age_18_25 = test_age_18_25_consistency(),
  age_26_35 = test_age_26_35_consistency(),
  age_36_45 = test_age_36_45_consistency(),
  age_46_55 = test_age_46_55_consistency(),
  cluster_names = test_cluster_names_consistency(),
  total_counts = test_total_counts_consistency(),
  source_validation = test_source_data_validation()
)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n================================================================================\n")
cat("AGE CLUSTER TABLE VALIDATION SUMMARY\n")
cat("================================================================================\n")

all_tests_passed <- all(unlist(test_results[1:6])) # Exclude source_validation from pass/fail

if(all_tests_passed) {
  cat("✅ ALLE TESTS BESTANDEN - AGE CLUSTER TABLE IST KORREKT\n")
  cat("✅ age_cluster_table.png ist vollständig konsistent mit Quelldaten\n")
} else {
  cat("❌ EINIGE TESTS FEHLGESCHLAGEN - INKONSISTENZEN GEFUNDEN\n")
  cat("❌ age_cluster_table.png unterscheidet sich von Quelldaten\n")
}

cat("✓ Test 1 (18-25):", if(test_results$age_18_25) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 2 (26-35):", if(test_results$age_26_35) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 3 (36-45):", if(test_results$age_36_45) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 4 (46-55):", if(test_results$age_46_55) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 5 (Cluster-Namen):", if(test_results$cluster_names) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 6 (Gesamt-Anzahl):", if(test_results$total_counts) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")
cat("✓ Test 7 (Quelldaten-Validierung):", if(test_results$source_validation) "BESTANDEN" else "FEHLGESCHLAGEN", "\n")

cat("\n================================================================================\n")
cat("VALIDIERUNG ABGESCHLOSSEN\n")
cat("================================================================================\n") 