# =============================================================================
# AGE GROUPS DATA VALIDATION
# =============================================================================
# Validiert die Daten der neuen Altersgruppen-Visualisierungen gegen
# die ursprünglichen Daten und andere Tabellen

library(dplyr)
library(ggplot2)

# =============================================================================
# LOAD ORIGINAL DATA
# =============================================================================

cat("================================================================================\n")
cat("AGE GROUPS DATA VALIDATION\n")
cat("================================================================================\n")

cat("Lade ursprüngliche Daten...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data (FINISHED=1)
data_processed <- data %>% filter(FINISHED == 1)

# Separate KI and Mensch groups
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("✓ Gesamte Stichprobe: n =", nrow(data_processed), "\n")
cat("✓ KI-Gruppe: n =", nrow(data_ki), "\n")
cat("✓ Mensch-Gruppe: n =", nrow(data_mensch), "\n")

# =============================================================================
# VALIDATE AGE GROUPS
# =============================================================================

cat("\n=== VALIDIERUNG ALTERSGRUPPEN ===\n")

# Create age groups for validation
data_ki$Age_Group <- cut(data_ki$SO01, 
                        breaks = c(17, 25, 35, 45, 55, 100),
                        labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
                        include.lowest = TRUE)

data_mensch$Age_Group <- cut(data_mensch$SO01, 
                            breaks = c(17, 25, 35, 45, 55, 100),
                            labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
                            include.lowest = TRUE)

# Count age groups for validation
age_counts_ki <- table(data_ki$Age_Group)
age_counts_mensch <- table(data_mensch$Age_Group)

cat("KI-Gruppe Altersgruppen (alle):\n")
print(age_counts_ki)
cat("\nMensch-Gruppe Altersgruppen (alle):\n")
print(age_counts_mensch)

# =============================================================================
# PERFORM CLUSTERING FOR VALIDATION
# =============================================================================

cat("\n=== CLUSTERING VALIDIERUNG ===\n")

# Calculate composite scores
mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")

# KI Group clustering
cluster_data_ki <- data.frame(
  VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
)

cluster_data_ki_clean <- cluster_data_ki[complete.cases(cluster_data_ki), ]
data_ki_clean <- data_ki[complete.cases(cluster_data_ki), ]

set.seed(123)
kmeans_result_ki <- kmeans(cluster_data_ki_clean, centers = 3, nstart = 25)
clusters_ki <- kmeans_result_ki$cluster

cluster_means_ki <- data.frame(
  cluster_number = 1:3,
  n = as.numeric(table(clusters_ki)),
  VS = round(tapply(cluster_data_ki_clean$VS, clusters_ki, mean), 3),
  MN = round(tapply(cluster_data_ki_clean$MN, clusters_ki, mean), 3),
  ID = round(tapply(cluster_data_ki_clean$ID, clusters_ki, mean), 3),
  EA = round(tapply(cluster_data_ki_clean$EA, clusters_ki, mean), 3)
)

cluster_means_ki$overall_mean <- (cluster_means_ki$VS + cluster_means_ki$MN + 
                                 cluster_means_ki$ID + cluster_means_ki$EA) / 4

cluster_means_ki_sorted <- cluster_means_ki[order(cluster_means_ki$overall_mean, decreasing = TRUE), ]
cluster_means_ki_sorted$cluster_name <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")

data_ki_clean$Cluster <- clusters_ki
data_ki_clean$Cluster_Name <- cluster_means_ki_sorted$cluster_name[clusters_ki]

# Mensch Group clustering
cluster_data_mensch <- data.frame(
  VS = rowMeans(data_mensch[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_mensch[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_mensch[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_mensch[, ea_cols], na.rm = TRUE)
)

cluster_data_mensch_clean <- cluster_data_mensch[complete.cases(cluster_data_mensch), ]
data_mensch_clean <- data_mensch[complete.cases(cluster_data_mensch), ]

set.seed(123)
kmeans_result_mensch <- kmeans(cluster_data_mensch_clean, centers = 3, nstart = 25)
clusters_mensch <- kmeans_result_mensch$cluster

cluster_means_mensch <- data.frame(
  cluster_number = 1:3,
  n = as.numeric(table(clusters_mensch)),
  VS = round(tapply(cluster_data_mensch_clean$VS, clusters_mensch, mean), 3),
  MN = round(tapply(cluster_data_mensch_clean$MN, clusters_mensch, mean), 3),
  ID = round(tapply(cluster_data_mensch_clean$ID, clusters_mensch, mean), 3),
  EA = round(tapply(cluster_data_mensch_clean$EA, clusters_mensch, mean), 3)
)

cluster_means_mensch$overall_mean <- (cluster_means_mensch$VS + cluster_means_mensch$MN + 
                                     cluster_means_mensch$ID + cluster_means_mensch$EA) / 4

cluster_means_mensch_sorted <- cluster_means_mensch[order(cluster_means_mensch$overall_mean, decreasing = TRUE), ]
cluster_means_mensch_sorted$cluster_name <- c("Emotional Offen", "Ambivalent", "Emotional Distanziert")

data_mensch_clean$Cluster <- clusters_mensch
data_mensch_clean$Cluster_Name <- cluster_means_mensch_sorted$cluster_name[clusters_mensch]

cat("KI-Gruppe Cluster-Validierung:\n")
print(cluster_means_ki_sorted[, c("cluster_name", "n", "VS", "MN", "ID", "EA")])
cat("\nMensch-Gruppe Cluster-Validierung:\n")
print(cluster_means_mensch_sorted[, c("cluster_name", "n", "VS", "MN", "ID", "EA")])

# =============================================================================
# VALIDATE AGE-CLUSTER COMBINATIONS
# =============================================================================

cat("\n=== VALIDIERUNG ALTERSGRUPPEN-CLUSTER KOMBINATIONEN ===\n")

# Create age groups for clean data
data_ki_clean$Age_Group <- cut(data_ki_clean$SO01, 
                              breaks = c(17, 25, 35, 45, 55, 100),
                              labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
                              include.lowest = TRUE)

data_mensch_clean$Age_Group <- cut(data_mensch_clean$SO01, 
                                  breaks = c(17, 25, 35, 45, 55, 100),
                                  labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
                                  include.lowest = TRUE)

# Count age-cluster combinations
age_cluster_ki <- data_ki_clean %>%
  group_by(Cluster_Name, Age_Group) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(Age_Group %in% c("18-25", "26-35", "36-45", "46-55"))

age_cluster_mensch <- data_mensch_clean %>%
  group_by(Cluster_Name, Age_Group) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(Age_Group %in% c("18-25", "26-35", "36-45", "46-55"))

cat("KI-Gruppe Altersgruppen nach Cluster (validiert):\n")
print(age_cluster_ki)
cat("\nMensch-Gruppe Altersgruppen nach Cluster (validiert):\n")
print(age_cluster_mensch)

# =============================================================================
# COMPARE WITH EXPECTED VALUES FROM SCRIPT OUTPUT
# =============================================================================

cat("\n=== VERGLEICH MIT ERWARTETEN WERTEN ===\n")

# Expected values from script output
expected_ki <- data.frame(
  Cluster_Name = c("Ambivalent", "Ambivalent", "KI-Offen", "KI-Offen", "KI-Offen", 
                   "KI-Skeptisch", "KI-Skeptisch", "KI-Skeptisch"),
  Age_Group = c("18-25", "26-35", "18-25", "26-35", "36-45", "18-25", "26-35", "36-45"),
  Count = c(7, 11, 7, 14, 2, 10, 12, 1)
)

expected_mensch <- data.frame(
  Cluster_Name = c("Ambivalent", "Ambivalent", "Ambivalent", "Emotional Distanziert", 
                   "Emotional Distanziert", "Emotional Distanziert", "Emotional Offen", 
                   "Emotional Offen", "Emotional Offen"),
  Age_Group = c("18-25", "26-35", "36-45", "18-25", "26-35", "36-45", "18-25", "26-35", "36-45"),
  Count = c(10, 11, 1, 17, 7, 4, 5, 9, 3)
)

cat("Erwartete KI-Werte (aus Script):\n")
print(expected_ki)
cat("\nTatsächliche KI-Werte (validiert):\n")
print(age_cluster_ki)

cat("\nErwartete Mensch-Werte (aus Script):\n")
print(expected_mensch)
cat("\nTatsächliche Mensch-Werte (validiert):\n")
print(age_cluster_mensch)

# =============================================================================
# VALIDATION CHECKS
# =============================================================================

cat("\n=== VALIDIERUNGS-CHECKS ===\n")

# Check 1: Total counts match
total_ki_expected <- sum(expected_ki$Count)
total_ki_actual <- sum(age_cluster_ki$Count)
total_mensch_expected <- sum(expected_mensch$Count)
total_mensch_actual <- sum(age_cluster_mensch$Count)

cat("KI-Gruppe Gesamtanzahl:\n")
cat("  Erwartet:", total_ki_expected, "\n")
cat("  Tatsächlich:", total_ki_actual, "\n")
cat("  ✅ Übereinstimmung:", total_ki_expected == total_ki_actual, "\n")

cat("Mensch-Gruppe Gesamtanzahl:\n")
cat("  Erwartet:", total_mensch_expected, "\n")
cat("  Tatsächlich:", total_mensch_actual, "\n")
cat("  ✅ Übereinstimmung:", total_mensch_expected == total_mensch_actual, "\n")

# Check 2: Individual values match
ki_match <- all.equal(expected_ki$Count, age_cluster_ki$Count)
mensch_match <- all.equal(expected_mensch$Count, age_cluster_mensch$Count)

cat("\nKI-Gruppe Einzelwerte:\n")
cat("  ✅ Übereinstimmung:", ki_match, "\n")

cat("Mensch-Gruppe Einzelwerte:\n")
cat("  ✅ Übereinstimmung:", mensch_match, "\n")

# Check 3: Cluster names match
ki_clusters_expected <- unique(expected_ki$Cluster_Name)
ki_clusters_actual <- unique(age_cluster_ki$Cluster_Name)
mensch_clusters_expected <- unique(expected_mensch$Cluster_Name)
mensch_clusters_actual <- unique(age_cluster_mensch$Cluster_Name)

cat("\nKI-Gruppe Cluster-Namen:\n")
cat("  Erwartet:", paste(ki_clusters_expected, collapse = ", "), "\n")
cat("  Tatsächlich:", paste(ki_clusters_actual, collapse = ", "), "\n")
cat("  ✅ Übereinstimmung:", all(ki_clusters_expected == ki_clusters_actual), "\n")

cat("Mensch-Gruppe Cluster-Namen:\n")
cat("  Erwartet:", paste(mensch_clusters_expected, collapse = ", "), "\n")
cat("  Tatsächlich:", paste(mensch_clusters_actual, collapse = ", "), "\n")
cat("  ✅ Übereinstimmung:", all(mensch_clusters_expected == mensch_clusters_actual), "\n")

# =============================================================================
# COMPARE WITH OTHER TABLES
# =============================================================================

cat("\n=== VERGLEICH MIT ANDEREN TABELLEN ===\n")

# Compare with ki_specific_apa.png values
ki_apa_expected <- data.frame(
  Cluster_Name = c("KI-Offen", "Ambivalent", "KI-Skeptisch"),
  n = c(18, 23, 23)
)

ki_apa_actual <- data.frame(
  Cluster_Name = cluster_means_ki_sorted$cluster_name,
  n = cluster_means_ki_sorted$n
)

cat("KI-Gruppe Vergleich mit ki_specific_apa.png:\n")
cat("  Erwartet (APA):", paste(ki_apa_expected$n, collapse = ", "), "\n")
cat("  Tatsächlich:", paste(ki_apa_actual$n, collapse = ", "), "\n")
cat("  ✅ Übereinstimmung:", all(ki_apa_expected$n == ki_apa_actual$n), "\n")

# Compare with human_specific_apa.png values
mensch_apa_expected <- data.frame(
  Cluster_Name = c("Emotional Offen", "Ambivalent", "Emotional Distanziert"),
  n = c(17, 22, 28)
)

mensch_apa_actual <- data.frame(
  Cluster_Name = cluster_means_mensch_sorted$cluster_name,
  n = cluster_means_mensch_sorted$n
)

cat("Mensch-Gruppe Vergleich mit human_specific_apa.png:\n")
cat("  Erwartet (APA):", paste(mensch_apa_expected$n, collapse = ", "), "\n")
cat("  Tatsächlich:", paste(mensch_apa_actual$n, collapse = ", "), "\n")
cat("  ✅ Übereinstimmung:", all(mensch_apa_expected$n == mensch_apa_actual$n), "\n")

# =============================================================================
# FINAL VALIDATION SUMMARY
# =============================================================================

cat("\n================================================================================\n")
cat("VALIDIERUNGS-ZUSAMMENFASSUNG\n")
cat("================================================================================\n")

all_checks_passed <- (total_ki_expected == total_ki_actual) &&
                    (total_mensch_expected == total_mensch_actual) &&
                    (ki_match == TRUE) &&
                    (mensch_match == TRUE) &&
                    all(ki_clusters_expected == ki_clusters_actual) &&
                    all(mensch_clusters_expected == mensch_clusters_actual) &&
                    all(ki_apa_expected$n == ki_apa_actual$n) &&
                    all(mensch_apa_expected$n == mensch_apa_actual$n)

if(all_checks_passed) {
  cat("✅ ALLE VALIDIERUNGS-CHECKS BESTANDEN\n")
  cat("✅ Daten der neuen Altersgruppen-Visualisierungen sind korrekt\n")
  cat("✅ Übereinstimmung mit ursprünglichen Daten bestätigt\n")
  cat("✅ Übereinstimmung mit APA-Tabellen bestätigt\n")
} else {
  cat("❌ EINIGE VALIDIERUNGS-CHECKS FEHLGESCHLAGEN\n")
  cat("❌ Daten müssen überprüft werden\n")
}

cat("\n=== DETAILS ===\n")
cat("✓ KI-Gruppe Gesamtanzahl:", total_ki_actual, "/", total_ki_expected, "\n")
cat("✓ Mensch-Gruppe Gesamtanzahl:", total_mensch_actual, "/", total_mensch_expected, "\n")
cat("✓ KI-Gruppe Einzelwerte:", ifelse(ki_match == TRUE, "KORREKT", "FEHLER"), "\n")
cat("✓ Mensch-Gruppe Einzelwerte:", ifelse(mensch_match == TRUE, "KORREKT", "FEHLER"), "\n")
cat("✓ KI-Gruppe Cluster-Namen:", ifelse(all(ki_clusters_expected == ki_clusters_actual), "KORREKT", "FEHLER"), "\n")
cat("✓ Mensch-Gruppe Cluster-Namen:", ifelse(all(mensch_clusters_expected == mensch_clusters_actual), "KORREKT", "FEHLER"), "\n")
cat("✓ KI-Gruppe APA-Vergleich:", ifelse(all(ki_apa_expected$n == ki_apa_actual$n), "KORREKT", "FEHLER"), "\n")
cat("✓ Mensch-Gruppe APA-Vergleich:", ifelse(all(mensch_apa_expected$n == mensch_apa_actual$n), "KORREKT", "FEHLER"), "\n")

cat("\n================================================================================\n")
cat("VALIDIERUNG ABGESCHLOSSEN\n")
cat("================================================================================\n") 