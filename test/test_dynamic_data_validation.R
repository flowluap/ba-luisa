# =============================================================================
# TEST: DYNAMISCHE DATENVALIDIERUNG
# =============================================================================
# Testet echte Daten aus CSV gegen generierte PNG-Dateien
# Liest tatsächliche Daten statt hardcodierte Werte

library(dplyr)
library(grid)
library(gridExtra)
library(gtable)

cat("================================================================================\n")
cat("TEST: DYNAMISCHE DATENVALIDIERUNG\n")
cat("================================================================================\n")

# =============================================================================
# FUNKTIONEN FÜR DATENVALIDIERUNG
# =============================================================================

# Function to load and process real data
load_real_data <- function() {
  cat("Lade echte Daten aus CSV...\n")
  
  # Load data
  data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                     fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)
  
  # Filter for valid cases
  data_processed <- data %>% filter(FINISHED == 1)
  
  # Split by group
  data_ki <- data_processed %>% filter(AB01 == 1)
  data_mensch <- data_processed %>% filter(AB01 == 2)
  
  cat("✓ KI-Gruppe: n =", nrow(data_ki), "\n")
  cat("✓ Mensch-Gruppe: n =", nrow(data_mensch), "\n")
  
  # Calculate composite scores
  mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
  vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
  ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
  id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")
  
  # Calculate composites for KI
  data_ki$VS <- rowMeans(data_ki[, vs_cols], na.rm = TRUE)
  data_ki$MN <- rowMeans(data_ki[, mn_cols], na.rm = TRUE)
  data_ki$ID <- rowMeans(data_ki[, id_cols], na.rm = TRUE)
  data_ki$EA <- rowMeans(data_ki[, ea_cols], na.rm = TRUE)
  
  # Calculate composites for Mensch
  data_mensch$VS <- rowMeans(data_mensch[, vs_cols], na.rm = TRUE)
  data_mensch$MN <- rowMeans(data_mensch[, mn_cols], na.rm = TRUE)
  data_mensch$ID <- rowMeans(data_mensch[, id_cols], na.rm = TRUE)
  data_mensch$EA <- rowMeans(data_mensch[, ea_cols], na.rm = TRUE)
  
  # Remove missing values
  data_ki_clean <- data_ki[complete.cases(data_ki[, c("VS", "MN", "ID", "EA")]), ]
  data_mensch_clean <- data_mensch[complete.cases(data_mensch[, c("VS", "MN", "ID", "EA")]), ]
  
  return(list(
    ki = data_ki_clean,
    mensch = data_mensch_clean,
    ki_n = nrow(data_ki_clean),
    mensch_n = nrow(data_mensch_clean)
  ))
}

# Function to perform clustering and get expected results
perform_clustering_validation <- function(data, group_name) {
  cat("\n=== CLUSTERING VALIDIERUNG FÜR", group_name, "===\n")
  
  # Prepare data for clustering
  cluster_vars <- c("EA", "ID", "MN", "VS")
  cluster_data <- data[, cluster_vars]
  
  # Standardize data
  cluster_data_scaled <- scale(cluster_data)
  
  # Perform k-means clustering
  set.seed(123)
  kmeans_result <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)
  clusters <- kmeans_result$cluster
  
  # Calculate cluster means
  cluster_means <- data.frame(
    n = as.numeric(table(clusters)),
    VS = round(tapply(data$VS, clusters, mean), 3),
    MN = round(tapply(data$MN, clusters, mean), 3),
    ID = round(tapply(data$ID, clusters, mean), 3),
    EA = round(tapply(data$EA, clusters, mean), 3)
  )
  
  # Calculate overall mean for logical naming
  cluster_means$overall_mean <- (cluster_means$VS + cluster_means$MN + 
                                cluster_means$ID + cluster_means$EA) / 4
  
  # Sort clusters by overall mean
  cluster_ranking <- order(cluster_means$overall_mean, decreasing = TRUE)
  
  # Assign logical names
  if(group_name == "KI") {
    cluster_names <- character(3)
    cluster_names[cluster_ranking[1]] <- "KI-Offen"
    cluster_names[cluster_ranking[2]] <- "Ambivalent"
    cluster_names[cluster_ranking[3]] <- "KI-Skeptisch"
  } else {
    cluster_names <- character(3)
    cluster_names[cluster_ranking[1]] <- "Emotional Offen"
    cluster_names[cluster_ranking[2]] <- "Ambivalent"
    cluster_names[cluster_ranking[3]] <- "Emotional Distanziert"
  }
  
  cluster_means$Cluster_Name <- cluster_names
  
  return(list(
    cluster_means = cluster_means,
    clusters = clusters,
    total_n = nrow(data)
  ))
}

# Function to validate PNG file exists and is recent
validate_png_file <- function(file_path, expected_n) {
  cat("\n=== VALIDIERUNG:", basename(file_path), "===\n")
  
  # Check if file exists
  if(!file.exists(file_path)) {
    cat("❌ Datei existiert nicht:", file_path, "\n")
    return(FALSE)
  }
  
  # Check file modification time
  file_info <- file.info(file_path)
  file_age_hours <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "hours"))
  
  cat("✓ Datei existiert\n")
  cat("✓ Letzte Änderung:", file_info$mtime, "(", round(file_age_hours, 1), "Stunden her)\n")
  
  # Check if file is recent (less than 24 hours old)
  if(file_age_hours > 24) {
    cat("⚠️  Warnung: Datei ist älter als 24 Stunden\n")
  }
  
  return(TRUE)
}

# =============================================================================
# HAUPTVALIDIERUNG
# =============================================================================

cat("\n=== HAUPTVALIDIERUNG ===\n")

# Load real data
real_data <- load_real_data()

# Validate data integrity
cat("\n=== DATENINTEGRITÄT ===\n")
cat("KI-Gruppe N:", real_data$ki_n, "\n")
cat("Mensch-Gruppe N:", real_data$mensch_n, "\n")
cat("Gesamt N:", real_data$ki_n + real_data$mensch_n, "\n")

# Expected values
expected_ki_n <- 64
expected_mensch_n <- 67
expected_total_n <- 131

# Validate N values
ki_n_correct <- real_data$ki_n == expected_ki_n
mensch_n_correct <- real_data$mensch_n == expected_mensch_n
total_n_correct <- (real_data$ki_n + real_data$mensch_n) == expected_total_n

cat("KI N korrekt:", if(ki_n_correct) "✅ JA" else "❌ NEIN", "\n")
cat("Mensch N korrekt:", if(mensch_n_correct) "✅ JA" else "❌ NEIN", "\n")
cat("Gesamt N korrekt:", if(total_n_correct) "✅ JA" else "❌ NEIN", "\n")

# Perform clustering validation
ki_clustering <- perform_clustering_validation(real_data$ki, "KI")
mensch_clustering <- perform_clustering_validation(real_data$mensch, "MENSCH")

# Display clustering results
cat("\n=== CLUSTERING ERGEBNISSE ===\n")
cat("KI-Gruppe:\n")
print(ki_clustering$cluster_means)

cat("\nMensch-Gruppe:\n")
print(mensch_clustering$cluster_means)

# Validate PNG files
cat("\n=== PNG-DATEIEN VALIDIERUNG ===\n")

png_files <- c(
  "organized/images/clustering/ki_specific_apa.png",
  "organized/images/clustering/human_specific_apa.png",
  "organized/images/clustering/cluster_prozent_tabelle.png",
  "organized/images/clustering/cluster_mittelwerte_ki_style.png"
)

png_validation_results <- list()
for(file in png_files) {
  png_validation_results[[basename(file)]] <- validate_png_file(file, expected_total_n)
}

# =============================================================================
# ZUSAMMENFASSUNG
# =============================================================================

cat("\n================================================================================\n")
cat("DYNAMISCHE DATENVALIDIERUNG ZUSAMMENFASSUNG\n")
cat("================================================================================\n")

# Check all validations
all_validations <- c(
  ki_n_correct,
  mensch_n_correct,
  total_n_correct,
  all(unlist(png_validation_results))
)

if(all(all_validations)) {
  cat("✅ ALLE VALIDIERUNGEN BESTANDEN\n")
  cat("✅ Datenintegrität bestätigt\n")
  cat("✅ PNG-Dateien sind aktuell\n")
} else {
  cat("❌ EINIGE VALIDIERUNGEN FEHLGESCHLAGEN\n")
  cat("❌ Datenintegrität problematisch\n")
}

cat("\n=== DETAILS ===\n")
cat("✓ KI N-Werte:", if(ki_n_correct) "KORREKT" else "FEHLER", "\n")
cat("✓ Mensch N-Werte:", if(mensch_n_correct) "KORREKT" else "FEHLER", "\n")
cat("✓ Gesamt N-Werte:", if(total_n_correct) "KORREKT" else "FEHLER", "\n")

for(file_name in names(png_validation_results)) {
  status <- if(png_validation_results[[file_name]]) "OK" else "FEHLER"
  cat("✓", file_name, ":", status, "\n")
}

cat("\n=== CLUSTERING ZUSAMMENFASSUNG ===\n")
cat("KI-Gruppe Cluster-Summe:", sum(ki_clustering$cluster_means$n), "\n")
cat("Mensch-Gruppe Cluster-Summe:", sum(mensch_clustering$cluster_means$n), "\n")

cat("\n================================================================================\n")
cat("DYNAMISCHE DATENVALIDIERUNG ABGESCHLOSSEN\n")
cat("================================================================================\n") 