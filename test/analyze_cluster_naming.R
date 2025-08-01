# =============================================================================
# ANALYSE: CLUSTER-NAMEN-ZUORDNUNG LOGIK
# =============================================================================
# Analysiert ob die Cluster-Namen logisch korrekt zugeordnet sind

library(dplyr)

cat("================================================================================\n")
cat("ANALYSE: CLUSTER-NAMEN-ZUORDNUNG LOGIK\n")
cat("================================================================================\n")

# Load data
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)
data_processed <- data %>% filter(FINISHED == 1, AB01 == 1)  # KI-Gruppe

cat("✓ Daten geladen: N =", nrow(data_processed), "\n")

# Calculate composite scores
mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")

composites <- data.frame(
  VS = rowMeans(data_processed[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_processed[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_processed[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_processed[, ea_cols], na.rm = TRUE)
)

# Perform clustering
set.seed(123)
kmeans_result <- kmeans(composites, centers = 3, nstart = 25)
clusters <- kmeans_result$cluster

# Calculate cluster means
cluster_means <- data.frame(
  Cluster_Number = 1:3,
  n = as.numeric(table(clusters)),
  VS = round(tapply(composites$VS, clusters, mean), 2),
  MN = round(tapply(composites$MN, clusters, mean), 2),
  ID = round(tapply(composites$ID, clusters, mean), 2),
  EA = round(tapply(composites$EA, clusters, mean), 2)
)

cat("\n=== ROHER CLUSTER-OUTPUT (vor Namenszuordnung) ===\n")
print(cluster_means)

# Calculate overall means for each variable
cat("\n=== GESAMTMITTELWERTE ===\n")
overall_means <- colMeans(composites)
cat("VS (Vertrauen & Sympathie):", round(overall_means["VS"], 2), "\n")
cat("MN (Menschlichkeit & Natürlichkeit):", round(overall_means["MN"], 2), "\n")
cat("ID (Identifikation):", round(overall_means["ID"], 2), "\n")
cat("EA (Emotionale Ansprache):", round(overall_means["EA"], 2), "\n")

# Analyze each cluster
cat("\n=== CLUSTER-ANALYSE ===\n")

for(i in 1:3) {
  cat("\n--- Cluster", i, "---\n")
  cat("N =", cluster_means$n[i], "\n")
  cat("VS =", cluster_means$VS[i], "(", if(cluster_means$VS[i] > overall_means["VS"]) "ÜBER" else "UNTER", "Durchschnitt)\n")
  cat("MN =", cluster_means$MN[i], "(", if(cluster_means$MN[i] > overall_means["MN"]) "ÜBER" else "UNTER", "Durchschnitt)\n")
  cat("ID =", cluster_means$ID[i], "(", if(cluster_means$ID[i] > overall_means["ID"]) "ÜBER" else "UNTER", "Durchschnitt)\n")
  cat("EA =", cluster_means$EA[i], "(", if(cluster_means$EA[i] > overall_means["EA"]) "ÜBER" else "UNTER", "Durchschnitt)\n")
  
  # Count how many variables are above average
  above_avg <- sum(
    cluster_means$VS[i] > overall_means["VS"],
    cluster_means$MN[i] > overall_means["MN"],
    cluster_means$ID[i] > overall_means["ID"],
    cluster_means$EA[i] > overall_means["EA"]
  )
  cat("Variablen über Durchschnitt:", above_avg, "von 4\n")
}

# Current naming logic
cat("\n=== AKTUELLE NAMENS-ZUORDNUNG ===\n")
cat("Cluster 1 → KI-Offen\n")
cat("Cluster 2 → Ambivalent\n")
cat("Cluster 3 → KI-Skeptisch\n")

# Logical naming suggestion
cat("\n=== LOGISCHE NAMENS-ZUORDNUNG VORSCHLAG ===\n")

# Sort clusters by overall positivity (average of all variables)
cluster_overall_means <- (cluster_means$VS + cluster_means$MN + cluster_means$ID + cluster_means$EA) / 4
cluster_ranking <- order(cluster_overall_means, decreasing = TRUE)

cat("Basierend auf Gesamtdurchschnitt der Variablen:\n")
for(i in 1:3) {
  rank <- which(cluster_ranking == i)
  overall_mean <- cluster_overall_means[i]
  cat("Cluster", i, "→ Rang", rank, "(Gesamtdurchschnitt:", round(overall_mean, 2), ")\n")
}

cat("\nLogische Zuordnung wäre:\n")
cat("Cluster", cluster_ranking[1], "→ KI-Offen (höchste Werte)\n")
cat("Cluster", cluster_ranking[2], "→ Ambivalent (mittlere Werte)\n")
cat("Cluster", cluster_ranking[3], "→ KI-Skeptisch (niedrigste Werte)\n")

# Check if current naming matches logical naming
current_ki_offen_cluster <- 1
logical_ki_offen_cluster <- cluster_ranking[1]

cat("\n=== VERGLEICH AKTUELL vs LOGISCH ===\n")
cat("Aktuell: KI-Offen = Cluster", current_ki_offen_cluster, "\n")
cat("Logisch: KI-Offen = Cluster", logical_ki_offen_cluster, "\n")

if(current_ki_offen_cluster == logical_ki_offen_cluster) {
  cat("✅ AKTUELLE ZUORDNUNG IST LOGISCH KORREKT!\n")
} else {
  cat("❌ AKTUELLE ZUORDNUNG IST NICHT LOGISCH!\n")
  cat("Cluster", current_ki_offen_cluster, "hat nicht die höchsten Werte!\n")
}

cat("\n================================================================================\n")
cat("ANALYSE ABGESCHLOSSEN\n")
cat("================================================================================\n") 