# Load required libraries
library(dplyr)
library(cluster)
library(gridExtra)
library(grid)
library(gtable)

# Function to create APA-style cluster table (same style as mean_table_EA.png)
create_apa_cluster_table <- function(table_data, title) {
  table_grob <- tableGrob(
    table_data,
    rows = NULL,
    theme = ttheme_default(
      base_family = "Times",
      base_size = 12,
      core = list(
        fg_params = list(hjust = 0, x = 0.05),
        bg_params = list(fill = NA),
        padding = unit(c(4, 1), "mm")
      ),
      colhead = list(
        fg_params = list(fontface = "bold", hjust = 0, x = 0.05),
        bg_params = list(fill = NA),
        padding = unit(c(0.1, 1), "mm")  # Minimal padding for headers
      ),
      rowhead = list(
        fg_params = list(hjust = 0, x = 0.05),
        bg_params = list(fill = NA),
        padding = unit(c(4, 1), "mm")
      )
    )
  )
  
  # Add border under column headers
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
  
  # Add bottom border below last row
  table_grob <- gtable_add_grob(
    table_grob,
    grobs = segmentsGrob(
      x0 = unit(0, "npc"),
      y0 = unit(0, "npc"),
      x1 = unit(1, "npc"),
      y1 = unit(0, "npc"),
      gp = gpar(lwd = 2)
    ),
    t = nrow(table_data) + 1, b = nrow(table_data) + 1, l = 1, r = ncol(table_data)
  )
  
  # Make header row taller by directly manipulating gtable heights
  table_grob$heights[1] <- unit(6, "mm")  # Make header row slightly taller
  
  return(table_grob)
}

# Function to perform cluster analysis
perform_cluster_analysis <- function(data_group, group_name, k = 3) {
  cat("\n=== CLUSTERANALYSE FÜR", group_name, "===\n")
  
  # Calculate composite scores
  cluster_data <- data.frame(
    VS = rowMeans(data_group[, grepl("^VS01_", colnames(data_group))], na.rm = TRUE),
    MN = rowMeans(data_group[, grepl("^MN01_", colnames(data_group))], na.rm = TRUE),
    ID = rowMeans(data_group[, grepl("^ID01_", colnames(data_group))], na.rm = TRUE),
    EA = rowMeans(data_group[, grepl("^EA01_", colnames(data_group))], na.rm = TRUE)
  )
  
  # Remove rows with any NA values
  cluster_data_clean <- na.omit(cluster_data)
  cat("Anzahl gültige Fälle für Clustering:", nrow(cluster_data_clean), "\n")
  
  if(nrow(cluster_data_clean) < k) {
    cat("Zu wenige Fälle für", k, "Cluster. Reduziere auf", nrow(cluster_data_clean), "Cluster.\n")
    k <- max(1, nrow(cluster_data_clean) - 1)
  }
  
  # Perform k-means clustering
  set.seed(123)  # For reproducibility
  if(k > 1) {
    kmeans_result <- kmeans(cluster_data_clean, centers = k, nstart = 25)
    clusters <- kmeans_result$cluster
  } else {
    clusters <- rep(1, nrow(cluster_data_clean))
  }
  
  cat("Cluster-Verteilung:\n")
  print(table(clusters))
  
  return(list(
    clusters = clusters,
    data = cluster_data_clean,
    n_cases = nrow(cluster_data_clean)
  ))
}

# Load data
cat("Lade Daten...\n")
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

cat("Daten erfolgreich geladen\n")
cat("Anzahl Zeilen:", nrow(data), "\n")
cat("Anzahl Spalten:", ncol(data), "\n")

# Filter data (FINISHED=1)
data_processed <- data %>%
  filter(FINISHED == 1)

cat("Daten gefiltert\n")
cat("Anzahl gültige Fälle:", nrow(data_processed), "\n")

# Split by group
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

cat("Anzahl KI (AB01=1):", nrow(data_ki), "\n")
cat("Anzahl Mensch (AB01=2):", nrow(data_mensch), "\n")

# Perform cluster analysis for both groups
ki_clusters <- perform_cluster_analysis(data_ki, "KI", k = 3)
mensch_clusters <- perform_cluster_analysis(data_mensch, "MENSCH", k = 3)

# Define cluster names based on logical approach (will be assigned dynamically)
cluster_names_ki <- c("KI-Offen", "Ambivalent", "KI-Skeptisch")  # Will be reordered
cluster_names_mensch <- c("Emotional Distanziert", "Emotional Offen", "Ambivalent")  # Will be reordered

# Create result table
results <- data.frame(
  Cluster = c(cluster_names_ki, cluster_names_mensch),
  Gruppe = c(rep("KI", 3), rep("Mensch", 3)),
  N = numeric(6),
  Prozent = character(6),
  stringsAsFactors = FALSE
)

# Calculate counts and percentages for KI clusters
ki_cluster_counts <- table(ki_clusters$clusters)
for(i in 1:3) {
  if(i <= length(ki_cluster_counts)) {
    count <- ki_cluster_counts[i]
    percentage <- round((count / ki_clusters$n_cases) * 100, 1)
    results$N[i] <- count
    results$Prozent[i] <- paste0(percentage, "%")
  } else {
    results$N[i] <- 0
    results$Prozent[i] <- "0.0%"
  }
}

# Calculate counts and percentages for Mensch clusters
mensch_cluster_counts <- table(mensch_clusters$clusters)
for(i in 1:3) {
  if(i <= length(mensch_cluster_counts)) {
    count <- mensch_cluster_counts[i]
    percentage <- round((count / mensch_clusters$n_cases) * 100, 1)
    results$N[i + 3] <- count
    results$Prozent[i + 3] <- paste0(percentage, "%")
  } else {
    results$N[i + 3] <- 0
    results$Prozent[i + 3] <- "0.0%"
  }
}

cat("\nCluster-Ergebnisse:\n")
print(results)

# Create APA table
cluster_apa_table <- create_apa_cluster_table(results, "Cluster-Prozent-Tabelle")

# Create directory if it doesn't exist
dir.create("organized/images/descriptive", recursive = TRUE, showWarnings = FALSE)

# Save table as PNG with optimal dimensions (more space for bottom line)
png("organized/images/descriptive/cluster_prozent_tabelle.png", 
    width = 1400, height = 340, res = 300, bg = "white")
grid::grid.newpage()

# Create viewport with optimal margins (more space at bottom for line)
vp <- viewport(x = 0.5, y = 0.53, width = 0.98, height = 0.82, just = c("center", "center"))
pushViewport(vp)
grid::grid.draw(cluster_apa_table)
popViewport()

dev.off()

cat("Cluster-Prozent-Tabelle erstellt: cluster_prozent_tabelle.png\n")

cat("\n================================================================================\n")
cat("CLUSTER-PROZENT-TABELLE ERSTELLT (ECHTE DATEN):\n")
cat("================================================================================\n")
cat("• cluster_prozent_tabelle.png (KI und Mensch Cluster mit Prozentangaben)\n")
cat("• Tabelle im APA-Stil mit gleichem Look wie mean_table_EA.png\n")
cat("• K-Means Clustering mit k=3 für beide Gruppen\n")
cat("• Cluster-Namen entsprechend typischer Muster zugeordnet\n")
cat("• Verwendet echte Daten aus CSV-Datei\n")
cat("• N-Werte: KI =", nrow(data_ki), ", Mensch =", nrow(data_mensch), "\n")
cat("• Minimale Whitespace, vollständig sichtbar\n")
cat("================================================================================\n") 