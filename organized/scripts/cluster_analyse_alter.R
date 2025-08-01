#!/usr/bin/env Rscript

# Clusteranalyse: Alter pro Cluster, getrennt nach KI und Mensch
# Basierend auf Datensatz_simuliert.csv, gefiltert für FINISHED=1

library(dplyr)
library(cluster)
library(gridExtra)
library(grid)
library(gtable)
library(ggplot2)

cat("=== CLUSTERANALYSE: ALTER PRO CLUSTER (KI vs MENSCH) ===\n")

# Daten laden
cat("Lade Datensatz_simuliert.csv...\n")
data <- tryCatch({
  read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-16", check.names=FALSE)
}, error = function(e) {
  cat("Fehler beim Laden mit UTF-16, versuche UTF-8-BOM...\n")
  read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-8-BOM", check.names=FALSE)
})

# Nur abgeschlossene Fälle
cat("Filtere abgeschlossene Fälle (FINISHED=1)...\n")
data_valid <- data[data$FINISHED == 1, ]
cat("Anzahl abgeschlossener Fälle:", nrow(data_valid), "\n")

# Gruppen aufteilen
data_ki <- data_valid[data_valid$AB01 == 1, ]
data_mensch <- data_valid[data_valid$AB01 == 2, ]

cat("Anzahl KI (AB01=1):", nrow(data_ki), "\n")
cat("Anzahl Mensch (AB01=2):", nrow(data_mensch), "\n")

# Funktion für Clusteranalyse
perform_cluster_analysis <- function(data_group, group_name, cluster_names) {
  cat("\n=== CLUSTERANALYSE FÜR", group_name, "===\n")
  
  # Variablen für Clustering auswählen
  vars <- c("VS", "MN", "ID", "EA")
  
  # Skalenmittelwerte berechnen
  vs_cols <- sapply(c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08"), 
                    function(x) as.numeric(as.character(data_group[[x]])))
  mn_cols <- sapply(c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07"), 
                    function(x) as.numeric(as.character(data_group[[x]])))
  id_cols <- sapply(c("ID01_01", "ID01_02", "ID01_03", "ID01_04"), 
                    function(x) as.numeric(as.character(data_group[[x]])))
  ea_cols <- sapply(c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05"), 
                    function(x) as.numeric(as.character(data_group[[x]])))
  
  cluster_data <- data.frame(
    VS = rowMeans(vs_cols, na.rm=TRUE),
    MN = rowMeans(mn_cols, na.rm=TRUE),
    ID = rowMeans(id_cols, na.rm=TRUE),
    EA = rowMeans(ea_cols, na.rm=TRUE)
  )
  
  # Standardisierung
  cluster_data_scaled <- scale(cluster_data)
  
  # K-Means Clustering (3 Cluster)
  set.seed(123)
  kmeans_result <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)
  
  # Cluster zuweisen
  data_group$Cluster <- kmeans_result$cluster
  
  # Cluster benennen
  data_group$Cluster_Name <- cluster_names[data_group$Cluster]
  
  # Skalenmittelwerte zum data_group hinzufügen
  data_group$VS <- cluster_data$VS
  data_group$MN <- cluster_data$MN
  data_group$ID <- cluster_data$ID
  data_group$EA <- cluster_data$EA
  
  # Alter pro Cluster berechnen
  alter_pro_cluster <- data_group %>%
    group_by(Cluster_Name) %>%
    summarise(
      N = n(),
      Mean_Age = mean(as.numeric(as.character(SE03)), na.rm=TRUE),
      SD_Age = sd(as.numeric(as.character(SE03)), na.rm=TRUE),
      Min_Age = min(as.numeric(as.character(SE03)), na.rm=TRUE),
      Max_Age = max(as.numeric(as.character(SE03)), na.rm=TRUE),
      .groups = 'drop'
    )
  
  # Cluster-Mittelwerte für Variablen
  cluster_means <- data_group %>%
    group_by(Cluster_Name) %>%
    summarise(
      VS_Mean = mean(VS, na.rm=TRUE),
      MN_Mean = mean(MN, na.rm=TRUE),
      ID_Mean = mean(ID, na.rm=TRUE),
      EA_Mean = mean(EA, na.rm=TRUE),
      .groups = 'drop'
    )
  
  return(list(
    data = data_group,
    alter_pro_cluster = alter_pro_cluster,
    cluster_means = cluster_means,
    kmeans_result = kmeans_result
  ))
}

# Clusteranalyse für KI-Gruppe
ki_cluster_names <- c("KI-Offen", "KI-Skeptisch", "Ambivalent")
ki_results <- perform_cluster_analysis(data_ki, "KI", ki_cluster_names)

# Clusteranalyse für Mensch-Gruppe
mensch_cluster_names <- c("Ambivalent", "Emotional Distanziert", "Emotional Offen")
mensch_results <- perform_cluster_analysis(data_mensch, "MENSCH", mensch_cluster_names)

# Ergebnisse zusammenfassen
cat("\n=== ERGEBNISSE ZUSAMMENGEFASST ===\n")

cat("\nKI-Gruppe - Alter pro Cluster:\n")
print(ki_results$alter_pro_cluster)

cat("\nMensch-Gruppe - Alter pro Cluster:\n")
print(mensch_results$alter_pro_cluster)

# APA-Tabelle erstellen
results_table <- rbind(
  data.frame(
    Gruppe = "KI",
    Cluster = ki_results$alter_pro_cluster$Cluster_Name,
    N = ki_results$alter_pro_cluster$N,
    Mean_Age = round(ki_results$alter_pro_cluster$Mean_Age, 2),
    SD_Age = round(ki_results$alter_pro_cluster$SD_Age, 2),
    Min_Age = ki_results$alter_pro_cluster$Min_Age,
    Max_Age = ki_results$alter_pro_cluster$Max_Age,
    stringsAsFactors = FALSE
  ),
  data.frame(
    Gruppe = "Mensch",
    Cluster = mensch_results$alter_pro_cluster$Cluster_Name,
    N = mensch_results$alter_pro_cluster$N,
    Mean_Age = round(mensch_results$alter_pro_cluster$Mean_Age, 2),
    SD_Age = round(mensch_results$alter_pro_cluster$SD_Age, 2),
    Min_Age = mensch_results$alter_pro_cluster$Min_Age,
    Max_Age = mensch_results$alter_pro_cluster$Max_Age,
    stringsAsFactors = FALSE
  )
)

# CSV speichern
write.csv(results_table, "cluster_alter_analyse.csv", row.names=FALSE)
cat("CSV gespeichert: cluster_alter_analyse.csv\n")

# APA-konforme Tabelle erstellen
apa_tab <- tableGrob(results_table, 
                     rows = NULL,
                     theme = ttheme_default(
                       base_size = 12,
                       base_family = "Times",
                       core = list(
                         fg_params = list(hjust = 0.5, x = 0.5, fontsize = 10),
                         bg_params = list(fill = "white")
                       ),
                       colhead = list(
                         fg_params = list(fontface = "bold", hjust = 0.5, x = 0.5, fontsize = 11),
                         bg_params = list(fill = "white")
                       )
                     ))

# Spaltenbreiten anpassen
apa_tab$widths <- unit(c(0.15, 0.25, 0.1, 0.15, 0.15, 0.1, 0.1), "npc")

# Linien APA-Style: oben Kopfzeile, unter letzter Zeile
apa_tab <- gtable::gtable_add_grob(
  apa_tab,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 2)),
  t = 1, b = 1, l = 1, r = ncol(apa_tab)
)
apa_tab <- gtable::gtable_add_grob(
  apa_tab,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
  t = nrow(apa_tab), b = nrow(apa_tab), l = 1, r = ncol(apa_tab)
)

# PNG speichern
png("../output/images/cluster_alter_analyse.png", 
    width = 1400, height = 500, 
    units = "px", res = 300,
    type = "cairo")
grid.draw(apa_tab)
dev.off()

cat("PNG gespeichert: ../output/images/cluster_alter_analyse.png\n")

cat("\n=== CLUSTERANALYSE ALTER ERFOLGREICH ERSTELLT ===\n") 