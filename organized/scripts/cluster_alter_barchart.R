#!/usr/bin/env Rscript

# Balkendiagramm: Alter pro Cluster, getrennt nach KI und Mensch
# Basierend auf Datensatz_simuliert.csv, gefiltert für FINISHED=1

library(dplyr)
library(cluster)
library(ggplot2)

cat("=== BALKENDIAGRAMM: ALTER PRO CLUSTER (KI vs MENSCH) ===\n")

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
  
  return(list(
    data = data_group,
    alter_pro_cluster = alter_pro_cluster,
    kmeans_result = kmeans_result
  ))
}

# Clusteranalyse für KI-Gruppe
ki_cluster_names <- c("KI-Offen", "KI-Skeptisch", "Ambivalent")
ki_results <- perform_cluster_analysis(data_ki, "KI", ki_cluster_names)

# Clusteranalyse für Mensch-Gruppe
mensch_cluster_names <- c("Ambivalent", "Emotional Distanziert", "Emotional Offen")
mensch_results <- perform_cluster_analysis(data_mensch, "MENSCH", mensch_cluster_names)

# Daten für Balkendiagramm vorbereiten
plot_data <- rbind(
  data.frame(
    Gruppe = "KI",
    Cluster = ki_results$alter_pro_cluster$Cluster_Name,
    N = ki_results$alter_pro_cluster$N,
    Mean_Age = ki_results$alter_pro_cluster$Mean_Age,
    SD_Age = ki_results$alter_pro_cluster$SD_Age,
    stringsAsFactors = FALSE
  ),
  data.frame(
    Gruppe = "Mensch",
    Cluster = mensch_results$alter_pro_cluster$Cluster_Name,
    N = mensch_results$alter_pro_cluster$N,
    Mean_Age = mensch_results$alter_pro_cluster$Mean_Age,
    SD_Age = mensch_results$alter_pro_cluster$SD_Age,
    stringsAsFactors = FALSE
  )
)

# Cluster-Reihenfolge festlegen
plot_data$Cluster <- factor(plot_data$Cluster, 
                           levels = c("KI-Offen", "KI-Skeptisch", "Ambivalent", 
                                    "Emotional Offen", "Emotional Distanziert"))

# Balkendiagramm erstellen
p <- ggplot(plot_data, aes(x = Cluster, y = Mean_Age, fill = Gruppe)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean_Age - SD_Age, ymax = Mean_Age + SD_Age), 
                position = position_dodge(width = 0.8), width = 0.25) +
  geom_text(aes(label = paste0("n=", N)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("KI" = "#f1c683", "Mensch" = "#b84d5c")) +
  labs(
    title = NULL,
    x = "Cluster",
    y = "Durchschnittsalter (Kategorien)",
    fill = "Gruppe"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(breaks = 1:5, 
                     labels = c("18-25", "26-35", "36-45", "46-55", "56+"),
                     limits = c(0, 5.5))

# PNG speichern
ggsave("../output/images/cluster_alter_barchart.png", 
       plot = p, 
       width = 12, 
       height = 8, 
       dpi = 300, 
       units = "in")

cat("PNG gespeichert: ../output/images/cluster_alter_barchart.png\n")

# Zusätzliche Informationen
cat("\n=== ZUSÄTZLICHE INFORMATIONEN ===\n")
cat("Alter-Kategorien: 1=18-25, 2=26-35, 3=36-45, 4=46-55, 5=56+\n")

cat("\nKI-Gruppe - Alter pro Cluster:\n")
print(ki_results$alter_pro_cluster)

cat("\nMensch-Gruppe - Alter pro Cluster:\n")
print(mensch_results$alter_pro_cluster)

cat("\n=== BALKENDIAGRAMM ERFOLGREICH ERSTELLT ===\n") 