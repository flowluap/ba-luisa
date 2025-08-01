#!/usr/bin/env Rscript

# Clusteranalyse: Alter pro Cluster im KI-Stil
# Basierend auf Datensatz_simuliert.csv, gefiltert für FINISHED=1

library(dplyr)
library(cluster)
library(ggplot2)
library(tidyr)

cat("=== CLUSTERANALYSE: ALTER PRO CLUSTER (KI-STIL) ===\n")

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

# Daten für Balkendiagramm vorbereiten (nur KI-Gruppe)
ki_alter_long <- ki_results$alter_pro_cluster %>%
  select(Cluster_Name, Mean_Age, SD_Age, N) %>%
  mutate(
    Variable = "Alter",
    Mean = Mean_Age,
    SD = SD_Age
  )

cat("\nKI-Gruppe - Alter pro Cluster:\n")
print(ki_results$alter_pro_cluster)

cat("\nMensch-Gruppe - Alter pro Cluster:\n")
print(mensch_results$alter_pro_cluster)

# Balkendiagramm erstellen im Stil von ki_specific_barplot.png
p_ki_alter <- ggplot(ki_alter_long, aes(x = Cluster_Name, y = Mean, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), 
           color = "black", linewidth = 0.5) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                position = position_dodge(width = 0.9), width = 0.25) +
  geom_text(aes(label = paste0("n=", N)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Alter" = "#f1c683"),
                    labels = c("Alter" = "Alter")) +
  labs(x = "Cluster",
       y = "Alter (Kategorien)",
       fill = "Variable") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  ylim(0, 5) +
  scale_y_continuous(breaks = 1:5, 
                     labels = c("18-25", "26-35", "36-45", "46-55", "56+")) +
  scale_x_discrete(labels = c("KI-Offen" = "KI-Offen",
                             "Ambivalent" = "Ambivalent", 
                             "KI-Skeptisch" = "KI-Skeptisch"))

# PNG speichern
ggsave("../output/images/cluster_alter_ki_style_final.png", p_ki_alter, 
       width = 12, height = 8, dpi = 300, bg = "white")

cat("PNG gespeichert: ../output/images/cluster_alter_ki_style_final.png\n")

# Zusätzliche Informationen
cat("\n=== ZUSÄTZLICHE INFORMATIONEN ===\n")
cat("Alter-Kategorien: 1=18-25, 2=26-35, 3=36-45, 4=46-55, 5=56+\n")

cat("\nKI-Cluster-Größen:\n")
print(table(ki_results$data$Cluster_Name))

cat("\nMensch-Cluster-Größen:\n")
print(table(mensch_results$data$Cluster_Name))

cat("\n=== CLUSTERANALYSE ALTER ERFOLGREICH ERSTELLT ===\n") 