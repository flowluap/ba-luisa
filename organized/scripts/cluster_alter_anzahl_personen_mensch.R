#!/usr/bin/env Rscript

# Balkendiagramm: Anzahl Personen pro Altersgruppe - MENSCH-Gruppe
# Basierend auf Datensatz_simuliert.csv, gefiltert für FINISHED=1

library(dplyr)
library(cluster)
library(ggplot2)
library(tidyr)

cat("=== BALKENDIAGRAMM: ANZAHL PERSONEN PRO ALTERSGRUPPE (MENSCH) ===\n")

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
  
  # Alter kategorisieren
  data_group$Alter_Kat <- factor(data_group$SE03, 
                                 levels = 1:5,
                                 labels = c("18-25", "26-35", "36-45", "46-55", "56+"))
  
  return(list(
    data = data_group,
    kmeans_result = kmeans_result
  ))
}

# Clusteranalyse für KI-Gruppe
ki_cluster_names <- c("KI-Offen", "KI-Skeptisch", "Ambivalent")
ki_results <- perform_cluster_analysis(data_ki, "KI", ki_cluster_names)

# Clusteranalyse für Mensch-Gruppe
mensch_cluster_names <- c("Ambivalent", "Emotional Distanziert", "Emotional Offen")
mensch_results <- perform_cluster_analysis(data_mensch, "MENSCH", mensch_cluster_names)

# Daten für Balkendiagramm vorbereiten (nur Mensch-Gruppe)
mensch_alter_count <- mensch_results$data %>%
  group_by(Cluster_Name, Alter_Kat) %>%
  summarise(Anzahl = n(), .groups = 'drop') %>%
  filter(Alter_Kat %in% c("18-25", "26-35", "36-45", "46-55")) # Nur die gewünschten Altersgruppen

cat("\nMensch-Gruppe - Anzahl Personen pro Altersgruppe und Cluster:\n")
print(mensch_alter_count)

# Balkendiagramm erstellen
p_mensch_alter_count <- ggplot(mensch_alter_count, aes(x = Cluster_Name, y = Anzahl, fill = Alter_Kat)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), 
           color = "black", linewidth = 0.5) +
  scale_fill_manual(values = c("18-25" = "#f1c683", 
                               "26-35" = "#b84d5c", 
                               "36-45" = "#aacd9b",
                               "46-55" = "#8dd3c7"),
                    labels = c("18-25" = "18-25",
                              "26-35" = "26-35",
                              "36-45" = "36-45",
                              "46-55" = "46-55")) +
  labs(x = "Cluster",
       y = "Anzahl an Personen",
       fill = "Altersgruppe") +
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
  scale_x_discrete(labels = c("Ambivalent" = "Ambivalent",
                             "Emotional Distanziert" = "Emotional Distanziert", 
                             "Emotional Offen" = "Emotional Offen"))

# PNG speichern
ggsave("../output/images/cluster_alter_anzahl_personen_mensch.png", p_mensch_alter_count, 
       width = 12, height = 8, dpi = 300, bg = "white")

cat("PNG gespeichert: ../output/images/cluster_alter_anzahl_personen_mensch.png\n")

# Zusätzliche Informationen
cat("\n=== ZUSÄTZLICHE INFORMATIONEN ===\n")

cat("\nMensch-Cluster-Größen:\n")
print(table(mensch_results$data$Cluster_Name))

cat("\nAltersverteilung in Mensch-Gruppe:\n")
print(table(mensch_results$data$Alter_Kat))

cat("\n=== BALKENDIAGRAMM ERFOLGREICH ERSTELLT ===\n") 