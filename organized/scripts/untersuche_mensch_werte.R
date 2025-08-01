# Untersuchung der Mensch-Werte - warum sind sie so glatt?
library(dplyr)

# Lade Daten
cat("Lade Datensatz_simuliert.csv...\n")
data <- read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-16", check.names=FALSE)

# Filtere nur gültige Fälle (FINISHED=1)
data_valid <- data[data$FINISHED == 1, ]

# Trenne nach Gruppen (korrekt)
data_ki <- data_valid[data_valid$AB01 == 1, ]
data_mensch <- data_valid[data_valid$AB01 == 2, ]

cat("Anzahl KI (AB01=1):", nrow(data_ki), "\n")
cat("Anzahl Mensch (AB01=2):", nrow(data_mensch), "\n")

# Untersuche Mensch-Gruppe detailliert
cat("\n=== UNTERSUCHUNG MENSCH-GRUPPE (AB01=2) ===\n")

# Zeige AB01 Verteilung
cat("AB01 Verteilung:\n")
print(table(data_valid$AB01))

# Zeige erste 5 Personen der Mensch-Gruppe
cat("\nErste 5 Personen der Mensch-Gruppe:\n")
for (i in 1:min(5, nrow(data_mensch))) {
  person <- data_mensch[i, ]
  cat("\nPerson", i, ":\n")
  
  # MN01_ Items
  mn_items <- select(person, starts_with("MN01_"))
  mn_mean <- rowMeans(mn_items, na.rm = TRUE)
  cat("MN01_ Items:", as.numeric(mn_items), "→ Mittelwert:", round(mn_mean, 2), "\n")
  
  # VS01_ Items
  vs_items <- select(person, starts_with("VS01_"))
  vs_mean <- rowMeans(vs_items, na.rm = TRUE)
  cat("VS01_ Items:", as.numeric(vs_items), "→ Mittelwert:", round(vs_mean, 2), "\n")
  
  # EA01_ Items
  ea_items <- select(person, starts_with("EA01_"))
  ea_mean <- rowMeans(ea_items, na.rm = TRUE)
  cat("EA01_ Items:", as.numeric(ea_items), "→ Mittelwert:", round(ea_mean, 2), "\n")
  
  # ID01_ Items
  id_items <- select(person, starts_with("ID01_"))
  id_mean <- rowMeans(id_items, na.rm = TRUE)
  cat("ID01_ Items:", as.numeric(id_items), "→ Mittelwert:", round(id_mean, 2), "\n")
}

# Berechne alle Mittelwerte für Mensch-Gruppe
cat("\n=== ALLE MITTELWERTE MENSCH-GRUPPE ===\n")

# MN
mn_means <- rowMeans(select(data_mensch, starts_with("MN01_")), na.rm = TRUE)
cat("MN Mittelwerte (Mensch):\n")
print(summary(mn_means))
cat("Min:", min(mn_means), "Max:", max(mn_means), "\n")

# VS
vs_means <- rowMeans(select(data_mensch, starts_with("VS01_")), na.rm = TRUE)
cat("\nVS Mittelwerte (Mensch):\n")
print(summary(vs_means))
cat("Min:", min(vs_means), "Max:", max(vs_means), "\n")

# EA
ea_means <- rowMeans(select(data_mensch, starts_with("EA01_")), na.rm = TRUE)
cat("\nEA Mittelwerte (Mensch):\n")
print(summary(ea_means))
cat("Min:", min(ea_means), "Max:", max(ea_means), "\n")

# ID
id_means <- rowMeans(select(data_mensch, starts_with("ID01_")), na.rm = TRUE)
cat("\nID Mittelwerte (Mensch):\n")
print(summary(id_means))
cat("Min:", min(id_means), "Max:", max(id_means), "\n")

# Zeige Histogramm-ähnliche Verteilung
cat("\n=== VERBREITUNG DER MITTELWERTE ===\n")
cat("MN Werte:", sort(round(mn_means, 2)), "\n")
cat("VS Werte:", sort(round(vs_means, 2)), "\n")
cat("EA Werte:", sort(round(ea_means, 2)), "\n")
cat("ID Werte:", sort(round(id_means, 2)), "\n")

# Prüfe ob es ein Problem mit der Datengenerierung gibt
cat("\n=== PRÜFUNG DATENGENERIERUNG ===\n")
cat("Anzahl einzigartige MN Werte:", length(unique(round(mn_means, 2))), "\n")
cat("Anzahl einzigartige VS Werte:", length(unique(round(vs_means, 2))), "\n")
cat("Anzahl einzigartige EA Werte:", length(unique(round(ea_means, 2))), "\n")
cat("Anzahl einzigartige ID Werte:", length(unique(round(id_means, 2))), "\n") 