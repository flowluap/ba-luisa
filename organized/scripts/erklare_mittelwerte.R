# Erklärung der Mittelwertberechnung
# Warum entstehen Kommazahlen bei 1-5 Likert-Skala?

library(dplyr)

# Lade Daten
cat("Lade Datensatz_simuliert.csv...\n")
data <- read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-16", check.names=FALSE)

# Filtere nur gültige Fälle (FINISHED=1)
data_valid <- data[data$FINISHED == 1, ]

# Zeige Beispiel für eine Person
cat("\n=== BEISPIEL: Person 1 ===\n")
person1 <- data_valid[1, ]

# Zeige alle MN01_ Items für Person 1
mn_items <- select(person1, starts_with("MN01_"))
cat("MN01_ Items für Person 1:\n")
print(mn_items)

# Berechne Mittelwert
mn_mean <- rowMeans(mn_items, na.rm = TRUE)
cat("Mittelwert MN für Person 1:", mn_mean, "\n")

# Zeige alle VS01_ Items für Person 1
vs_items <- select(person1, starts_with("VS01_"))
cat("\nVS01_ Items für Person 1:\n")
print(vs_items)

# Berechne Mittelwert
vs_mean <- rowMeans(vs_items, na.rm = TRUE)
cat("Mittelwert VS für Person 1:", vs_mean, "\n")

# Zeige alle EA01_ Items für Person 1
ea_items <- select(person1, starts_with("EA01_"))
cat("\nEA01_ Items für Person 1:\n")
print(ea_items)

# Berechne Mittelwert
ea_mean <- rowMeans(ea_items, na.rm = TRUE)
cat("Mittelwert EA für Person 1:", ea_mean, "\n")

# Zeige alle ID01_ Items für Person 1
id_items <- select(person1, starts_with("ID01_"))
cat("\nID01_ Items für Person 1:\n")
print(id_items)

# Berechne Mittelwert
id_mean <- rowMeans(id_items, na.rm = TRUE)
cat("Mittelwert ID für Person 1:", id_mean, "\n")

# Zeige alle KI01_ Items für Person 1
ki01_items <- select(person1, starts_with("KI01_"))
cat("\nKI01_ Items für Person 1:\n")
print(ki01_items)

# Berechne Mittelwert
ki01_mean <- rowMeans(ki01_items, na.rm = TRUE)
cat("Mittelwert KI01 für Person 1:", ki01_mean, "\n")

# Zeige alle KI02_ Items für Person 1
ki02_items <- select(person1, starts_with("KI02_"))
cat("\nKI02_ Items für Person 1:\n")
print(ki02_items)

# Berechne Mittelwert
ki02_mean <- rowMeans(ki02_items, na.rm = TRUE)
cat("Mittelwert KI02 für Person 1:", ki02_mean, "\n")

cat("\n=== ERKLÄRUNG ===\n")
cat("Die Kommazahlen entstehen, weil:\n")
cat("1. Jede Variable (MN, VS, EA, ID, KI01, KI02) besteht aus mehreren Items\n")
cat("2. Jedes Item hat Werte von 1-5 (Likert-Skala)\n")
cat("3. Wir berechnen den Mittelwert über alle Items einer Variable\n")
cat("4. Beispiel: Wenn jemand bei MN01_01=3, MN01_02=4, MN01_03=2, MN01_04=5 antwortet:\n")
cat("   Mittelwert = (3+4+2+5)/4 = 14/4 = 3.5\n")
cat("5. Daher können Mittelwerte zwischen 1.0 und 5.0 liegen, auch mit Kommazahlen\n")

# Zeige Anzahl Items pro Variable
cat("\n=== ANZAHL ITEMS PRO VARIABLE ===\n")
cat("MN01_ Items:", length(grep("^MN01_", names(data_valid))), "\n")
cat("VS01_ Items:", length(grep("^VS01_", names(data_valid))), "\n")
cat("EA01_ Items:", length(grep("^EA01_", names(data_valid))), "\n")
cat("ID01_ Items:", length(grep("^ID01_", names(data_valid))), "\n")
cat("KI01_ Items:", length(grep("^KI01_", names(data_valid))), "\n")
cat("KI02_ Items:", length(grep("^KI02_", names(data_valid))), "\n") 