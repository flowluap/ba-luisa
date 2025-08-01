#!/usr/bin/env Rscript

# Text-Tabelle für Korrelationsanalyse: SE02 vs. KI01

library(dplyr)

cat("=== TEXT-TABELLE KORRELATIONSANALYSE: SE02 VS. KI01 ===\n")

# Daten laden
data <- tryCatch({
  read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-16", check.names=FALSE)
}, error = function(e) {
  read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-8-BOM", check.names=FALSE)
})

# Nur abgeschlossene Fälle
data_valid <- data[data$FINISHED == 1, ]

# SE02 berechnen (SE02_01)
se02_data <- as.numeric(as.character(data_valid$SE02_01))

# KI01 berechnen (KI01_01 bis KI01_04)
ki01_cols <- data_valid[, c("KI01_01", "KI01_02", "KI01_03", "KI01_04")]
ki01_cols <- sapply(ki01_cols, function(x) as.numeric(as.character(x)))
ki01_data <- rowMeans(ki01_cols, na.rm=TRUE)

# Korrelation berechnen
cor_data <- data.frame(SE02 = se02_data, KI01 = ki01_data)
cor_data_clean <- na.omit(cor_data)
correlation <- cor.test(cor_data_clean$SE02, cor_data_clean$KI01, method = "pearson")

# Ergebnisse
r_val <- correlation$estimate
p_val <- correlation$p.value
n_val <- nrow(cor_data_clean)

# Signifikanz bestimmen
if (p_val < 0.001) {
  significance <- "***"
} else if (p_val < 0.01) {
  significance <- "**"
} else if (p_val < 0.05) {
  significance <- "*"
} else {
  significance <- "n.s."
}

cat("\n=== APA-KONFORME KORRELATIONSTABELLE ===\n")
cat("┌──────────┬──────────┬─────────┬─────────────┬─────┬─────────┐\n")
cat("│ Variable1│ Variable2│    r    │    p-Wert   │  n  │   Sig   │\n")
cat("├──────────┼──────────┼─────────┼─────────────┼─────┼─────────┤\n")
cat(sprintf("│ %-8s │ %-8s │ %7.3f │ %11s │ %3d │ %-7s │\n", 
            "SE02", "KI01", r_val, format(p_val, scientific = TRUE, digits = 3), n_val, significance))
cat("└──────────┴──────────┴─────────┴─────────────┴─────┴─────────┘\n")

cat("\n=== ERGEBNISSE ===\n")
cat("Pearson-Korrelation zwischen SE02 und KI01:\n")
cat("r =", round(r_val, 3), "\n")
cat("p-Wert =", format(p_val, scientific = TRUE, digits = 3), "\n")
cat("n =", n_val, "\n")
cat("Signifikanz:", significance, "\n")

cat("\n=== TEXT-TABELLE ERFOLGREICH ERSTELLT ===\n") 