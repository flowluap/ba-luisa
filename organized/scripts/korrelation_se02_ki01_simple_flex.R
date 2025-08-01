#!/usr/bin/env Rscript

# Einfache APA-Tabelle für Korrelationsanalyse: SE02 vs. KI01

library(dplyr)
library(flextable)

cat("=== EINFACHE APA-TABELLE KORRELATIONSANALYSE: SE02 VS. KI01 ===\n")

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

# Tabelle erstellen
table_data <- data.frame(
  Variable1 = "SE02",
  Variable2 = "KI01", 
  r = round(r_val, 3),
  p = format(p_val, scientific = TRUE, digits = 3),
  n = n_val,
  Sig = significance
)

# Einfache Flextable erstellen
ft <- flextable(table_data) %>%
  set_header_labels(
    Variable1 = "Variable 1",
    Variable2 = "Variable 2",
    r = "r",
    p = "p-Wert",
    n = "n",
    Sig = "Sig."
  ) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  autofit()

# PNG speichern
save_as_image(ft, path = "../output/images/korrelation_se02_ki01_tabelle.png", 
              width = 8, height = 2, res = 300)

cat("\n=== ERGEBNISSE ===\n")
cat("Pearson-Korrelation zwischen SE02 und KI01:\n")
cat("r =", round(r_val, 3), "\n")
cat("p-Wert =", format(p_val, scientific = TRUE, digits = 3), "\n")
cat("n =", n_val, "\n")
cat("Signifikanz:", significance, "\n")

cat("\nPNG gespeichert: ../output/images/korrelation_se02_ki01_tabelle.png\n")
cat("\n=== EINFACHE APA-TABELLE ERFOLGREICH ERSTELLT ===\n") 