#!/usr/bin/env Rscript

# ================================================================================
# HÄUFIGKEIT DER NENNUNG - SOCIAL MEDIA NETZWERKE
# ================================================================================

cat("================================================================================\n")
cat("SOCIAL MEDIA HÄUFIGKEIT CHART ERSTELLEN\n")
cat("================================================================================\n\n")

# Bibliotheken laden
library(ggplot2)

# Daten erstellen (exakt wie im Original - in der gleichen Reihenfolge)
social_media_data <- data.frame(
  Netzwerk = c("Instagram", "TikTok", "Facebook", "Snapchat", "Youtube", 
               "X", "LinkedIn", "Discord", "Pinterest", "Reddit"),
  Häufigkeit = c(112, 89, 48, 35, 114, 21, 41, 4, 6, 4)
)

cat("✓ Daten vorbereitet:\n")
for (i in 1:nrow(social_media_data)) {
  cat("  ", social_media_data$Netzwerk[i], ":", social_media_data$Häufigkeit[i], "\n")
}

# Bar Chart erstellen - exakt wie im Original
cat("\n=== CHART ERSTELLEN ===\n")

chart <- ggplot(social_media_data, aes(x = Netzwerk, y = Häufigkeit)) +
  geom_bar(stat = "identity", fill = "gray", color = "black", width = 0.7) +
  labs(
    title = "Häufigkeit der Nennung",
    x = "Social Media Netzwerk",
    y = "Häufigkeit der Nennung"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),  # Gerade X-Achsen-Namen
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "lightgray", linetype = "dashed")
  ) +
  scale_y_continuous(breaks = seq(0, 125, 25), limits = c(0, 125)) +
  scale_x_discrete(limits = social_media_data$Netzwerk)  # Behalte die ursprüngliche Reihenfolge

# Chart anzeigen
cat("✓ Chart erstellt\n")
print(chart)

# Chart speichern
cat("\n=== CHART SPEICHERN ===\n")

# PNG speichern
png_file <- "organized/images/clustering/social_media_frequency_chart.png"
ggsave(png_file, chart, width = 12, height = 8, dpi = 300)
cat("✓ PNG gespeichert:", png_file, "\n")

# PDF speichern
pdf_file <- "organized/images/clustering/social_media_frequency_chart.pdf"
ggsave(pdf_file, chart, width = 12, height = 8)
cat("✓ PDF gespeichert:", pdf_file, "\n")

cat("\n================================================================================\n")
cat("CHART ERFOLGREICH ERSTELLT!\n")
cat("================================================================================\n")

cat("\n📊 ZUSAMMENFASSUNG:\n")
cat("• Chart im exakt gleichen Layout wie im Original erstellt\n")
cat("• Vertikale Balken (nicht horizontal)\n")
cat("• X-Achsen-Namen sind gerade (nicht gedreht)\n")
cat("• Alle Balken sind grau\n")
cat("• Reihenfolge der Netzwerke entspricht dem Original:\n")
cat("  - Instagram: 112\n")
cat("  - TikTok: 89\n")
cat("  - Facebook: 48\n")
cat("  - Snapchat: 35\n")
cat("  - Youtube: 114\n")
cat("  - X: 21\n")
cat("  - LinkedIn: 41\n")
cat("  - Discord: 4\n")
cat("  - Pinterest: 6\n")
cat("  - Reddit: 4\n") 