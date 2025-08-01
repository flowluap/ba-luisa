# Load packages
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")

library(ggplot2)
library(dplyr)
library(tidyr)

# Read data
data <- read.delim("../data/Datensatz_simuliert.csv", sep="\t", fileEncoding="UTF-16")

# DATA PREPARATION
scale_data <- data %>%
  filter(FINISHED == 1) %>%
  mutate(
    Gruppe = ifelse(AB01 == 1, "KI", "Mensch"),
    MC = MC01,
    MN = rowMeans(select(., MN01_01:MN01_07), na.rm = TRUE),
    VS = rowMeans(select(., VS01_01:VS01_08), na.rm = TRUE),
    EA = rowMeans(select(., EA01_01:EA01_05), na.rm = TRUE),
    ID = rowMeans(select(., ID01_01:ID01_04), na.rm = TRUE)
  ) %>%
  filter(!is.na(Gruppe)) %>%
  select(Gruppe, MC, MN, VS, EA, ID)

# Reshape data for plotting
plot_data <- scale_data %>%
  pivot_longer(
    cols = c(MC, MN, VS, EA, ID),
    names_to = "Variable",
    values_to = "Score"
  ) %>%
  mutate(
    Variable = factor(Variable, 
                     levels = c("MC", "MN", "VS", "EA", "ID"))
  ) %>%
  filter(!is.na(Score))

# Create split violin plot
plot_data_split <- plot_data %>%
  mutate(
    var_numeric = as.numeric(Variable),
    x_pos = case_when(
      Gruppe == "KI" ~ var_numeric - 0.2,
      Gruppe == "Mensch" ~ var_numeric + 0.2
    )
  )

p_violin <- ggplot() +
  geom_violin(data = plot_data_split %>% filter(Gruppe == "KI"),
              aes(x = x_pos, y = Score, fill = Gruppe, group = Variable),
              color = "#545454", 
              linewidth = 0.5,
              alpha = 0.9,
              trim = FALSE,
              scale = "width",
              width = 0.4) +
  geom_violin(data = plot_data_split %>% filter(Gruppe == "Mensch"),
              aes(x = x_pos, y = Score, fill = Gruppe, group = Variable),
              color = "#545454", 
              linewidth = 0.5,
              alpha = 0.9,
              trim = FALSE,
              scale = "width",
              width = 0.4) +
  scale_fill_manual(values = c("KI" = "#89cdcf", "Mensch" = "#fbb9c2")) +
  scale_x_continuous(breaks = 1:5, 
                     labels = c("MC", "MN", "VS", "EA", "ID"),
                     limits = c(0.5, 5.5)) +
  scale_y_continuous(breaks = 1:5, limits = c(1, 5),
                     labels = as.character(1:5),
                     expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = "Zielvariable", y = "Wert", fill = "Gruppe") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "#545454", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.ticks = element_line(color = "#545454", linewidth = 0.3),
    axis.ticks.length = unit(0.2, "cm")
  )

ggsave("../output/images/target_variable_distribution_mc_mn_vs_ea_id.png", p_violin, 
       width = 12, height = 7, dpi = 300, bg = "white")

print("Chart saved: target_variable_distribution_mc_mn_vs_ea_id.png")

# Print summary statistics
summary_stats <- plot_data %>%
  group_by(Variable, Gruppe) %>%
  summarise(
    N = n(),
    Mean = round(mean(Score, na.rm = TRUE), 2),
    SD = round(sd(Score, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(summary_stats) 

# Einzelner Violinplot für MC01
if ("MC01" %in% names(data)) {
  plot_data_mc <- data %>%
    filter(FINISHED == 1) %>%
    mutate(Gruppe = ifelse(AB01 == 1, "KI", "Mensch")) %>%
    select(Gruppe, MC01) %>%
    filter(!is.na(MC01))

  p_mc <- ggplot(plot_data_mc, aes(x = Gruppe, y = MC01, fill = Gruppe)) +
    geom_violin(trim = FALSE, color = "#545454", linewidth = 0.5, alpha = 0.9, scale = "width", width = 0.7) +
    scale_fill_manual(values = c("KI" = "#89cdcf", "Mensch" = "#fbb9c2")) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5), labels = as.character(1:5), expand = expansion(mult = c(0.01, 0.02))) +
    labs(x = "Gruppe", y = "MC01", fill = "Gruppe") +
    theme_minimal() +
    theme(
      text = element_text(family = "Times New Roman"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "none",
      axis.text.x = element_text(size = 12, color = "black"),
      axis.text.y = element_text(size = 11, color = "black"),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.ticks = element_line(color = "#545454", linewidth = 0.3),
      axis.ticks.length = unit(0.2, "cm")
    )
  ggsave("../output/images/violinplot_mc01.png", p_mc, width = 5, height = 7, dpi = 300, bg = "white")
  print("Chart saved: violinplot_mc01.png")
} 

# Violinplot für alle MN-Items in einem Diagramm
mn_items <- grep("^MN01_\\d{2}$", names(data), value = TRUE)
if (length(mn_items) > 0) {
  plot_data_mn_all <- data %>%
    filter(FINISHED == 1) %>%
    mutate(Gruppe = ifelse(AB01 == 1, "KI", "Mensch")) %>%
    select(Gruppe, all_of(mn_items)) %>%
    pivot_longer(cols = all_of(mn_items), names_to = "Item", values_to = "Score") %>%
    filter(!is.na(Score))

  p_mn_all <- ggplot(plot_data_mn_all, aes(x = Item, y = Score, fill = Gruppe)) +
    geom_violin(position = position_dodge(width = 0.8), trim = FALSE, color = "#545454", linewidth = 0.5, alpha = 0.9, scale = "width", width = 0.7) +
    scale_fill_manual(values = c("KI" = "#89cdcf", "Mensch" = "#fbb9c2")) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5), labels = as.character(1:5), expand = expansion(mult = c(0.01, 0.02))) +
    labs(x = "MN-Item", y = "Wert", fill = "Gruppe") +
    theme_minimal() +
    theme(
      text = element_text(family = "Times New Roman"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11, color = "black"),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.ticks = element_line(color = "#545454", linewidth = 0.3),
      axis.ticks.length = unit(0.2, "cm")
    )
  ggsave("../output/images/violinplot_MN_items.png", p_mn_all, width = 10, height = 7, dpi = 300, bg = "white")
  print("Chart saved: violinplot_MN_items.png")
} 

# Violinplot für alle VS-Items in einem Diagramm
vs_items <- grep("^VS01_\\d{2}$", names(data), value = TRUE)
if (length(vs_items) > 0) {
  plot_data_vs_all <- data %>%
    filter(FINISHED == 1) %>%
    mutate(Gruppe = ifelse(AB01 == 1, "KI", "Mensch")) %>%
    select(Gruppe, all_of(vs_items)) %>%
    pivot_longer(cols = all_of(vs_items), names_to = "Item", values_to = "Score") %>%
    filter(!is.na(Score))

  p_vs_all <- ggplot(plot_data_vs_all, aes(x = Item, y = Score, fill = Gruppe)) +
    geom_violin(position = position_dodge(width = 0.8), trim = FALSE, color = "#545454", linewidth = 1, alpha = 0.7, scale = "width", width = 0.7) +
    scale_fill_manual(values = c("KI" = "#1f78b4", "Mensch" = "#e31a1c")) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5), labels = as.character(1:5), expand = expansion(mult = c(0.01, 0.02))) +
    labs(x = "VS-Item", y = "Wert", fill = "Gruppe") +
    theme_minimal() +
    theme(
      text = element_text(family = "Times New Roman"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11, color = "black"),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.ticks = element_line(color = "#545454", linewidth = 0.3),
      axis.ticks.length = unit(0.2, "cm")
    )
  ggsave("../output/images/violinplot_VS_items.png", p_vs_all, width = 10, height = 7, dpi = 300, bg = "white")
  print("Chart saved: violinplot_VS_items.png")
} 

# 1. Gesamtplot für alle Zielvariablen (MC, MN, VS, EA, ID) mit Suffix _neu
plot_data_neu <- scale_data %>%
  pivot_longer(
    cols = c(MC, MN, VS, EA, ID),
    names_to = "Variable",
    values_to = "Score"
  ) %>%
  mutate(
    Variable = factor(Variable, levels = c("MC", "MN", "VS", "EA", "ID"))
  ) %>%
  filter(!is.na(Score))

plot_data_split_neu <- plot_data_neu %>%
  mutate(
    var_numeric = as.numeric(Variable),
    x_pos = case_when(
      Gruppe == "KI" ~ var_numeric - 0.2,
      Gruppe == "Mensch" ~ var_numeric + 0.2
    )
  )

p_violin_neu <- ggplot() +
  geom_violin(data = plot_data_split_neu %>% filter(Gruppe == "KI"),
              aes(x = x_pos, y = Score, fill = Gruppe, group = Variable),
              color = "#545454", 
              linewidth = 0.5,
              alpha = 0.9,
              trim = FALSE,
              scale = "width",
              width = 0.4) +
  geom_violin(data = plot_data_split_neu %>% filter(Gruppe == "Mensch"),
              aes(x = x_pos, y = Score, fill = Gruppe, group = Variable),
              color = "#545454", 
              linewidth = 0.5,
              alpha = 0.9,
              trim = FALSE,
              scale = "width",
              width = 0.4) +
  scale_fill_manual(values = c("KI" = "#89cdcf", "Mensch" = "#fbb9c2")) +
  scale_x_continuous(breaks = 1:5, 
                     labels = c("MC", "MN", "VS", "EA", "ID"),
                     limits = c(0.5, 5.5)) +
  scale_y_continuous(breaks = 1:5, limits = c(1, 5),
                     labels = as.character(1:5),
                     expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = "Zielvariable", y = "Wert", fill = "Gruppe") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#e0e0e0", linewidth = 0.3),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "#545454", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.ticks = element_line(color = "#545454", linewidth = 0.3),
    axis.ticks.length = unit(0.2, "cm")
  )
ggsave("../output/images/violinplot_zielvariablen_neu.png", p_violin_neu, width = 12, height = 7, dpi = 300, bg = "white")
print("Chart saved: violinplot_zielvariablen_neu.png")

# 2. Einzelplots für alle Untervariablen pro Variablengruppe (MN, VS, EA, ID)
item_groups <- list(
  MN = grep("^MN01_\\d{2}$", names(data), value = TRUE),
  VS = grep("^VS01_\\d{2}$", names(data), value = TRUE),
  EA = grep("^EA01_\\d{2}$", names(data), value = TRUE),
  ID = grep("^ID01_\\d{2}$", names(data), value = TRUE)
)

for (group in names(item_groups)) {
  items <- item_groups[[group]]
  if (length(items) > 0) {
    plot_data_items <- data %>%
      filter(FINISHED == 1) %>%
      mutate(Gruppe = ifelse(AB01 == 1, "KI", "Mensch")) %>%
      select(Gruppe, all_of(items)) %>%
      pivot_longer(cols = all_of(items), names_to = "Item", values_to = "Score") %>%
      filter(!is.na(Score))

    p_items <- ggplot(plot_data_items, aes(x = Item, y = Score, fill = Gruppe)) +
      geom_violin(position = position_dodge(width = 0.8), trim = FALSE, color = "#545454", linewidth = 0.5, alpha = 0.9, scale = "width", width = 0.7) +
      scale_fill_manual(values = c("KI" = "#89cdcf", "Mensch" = "#fbb9c2")) +
      scale_y_continuous(breaks = 1:5, limits = c(1, 5), labels = as.character(1:5), expand = expansion(mult = c(0.01, 0.02))) +
      labs(x = paste0(group, "-Item"), y = "Wert", fill = "Gruppe") +
      theme_minimal() +
      theme(
        text = element_text(family = "Times New Roman"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom",
        axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.title.x = element_text(size = 12, margin = margin(t = 10)),
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),
        axis.ticks = element_line(color = "#545454", linewidth = 0.3),
        axis.ticks.length = unit(0.2, "cm")
      )
    ggsave(paste0("../output/images/violinplot_", group, "_items_neu.png"), p_items, width = 10, height = 7, dpi = 300, bg = "white")
    print(paste("Chart saved:", paste0("violinplot_", group, "_items_neu.png")))
  }
} 

# Violinplot für KI01-Items (KI01_01 bis KI01_04)
ki01_items <- grep("^KI01_\\d{2}$", names(data), value = TRUE)
if (length(ki01_items) > 0) {
  plot_data_ki01 <- data %>%
    filter(FINISHED == 1) %>%
    mutate(Gruppe = ifelse(AB01 == 1, "KI", "Mensch")) %>%
    select(Gruppe, all_of(ki01_items)) %>%
    pivot_longer(cols = all_of(ki01_items), names_to = "Item", values_to = "Score") %>%
    filter(!is.na(Score))

  p_ki01 <- ggplot(plot_data_ki01, aes(x = Item, y = Score, fill = Gruppe)) +
    geom_violin(position = position_dodge(width = 0.8), trim = FALSE, color = "#545454", linewidth = 0.5, alpha = 0.9, scale = "width", width = 0.7) +
    scale_fill_manual(values = c("KI" = "#89cdcf", "Mensch" = "#fbb9c2")) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5), labels = as.character(1:5), expand = expansion(mult = c(0.01, 0.02))) +
    labs(x = "KI01-Item", y = "Wert", fill = "Gruppe") +
    theme_minimal() +
    theme(
      text = element_text(family = "Times New Roman"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11, color = "black"),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.ticks = element_line(color = "#545454", linewidth = 0.3),
      axis.ticks.length = unit(0.2, "cm")
    )
  ggsave("../output/images/violinplot_KI01_items_neu.png", p_ki01, width = 7, height = 7, dpi = 300, bg = "white")
  print("Chart saved: violinplot_KI01_items_neu.png")
} 

# Violinplot für KI02-Items (KI02_01 bis KI02_08)
ki02_items <- grep("^KI02_\\d{2}$", names(data), value = TRUE)
if (length(ki02_items) > 0) {
  plot_data_ki02 <- data %>%
    filter(FINISHED == 1) %>%
    mutate(Gruppe = ifelse(AB01 == 1, "KI", "Mensch")) %>%
    select(Gruppe, all_of(ki02_items)) %>%
    pivot_longer(cols = all_of(ki02_items), names_to = "Item", values_to = "Score") %>%
    filter(!is.na(Score))

  p_ki02 <- ggplot(plot_data_ki02, aes(x = Item, y = Score, fill = Gruppe)) +
    geom_violin(position = position_dodge(width = 0.8), trim = FALSE, color = "#545454", linewidth = 0.5, alpha = 0.9, scale = "width", width = 0.7) +
    scale_fill_manual(values = c("KI" = "#89cdcf", "Mensch" = "#fbb9c2")) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5), labels = as.character(1:5), expand = expansion(mult = c(0.01, 0.02))) +
    labs(x = "KI02-Item", y = "Wert", fill = "Gruppe") +
    theme_minimal() +
    theme(
      text = element_text(family = "Times New Roman"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11, color = "black"),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.ticks = element_line(color = "#545454", linewidth = 0.3),
      axis.ticks.length = unit(0.2, "cm")
    )
  ggsave("../output/images/violinplot_KI02_items.png", p_ki02, width = 10, height = 7, dpi = 300, bg = "white")
  print("Chart saved: violinplot_KI02_items.png")
} 

# Balkendiagramm zur Gruppengröße (KI vs Mensch)
group_size_data <- data %>%
  filter(FINISHED == 1) %>%
  mutate(Gruppe = ifelse(AB01 == 1, "KI", "Mensch")) %>%
  group_by(Gruppe) %>%
  summarise(n = n(), .groups = "drop")

p_group_size <- ggplot(group_size_data, aes(x = Gruppe, y = n, fill = Gruppe)) +
  geom_col(width = 0.6, color = "#545454", linewidth = 0.3) +
  geom_text(aes(label = n), vjust = -0.3, size = 5, family = "Times New Roman") +
  scale_fill_manual(values = c("KI" = "#89cdcf", "Mensch" = "#fbb9c2")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Gruppe", y = "Anzahl", fill = "Gruppe") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none",
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    axis.ticks = element_line(color = "#545454", linewidth = 0.3),
    axis.ticks.length = unit(0.2, "cm")
  )
ggsave("../output/images/balkendiagramm_gruppengroesse.png", p_group_size, width = 5, height = 7, dpi = 300, bg = "white")
print("Chart saved: balkendiagramm_gruppengroesse.png") 

# Violinplot für alle SE01-Items
se01_items <- grep("^SE01_\\d{2}$", names(data), value = TRUE)
if (length(se01_items) > 0) {
  plot_data_se01 <- data %>%
    filter(FINISHED == 1) %>%
    mutate(Gruppe = ifelse(AB01 == 1, "KI", "Mensch")) %>%
    select(Gruppe, all_of(se01_items)) %>%
    pivot_longer(cols = all_of(se01_items), names_to = "Item", values_to = "Score") %>%
    filter(!is.na(Score))

  p_se01 <- ggplot(plot_data_se01, aes(x = Item, y = Score, fill = Gruppe)) +
    geom_violin(position = position_dodge(width = 0.8), trim = FALSE, color = "#545454", linewidth = 0.5, alpha = 0.9, scale = "width", width = 0.7) +
    scale_fill_manual(values = c("KI" = "#89cdcf", "Mensch" = "#fbb9c2")) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5), labels = as.character(1:5), expand = expansion(mult = c(0.01, 0.02))) +
    labs(x = "SE01-Item", y = "Wert", fill = "Gruppe") +
    theme_minimal() +
    theme(
      text = element_text(family = "Times New Roman"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "bottom",
      axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11, color = "black"),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10)),
      axis.ticks = element_line(color = "#545454", linewidth = 0.3),
      axis.ticks.length = unit(0.2, "cm")
    )
  ggsave("../output/images/violinplot_SE01_items.png", p_se01, width = 10, height = 7, dpi = 300, bg = "white")
  print("Chart saved: violinplot_SE01_items.png")
} 

# Balkendiagramm: Social Media Netzwerke (SE03_01-07 + SE04_01, Layout wie Violinplot, inkl. Varianten und zusätzlicher Netzwerke)
se03_items <- c("SE03_01", "SE03_02", "SE03_03", "SE03_04", "SE03_05", "SE03_06", "SE03_07")
se03_names <- c("Instagram", "TikTok", "Facebook", "Snapchat", "Youtube", "X", "LinkedIn", "Discord", "Pinterest", "Reddit")
farben <- c("#89cdcf", "#fbb9c2", "#b3b3b3", "#f7e1a0", "#b2d7e6", "#e6b2d7", "#b2e6b7", "#c2b2e6", "#e6d7b2", "#e6b2b2")

# SE03: Häufigkeit der Nennung (1)
se03_counts <- data.frame(Netzwerk = se03_names, Nennungen = 0)
if (all(se03_items %in% names(data))) {
  n_se03 <- data %>%
    filter(FINISHED == 1) %>%
    select(all_of(se03_items)) %>%
    summarise(across(everything(), ~sum(. == 1, na.rm = TRUE))) %>%
    as.numeric()
  se03_counts$Nennungen[1:7] <- n_se03
}

# SE04: Häufigkeit der Nennung als Freitext (inkl. Varianten)
if ("SE04_01" %in% names(data)) {
  se04_map <- list(
    Instagram = c("instagram", "insta"),
    TikTok = c("tiktok", "tik tok", "tik-tok"),
    Facebook = c("facebook", "fb"),
    Snapchat = c("snapchat", "snap"),
    Youtube = c("youtube", "you tube", "yt"),
    X = c("x", "twitter", "x (twitter)", "twitter (x)", "twitter/x", "x/twitter"),
    LinkedIn = c("linkedin", "linked in", "linked-in"),
    Discord = c("discord"),
    Pinterest = c("pinterest"),
    Reddit = c("reddit")
  )
  se04_vec <- data %>%
    filter(FINISHED == 1) %>%
    pull(SE04_01)
  se04_vec <- tolower(trimws(se04_vec))
  for (netz in names(se04_map)) {
    muster <- se04_map[[netz]]
    nennungen <- sum(sapply(muster, function(m) sum(grepl(m, se04_vec, fixed = TRUE))))
    idx <- which(se03_counts$Netzwerk == netz)
    if (length(idx) == 1) {
      se03_counts$Nennungen[idx] <- se03_counts$Nennungen[idx] + nennungen
    }
  }
}

plot_data_se03 <- se03_counts
plot_data_se03$Netzwerk <- factor(plot_data_se03$Netzwerk, levels = se03_names)

p_se03 <- ggplot(plot_data_se03, aes(x = Netzwerk, y = Nennungen, fill = Netzwerk)) +
  geom_col(width = 0.6, color = "#545454", linewidth = 0.3) +
  geom_text(aes(label = Nennungen), vjust = -0.3, size = 5, family = "Times New Roman") +
  scale_fill_manual(values = farben, name = "Netzwerk") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Social Media Netzwerk", y = "Häufigkeit der Nennung") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 13),
    axis.text.y = element_text(size = 13),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave("../output/images/balkendiagramm_SE03_netzwerke.png", p_se03, width = 12, height = 6, dpi = 300) 

# Clusteranalyse (k=3) auf Skalenmittelwerten (MN, VS, EA, ID)
set.seed(123)
cluster_data <- data %>%
  filter(FINISHED == 1) %>%
  mutate(
    MN = rowMeans(select(., MN01_01:MN01_07), na.rm = TRUE),
    VS = rowMeans(select(., VS01_01:VS01_08), na.rm = TRUE),
    EA = rowMeans(select(., EA01_01:EA01_05), na.rm = TRUE),
    ID = rowMeans(select(., ID01_01:ID01_04), na.rm = TRUE)
  ) %>%
  select(MN, VS, EA, ID) %>%
  na.omit()

# k-means Clusteranalyse (k=3)
kmeans_res <- kmeans(cluster_data, centers = 3, nstart = 25)
cluster_data$Cluster <- factor(kmeans_res$cluster)

# Mittelwerte pro Cluster und Skala
cluster_means <- cluster_data %>%
  pivot_longer(cols = c(MN, VS, EA, ID), names_to = "Variable", values_to = "Score") %>%
  group_by(Cluster, Variable) %>%
  summarise(Mean = mean(Score), .groups = "drop")

# Farben für Cluster (kräftig)
cluster_colors <- c("1" = "#f1c683", "2" = "#b84d5c", "3" = "#4da86e")

# Balkendiagramm der Cluster-Mittelwerte (sichtbare Balken!)
p_cluster <- ggplot(cluster_means, aes(x = Variable, y = Mean, fill = Cluster)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "#545454", linewidth = 0.3, alpha = 1) +
  geom_text(aes(label = round(Mean, 2)), position = position_dodge(width = 0.7), vjust = -0.3, size = 5, family = "Times New Roman") +
  scale_fill_manual(values = cluster_colors, name = "Cluster") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(1, 5), breaks = 1:5) +
  labs(x = "Variablengruppe", y = "Mittelwert", fill = "Cluster") +
  theme_classic() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title = element_text(size = 15),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )
ggsave("../output/images/egalbalkendiagramm_Cluster_Mittelwerte.png", p_cluster, width = 8, height = 6, dpi = 300) 

# Clustergrößen und Mittelwerte ausgeben
write.csv(cluster_means, "cluster_means.csv", row.names = FALSE)
cat("\nCluster-Mittelwerte pro Skala:\n")
print(cluster_means)
cat("\nClustergrößen:\n")
print(table(cluster_data$Cluster)) 

# APA-Tabelle: SD, Schiefe, Kurtosis, Shapiro-Wilk Test für MN, VS, EA, ID, KI01, KI02
library(e1071) # für skewness, kurtosis

# Erstelle die Kennwerte-Tabelle korrekt
apa_vars <- list(
  MN = rowMeans(select(data, starts_with("MN01_")), na.rm = TRUE),
  VS = rowMeans(select(data, starts_with("VS01_")), na.rm = TRUE),
  EA = rowMeans(select(data, starts_with("EA01_")), na.rm = TRUE),
  ID = rowMeans(select(data, starts_with("ID01_")), na.rm = TRUE),
  KI01 = rowMeans(select(data, starts_with("KI01_")), na.rm = TRUE),
  KI02 = rowMeans(select(data, starts_with("KI02_")), na.rm = TRUE)
)

# Erstelle die Tabelle Schritt für Schritt
apa_results <- list()
for (var_name in names(apa_vars)) {
  x <- apa_vars[[var_name]]
  x <- x[!is.na(x)]
  
  # Shapiro-Wilk Test für Normalverteilung
  shapiro_test <- shapiro.test(x)
  
  apa_results[[var_name]] <- data.frame(
    Variable = var_name,
    SD = round(sd(x), 2),
    Schiefe = round(e1071::skewness(x, na.rm=TRUE, type=2), 2),
    Kurtosis = round(e1071::kurtosis(x, na.rm=TRUE, type=2), 2),
    Shapiro_W = round(shapiro_test$statistic, 3),
    Shapiro_p = signif(shapiro_test$p.value, 3),
    stringsAsFactors = FALSE
  )
}

# Kombiniere alle Ergebnisse
apa_table <- do.call(rbind, apa_results)
rownames(apa_table) <- NULL

write.csv(apa_table, "apa_tabelle_kennwerte.csv", row.names = FALSE)
cat("\nAPA-Tabelle Kennwerte:\n")
print(apa_table) 

# APA-Tabelle als PNG im klassischen APA-Layout (Linien nur oben/unten Kopfzeile und unten letzte Zeile, kein Hintergrund)
library(grid)
library(gridExtra)
apa_table <- read.csv("apa_tabelle_kennwerte.csv")
apa_tab <- tableGrob(
  apa_table,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 14),
      bg_params = list(fill = NA)
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 15, fontface = 2),
      bg_params = list(fill = NA)
    ),
    padding = unit(c(8, 4), "mm")
  )
)
# Linien APA-Style: oben Kopfzeile, unter Kopfzeile (kein Strich in der Mitte), unter letzter Zeile
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
png("../output/images/apa_tabelle_kennwerte.png", width = 900, height = 350, res = 120)
grid::grid.newpage()
grid::grid.draw(apa_tab)
dev.off() 

# Hilfsfunktionen und Gruppenzuordnung für reduzierte Tabelle
group <- ifelse(data$AB01 == 1, 1, 0) # 1=KI, 0=Mensch
alpha_fun <- function(df) if (ncol(df) > 1) psych::alpha(df)$total$raw_alpha else NA
cohen_d <- function(x, g) {
  m1 <- mean(x[g==1], na.rm=TRUE); m2 <- mean(x[g==0], na.rm=TRUE)
  s1 <- sd(x[g==1], na.rm=TRUE); s2 <- sd(x[g==0], na.rm=TRUE)
  n1 <- sum(g==1 & !is.na(x)); n2 <- sum(g==0 & !is.na(x))
  sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
  (m1-m2)/sp
}

# Tabelle: Cronbachs Alpha, p-Wert, Cohen's d für MN, VS, EA, ID (ohne KI01 & KI02)
scales_reduced <- list(
  MN = grep("^MN01_", names(data), value=TRUE),
  VS = grep("^VS01_", names(data), value=TRUE),
  EA = grep("^EA01_", names(data), value=TRUE),
  ID = grep("^ID01_", names(data), value=TRUE)
)
res_reduced <- lapply(names(scales_reduced), function(s) {
  items <- scales_reduced[[s]]
  dat <- data[, items]
  alpha <- alpha_fun(dat)
  mw <- rowMeans(dat, na.rm=TRUE)
  ttest <- t.test(mw ~ group)
  d <- cohen_d(mw, group)
  c(Alpha = round(alpha, 2), p = signif(ttest$p.value, 2), d = round(d, 2))
})
tab_reduced <- do.call(rbind, res_reduced)
colnames(tab_reduced) <- c("Cronbachs Alpha", "p-Wert", "Cohen's d")
rownames(tab_reduced) <- names(scales_reduced)
tab_reduced <- as.data.frame(tab_reduced)
write.csv(tab_reduced, "apa_tabelle_alpha_p_d_reduced.csv")
cat("\nTabelle Alpha, p, d (nur MN, VS, EA, ID):\n")
print(tab_reduced)

# PNG-Export im APA-Layout mit expliziter Variablen-Spalte
apa_tab2_df <- tab_reduced
apa_tab2_df$Variable <- rownames(apa_tab2_df)
apa_tab2_df <- apa_tab2_df[, c("Variable", "Cronbachs Alpha", "p-Wert", "Cohen's d")]
rownames(apa_tab2_df) <- NULL
apa_tab2 <- tableGrob(
  apa_tab2_df,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 14),
      bg_params = list(fill = NA)
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 15, fontface = 2),
      bg_params = list(fill = NA)
    ),
    padding = unit(c(8, 4), "mm")
  )
)
# Linien APA-Style: oben Kopfzeile, unter Kopfzeile, unter letzter Zeile
apa_tab2 <- gtable::gtable_add_grob(
  apa_tab2,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 2)),
  t = 1, b = 1, l = 1, r = ncol(apa_tab2)
)
apa_tab2 <- gtable::gtable_add_grob(
  apa_tab2,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
  t = 2, b = 2, l = 1, r = ncol(apa_tab2)
)
apa_tab2 <- gtable::gtable_add_grob(
  apa_tab2,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
  t = nrow(apa_tab2), b = nrow(apa_tab2), l = 1, r = ncol(apa_tab2)
)
png("../output/images/apa_tabelle_alpha_p_d_reduced.png", width = 900, height = 250, res = 120)
grid::grid.newpage()
grid::grid.draw(apa_tab2)
dev.off() 

# MANOVA für MN, VS, EA, ID (Gruppe = AB01)
manova_data <- data.frame(
  Gruppe = factor(ifelse(data$AB01 == 1, "KI", "Mensch")),
  MN = rowMeans(select(data, starts_with("MN01_")), na.rm = TRUE),
  VS = rowMeans(select(data, starts_with("VS01_")), na.rm = TRUE),
  EA = rowMeans(select(data, starts_with("EA01_")), na.rm = TRUE),
  ID = rowMeans(select(data, starts_with("ID01_")), na.rm = TRUE)
)
manova_data <- na.omit(manova_data)
manova_res <- summary.aov(manova(cbind(MN, VS, EA, ID) ~ Gruppe, data = manova_data))
manova_tab <- do.call(rbind, lapply(manova_res, function(x) {
  SS <- as.data.frame(x[[1]])
  if ("Gruppe" %in% rownames(SS)) {
    Fval <- suppressWarnings(as.numeric(SS["Gruppe", "F value"]))
    pval <- suppressWarnings(as.numeric(SS["Gruppe", "Pr(>F)"]))
    eta2 <- suppressWarnings(as.numeric(SS["Gruppe", "Sum Sq"])) / sum(suppressWarnings(as.numeric(SS$"Sum Sq")))
  } else {
    Fval <- NA; pval <- NA; eta2 <- NA
  }
  c(F = round(Fval, 2), p = signif(pval, 2), Eta2 = round(eta2, 2))
}))
colnames(manova_tab) <- c("F-Wert", "p-Wert", "Eta²")
manova_tab <- as.data.frame(manova_tab)
manova_tab$Variable <- rownames(manova_tab)
manova_tab <- manova_tab[, c("Variable", "F-Wert", "p-Wert", "Eta²")]
write.csv(manova_tab, "apa_tabelle_manova.csv", row.names = FALSE)
cat("\nMANOVA-Tabelle:\n")
print(manova_tab) 

# =============================================================================
# MANOVA & SIGNIFIKANZ-TABELLE - ALLE ZIELVARIABLEN
# =============================================================================

# Load required packages for MANOVA
if (!require(car)) install.packages("car")
if (!require(psych)) install.packages("psych")
library(car)
library(psych)

# Prepare data for MANOVA analysis
manova_data_full <- data %>%
  filter(FINISHED == 1) %>%
  mutate(
    Gruppe = factor(ifelse(AB01 == 1, "KI", "Mensch")),
    # Calculate scale means
    MN = rowMeans(select(., starts_with("MN01_")), na.rm = TRUE),
    VS = rowMeans(select(., starts_with("VS01_")), na.rm = TRUE),
    EA = rowMeans(select(., starts_with("EA01_")), na.rm = TRUE),
    ID = rowMeans(select(., starts_with("ID01_")), na.rm = TRUE),
    KI01 = rowMeans(select(., starts_with("KI01_")), na.rm = TRUE),
    KI02 = rowMeans(select(., starts_with("KI02_")), na.rm = TRUE)
  ) %>%
  select(Gruppe, MN, VS, EA, ID, KI01, KI02) %>%
  na.omit()

# Function to calculate Cohen's d
cohen_d <- function(x, group) {
  m1 <- mean(x[group == "KI"], na.rm = TRUE)
  m2 <- mean(x[group == "Mensch"], na.rm = TRUE)
  s1 <- sd(x[group == "KI"], na.rm = TRUE)
  s2 <- sd(x[group == "Mensch"], na.rm = TRUE)
  n1 <- sum(group == "KI" & !is.na(x))
  n2 <- sum(group == "Mensch" & !is.na(x))
  sp <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
  (m1-m2)/sp
}

# Function to calculate Cronbach's Alpha
alpha_fun <- function(df) {
  if (ncol(df) > 1) {
    psych::alpha(df)$total$raw_alpha
  } else {
    NA
  }
}

# Create comprehensive results table
variables <- c("MN", "VS", "EA", "ID", "KI01", "KI02")
results_list <- list()

for (var in variables) {
  # Get individual items for alpha calculation
  if (var == "MN") {
    items <- grep("^MN01_", names(data), value = TRUE)
  } else if (var == "VS") {
    items <- grep("^VS01_", names(data), value = TRUE)
  } else if (var == "EA") {
    items <- grep("^EA01_", names(data), value = TRUE)
  } else if (var == "ID") {
    items <- grep("^ID01_", names(data), value = TRUE)
  } else if (var == "KI01") {
    items <- grep("^KI01_", names(data), value = TRUE)
  } else if (var == "KI02") {
    items <- grep("^KI02_", names(data), value = TRUE)
  }
  
  # Calculate alpha
  if (length(items) > 1) {
    alpha_val <- alpha_fun(data[items])
  } else {
    alpha_val <- NA
  }
  
  # Get scale values
  scale_values <- manova_data_full[[var]]
  
  # Calculate means by group
  mean_ki <- mean(scale_values[manova_data_full$Gruppe == "KI"], na.rm = TRUE)
  mean_mensch <- mean(scale_values[manova_data_full$Gruppe == "Mensch"], na.rm = TRUE)
  
  # Calculate standard deviations
  sd_ki <- sd(scale_values[manova_data_full$Gruppe == "KI"], na.rm = TRUE)
  sd_mensch <- sd(scale_values[manova_data_full$Gruppe == "Mensch"], na.rm = TRUE)
  
  # T-test
  t_test <- t.test(scale_values ~ manova_data_full$Gruppe)
  
  # Cohen's d
  d_val <- cohen_d(scale_values, manova_data_full$Gruppe)
  
  # Store results
  results_list[[var]] <- c(
    Variable = var,
    Alpha = round(alpha_val, 3),
    M_KI = round(mean_ki, 2),
    M_Mensch = round(mean_mensch, 2),
    SD_KI = round(sd_ki, 2),
    SD_Mensch = round(sd_mensch, 2),
    t = round(t_test$statistic, 2),
    p = signif(t_test$p.value, 3),
    d = round(d_val, 2)
  )
}

# Combine results
results_df <- do.call(rbind, results_list)
results_df <- as.data.frame(results_df, stringsAsFactors = FALSE)

# MANOVA for all variables
manova_formula <- as.formula("cbind(MN, VS, EA, ID, KI01, KI02) ~ Gruppe")
manova_result <- summary(manova(manova_formula, data = manova_data_full))

# Extract MANOVA statistics - the output uses Pillai's Trace instead of Wilks Lambda
if ("Gruppe" %in% rownames(manova_result$stats)) {
  pillai_trace <- manova_result$stats["Gruppe", "Pillai"]
  manova_p <- manova_result$stats["Gruppe", "Pr(>F)"]
  manova_f <- manova_result$stats["Gruppe", "approx F"]
  num_df <- manova_result$stats["Gruppe", "num Df"]
  den_df <- manova_result$stats["Gruppe", "den Df"]
} else {
  # Fallback values
  pillai_trace <- NA
  manova_p <- NA
  manova_f <- NA
  num_df <- NA
  den_df <- NA
}

# Calculate additional MANOVA statistics
# Wilks Lambda = 1 - Pillai's Trace (approximately)
wilks_lambda <- 1 - pillai_trace

# Hotelling's Trace = Pillai's Trace / (1 - Pillai's Trace)
hotelling_trace <- pillai_trace / (1 - pillai_trace)

# Create MANOVA-only table
manova_stats <- data.frame(
  Statistic = c("Pillai's Trace", "Wilks Lambda", "Hotelling's Trace"),
  Value = c(round(pillai_trace, 3), round(wilks_lambda, 3), round(hotelling_trace, 3)),
  F_Value = c(round(manova_f, 2), round(manova_f, 2), round(manova_f, 2)),
  p_Value = c(signif(manova_p, 3), signif(manova_p, 3), signif(manova_p, 3)),
  df1 = c(num_df, num_df, num_df),
  df2 = c(den_df, den_df, den_df)
)

# Create individual variable table (without MANOVA stats)
individual_stats <- results_df[, c("Variable", "Alpha", "M_KI", "M_Mensch", "SD_KI", "SD_Mensch", 
                                   "t.t", "p", "d")]

# Rename columns for better readability
colnames(individual_stats) <- c("Variable", "α", "M_KI", "M_Mensch", "SD_KI", "SD_Mensch", 
                                "t", "p", "d")

# Save as CSV
write.csv(individual_stats, "einzelvariablen_tabelle.csv", row.names = FALSE)
write.csv(manova_stats, "manova_statistiken.csv", row.names = FALSE)

# Create APA-style table for display
cat("\n")
cat("================================================================================\n")
cat("MANOVA-STATISTIKEN (Gesamttest)\n")
cat("================================================================================\n")
print(manova_stats)

cat("\n")
cat("================================================================================\n")
cat("EINZELVARIABLEN-STATISTIKEN\n")
cat("================================================================================\n")
print(individual_stats)

# Create APA-style PNG table for MANOVA statistics
library(gridExtra)
library(grid)

# Create table grob with APA styling for MANOVA stats (original layout)
apa_table_manova <- tableGrob(
  manova_stats,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 12),
      bg_params = list(fill = NA)
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 13, fontface = "bold"),
      bg_params = list(fill = NA)
    ),
    padding = unit(c(6, 4), "mm")
  )
)

# Add APA-style lines - only top and bottom, no middle line
apa_table_manova <- gtable::gtable_add_grob(
  apa_table_manova,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), 
                       y0 = unit(1, "npc"), y1 = unit(1, "npc"), 
                       gp = gpar(lwd = 2)),
  t = 1, b = 1, l = 1, r = ncol(apa_table_manova)
)

apa_table_manova <- gtable::gtable_add_grob(
  apa_table_manova,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), 
                       y0 = unit(0, "npc"), y1 = unit(0, "npc"), 
                       gp = gpar(lwd = 2)),
  t = nrow(apa_table_manova), b = nrow(apa_table_manova), 
  l = 1, r = ncol(apa_table_manova)
)

# Save as PNG - original dimensions
png("../output/images/manova_statistiken.png", width = 800, height = 300, res = 120, bg = "white")
grid::grid.newpage()
grid::grid.draw(apa_table_manova)
dev.off()

# Create APA-style PNG table for individual variables
apa_table_individual <- tableGrob(
  individual_stats,
  rows = NULL,
  theme = ttheme_default(
    core = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 11),
      bg_params = list(fill = NA)
    ),
    colhead = list(
      fg_params = list(fontfamily = "Times New Roman", fontsize = 12, fontface = "bold"),
      bg_params = list(fill = NA)
    ),
    padding = unit(c(6, 4), "mm")
  )
)

png("../output/images/einzelvariablen_tabelle.png", width = 1000, height = 400, res = 120, bg = "white")
grid::grid.newpage()
grid::grid.draw(apa_table_individual)
dev.off()

cat("\n")
cat("================================================================================\n")
cat("TABELLEN ERFOLGREICH ERSTELLT:\n")
cat("• manova_statistiken.csv & .png (MANOVA-Statistiken)\n")
cat("• einzelvariablen_tabelle.csv & .png (Einzelvariablen-Statistiken)\n")
cat("================================================================================\n")

# Print MANOVA summary
cat("\nMANOVA-ZUSAMMENFASSUNG:\n")
cat("Pillai's Trace:", round(pillai_trace, 3), "\n")
cat("F-Wert:", round(manova_f, 2), "\n")
cat("p-Wert:", signif(manova_p, 3), "\n")
cat("Freiheitsgrade:", num_df, "/", den_df, "\n")

# =============================================================================
# ENDE DER ANALYSE
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("ANALYSE ABGESCHLOSSEN\n")
cat("================================================================================\n")
cat("Für Korrelationsanalysen siehe: ../correlations/correlation_analysis.R\n")
cat("================================================================================\n") 

# Pearson-Korrelation für MN, VS, EA, ID, SE01, SE02, SO01, SO02 (nur vorhandene Items)
kor_data <- data.frame(
  MN = rowMeans(select(data, starts_with("MN01_")), na.rm = TRUE),
  VS = rowMeans(select(data, starts_with("VS01_")), na.rm = TRUE),
  EA = rowMeans(select(data, starts_with("EA01_")), na.rm = TRUE),
  ID = rowMeans(select(data, starts_with("ID01_")), na.rm = TRUE),
  SE01 = rowMeans(select(data, any_of(c("SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06"))), na.rm = TRUE),
  SE02 = data$SE02_01,
  SO01 = data$SO01_01,
  SO02 = data$SO02
)
kor_data <- na.omit(kor_data)
# Korrelationsmatrix berechnen
kor_mat <- round(cor(kor_data, use = "pairwise.complete.obs", method = "pearson"), 2)
write.csv(kor_mat, "../output/images/korrelation_pearson.csv")
cat("\nPearson-Korrelationsmatrix:\n")
print(kor_mat)
# PNG-Export im APA-Layout
library(grid)
library(gridExtra)
apa_kor <- tableGrob(
  as.data.frame(kor_mat),
  rows = rownames(kor_mat),
  theme = ttheme_default(
    core = list(fg_params = list(fontfamily = "Times New Roman", fontsize = 13), bg_params = list(fill = NA)),
    colhead = list(fg_params = list(fontfamily = "Times New Roman", fontsize = 14, fontface = 2), bg_params = list(fill = NA)),
    padding = unit(c(6, 4), "mm")
  )
)
# Linien oben/unten Kopfzeile und unter letzter Zeile
apa_kor <- gtable::gtable_add_grob(
  apa_kor,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 2)),
  t = 1, b = 1, l = 1, r = ncol(apa_kor)
)
apa_kor <- gtable::gtable_add_grob(
  apa_kor,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
  t = 2, b = 2, l = 1, r = ncol(apa_kor)
)
apa_kor <- gtable::gtable_add_grob(
  apa_kor,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
  t = nrow(apa_kor), b = nrow(apa_kor), l = 1, r = ncol(apa_kor)
)
png("../output/images/korrelation_pearson.png", width = 900, height = 500, res = 120)
grid::grid.newpage()
grid::grid.draw(apa_kor)
dev.off()

# Übersicht: Anzahl gültiger Werte pro Zielvariable für Korrelation
cat("\nAnzahl gültiger Werte pro Variable (non-NA):\n")
var_names <- c("MN", "VS", "EA", "ID", "SE01", "SE02", "SO01", "SO02")
for (v in var_names) {
  if (v %in% names(kor_data)) {
    n_valid <- sum(!is.na(kor_data[[v]]))
    cat(v, ": ", n_valid, "\n")
  } else {
    cat(v, ": nicht im Datensatz\n")
  }
} 

# Korrelationsmatrix: Einfluss von Gruppe, Alter, Geschlecht, Vorwissen auf MN, VS, EA, ID
# Zielvariablen: MN, VS, EA, ID (Skalenmittelwerte)
zielvariablen <- data.frame(
  MN = rowMeans(select(data, starts_with("MN01_")), na.rm = TRUE),
  VS = rowMeans(select(data, starts_with("VS01_")), na.rm = TRUE),
  EA = rowMeans(select(data, starts_with("EA01_")), na.rm = TRUE),
  ID = rowMeans(select(data, starts_with("ID01_")), na.rm = TRUE)
)
# Einflussvariablen: alle SE01_*, SE02_*, SO01_*, SO02_*, AB01, Alter, Geschlecht
influ_vars <- c(
  grep("^SE01_", names(data), value = TRUE),
  grep("^SE02_", names(data), value = TRUE),
  grep("^SO01_", names(data), value = TRUE),
  grep("^SO02_", names(data), value = TRUE),
  "AB01", "SE04_01", "Geschlecht", "Alter"
)
influ_vars <- influ_vars[influ_vars %in% names(data)]
kor_data2 <- cbind(data[influ_vars], zielvariablen)
kor_data2[] <- lapply(kor_data2, function(x) as.numeric(as.character(x)))
kor_data2 <- na.omit(kor_data2)
# Korrelationsmatrix berechnen (nur Einflussvariablen vs. Zielvariablen)
kor_mat2 <- cor(kor_data2[, influ_vars], kor_data2[, c("MN", "VS", "EA", "ID")], use = "pairwise.complete.obs", method = "pearson")
kor_mat2 <- round(kor_mat2, 2)
write.csv(kor_mat2, "../output/images/korrelation_einflussmatrix.csv")
cat("\nKorrelationsmatrix Einflussvariablen vs. Zielvariablen:\n")
print(kor_mat2)
# PNG-Export im APA-Layout
library(grid)
library(gridExtra)
apa_kor2 <- tableGrob(
  as.data.frame(kor_mat2),
  rows = rownames(kor_mat2),
  theme = ttheme_default(
    core = list(fg_params = list(fontfamily = "Times New Roman", fontsize = 12), bg_params = list(fill = NA)),
    colhead = list(fg_params = list(fontfamily = "Times New Roman", fontsize = 13, fontface = 2), bg_params = list(fill = NA)),
    padding = unit(c(5, 3), "mm")
  )
)
# Linien oben/unten Kopfzeile und unter letzter Zeile
apa_kor2 <- gtable::gtable_add_grob(
  apa_kor2,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 2)),
  t = 1, b = 1, l = 1, r = ncol(apa_kor2)
)
apa_kor2 <- gtable::gtable_add_grob(
  apa_kor2,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
  t = 2, b = 2, l = 1, r = ncol(apa_kor2)
)
apa_kor2 <- gtable::gtable_add_grob(
  apa_kor2,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
  t = nrow(apa_kor2), b = nrow(apa_kor2), l = 1, r = ncol(apa_kor2)
)
png("../output/images/korrelation_einflussmatrix.png", width = 900, height = 600, res = 120)
grid::grid.newpage()
grid::grid.draw(apa_kor2)
dev.off() 

# Korrelationsmatrix: SE01_01, SE01_02, SE01_03, SE01_05, SE01_06, SE02_01, SO01_01, SO02, AB01 vs. MN, VS, EA, ID
kor_vars <- c("SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06", "SE02_01", "SO01_01", "SO02", "AB01")
ziel_vars <- c(
  MN = rowMeans(select(data, starts_with("MN01_")), na.rm = TRUE),
  VS = rowMeans(select(data, starts_with("VS01_")), na.rm = TRUE),
  EA = rowMeans(select(data, starts_with("EA01_")), na.rm = TRUE),
  ID = rowMeans(select(data, starts_with("ID01_")), na.rm = TRUE)
)
kor_data3 <- data[, kor_vars]
kor_data3[] <- lapply(kor_data3, function(x) as.numeric(as.character(x)))
kor_data3 <- cbind(kor_data3, ziel_vars)
kor_data3 <- na.omit(kor_data3)
kor_mat3 <- cor(kor_data3[, kor_vars], kor_data3[, c("MN", "VS", "EA", "ID")], use = "pairwise.complete.obs", method = "pearson")
kor_mat3 <- round(kor_mat3, 2)
write.csv(kor_mat3, "../output/images/korrelation_einflussmatrix_simuliert.csv")
cat("\nKorrelationsmatrix (simuliert):\n")
print(kor_mat3)
# PNG-Export im APA-Layout
library(grid)
library(gridExtra)
apa_kor3 <- tableGrob(
  as.data.frame(kor_mat3),
  rows = rownames(kor_mat3),
  theme = ttheme_default(
    core = list(fg_params = list(fontfamily = "Times New Roman", fontsize = 12), bg_params = list(fill = NA)),
    colhead = list(fg_params = list(fontfamily = "Times New Roman", fontsize = 13, fontface = 2), bg_params = list(fill = NA)),
    padding = unit(c(5, 3), "mm")
  )
)
# Linien oben/unten Kopfzeile und unter letzter Zeile
apa_kor3 <- gtable::gtable_add_grob(
  apa_kor3,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 2)),
  t = 1, b = 1, l = 1, r = ncol(apa_kor3)
)
apa_kor3 <- gtable::gtable_add_grob(
  apa_kor3,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
  t = 2, b = 2, l = 1, r = ncol(apa_kor3)
)
apa_kor3 <- gtable::gtable_add_grob(
  apa_kor3,
  grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
  t = nrow(apa_kor3), b = nrow(apa_kor3), l = 1, r = ncol(apa_kor3)
)
png("../output/images/korrelation_einflussmatrix_simuliert.png", width = 900, height = 600, res = 120)
grid::grid.newpage()
grid::grid.draw(apa_kor3)
dev.off() 

# --- APA-Korrelationsmatrix für simulierte Einflussvariablen vs. Zielvariablen ---
tryCatch({
  cat("\n[DEBUG] Starte Korrelationsmatrix-Block (simuliert)\n")
  kor_vars <- c("SE01_01", "SE01_02", "SE01_03", "SE01_05", "SE01_06", "SE02_01", "SO01_01", "SO02", "AB01")
  ziel_vars <- c(
    MN = rowMeans(select(data, starts_with("MN01_")), na.rm = TRUE),
    VS = rowMeans(select(data, starts_with("VS01_")), na.rm = TRUE),
    EA = rowMeans(select(data, starts_with("EA01_")), na.rm = TRUE),
    ID = rowMeans(select(data, starts_with("ID01_")), na.rm = TRUE)
  )
  cat("[DEBUG] kor_vars: ", paste(kor_vars, collapse=", "), "\n")
  cat("[DEBUG] Zielvariablen: MN, VS, EA, ID\n")
  kor_data3 <- data[, kor_vars]
  cat("[DEBUG] kor_data3 Spalten: ", paste(colnames(kor_data3), collapse=", "), "\n")
  kor_data3[] <- lapply(kor_data3, function(x) as.numeric(as.character(x)))
  kor_data3 <- cbind(kor_data3, ziel_vars)
  cat("[DEBUG] Nach cbind: ", paste(colnames(kor_data3), collapse=", "), "\n")
  cat("[DEBUG] Anzahl Zeilen vor na.omit: ", nrow(kor_data3), "\n")
  kor_data3 <- na.omit(kor_data3)
  cat("[DEBUG] Anzahl Zeilen nach na.omit: ", nrow(kor_data3), "\n")
  cat("[DEBUG] Prüfe Dimensions: Einfluss (", ncol(kor_data3[, kor_vars, drop=FALSE]), ") x Ziel (", ncol(kor_data3[, c('MN','VS','EA','ID'), drop=FALSE]), ")\n")
  cat("[DEBUG] NA-Anteil pro Einflussvariable:\n")
  for (v in kor_vars) cat(v, ": ", sum(is.na(kor_data3[[v]])), " NAs\n")
  cat("[DEBUG] NA-Anteil pro Zielvariable:\n")
  for (v in c('MN','VS','EA','ID')) cat(v, ": ", sum(is.na(kor_data3[[v]])), " NAs\n")
  kor_mat3 <- cor(kor_data3[, kor_vars, drop=FALSE], kor_data3[, c("MN", "VS", "EA", "ID"), drop=FALSE], use = "pairwise.complete.obs", method = "pearson")
  kor_mat3 <- round(kor_mat3, 2)
  write.csv(kor_mat3, "../output/images/korrelation_einflussmatrix_simuliert.csv")
  cat("\nKorrelationsmatrix (simuliert):\n")
  print(kor_mat3)
  # PNG-Export im APA-Layout
  library(grid)
  library(gridExtra)
  apa_kor3 <- tableGrob(
    as.data.frame(kor_mat3),
    rows = rownames(kor_mat3),
    theme = ttheme_default(
      core = list(fg_params = list(fontfamily = "Times New Roman", fontsize = 12), bg_params = list(fill = NA)),
      colhead = list(fg_params = list(fontfamily = "Times New Roman", fontsize = 13, fontface = 2), bg_params = list(fill = NA)),
      padding = unit(c(5, 3), "mm")
    )
  )
  # Linien oben/unten Kopfzeile und unter letzter Zeile
  apa_kor3 <- gtable::gtable_add_grob(
    apa_kor3,
    grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(1, "npc"), y1 = unit(1, "npc"), gp = gpar(lwd = 2)),
    t = 1, b = 1, l = 1, r = ncol(apa_kor3)
  )
  apa_kor3 <- gtable::gtable_add_grob(
    apa_kor3,
    grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
    t = 2, b = 2, l = 1, r = ncol(apa_kor3)
  )
  apa_kor3 <- gtable::gtable_add_grob(
    apa_kor3,
    grobs = segmentsGrob(x0 = unit(0, "npc"), x1 = unit(1, "npc"), y0 = unit(0, "npc"), y1 = unit(0, "npc"), gp = gpar(lwd = 2)),
    t = nrow(apa_kor3), b = nrow(apa_kor3), l = 1, r = ncol(apa_kor3)
  )
  png("../output/images/korrelation_einflussmatrix_simuliert.png", width = 900, height = 600, res = 120)
  grid::grid.newpage()
  grid::grid.draw(apa_kor3)
  dev.off()
  cat("[DEBUG] PNG erfolgreich gespeichert!\n")
}, error = function(e) cat("Fehler beim Erstellen der Korrelationsmatrix (simuliert): ", e$message, "\n")) 