# =============================================================================
# GT LIBRARY BEISPIEL - TABELLEN IM SCREENSHOT-STIL
# =============================================================================
# Erstellt Tabellen im exakten Layout der Tabelle 1 aus dem Screenshot
# Verwendet die gt (Grammar of Tables) Library für professionelle Tabellen

library(gt)
library(dplyr)

# =============================================================================
# BEISPIEL 1: EXAKTE NACHBILDUNG DER SCREENSHOT-TABELLE
# =============================================================================

cat("================================================================================\n")
cat("GT LIBRARY BEISPIEL - TABELLEN IM SCREENSHOT-STIL\n")
cat("================================================================================\n")

# Daten für die Beispieltabelle (wie im Screenshot)
age_group_data <- data.frame(
  Note = c("4", "5", "6", "Gesamt"),
  Jungen = c(115, 130, 117, 362),
  Mädchen = c(126, 119, 124, 369)
)

cat("\n=== BEISPIEL 1: EXAKTE NACHBILDUNG DER SCREENSHOT-TABELLE ===\n")

# Erstelle die Tabelle mit gt
table_1 <- age_group_data %>%
  gt() %>%
  tab_header(
    title = "Tabelle 1",
    subtitle = "Anzahl der Jungen und Mädchen per Altersgruppe"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(
      rows = c(1, nrow(age_group_data))
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(
      rows = nrow(age_group_data) - 1
    )
  ) %>%
  cols_align(
    align = "left",
    columns = Note
  ) %>%
  cols_align(
    align = "right",
    columns = c(Jungen, Mädchen)
  ) %>%
  tab_footnote(
    footnote = "In Anlehnung an Beispiel Buch, von M. Müller, 2019, S. 23. Copyright 2019 durch Springer.",
    placement = "left"
  ) %>%
  tab_options(
    table.font.size = px(12),
    table.width = px(400),
    column_labels.font.weight = "bold",
    data_row.padding = px(8),
    footnotes.padding = px(8)
  )

# Zeige die Tabelle
print(table_1)

# =============================================================================
# BEISPIEL 2: ANWENDUNG AUF IHRE CLUSTER-DATEN
# =============================================================================

cat("\n=== BEISPIEL 2: ANWENDUNG AUF IHRE CLUSTER-DATEN ===\n")

# Lade Ihre Daten
data <- read.delim("organized/data/Bereinigte Daten von WhatsApp Business.csv", 
                   fileEncoding = "UTF-16LE", stringsAsFactors = FALSE)

# Filter data (FINISHED=1)
data_processed <- data %>% filter(FINISHED == 1)

# Separate KI and Mensch groups
data_ki <- data_processed %>% filter(AB01 == 1)
data_mensch <- data_processed %>% filter(AB01 == 2)

# Define variable columns
mn_cols <- c("MN01_01", "MN01_02", "MN01_03", "MN01_04", "MN01_05", "MN01_06", "MN01_07")
vs_cols <- c("VS01_01", "VS01_02", "VS01_03", "VS01_04", "VS01_05", "VS01_06", "VS01_07", "VS01_08")
ea_cols <- c("EA01_01", "EA01_02", "EA01_03", "EA01_04", "EA01_05")
id_cols <- c("ID01_01", "ID01_02", "ID01_03", "ID01_04")

# Create composite scores
cluster_data_ki <- data.frame(
  VS = rowMeans(data_ki[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_ki[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_ki[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_ki[, ea_cols], na.rm = TRUE)
)

cluster_data_mensch <- data.frame(
  VS = rowMeans(data_mensch[, vs_cols], na.rm = TRUE),
  MN = rowMeans(data_mensch[, mn_cols], na.rm = TRUE),
  ID = rowMeans(data_mensch[, id_cols], na.rm = TRUE),
  EA = rowMeans(data_mensch[, ea_cols], na.rm = TRUE)
)

# Remove missing values
cluster_data_ki_clean <- cluster_data_ki[complete.cases(cluster_data_ki), ]
cluster_data_mensch_clean <- cluster_data_mensch[complete.cases(cluster_data_mensch), ]

# Perform clustering
set.seed(123)
kmeans_ki <- kmeans(cluster_data_ki_clean, centers = 3, nstart = 25)
kmeans_mensch <- kmeans(cluster_data_mensch_clean, centers = 3, nstart = 25)

# Add cluster assignments
cluster_data_ki_clean$cluster <- kmeans_ki$cluster
cluster_data_mensch_clean$cluster <- kmeans_mensch$cluster

# Calculate cluster means
ki_means <- cluster_data_ki_clean %>%
  group_by(cluster) %>%
  summarise(
    N = n(),
    VS = round(mean(VS, na.rm = TRUE), 3),
    MN = round(mean(MN, na.rm = TRUE), 3),
    ID = round(mean(ID, na.rm = TRUE), 3),
    EA = round(mean(EA, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  mutate(
    cluster_name = case_when(
      cluster == 1 ~ "Ambivalent",
      cluster == 2 ~ "KI-Offen",
      cluster == 3 ~ "KI-Skeptisch"
    )
  )

mensch_means <- cluster_data_mensch_clean %>%
  group_by(cluster) %>%
  summarise(
    N = n(),
    VS = round(mean(VS, na.rm = TRUE), 3),
    MN = round(mean(MN, na.rm = TRUE), 3),
    ID = round(mean(ID, na.rm = TRUE), 3),
    EA = round(mean(EA, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  mutate(
    cluster_name = case_when(
      cluster == 1 ~ "Emotional Distanziert",
      cluster == 2 ~ "Emotional Offen",
      cluster == 3 ~ "Ambivalent"
    )
  )

# Create KI-Gruppe table
ki_table <- ki_means %>%
  select(cluster_name, N, VS, MN, ID, EA) %>%
  gt() %>%
  tab_header(
    title = "Tabelle 2",
    subtitle = "Cluster-Mittelwerte der KI-Gruppe"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(
      rows = c(1, nrow(ki_means))
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(
      rows = nrow(ki_means) - 1
    )
  ) %>%
  cols_align(
    align = "left",
    columns = cluster_name
  ) %>%
  cols_align(
    align = "right",
    columns = c(N, VS, MN, ID, EA)
  ) %>%
  tab_footnote(
    footnote = "Anmerkung: VS = Vertrauen & Sympathie, MN = Menschlichkeit & Natürlichkeit, ID = Identifikation, EA = Emotionale Ansprache",
    placement = "left"
  ) %>%
  tab_options(
    table.font.size = px(12),
    
    table.width = px(600),
    column_labels.font.weight = "bold",
    data_row.padding = px(8),
    footnotes.padding = px(8)
  )

# Create Mensch-Gruppe table
mensch_table <- mensch_means %>%
  select(cluster_name, N, VS, MN, ID, EA) %>%
  gt() %>%
  tab_header(
    title = "Tabelle 3",
    subtitle = "Cluster-Mittelwerte der Mensch-Gruppe"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(
      rows = c(1, nrow(mensch_means))
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(
      rows = nrow(mensch_means) - 1
    )
  ) %>%
  cols_align(
    align = "left",
    columns = cluster_name
  ) %>%
  cols_align(
    align = "right",
    columns = c(N, VS, MN, ID, EA)
  ) %>%
  tab_footnote(
    footnote = "Anmerkung: VS = Vertrauen & Sympathie, MN = Menschlichkeit & Natürlichkeit, ID = Identifikation, EA = Emotionale Ansprache",
    placement = "left"
  ) %>%
  tab_options(
    table.font.size = px(12),
    
    table.width = px(600),
    column_labels.font.weight = "bold",
    data_row.padding = px(8),
    footnotes.padding = px(8)
  )

# Zeige die Tabellen
cat("\nKI-Gruppe Tabelle:\n")
print(ki_table)

cat("\nMensch-Gruppe Tabelle:\n")
print(mensch_table)

# =============================================================================
# BEISPIEL 3: ERWEITERTE FORMATIERUNG
# =============================================================================

cat("\n=== BEISPIEL 3: ERWEITERTE FORMATIERUNG ===\n")

# Erstelle eine erweiterte Tabelle mit zusätzlichen Formatierungen
extended_table <- age_group_data %>%
  gt() %>%
  tab_header(
    title = "Tabelle 4",
    subtitle = "Erweiterte Formatierung mit gt"
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(
      rows = c(1, nrow(age_group_data))
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(1)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(1)
    ),
    locations = cells_body(
      rows = nrow(age_group_data) - 1
    )
  ) %>%
  cols_align(
    align = "left",
    columns = Note
  ) %>%
  cols_align(
    align = "right",
    columns = c(Jungen, Mädchen)
  ) %>%
  tab_style(
    style = cell_text(
      weight = "bold",
      color = "black"
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_body(
      rows = nrow(age_group_data)
    )
  ) %>%
  tab_footnote(
    footnote = "In Anlehnung an Beispiel Buch, von M. Müller, 2019, S. 23. Copyright 2019 durch Springer.",
    placement = "left"
  ) %>%
  tab_options(
    table.font.size = px(12),
    
    table.width = px(400),
    column_labels.font.weight = "bold",
    data_row.padding = px(8),
    footnotes.padding = px(8),
    table.background.color = "white",
    table.border.top.color = "black",
    table.border.bottom.color = "black"
  )

print(extended_table)

# =============================================================================
# INSTALLATION UND VERWENDUNG
# =============================================================================

cat("\n================================================================================\n")
cat("INSTALLATION UND VERWENDUNG DER GT LIBRARY\n")
cat("================================================================================\n")

cat("\n1. INSTALLATION:\n")
cat("   install.packages('gt')\n")
cat("   library(gt)\n")

cat("\n2. HAUPTVORTEILE:\n")
cat("   ✓ Exaktes Layout wie im Screenshot\n")
cat("   ✓ Minimale horizontale Linien\n")
cat("   ✓ Keine vertikalen Linien\n")
cat("   ✓ Linksbündige Textspalten\n")
cat("   ✓ Rechtsbündige Zahlen\n")
cat("   ✓ Professionelle Fußnoten\n")
cat("   ✓ Export zu HTML, LaTeX, RTF\n")

cat("\n3. ALTERNATIVE LIBRARIES:\n")
cat("   • kableExtra: Auch gut, aber weniger flexibel\n")
cat("   • flextable: Gut für Word-Export\n")
cat("   • huxtable: Einfach, aber weniger Features\n")

cat("\n4. EMPFEHLUNG:\n")
cat("   Die gt Library ist die beste Wahl für Ihr Layout!\n")

cat("\n================================================================================\n")
cat("BEISPIEL ERFOLGREICH ERSTELLT\n")
cat("================================================================================\n") 