# WICHTIGE LERNINGS & REGELN

## Datenquellen & Filter:
- AB01 1=KI 2=Mensch
- Immer Bereinigte Daten von WhatsApp Business.csv nutzen
- Datensätze: Datensatz_simuliert.csv und Bereinigte Daten von WhatsApp Business.csv sind identisch nach FINISHED=1 Filter

## Cluster-Namen & Reihenfolge:
- **LOGISCHE Cluster-Namen-Zuordnung**: Basierend auf tatsächlichen Werten (höchste → mittlere → niedrigste)
- **KI-Gruppe**: KI-Offen (höchste Werte) → Ambivalent (mittlere Werte) → KI-Skeptisch (niedrigste Werte)
- **Mensch-Gruppe**: Emotional Offen (höchste Werte) → Ambivalent (mittlere Werte) → Emotional Distanziert (niedrigste Werte)
- **Dynamische Zuordnung**: Cluster werden nach Gesamtdurchschnitt der Variablen sortiert
- **KEINE feste Zuordnung** mehr - logische Zuordnung basierend auf Daten

## KRITISCHE ERKENNTNIS - CLUSTERING-INKONSISTENZ:
- **PROBLEM**: `cluster_prozent_tabelle.png` zeigt andere Werte als `ki_specific_apa.png` und `human_specific_apa.png`
- **URSACHE**: Unterschiedliche Clustering-Logik zwischen Scripts
- **Individual Scripts** (`ki_specific_cluster.R`, `human_specific_cluster.R`):
  - Verwenden `scale()` für Standardisierung vor Clustering
  - Dynamische Cluster-Zuordnung basierend auf `overall_mean`
- **Prozent-Tabelle Script** (`cluster_prozent_tabelle_logical.R`):
  - Verwendet rohe Werte ohne Standardisierung
  - Andere dynamische Zuordnung
- **LÖSUNG**: Alle Scripts müssen identische Clustering-Logik verwenden
- **ERWARTETE WERTE** (aus individual Scripts):
  - KI: Ambivalent (24), KI-Offen (19), KI-Skeptisch (21)
  - Mensch: Emotional Distanziert (16), Emotional Offen (31), Ambivalent (20)

## Qualitätssicherung:
- **Reihenfolge-Tests**: Alle Tests prüfen die korrekte Cluster-Reihenfolge
- **Werte-Konsistenz**: Numerische Werte müssen zwischen allen Tabellen identisch sein
- **Namen-Konsistenz**: Cluster-Namen müssen zwischen allen Tabellen identisch sein
- **CLUSTERING-KONSISTENZ**: Alle Scripts müssen identische Clustering-Parameter verwenden

## Häufige Fehler vermeiden:
- ❌ Feste Cluster-Zuordnung ohne Berücksichtigung der tatsächlichen Werte
- ❌ Unterschiedliche Reihenfolgen in verschiedenen Tabellen
- ❌ Inkonsistente Cluster-Namen zwischen Tabellen
- ❌ **UNTERSCHIEDLICHE CLUSTERING-LOGIK** zwischen Scripts
- ✅ Logische Zuordnung basierend auf tatsächlichen Werten verwenden
- ✅ Alle Tabellen gegen gleiche Datenquelle validieren
- ✅ Cluster nach Gesamtdurchschnitt der Variablen sortieren
- ✅ **IDENTISCHE CLUSTERING-PARAMETER** in allen Scripts

## Wichtige Hinweise:
- **ALLE TABELLEN NEU GENERIERT**: Mit logischer Cluster-Zuordnung basierend auf tatsächlichen Werten
- **Tests sind streng**: Nur die logische Reihenfolge wird akzeptiert
- **Keine Duplikate**: Alle PNG-Dateien sind eindeutig und konsistent
- **Logische Zuordnung**: KI-Offen hat jetzt die höchsten Werte, KI-Skeptisch die niedrigsten
- **Wissenschaftliche Fundierung**: Cluster-Analyse mit Elbow-Methode, Silhouette-Analyse und Signifikanz-Tests

## WICHTIGE LERNINGS:
- **Cluster-Namen müssen logisch sein**: KI-Offen sollte höhere Werte haben als KI-Skeptisch
- **Dynamische Zuordnung**: Cluster-Namen basierend auf tatsächlichen Daten, nicht festen Annahmen
- **Gesamtdurchschnitt**: Wichtig für die logische Zuordnung der Cluster
- **Konsistenz**: Alle Tabellen verwenden die gleiche logische Zuordnung

## WISSENSCHAFTLICHE CLUSTER-ANALYSE:
- **Optimale Cluster-Anzahl**: Elbow-Methode + Silhouette-Analyse verwenden
- **Cluster-Validierung**: Silhouette Width > 0.1 für akzeptable Qualität
- **Signifikanz-Tests**: ANOVA + Post-hoc Tests (Tukey HSD) für Cluster-Unterschiede
- **Effektstärken**: η² (Eta-squared) für praktische Bedeutsamkeit
- **Reproduzierbarkeit**: set.seed() für konsistente Ergebnisse
- **Standardisierung**: Z-Score-Standardisierung vor Clustering
- **Dokumentation**: Alle Parameter und Entscheidungen transparent dokumentieren

## DIAGRAMM-GENERIERUNG LEARNINGS:
- **Schriftart**: Times New Roman für alle Textelemente (`family = "Times New Roman"`)
- **Titel entfernen**: Für saubere Visualisierungen ohne Titel (`labs()` ohne `title`)
- **Exakte Farben**: Spezifische Hex-Codes verwenden, nicht Standard-Farben
  - Emotionale Ansprache: `#8DD3C8` (Hellblau/Türkis)
  - Identifikation: `#ABCD9B` (Hellgrün)
  - Menschlichkeit & Natürlichkeit: `#BE4B5A` (Rot)
  - Vertrauen & Sympathie: `#F1C682` (Gelb/Orange)
- **Balken-Style**: Schwarze Umrandung (`color = "black", linewidth = 0.3`)
- **Hintergrund**: Weißer Hintergrund ohne Rahmen (`panel.background = element_rect(fill = "white", color = NA)`)
- **Plot-Hintergrund**: Weißer Hintergrund ohne Rahmen (`plot.background = element_rect(fill = "white", color = NA)`)
- **Gitternetz**: Graue horizontale Linien (`panel.grid.major = element_line(color = "gray90")`)
- **Y-Achse**: 0-5 Skala mit ganzen Zahlen (`ylim(0, 5)`)
- **Gruppierung**: Dodge-Positionierung für gruppierte Balken (`position_dodge(width = 0.8)`)
- **Rahmen entfernen**: Alle Achsen-Linien und Ticks entfernen (`axis.line = element_blank()`, `axis.ticks = element_blank()`)
- **Margins**: Plot-Margins für Y-Achsen-Beschriftung anpassen (`plot.margin = margin(0, 0, 0, 20)`)

## TABELLEN-GENERIERUNG LEARNINGS:
- **Header-Padding**: Minimales Padding für Header (`padding = unit(c(3, 1), "mm")`)
- **Header-Höhe**: Ausreichende Höhe für Header (`heights[1] = unit(8, "mm")`)
- **Bildbreite**: Ausreichende Breite für Tabellen (`width = 1600` statt 1200)
- **Viewport-Probleme**: Viewport kann Z-Index Probleme verursachen - bei Problemen entfernen
- **Z-Index**: Direktes Rendering ohne Viewport vermeidet Layering-Probleme
- **Balken-Umrandung**: Schwarze Umrandung der Balken beibehalten (`color = "black", linewidth = 0.3`)

## CLUSTER-VISUALISIERUNG LEARNINGS:
- **Datenbasierte Visualisierung**: Exakte Werte aus `ki_specific_apa.png` und `cluster_prozent_tabelle.png` verwenden
- **3 Cluster für KI**: KI-Offen, Ambivalent, KI-Skeptisch
- **Cluster-Verteilung**: 28.1%, 35.9%, 35.9% für KI-Gruppe
- **Style-Matching**: Exakte Farben, Schriftart und Layout wie Referenzdiagramme
- **Y-Achsen-Positionierung**: Ausreichend Platz links für "Mittelwert"-Beschriftung
- **Werte-Labels**: Numerische Werte über jedem Balken für bessere Lesbarkeit
  - **Mittelwerte**: `sprintf("%.1f", Value)` für 1 Dezimalstelle
  - **Anzahl Personen**: `sprintf("%d", Count)` für ganze Zahlen
  - **Positionierung**: `position = position_dodge(width = 0.8)` über den Balken
  - **Abstand**: `vjust = -0.5` für optimalen Abstand über Balken
  - **Schriftgröße**: `size = 3` für angemessene Lesbarkeit
  - **Schriftart**: `family = "Times New Roman"` für Konsistenz 




# Grafiken, die wir wirklich brauchen:

## Master-Script für alle Cluster-Analysen:
**Datei**: `organized/scripts/generate_all_cluster_analyses.R`
**Ausführung**: `Rscript organized/scripts/generate_all_cluster_analyses.R`

## Generierte Ausgaben:

| Typ | Dateiname | Beschreibung | Speicherort |
|-----|-----------|--------------|-------------|
| **Tabelle** | `ki_specific_apa.png` | KI-spezifische Cluster-Analyse | `organized/images/clustering/` |
| **Tabelle** | `human_specific_apa.png` | Mensch-spezifische Cluster-Analyse | `organized/images/clustering/` |
| **Tabelle** | `cluster_prozent_tabelle.png` | Cluster-Prozent-Verteilung | `organized/images/clustering/` |
| **Tabelle** | `age_cluster_table.png` | Altersgruppen nach Clustern | `organized/images/clustering/` |

## Cluster-Visualisierungen mit Wert-Labels:

| Typ | Dateiname | Beschreibung | Speicherort |
|-----|-----------|--------------|-------------|
| **Visualisierung** | `ki_cluster_visualization.png` | KI-Cluster mit Mittelwerten | `organized/images/clustering/` |
| **Visualisierung** | `human_cluster_visualization.png` | Mensch-Cluster mit Mittelwerten | `organized/images/clustering/` |
| **Visualisierung** | `age_cluster_visualization.png` | Altersgruppen mit Personenanzahl | `organized/images/clustering/` |

## Vorteile des Master-Scripts:
- ✅ **Einheitliche Logik**: Alle Analysen verwenden identische Clustering-Parameter
- ✅ **Konsistente Cluster-Namen**: Logische Zuordnung basierend auf tatsächlichen Werten
- ✅ **Minimaler Abstand**: Optimierte Header-Zeilen mit 0.1mm Padding
- ✅ **Einmalige Ausführung**: Alle vier Elemente in einem Durchlauf
- ✅ **Datenquelle**: Verwendet "Bereinigte Daten von WhatsApp Business.csv"
- ✅ **Wert-Labels**: Alle Visualisierungen zeigen exakte Werte über den Balken  


## Tests:
- /test/test_dynamic_data_validation.R - **DYNAMISCHE DATENVALIDIERUNG** (liest echte Daten)
- /test/test_data_integrity_dynamic.R - **DATENINTEGRITÄT (KOMPLETT DYNAMISCH)** (keine hardcodierten Werte)
- /test/test_cluster_consistency.R - Testet Konsistenz zwischen allen Tabellen
- /test/test_cluster_mittelwerte_ki_style.R - Testet cluster_mittelwerte_ki_style.png
- /test/test_cluster_order_validation.R - Verallgemeinerte Cluster-Reihenfolge Validierung
- /test/test_logical_cluster_validation.R - Logische Cluster-Zuordnung Validierung
- /test/test_scientific_clustering.R - Wissenschaftliche Cluster-Analyse Validierung
- /test/run_all_tests.R - Master-Script für alle Tests
- Ausführung: `Rscript test/run_all_tests.R`

## WICHTIGE TEST-LEARNINGS:
- **❌ KEINE HARDCODED WERTE**: Tests müssen echte Daten lesen, nicht feste Werte verwenden
- **✅ DYNAMISCHE VALIDIERUNG**: Tests berechnen erwartete Werte aus echten Daten
- **✅ PNG-DATEIEN VALIDIEREN**: Tests prüfen ob Dateien existieren und aktuell sind
- **✅ DATENINTEGRITÄT**: Tests vergleichen CSV-Daten gegen generierte Tabellen
- **✅ ECHTZEIT-ÜBERPRÜFUNG**: Tests sind nicht statisch, sondern dynamisch