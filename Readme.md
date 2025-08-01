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
| **Diagramm** | `cluster_mittelwerte_ki_style.png` | Cluster-Mittelwerte Visualisierung | `organized/images/clustering/` |

## Vorteile des Master-Scripts:
- ✅ **Einheitliche Logik**: Alle Analysen verwenden identische Clustering-Parameter
- ✅ **Konsistente Cluster-Namen**: Logische Zuordnung basierend auf tatsächlichen Werten
- ✅ **Minimaler Abstand**: Optimierte Header-Zeilen mit 0.1mm Padding
- ✅ **Einmalige Ausführung**: Alle vier Elemente in einem Durchlauf
- ✅ **Datenquelle**: Verwendet "Bereinigte Daten von WhatsApp Business.csv"  


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