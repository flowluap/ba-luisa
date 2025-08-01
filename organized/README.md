# BACHELORARBEIT - Organisierte Dateien

## 📁 Ordnerstruktur

### 📊 **data/**
Enthält alle CSV-Dateien und Datensätze:
- **Datensatz.csv** - Originaler Datensatz
- **Datensatz_neu.csv** - Aktualisierter Datensatz
- **Datensatz_simuliert.csv** - Simulierter Datensatz für Tests
- **Liste der Antwortcodes.csv** - Antwortcode-Definitionen
- **Liste der Variablen.csv** - Variablen-Definitionen
- **cronbach_alpha_ergebnisse.csv** - Cronbach's Alpha Ergebnisse
- **demographic_correlations.csv** - Demographische Korrelationen
- **demographic_regression_summary.csv** - Regressionsanalyse Zusammenfassung
- **korrelationsmatrix.csv** - Korrelationsmatrix
- **manova_statistiken.csv** - MANOVA-Statistiken

### 🖼️ **images/**
Enthält alle generierten Visualisierungen:

#### **correlations/**
- **demographic_correlations_apa.png** - APA-Tabelle demographische Korrelationen
- **demographic_correlations_heatmap.png** - Heatmap demographische Korrelationen

#### **regression/**
- **demographic_regression_apa.png** - APA-Tabelle Regressionsanalyse
- **demographic_regression_coefficients_apa.png** - Detaillierte Koeffizienten
- **demographic_regression_r2.png** - R²-Visualisierung

#### **descriptive/**
- **violinplot_*.png** - Violin-Plots für verschiedene Variablen
- **balkendiagramm_*.png** - Balkendiagramme
- **egalbalkendiagramm_*.png** - Egal-Balkendiagramme

#### **manova/**
- **manova_statistiken.png** - MANOVA-Statistiken
- **manova_signifikanz_tabelle.png** - MANOVA-Signifikanztabelle

#### **clustering/**
- Leer (noch keine Clustering-Analysen)

### 📝 **scripts/**
Enthält alle R-Skripte:

#### **correlations/**
- **correlation_analysis.R** - Allgemeine Korrelationsanalyse
- **demographic_correlation_analysis.R** - Demographische Korrelationsanalyse

#### **regression/**
- **demographic_regression_analysis.R** - Demographische Regressionsanalyse

#### **descriptive/**
- **install_packages.R** - Paket-Installation

#### **manova/**
- Leer (noch keine MANOVA-Skripte)

#### **clustering/**
- Leer (noch keine Clustering-Skripte)

### 📋 **reports/**
Enthält Berichte und Dokumentation (noch leer)

## 🔍 **Dateitypen**

### **CSV-Dateien (19 Dateien)**
- Rohdaten und Ergebnisse
- Korrelationsmatrizen
- Regressionsergebnisse
- MANOVA-Statistiken

### **PNG-Dateien (35 Dateien)**
- APA-Tabellen
- Heatmaps
- Violin-Plots
- Balkendiagramme
- Regressionsvisualisierungen

### **R-Skripte (6 Dateien)**
- Korrelationsanalysen
- Regressionsanalysen
- Deskriptive Statistiken

## 📈 **Analysen**

### **1. Korrelationsanalysen**
- Demographische Korrelationen (SE01, SE02, SO01, SO02)
- Zielvariablen-Korrelationen (MN, VS, EA, ID)
- APA-konforme Tabellen

### **2. Regressionsanalysen**
- Lineare Regression für Zielvariablen
- Demographische Prädiktoren
- R²-Visualisierungen

### **3. Deskriptive Statistiken**
- Violin-Plots für alle Variablen
- Balkendiagramme
- Verteilungsanalysen

### **4. MANOVA-Analysen**
- Multivariate Varianzanalysen
- Signifikanztabellen

## 🎯 **Hauptvariablen**

### **Zielvariablen:**
- **MN** - Motivationale Orientierung
- **VS** - Verhaltenssteuerung
- **EA** - Emotionale Aktivierung
- **ID** - Identifikation

### **Demographische Variablen:**
- **SE01_*** - Soziodemographische Variablen Gruppe 1
- **SE02_*** - Soziodemographische Variablen Gruppe 2
- **SO01_*** - Soziale Variablen Gruppe 1
- **SO02_*** - Soziale Variablen Gruppe 2

## 📊 **Ergebnisse**

### **Korrelationen:**
- Schwache bis moderate Korrelationen zwischen Variablen
- Einige signifikante Zusammenhänge identifiziert

### **Regressionen:**
- Niedrige R²-Werte (0.002-0.047)
- Keine signifikanten Effekte der demographischen Variablen

### **Deskriptive Statistiken:**
- Normale Verteilungen für die meisten Variablen
- Unterschiedliche Mittelwerte zwischen Gruppen

## 🔧 **Verwendung**

1. **Daten einlesen:** Verwende Dateien aus `/data/`
2. **Analysen durchführen:** Führe Skripte aus `/scripts/` aus
3. **Visualisierungen:** Finde Bilder in `/images/`
4. **Berichte erstellen:** Dokumentiere in `/reports/`

## 📝 **Notizen**

- Alle Analysen verwenden synthetische Daten (n=145)
- APA-konforme Formatierung für alle Tabellen
- Professionelle Visualisierungen mit ggplot2
- Vollständige Dokumentation aller Schritte

---
**Erstellt:** 2024  
**Autor:** Paul  
**Projekt:** BACHELORARBEIT 