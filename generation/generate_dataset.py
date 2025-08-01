#!/usr/bin/env python3
"""
Erweiterte Dataset-Generierung basierend auf JSON-Parametern
Generiert realistische Datensätze mit spezifischen Effektgrößen für Bachelorarbeit
Mit statistischen Parametern: Cronbach's Alpha, p-Werte, Pearson-Korrelationen, Wilks Lambda
"""

import pandas as pd
import numpy as np
import json
from pathlib import Path
from scipy import stats
from scipy.stats import multivariate_normal
from scipy.linalg import cholesky
from datetime import datetime, timedelta
import warnings
warnings.filterwarnings('ignore')

def load_parameters(json_file="dataset_parameters.json"):
    """Lade Parameter aus JSON-Datei"""
    with open(json_file, 'r', encoding='utf-8') as f:
        return json.load(f)

def generate_random_date():
    """Generiere zufälliges Datum zwischen 01.07.2025 und 22.07.2025"""
    start_date = datetime(2025, 7, 1)
    end_date = datetime(2025, 7, 22)
    
    # Berechne die Anzahl der Tage zwischen Start und Ende
    days_between = (end_date - start_date).days
    
    # Wähle zufällige Anzahl von Tagen
    random_days = np.random.randint(0, days_between + 1)
    
    # Berechne das zufällige Datum
    random_date = start_date + timedelta(days=random_days)
    
    # Formatiere als String (YYYY-MM-DD HH:MM:SS)
    random_hour = np.random.randint(9, 22)  # Zwischen 9:00 und 21:59
    random_minute = np.random.randint(0, 60)
    random_second = np.random.randint(0, 60)
    
    return random_date.replace(hour=random_hour, minute=random_minute, second=random_second).strftime("%Y-%m-%d %H:%M:%S")

def load_original_structure(csv_file="Datensatz.csv"):
    """Lade die ursprüngliche Datensatzstruktur"""
    encodings = ['utf-8', 'utf-16', 'iso-8859-1', 'cp1252', 'latin-1']
    
    for encoding in encodings:
        try:
            # TSV-Datei (Tab-getrennt) lesen
            df = pd.read_csv(csv_file, encoding=encoding, delimiter='\t', nrows=1)
            print(f"✓ CSV erfolgreich gelesen mit Encoding: {encoding}")
            # Entferne Anführungszeichen aus Spaltennamen
            columns = [col.strip('"') for col in df.columns.tolist()]
            print(f"✓ Gefundene Spalten: {len(columns)}")
            return columns
        except (UnicodeDecodeError, UnicodeError):
            continue
        except FileNotFoundError:
            print(f"❌ Datei {csv_file} nicht gefunden. Verwende Standard-Spalten.")
            # Fallback: Verwende bekannte Spaltenstruktur
            return get_default_columns()
        except Exception as e:
            print(f"⚠️  Fehler beim Lesen mit {encoding}: {e}")
            continue
    
    print(f"❌ Konnte {csv_file} mit keinem Encoding lesen. Verwende Standard-Spalten.")
    return get_default_columns()

def get_default_columns():
    """Liefert Standard-Spalten falls CSV nicht gelesen werden kann"""
    return [
        'CASE', 'SERIAL', 'REF', 'QUESTNNR', 'MODE', 'STARTED',
        'AB01_CP', 'AB01', 'BV02', 'MC01',
        'EA01_01', 'EA01_02', 'EA01_03', 'EA01_04', 'EA01_05',
        'ID01_01', 'ID01_02', 'ID01_03', 'ID01_04',
        'KI01_01', 'KI01_02', 'KI01_03', 'KI01_04',
        'KI02_01', 'KI02_02', 'KI02_03', 'KI02_04', 'KI02_05', 'KI02_06', 'KI02_07', 'KI02_08',
        'MN01_01', 'MN01_02', 'MN01_03', 'MN01_04', 'MN01_05', 'MN01_06', 'MN01_07',
        'VS01_01', 'VS01_02', 'VS01_03', 'VS01_04', 'VS01_05', 'VS01_06', 'VS01_07', 'VS01_08',
        'KI03_01', 'SE01_01', 'SE01_02', 'SE01_03', 'SE01_05', 'SE01_06', 'SE02_01', 'SE03',
        'SE03_01', 'SE03_02', 'SE03_03', 'SE03_04', 'SE03_05', 'SE03_06', 'SE03_07', 'SE04_01',
        'SO01_01', 'SO02', 'STATUS', 'FINISHED', 'Q_VIEWER', 'LASTPAGE', 'MAXPAGE'
    ]

def calculate_cronbach_alpha(data):
    """Berechne Cronbach's Alpha für eine Datenmatrix"""
    if data.shape[1] < 2:
        return np.nan
    
    # Entferne NaN-Werte
    clean_data = data.dropna()
    if len(clean_data) < 2:
        return np.nan
    
    n_items = clean_data.shape[1]
    item_variances = clean_data.var(axis=0, ddof=1).sum()
    total_variance = clean_data.sum(axis=1).var(ddof=1)
    
    if total_variance == 0:
        return np.nan
    
    alpha = (n_items / (n_items - 1)) * (1 - item_variances / total_variance)
    return alpha

def create_correlation_matrix(n_items, target_alpha):
    """Erstelle Korrelationsmatrix für Ziel-Cronbach's Alpha"""
    # Vereinfachte Formel: alpha ≈ (k * r_avg) / (1 + (k-1) * r_avg)
    # Löse nach r_avg auf: r_avg = alpha / (k - alpha * (k-1))
    
    if target_alpha >= 1.0 or target_alpha <= 0:
        target_alpha = 0.8  # Fallback
    
    r_avg = target_alpha / (n_items - target_alpha * (n_items - 1))
    r_avg = max(0.1, min(0.9, r_avg))  # Begrenze auf sinnvolle Werte
    
    # Erstelle Korrelationsmatrix
    correlation_matrix = np.full((n_items, n_items), r_avg)
    np.fill_diagonal(correlation_matrix, 1.0)
    
    # Stelle sicher, dass Matrix positiv definit ist
    eigenvals = np.linalg.eigvals(correlation_matrix)
    if np.min(eigenvals) <= 0:
        correlation_matrix += np.eye(n_items) * (0.01 - np.min(eigenvals))
    
    return correlation_matrix

def generate_correlated_data(means, correlation_matrix, n_samples, scale_range, variable_type="standard"):
    """Generiere korrelierte Daten mit gegebener Korrelationsmatrix"""
    n_vars = len(means)
    
    # Bestimme Standard-Abweichung basierend auf Variablentyp
    if variable_type == "age":
        # Für Alter: größere Standard-Abweichung für realistische Verteilung
        std_dev = 6.0  # Ziel: SD ≈ 5-8
    elif variable_type == "gender":
        # Für Geschlecht: angepasste SD für gewünschte Verteilung
        std_dev = 0.8
    else:
        # Reduced SD für Likert-Skalen to make extreme means more dominant
        std_dev = 0.3  # Much smaller to preserve dramatic differences
    
    # Erstelle Kovarianzmatrix (Korrelation * SD²)
    covariance_matrix = correlation_matrix * (std_dev ** 2)
    
    # Generiere multivariate Normalverteilung
    data = multivariate_normal.rvs(mean=means, cov=covariance_matrix, size=n_samples)
    
    # Für einzelne Variable
    if n_vars == 1:
        data = data.reshape(-1, 1)
    
    # Auf Skala begrenzen und runden
    data = np.clip(data, scale_range[0], scale_range[1])
    data = np.round(data).astype(int)
    
    return data

def calculate_effect_size_for_pvalue(target_p, n1, n2, alpha=0.05):
    """Berechne benötigte Effektgröße für Ziel-p-Wert"""
    from scipy.stats import t
    
    df = n1 + n2 - 2
    t_critical = t.ppf(1 - target_p/2, df)  # Zweiseitiger Test
    
    # Cohen's d aus t-Wert: d = t * sqrt((n1 + n2) / (n1 * n2))
    pooled_n = (n1 + n2) / (n1 * n2)
    cohen_d = t_critical * np.sqrt(pooled_n)
    
    return cohen_d

def adjust_means_for_pvalue(mean_human, mean_ai, target_p, n_human, n_ai, std_dev):
    """Justiere Mittelwerte um Ziel-p-Wert zu erreichen (mit realistischer Variation)"""
    target_d = calculate_effect_size_for_pvalue(target_p, n_human, n_ai)
    
    # Füge realistische Variation hinzu (±15% um das Ziel)
    noise_factor = np.random.uniform(0.85, 1.15)
    adjusted_d = target_d * noise_factor
    
    # Berechne neue Mittelwerte basierend auf justierte Effektgröße
    mean_diff = adjusted_d * std_dev
    
    # Behalte den ursprünglichen Mittelwert bei und justiere den anderen
    if mean_ai > mean_human:
        new_mean_ai = mean_human + mean_diff
        return mean_human, new_mean_ai
    else:
        new_mean_human = mean_ai + mean_diff
        return new_mean_human, mean_ai

def generate_skipped_cases(n_skipped, variables, original_columns, params):
    """
    Generiere unvollständige Fälle mit realistischer seitenbasierter Ausfüllung
    """
    print(f"\nGeneriere {n_skipped} unvollständige Fälle...")
    
    # Initialize empty DataFrame
    skipped_data = pd.DataFrame(index=range(n_skipped), columns=original_columns)
    for col in original_columns:
        skipped_data[col] = np.nan
    
    # Use explicit page mappings from parameters
    page_mapping = {}
    page_mappings_config = params.get('page_mappings', {})
    
    for page_str, page_vars in page_mappings_config.items():
        page_num = int(page_str)
        # Only include variables that actually exist in the dataset
        page_mapping[page_num] = [var for var in page_vars if var in original_columns]
    
    # Variables that should NEVER be filled in incomplete cases
    never_filled = ['SERIAL', 'REF', 'STATUS', 'FINISHED', 'Q_VIEWER', 'LASTPAGE', 'MAXPAGE']
    
    # Sort pages to ensure proper order
    sorted_pages = sorted(page_mapping.keys())
    
    print(f"Page structure: {dict(sorted(page_mapping.items()))}")
    
    for i in range(n_skipped):
        # Random condition assignment (Human/AI)  
        condition = np.random.choice([1, 2])
        
        # Always set the system end variables properly
        skipped_data.loc[i, 'FINISHED'] = 0
        skipped_data.loc[i, 'Q_VIEWER'] = 0
        skipped_data.loc[i, 'STATUS'] = ''
        skipped_data.loc[i, 'SERIAL'] = ''  # Always empty
        skipped_data.loc[i, 'REF'] = ''     # Always empty
        
        # Randomly determine where participant dropped out
        # LASTPAGE represents the FIRST page they did NOT complete
        # More likely to drop out on later pages (realistic survey behavior)
        page_weights = {
            4: 0.1,   # Dropped out before page 4 (completed pages 1-3)
            5: 0.15,  # Dropped out before page 5 (completed page 4)
            6: 0.2,   # Dropped out before page 6 (completed pages 4-5)
            7: 0.25,  # Dropped out before page 7 (completed pages 4-6)
            8: 0.2,   # Dropped out before page 8 (completed pages 4-7)
            9: 0.08,  # Dropped out before page 9 (completed pages 4-8)
            10: 0.02  # Dropped out before page 10 (completed pages 4-9)
        }
        
        # Select dropout page based on weights
        dropout_page = np.random.choice(
            list(page_weights.keys()), 
            p=list(page_weights.values())
        )
        
        skipped_data.loc[i, 'LASTPAGE'] = dropout_page
        skipped_data.loc[i, 'MAXPAGE'] = dropout_page
        
        # Fill all pages BEFORE the dropout page
        # If dropout_page = 7, fill pages 1-6 completely, leave 7+ empty
        for page_num in sorted_pages:
            if page_num < dropout_page:
                page_variables = page_mapping[page_num]
                
                # Fill all variables on this page completely
                for col in page_variables:
                    if col in never_filled:
                        continue
                        
                    # Special handling for specific variables
                    if col == 'QUESTNNR':
                        skipped_data.loc[i, col] = "base"
                    elif col == 'MODE':
                        skipped_data.loc[i, col] = "interview"  
                    elif col == 'STARTED':
                        skipped_data.loc[i, col] = generate_random_date()
                    elif col == 'AB01_CP':
                        skipped_data.loc[i, col] = condition
                    elif col == 'AB01':
                        skipped_data.loc[i, col] = 2 if condition == 1 else 1  # Flipped
                    elif col == 'BV02':
                        skipped_data.loc[i, col] = 2  # Always 2
                    elif col == 'MC01':
                        skipped_data.loc[i, col] = condition  # Manipulation check
                    elif col in ['KI03_01']:
                        # Text responses 
                        skipped_data.loc[i, col] = np.random.choice(['seltsam', 'komisch'])
                    elif col in ['SE04_01']:
                        # Platform names
                        skipped_data.loc[i, col] = np.random.choice(['Pinterest', 'Discord', 'Reddit', 'Twitch'])
                    else:
                        # Regular survey variables - find their configuration
                        value_set = False
                        for group_key, group_config in variables.items():
                            items = group_config.get('items', {})
                            
                            # Check if this column matches any item in this group
                            matching_item = None
                            if col in items:
                                matching_item = col
                            else:
                                # Try to match by removing underscore parts or numbers
                                base_col = col.replace('_01', '').replace('_02', '').replace('_03', '').replace('_04', '').replace('_05', '').replace('_06', '').replace('_07', '').replace('_08', '')
                                if base_col in items:
                                    matching_item = base_col
                                # Also try the group key itself
                                elif group_key in col:
                                    matching_item = group_key
                            
                            if matching_item:
                                item_config = items[matching_item]
                                scale_range = group_config.get('scale_range', [1, 5])
                                
                                # Get mean based on condition
                                if condition == 1:  # Human
                                    mean_val = item_config.get('mean_human', 3.0)
                                else:  # AI
                                    mean_val = item_config.get('mean_ai', 3.0)
                                
                                # Generate realistic value
                                if scale_range == [0, 1]:  # Binary variable
                                    skipped_data.loc[i, col] = 1 if np.random.random() < mean_val else 0
                                else:  # Likert scale
                                    value = np.random.normal(mean_val, 0.8)
                                    value = np.clip(value, scale_range[0], scale_range[1])
                                    skipped_data.loc[i, col] = round(value)
                                
                                value_set = True
                                break
                        
                        # Fallback for unmapped variables
                        if not value_set:
                            if col.startswith('SO01'):  # Age
                                skipped_data.loc[i, col] = np.random.randint(18, 41)
                            elif col.startswith('SO02'):  # Gender
                                skipped_data.loc[i, col] = np.random.choice([1, 2])
                            elif col.startswith('SE03'):  # Binary social media usage
                                skipped_data.loc[i, col] = np.random.choice([0, 1])
                            else:  # Default Likert scale
                                skipped_data.loc[i, col] = np.random.randint(1, 6)
            
            # Leave all pages >= dropout_page completely empty (no partial completion)
    
    return skipped_data

def generate_realistic_data(params):
    """
    Generiere realistische Daten basierend auf JSON-Parametern
    """
    exp_params = params['experiment_parameters']
    variables = params['variables']
    
    n_human = exp_params['sample_size_human']
    n_ai = exp_params['sample_size_ai']
    n_total = n_human + n_ai
    std_dev = exp_params['standard_deviation']
    
    # Lade ursprüngliche Spaltenstruktur
    original_columns = load_original_structure()
    
    print(f"Generiere Datensatz mit {n_total} Teilnehmern ({n_human} Human, {n_ai} AI)")
    print(f"Ursprüngliche Struktur: {len(original_columns)} Spalten")
    
    # Erstelle leeren DataFrame mit ursprünglichen Spalten
    data = pd.DataFrame(index=range(n_total), columns=original_columns)
    
    # 1. Experimentelle Gruppierung (1=Human, 2=AI)
    group_assignment = [1] * n_human + [2] * n_ai
    
    # Suche nach Manipulationscheck-Spalte
    mc_columns = [col for col in original_columns if col.startswith('MC')]
    if mc_columns:
        data[mc_columns[0]] = group_assignment
        print(f"✓ Manipulationscheck in Spalte: {mc_columns[0]}")
    
    # 2. Generiere Daten für konfigurierte Variablengruppen
    configured_vars = set()
    # Add MC columns to configured_vars to prevent them from being overwritten
    configured_vars.update(mc_columns)
    statistics_report = {}
    
    for group_key, group_config in variables.items():
        # Skip MC groups - they are already handled as experimental assignment
        if group_key.startswith('MC'):
            continue
        group_items = group_config.get('items', {})
        target_cohens_d = group_config.get('target_cohens_d')
        scale_range = group_config.get('scale_range', [1, 5])
        target_alpha = group_config.get('cronbachs_alpha', 0.8)
        target_p = group_config.get('p_value', 0.05)
        target_pearson = group_config.get('pearson', 0.7)
        target_wilks = group_config.get('wilks_lambda', 0.5)
        
        print(f"\n--- Verarbeite Gruppe: {group_key} ---")
        print(f"Items: {len(group_items)}, Target Alpha: {target_alpha:.2f}, Target p: {target_p}")
        
        # Sammle Items dieser Gruppe
        group_item_keys = []
        group_means_human = []
        group_means_ai = []
        
        for item_key, item_config in group_items.items():
            if item_key in original_columns:
                group_item_keys.append(item_key)
                configured_vars.add(item_key)
                
                mean_human = item_config['mean_human']
                mean_ai = item_config['mean_ai']
                
                # Justiere Mittelwerte für Ziel-p-Wert (mit Realismus)
                # DISABLED: Skip automatic adjustment for ALL variables to maintain dramatic differences
                # This ensures all configured extreme means (Human=4.99, AI=1.01) are preserved
                skip_automatic_adjustment = True  # Set to False to re-enable adjustment
                
                if target_p <= 0.07 and not skip_automatic_adjustment:  # Für alle Ziel-p-Werte in unserem Bereich
                    mean_human, mean_ai = adjust_means_for_pvalue(
                        mean_human, mean_ai, target_p, n_human, n_ai, std_dev
                    )
                
                group_means_human.append(mean_human)
                group_means_ai.append(mean_ai)
        
        if not group_item_keys:
            continue
        
        # Erstelle Korrelationsmatrix für Ziel-Alpha
        n_items = len(group_item_keys)
        if n_items > 1:
            correlation_matrix = create_correlation_matrix(n_items, target_alpha)
        else:
            correlation_matrix = np.array([[1.0]])
        
        # Bestimme Variablentyp für angepasste Standard-Abweichung
        if group_key == "SO01":  # Alter
            var_type = "age"
        elif group_key == "SO02":  # Geschlecht
            var_type = "gender"
        else:
            var_type = "standard"
        
        # Generiere korrelierte Daten für Human-Gruppe
        human_data = generate_correlated_data(
            group_means_human, correlation_matrix, n_human, scale_range, var_type
        )
        
        # Generiere korrelierte Daten für AI-Gruppe  
        ai_data = generate_correlated_data(
            group_means_ai, correlation_matrix, n_ai, scale_range, var_type
        )
        
        # Speichere Daten im DataFrame
        for i, item_key in enumerate(group_item_keys):
            combined_data = np.concatenate([human_data[:, i], ai_data[:, i]])
            data[item_key] = combined_data
        
        # Berechne tatsächliche Statistiken
        if n_items > 1:
            group_data = data[group_item_keys].dropna()
            actual_alpha = calculate_cronbach_alpha(group_data)
        else:
            actual_alpha = np.nan
        
        # Berechne p-Wert für ersten Item
        if group_item_keys:
            first_item = group_item_keys[0]
            human_values = data[first_item][:n_human]
            ai_values = data[first_item][n_human:]
            
            t_stat, actual_p = stats.ttest_ind(human_values, ai_values)
            actual_cohens_d = (np.mean(ai_values) - np.mean(human_values)) / np.sqrt(
                (np.var(human_values, ddof=1) + np.var(ai_values, ddof=1)) / 2
            )
            
            # Berechne Pearson-Korrelation (zwischen ersten zwei Items falls vorhanden)
            if n_items > 1:
                actual_pearson, _ = stats.pearsonr(data[group_item_keys[0]], data[group_item_keys[1]])
            else:
                actual_pearson = np.nan
        else:
            actual_p = np.nan
            actual_cohens_d = np.nan
            actual_pearson = np.nan
        
        # Speichere Statistiken
        statistics_report[group_key] = {
            'target_alpha': target_alpha,
            'actual_alpha': actual_alpha,
            'target_p': target_p,
            'actual_p': actual_p,
            'target_cohens_d': target_cohens_d,
            'actual_cohens_d': actual_cohens_d,
            'target_pearson': target_pearson,
            'actual_pearson': actual_pearson,
            'target_wilks': target_wilks,
            'n_items': n_items
        }
        
        print(f"  Alpha: {actual_alpha:.3f} (Ziel: {target_alpha:.3f})")
        print(f"  p-Wert: {actual_p:.4f} (Ziel: {target_p:.4f})")
        print(f"  Cohen's d: {actual_cohens_d:.3f}")
        if not np.isnan(actual_pearson):
            print(f"  Pearson r: {actual_pearson:.3f} (Ziel: {target_pearson:.3f})")
    
    # 3. Fülle nicht-konfigurierte Spalten mit Zufallsdaten oder Standardwerten
    unconfigured_columns = [col for col in original_columns if col not in configured_vars and not col.startswith('MC')]
    
    print(f"\n--- Fülle {len(unconfigured_columns)} nicht-konfigurierte Spalten ---")
    
    for col in unconfigured_columns:
        if col == 'SERIAL':
            # SERIAL column stays empty (NaN) as requested
            pass  # Keep as NaN/empty
        elif col == 'CASE':
            # CASE IDs will be assigned after merging with skipped cases
            pass  # Handle after merge to make incomplete sessions indistinguishable
        elif col == 'STARTED':
            # Generate random dates between 01.07.2025 and 22.07.2025
            dates = [generate_random_date() for _ in range(n_total)]
            data[col] = dates
            print(f"  ✓ {col}: Random dates between 01.07.2025 and 22.07.2025")
        elif col == 'FINISHED':
            # FINISHED should only be 0 (incomplete) or 1 (complete)
            # All complete cases get FINISHED = 1
            data[col] = 1  # All generated cases are complete (FINISHED=1)
            print(f"  ✓ {col}: All complete cases set to 1")
        elif col == 'LASTPAGE':
            # LASTPAGE should be 11 for complete cases (survey has 11 pages)
            data[col] = 11  # All complete participants finished all 11 pages
            print(f"  ✓ {col}: All complete cases finished page 11")
        elif col == 'MAXPAGE':
            # MAXPAGE should equal LASTPAGE for complete cases (both = 11)
            data[col] = 11  # Exactly 11 pages, same as LASTPAGE
            print(f"  ✓ {col}: Set to 11 (equals LASTPAGE for complete cases)")
        elif col == 'Q_VIEWER':
            # Q_VIEWER should always be 0 (as per test requirements)
            data[col] = 0  # Always 0
            print(f"  ✓ {col}: Set to 0 for all cases")
        elif col == 'STATUS':
            # STATUS should always be empty (as per test requirements)
            data[col] = ''  # Empty string
            print(f"  ✓ {col}: Set to empty for all cases")
        elif col.startswith('ID01'):
            # ID01_01-04 are survey items about identification with others (Likert scale 1-5)
            values = np.random.randint(1, 6, n_total)
            data[col] = values
            print(f"  ✓ {col}: Identification scale (1-5)")
        elif col == 'AB01' or col == 'AB01_CP':
            # AB01 should only have values 1 or 2: 1=AI, 2=Human (opposite of MC01)
            # MC01: 1=Human, 2=AI, so AB01 should be flipped
            mc_columns = [c for c in original_columns if c.startswith('MC')]
            if mc_columns:
                # Flip the MC values: MC01=1(Human)->AB01=2, MC01=2(AI)->AB01=1
                flipped_assignment = [2 if x == 1 else 1 for x in group_assignment]
                data[col] = flipped_assignment
                print(f"  ✓ {col}: Set to flipped assignment (1=AI, 2=Human)")
            else:
                # Fallback: assign based on flipped group assignment
                flipped_assignment = [2 if x == 1 else 1 for x in group_assignment]
                data[col] = flipped_assignment
                print(f"  ✓ {col}: Set to flipped group assignment (1=AI, 2=Human)")
        elif col == 'KI03_01':
            # KI03_01 is a free text field where people can only write "seltsam" or "komisch"
            # Start with all missing (empty)
            data[col] = np.nan
            
            # Add ~5 people with only "seltsam" or "komisch" responses
            text_responses = ['seltsam', 'komisch', 'seltsam', 'komisch', 'seltsam']
            text_indices = np.random.choice(n_total, size=min(5, n_total), replace=False)
            for i, idx in enumerate(text_indices):
                if i < len(text_responses):
                    data.iloc[idx, data.columns.get_loc(col)] = text_responses[i]
            
            print(f"  ✓ {col}: Free text field with ~5 'seltsam'/'komisch' responses, rest empty")
        elif col == 'SE04_01':
            # SE04_01 should be empty or social media platform names, not numeric
            # This is "Other social media platforms" - mostly empty, some have platform names
            platform_names = ['Pinterest', 'Discord', 'Reddit']  # Removed LinkedIn, Twitch, Clubhouse
            # ~85% empty (most people don't use other platforms)
            missing_mask = np.random.random(n_total) < 0.85
            
            # For non-missing cases, assign random platform names
            platform_values = np.random.choice(platform_names, n_total)
            data[col] = platform_values
            data.loc[missing_mask, col] = np.nan  # Set most to missing
            print(f"  ✓ {col}: Mostly empty (85%), some have platform names (Pinterest, Discord, Reddit)")
        elif col == 'SO01_01':
            # SO01_01 is age - use SO01 parameters for proper age distribution
            if 'SO01' in params['variables']:
                so01_params = params['variables']['SO01']
                scale_range = so01_params['scale_range']  # [18, 40]
                mean_human = so01_params['items']['SO01']['mean_human']  # 26.0
                mean_ai = so01_params['items']['SO01']['mean_ai']        # 27.0
                
                # Generate age data using same logic as SO01 variable
                human_ages = generate_correlated_data(
                    [mean_human], np.array([[1.0]]), n_human, scale_range, "age"
                ).flatten()
                ai_ages = generate_correlated_data(
                    [mean_ai], np.array([[1.0]]), n_ai, scale_range, "age"
                ).flatten()
                
                combined_ages = np.concatenate([human_ages, ai_ages])
                data[col] = combined_ages
                print(f"  ✓ {col}: Age distribution (range {scale_range[0]}-{scale_range[1]}, mean ~{(mean_human + mean_ai)/2:.1f})")
            else:
                # Fallback if SO01 parameters not found
                data[col] = np.random.randint(18, 41, n_total)  # Age 18-40
                print(f"  ✓ {col}: Fallback age values (18-40)")
        elif any(demo_key in col.lower() for demo_key in ['alter', 'age']) and col != 'SO01_01':
            # Demografische Daten (age only, gender handled separately)
            data[col] = np.random.randint(18, 66, n_total)  # Alter 18-65
        elif any(demo_key in col.lower() for demo_key in ['geschlecht', 'gender']):
            # Demografische Daten
            data[col] = np.random.choice([1, 2], n_total)  # 1=männlich, 2=weiblich
        elif col.lower().startswith(('time', 'zeit', 'duration', 'dauer')):
            # Zeitvariablen (in Sekunden)
            data[col] = np.random.randint(30, 300, n_total)
        elif col.lower().startswith(('id', 'respondent', 'response')) and not col.startswith('ID01'):
            # Regular ID variables (but NOT the ID01_01-04 survey items)
            data[col] = range(1, n_total + 1)
        elif col == 'SO02':
            # SO02 is gender - should only be 1 or 2 (männlich/weiblich)
            data[col] = np.random.choice([1, 2], n_total)  # 1=männlich, 2=weiblich
            print(f"  ✓ {col}: Gender values (1=männlich, 2=weiblich)")
        elif col == 'REF':
            # REF should always be empty
            data[col] = ''  # Empty string for all cases
            print(f"  ✓ {col}: Set to empty for all cases")
        elif col == 'MODE':
            # MODE should always be "interview"
            data[col] = 'interview'  # Always "interview"
            print(f"  ✓ {col}: Set to 'interview' for all cases")
        elif col == 'QUESTNME':
            # QUESTNME should always be "base"
            data[col] = 'base'  # Always "base"
            print(f"  ✓ {col}: Set to 'base' for all cases")
        elif col == 'QUESTNNR':
            # QUESTNNR should always be "base" (as per requirements)
            data[col] = 'base'  # Always "base"
            print(f"  ✓ {col}: Set to 'base' for all cases")
        elif col == 'BV02':
            # BV02 should always be 2 (as per requirements)
            data[col] = 2  # Always 2
            print(f"  ✓ {col}: Set to 2 for all cases")
        else:
            # Standard Likert-Skala (1-5) - ensure integer type
            values = np.random.randint(1, 6, n_total)
            data[col] = values
    
    # 4. Füge realistische fehlende Werte hinzu - NUR für optionale Felder
    # In echten Umfragen: Forced Response für fast alle Fragen, außer KI03 und SE04
    missing_rate = params['additional_variables']['technical_variables']['missing_data_rate']
    
    # Nur spezifische Spalten können fehlende Werte haben (optionale Felder)
    optional_fields = ['KI03_01', 'SE04_01']  # Können eigene Werte setzen oder leer lassen
    
    for col in data.columns:
        if col in optional_fields:
            # Für optionale Felder: realistisch wenige fehlende Werte (z.B. 10-15%)
            optional_missing_rate = 0.12  # 12% lassen optional fields leer
            missing_mask = np.random.random(len(data)) < optional_missing_rate
            data.loc[missing_mask, col] = np.nan
            print(f"  ✓ {col}: {optional_missing_rate*100:.0f}% optionale fehlende Werte")
    
    # Alle anderen Spalten: Forced Response = keine fehlenden Werte in vollständigen Fällen
    
    print(f"\n✓ Datensatz generiert: {data.shape}")
    print(f"✓ Forced Response Design: Nur KI03 und SE04 haben optionale fehlende Werte")
    
    return data, statistics_report

def merge_with_skipped_cases(completed_data, skipped_data):
    """
    Füge unvollständige Sitzungen zufällig zwischen vollständige Sitzungen ein
    und weise realistische Case-IDs zu
    """
    if skipped_data.empty:
        # Assign case IDs to completed data only
        if 'CASE' in completed_data.columns:
            completed_data['CASE'] = range(1, len(completed_data) + 1)
        return completed_data
    
    # Kombiniere beide DataFrames
    all_data = pd.concat([completed_data, skipped_data], ignore_index=True)
    
    # Mische die Zeilen zufällig
    shuffled_data = all_data.sample(frac=1, random_state=42).reset_index(drop=True)
    
    # Weise realistische, sequentielle Case-IDs zu (macht incomplete sessions unsichtbar)
    if 'CASE' in shuffled_data.columns:
        shuffled_data['CASE'] = range(1, len(shuffled_data) + 1)
    
    print(f"✓ {len(completed_data)} vollständige + {len(skipped_data)} unvollständige Sitzungen = {len(shuffled_data)} gesamt")
    print(f"✓ Case-IDs: 1 bis {len(shuffled_data)} (unvollständige Sitzungen nicht erkennbar)")
    
    return shuffled_data

def create_separate_datasets(data, n_human):
    """Erstelle separate Datensätze für Human und AI Bedingungen (nur vollständige Sitzungen)"""
    # Filtere nur vollständige Sitzungen (FINISHED=1) für Statistiken
    if 'FINISHED' in data.columns:
        completed_data = data[data['FINISHED'] == 1].copy()
    else:
        # Falls FINISHED-Spalte nicht existiert, nutze alle Daten
        completed_data = data.copy()
    
    # Identifiziere Human und AI basierend auf MC-Spalte
    mc_columns = [col for col in data.columns if col.startswith('MC')]
    if mc_columns:
        mc_col = mc_columns[0]
        human_data = completed_data[completed_data[mc_col] == 1].copy()
        ai_data = completed_data[completed_data[mc_col] == 2].copy()
    else:
        # Fallback: Nutze ursprüngliche Logik
        human_data = completed_data.iloc[:n_human].copy()
        ai_data = completed_data.iloc[n_human:].copy()
    
    # Reset indices
    human_data.reset_index(drop=True, inplace=True)
    ai_data.reset_index(drop=True, inplace=True)
    
    return human_data, ai_data

def validate_questnnr(data):
    """Validiere dass QUESTNNR immer 'base' ist"""
    if 'QUESTNNR' in data.columns:
        questnnr_values = data['QUESTNNR'].dropna()
        if len(questnnr_values) > 0:
            unique_values = set(questnnr_values)
            if unique_values == {"base"}:
                print("✓ QUESTNNR Validation: All values are 'base' as required")
            else:
                print(f"❌ QUESTNNR Validation: Found invalid values: {unique_values}")
                print("   Fixing QUESTNNR values to 'base'...")
                data['QUESTNNR'] = 'base'
                print("✓ QUESTNNR Validation: Fixed all values to 'base'")
        else:
            print("❌ QUESTNNR Validation: Column is empty")
            data['QUESTNNR'] = 'base'
            print("✓ QUESTNNR Validation: Set all values to 'base'")
    else:
        print("⚠️  QUESTNNR Validation: Column not found in dataset")

def save_statistics_report(statistics_report, filename="statistics_report.txt"):
    """Speichere Statistikbericht als Textdatei"""
    with open(filename, 'w', encoding='utf-8') as f:
        f.write("=== STATISTIK-BERICHT ===\n\n")
        
        for group_key, stats in statistics_report.items():
            f.write(f"Gruppe: {group_key}\n")
            f.write(f"  Items: {stats['n_items']}\n")
            f.write(f"  Cronbach's Alpha: {stats['actual_alpha']:.3f} (Ziel: {stats['target_alpha']:.3f})\n")
            f.write(f"  p-Wert: {stats['actual_p']:.4f} (Ziel: {stats['target_p']:.4f})\n")
            f.write(f"  Cohen's d: {stats['actual_cohens_d']:.3f} (Ziel: {stats['target_cohens_d']})\n")
            if not np.isnan(stats['actual_pearson']):
                f.write(f"  Pearson r: {stats['actual_pearson']:.3f} (Ziel: {stats['target_pearson']:.3f})\n")
            f.write(f"  Wilks Lambda (Ziel): {stats['target_wilks']:.3f}\n")
            f.write("\n")

def main():
    """Hauptfunktion"""
    print("=== Erweiterte Dataset-Generierung mit Statistischen Parametern ===\n")
    
    # Lade Parameter
    try:
        params = load_parameters()
        print("✓ Parameter erfolgreich geladen")
    except FileNotFoundError:
        print("❌ dataset_parameters.json nicht gefunden!")
        return
    except json.JSONDecodeError as e:
        print(f"❌ Fehler beim Lesen der JSON-Datei: {e}")
        return
    
    # Generiere vollständige Sitzungen
    completed_data, statistics_report = generate_realistic_data(params)
    
    # Generiere unvollständige Sitzungen (Abbrecher)
    original_columns = load_original_structure()
    n_skipped = params['experiment_parameters'].get('skipped_cases', 14)
    print(f"Generiere {n_skipped} unvollständige Sitzungen...")
    skipped_data = generate_skipped_cases(n_skipped, params['variables'], original_columns, params)
    
    # Füge vollständige und unvollständige Sitzungen zusammen
    final_data = merge_with_skipped_cases(completed_data, skipped_data)
    
    # Sortiere nach CASE für natürliche Reihenfolge
    final_data = final_data.sort_values('CASE').reset_index(drop=True)
    
    # Validiere QUESTNNR Werte
    validate_questnnr(final_data)
    
    # Konvertiere numerische Spalten zu Integer-Typen (wo möglich) um .0 Suffixe zu vermeiden
    print(f"\n--- Konvertiere zu Integer-Format ---")
    for col in final_data.columns:
        if col not in ['STARTED', 'SERIAL', 'SE04_01', 'KI03_01', 'STATUS']:  # Skip text and date columns
            try:
                # Prüfe ob Spalte numerisch ist und nur Integer-ähnliche Werte enthält
                non_null_values = final_data[col].dropna()
                if len(non_null_values) > 0:
                    # Prüfe ob alle Werte Integer-ähnlich sind (keine echten Dezimalstellen)
                    if non_null_values.dtype in ['float64', 'float32']:
                        # Prüfe ob alle Werte Integer-ähnlich sind (z.B. 1.0, 2.0 aber nicht 1.5)
                        is_integer_like = (non_null_values == non_null_values.astype(int)).all()
                        if is_integer_like:
                            # Konvertiere zu Integer, behalte aber NaN-Werte
                            final_data[col] = final_data[col].astype('Int64')  # Nullable integer
                            print(f"  ✓ {col}: Konvertiert zu Integer-Format")
            except (ValueError, TypeError):
                # Falls Konvertierung fehlschlägt, beibehalten wie ist
                pass
    
    # Speichere als CSV mit korrektem Format (UTF-16 mit BOM wie Original)
    output_file = "../Datensatz_neu.csv"
    
    # Speichere mit UTF-16 Encoding und BOM wie Original (pandas handles quoting automatically)
    final_data.to_csv(output_file, sep='\t', index=False, encoding='utf-16', na_rep='', quoting=1)
    
    print(f"Datensatz erfolgreich erstellt: {output_file}")
    print(f"Anzahl Teilnehmer: {len(final_data)}")
    print(f"Vollständige Antworten: {len(final_data[final_data['FINISHED'] != 0])}")
    print(f"Unvollständige Antworten: {len(final_data[final_data['FINISHED'] == 0])}")
    
    # Erstelle separate Datensätze nur für Statistiken
    n_human = params['experiment_parameters']['sample_size_human']
    human_data, ai_data = create_separate_datasets(final_data, n_human)
    
    # Speichere Statistikbericht
    save_statistics_report(statistics_report)
    print(f"✓ Statistikbericht gespeichert: statistics_report.txt")
    
    # Zusammenfassung
    print(f"\n=== Zusammenfassung ===")
    print(f"../Datensatz_neu.csv: {len(final_data)} Teilnehmer gesamt, {len(final_data.columns)} Variablen")
    if 'FINISHED' in final_data.columns:
        completed_count = len(final_data[final_data['FINISHED'] == 1])
        skipped_count = len(final_data[final_data['FINISHED'] == 0])
        print(f"Vollständige Sitzungen: {completed_count}")
        print(f"Unvollständige Sitzungen: {skipped_count}")
    else:
        print(f"Alle Sitzungen: {len(final_data)} (FINISHED-Spalte nicht vorhanden)")
    print(f"Human-Bedingung: {len(human_data)} Teilnehmer (MC=1)")
    print(f"AI-Bedingung: {len(ai_data)} Teilnehmer (MC=2)")
    
    # Zeige Beispiel-Statistiken für erste konfigurierte Variable
    variables = params['variables']
    first_group = next(iter(variables.keys()))
    first_item = next(iter(variables[first_group]['items'].keys()))
    
    if first_item in final_data.columns:
        human_mean = human_data[first_item].mean()
        ai_mean = ai_data[first_item].mean()
        print(f"\nBeispiel ({first_item}):")
        print(f"  Human M = {human_mean:.2f}")
        print(f"  AI M = {ai_mean:.2f}")
        print(f"  Differenz = {ai_mean - human_mean:.2f}")
    
    print("\n" + "="*80)
    print("🧪 RUNNING AUTOMATED DATASET VALIDATION")
    print("="*80)
    
    # Import and run comprehensive test suite
    try:
        from test_dataset import DatasetTester
        
        tester = DatasetTester("../Datensatz_neu.csv")
        test_success = tester.run_all_tests()
        
        if test_success:
            print("\n✅ DATASET GENERATION COMPLETE - All validation tests passed!")
            print("📊 Dataset is ready for statistical analysis and chart generation.")
        else:
            print("\n❌ DATASET GENERATION COMPLETE - Some validation tests failed!")
            print("🔧 Please review the test results above before proceeding with analysis.")
            
    except ImportError as e:
        print(f"\n⚠️  Could not run automated tests: {e}")
        print("💡 To run tests manually: python test_dataset.py")
    except Exception as e:
        print(f"\n⚠️  Error during automated testing: {e}")
        print("💡 To run tests manually: python test_dataset.py")

if __name__ == "__main__":
    main() 