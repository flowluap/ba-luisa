#!/usr/bin/env python3
"""
Debug script to check actual means being used in data generation
"""

import json
import pandas as pd
import numpy as np

def load_parameters(json_file="dataset_parameters.json"):
    """Load parameters from JSON file"""
    with open(json_file, 'r', encoding='utf-8') as f:
        return json.load(f)

def main():
    # Load parameters
    params = load_parameters()
    variables = params['variables']
    
    print("=== Checking Configured Means for Key Variables ===")
    
    # Check EA variables
    print("\n--- EA Variables (Emotional Appeal) ---")
    for var_key in ['EA01', 'EA02', 'EA03', 'EA04', 'EA05']:
        if var_key in variables:
            var_config = variables[var_key]
            items = var_config['items']
            for item_key, item_config in items.items():
                mean_human = item_config['mean_human']
                mean_ai = item_config['mean_ai']
                cohens_d = var_config['target_cohens_d']
                print(f"{item_key}: Human={mean_human}, AI={mean_ai}, Cohen's d={cohens_d}")
    
    # Check MN variables  
    print("\n--- MN Variables (Naturalness) ---")
    for var_key in ['MN01', 'MN02', 'MN03', 'MN04', 'MN05', 'MN06', 'MN07']:
        if var_key in variables:
            var_config = variables[var_key]
            items = var_config['items']
            for item_key, item_config in items.items():
                mean_human = item_config['mean_human']
                mean_ai = item_config['mean_ai']
                cohens_d = var_config['target_cohens_d']
                print(f"{item_key}: Human={mean_human}, AI={mean_ai}, Cohen's d={cohens_d}")
    
    # Check ID variables
    print("\n--- ID Variables (Identification) ---")
    for var_key in ['ID01', 'ID02', 'ID03', 'ID04']:
        if var_key in variables:
            var_config = variables[var_key]
            items = var_config['items']
            for item_key, item_config in items.items():
                mean_human = item_config['mean_human']
                mean_ai = item_config['mean_ai']
                cohens_d = var_config['target_cohens_d']
                print(f"{item_key}: Human={mean_human}, AI={mean_ai}, Cohen's d={cohens_d}")

    # Load actual dataset and check means by group
    print("\n=== Checking Actual Dataset Means ===")
    
    # Read the generated dataset
    data = pd.read_csv("../Datensatz_neu.csv", sep="\t", encoding="utf-16")
    complete_data = data[data['FINISHED'] == 1].copy()
    
    # Create group variable (AB01=1 is AI, AB01=2 is Human)
    complete_data['Gruppe'] = complete_data['AB01'].map({1: 'KI', 2: 'Mensch'})
    
    print(f"\nDataset: {len(complete_data)} complete cases")
    print(f"Groups: {complete_data['Gruppe'].value_counts().to_dict()}")
    
    # Check EA variables in dataset
    print("\n--- EA Variables in Dataset ---")
    ea_vars = [col for col in complete_data.columns if col.startswith('EA01_')]
    for var in ea_vars:
        means = complete_data.groupby('Gruppe')[var].mean()
        print(f"{var}: KI={means.get('KI', 'N/A'):.2f}, Mensch={means.get('Mensch', 'N/A'):.2f}")
    
    # Check MN variables in dataset  
    print("\n--- MN Variables in Dataset ---")
    mn_vars = [col for col in complete_data.columns if col.startswith('MN01_')]
    for var in mn_vars:
        means = complete_data.groupby('Gruppe')[var].mean()
        print(f"{var}: KI={means.get('KI', 'N/A'):.2f}, Mensch={means.get('Mensch', 'N/A'):.2f}")

if __name__ == "__main__":
    main() 