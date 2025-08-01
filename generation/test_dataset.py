#!/usr/bin/env python3
"""
Comprehensive Dataset Testing Suite
Tests the generated Datensatz_neu.csv for structural integrity, data quality, and survey logic compliance.
"""

import pandas as pd
import numpy as np
import sys
import re
from datetime import datetime
from pathlib import Path
import warnings
warnings.filterwarnings('ignore')

class DatasetTester:
    def __init__(self, dataset_path="../Datensatz_neu.csv"):
        """Initialize tester with dataset path"""
        self.dataset_path = dataset_path
        self.df = None
        self.tests_passed = 0
        self.tests_failed = 0
        self.errors = []
        
    def load_dataset(self):
        """Load and validate basic file structure"""
        try:
            # Test UTF-16 encoding
            self.df = pd.read_csv(self.dataset_path, sep='\t', encoding='utf-16')
            self._log_success("Dataset loaded successfully with UTF-16 encoding")
            return True
        except FileNotFoundError:
            self._log_error(f"Dataset file not found: {self.dataset_path}")
            return False
        except Exception as e:
            self._log_error(f"Failed to load dataset: {e}")
            return False
    
    def test_data_structure(self):
        """Test basic data structure requirements"""
        print("\n=== DATA STRUCTURE TESTS ===")
        
        # Test participant count
        expected_total = 145
        actual_total = len(self.df)
        if actual_total == expected_total:
            self._log_success(f"Correct total participants: {actual_total}")
        else:
            self._log_error(f"Wrong participant count: {actual_total}, expected {expected_total}")
        
        # Test column count
        expected_cols = 69
        actual_cols = len(self.df.columns)
        if actual_cols == expected_cols:
            self._log_success(f"Correct column count: {actual_cols}")
        else:
            self._log_error(f"Wrong column count: {actual_cols}, expected {expected_cols}")
        
        # Test completion distribution
        if 'FINISHED' in self.df.columns:
            complete_count = len(self.df[self.df['FINISHED'] == 1])
            incomplete_count = len(self.df[self.df['FINISHED'] == 0])
            
            if complete_count == 131:
                self._log_success(f"Correct complete cases: {complete_count}")
            else:
                self._log_error(f"Wrong complete cases: {complete_count}, expected 131")
                
            if incomplete_count == 14:
                self._log_success(f"Correct incomplete cases: {incomplete_count}")
            else:
                self._log_error(f"Wrong incomplete cases: {incomplete_count}, expected 14")
        
        # Filter to complete cases only for remaining tests
        self.complete_df = self.df[self.df['FINISHED'] == 1].copy() if 'FINISHED' in self.df.columns else self.df.copy()
        print(f"  ✅ Testing only complete cases: {len(self.complete_df)} participants")
    
    def test_data_types(self):
        """Test data type requirements - COMPLETE CASES ONLY"""
        print("\n=== DATA TYPE TESTS ===")
        
        # Test ID variables have realistic values (Likert scale 1-5)
        id_cols = [col for col in self.complete_df.columns if col.startswith('ID01')]
        for col in id_cols:
            values = self.complete_df[col].dropna()
            if len(values) > 0:
                min_val, max_val = values.min(), values.max()
                if min_val >= 1 and max_val <= 5:
                    self._log_success(f"{col}: Correct Likert scale range (1-5)")
                else:
                    self._log_error(f"{col}: Wrong range ({min_val}-{max_val}), expected 1-5")
                
                # Check not sequential
                if not self._is_sequential(values):
                    self._log_success(f"{col}: IDs are not sequential (good)")
                else:
                    self._log_error(f"{col}: IDs appear sequential (bad)")
        
        # Test KI03_01 data type (only "seltsam", "komisch", or empty)
        if 'KI03_01' in self.complete_df.columns:
            ki03_values = self.complete_df['KI03_01'].dropna()
            if len(ki03_values) > 0:
                # Check that all values are only "seltsam" or "komisch"
                valid_values = ['seltsam', 'komisch']
                invalid_values = []
                
                for val in ki03_values:
                    if str(val).lower() not in valid_values:
                        invalid_values.append(val)
                
                if not invalid_values:
                    self._log_success("KI03_01: All values are valid ('seltsam' or 'komisch')")
                else:
                    self._log_error(f"KI03_01: Contains invalid values: {invalid_values[:5]}")
                
                # Check for expected text responses (should have ~5 responses)
                seltsam_count = sum(1 for val in ki03_values if 'seltsam' in str(val).lower())
                komisch_count = sum(1 for val in ki03_values if 'komisch' in str(val).lower())
                total_text = seltsam_count + komisch_count
                
                if total_text >= 3:
                    self._log_success(f"KI03_01: Contains expected text responses ({total_text} found: {seltsam_count} seltsam, {komisch_count} komisch)")
                else:
                    self._log_error(f"KI03_01: Too few text responses ({total_text} found)")
        
        # Test SE04_01 content
        if 'SE04_01' in self.complete_df.columns:
            se04_values = self.complete_df['SE04_01'].dropna()
            valid_platforms = ['Reddit', 'Pinterest', 'Discord', 'Twitch', 'LinkedIn', 'Clubhouse']
            
            if len(se04_values) > 0:
                invalid_values = [val for val in se04_values if val not in valid_platforms]
                if not invalid_values:
                    self._log_success("SE04_01: All values are valid platform names")
                else:
                    self._log_error(f"SE04_01: Invalid platform names found: {invalid_values}")
                
                # Check realistic missing percentage (80-90%)
                missing_pct = self.complete_df['SE04_01'].isna().sum() / len(self.complete_df) * 100
                if 80 <= missing_pct <= 90:
                    self._log_success(f"SE04_01: Realistic missing percentage ({missing_pct:.1f}%)")
                else:
                    self._log_error(f"SE04_01: Unrealistic missing percentage ({missing_pct:.1f}%)")
                

    
    def test_survey_logic(self):
        """Test survey logic compliance - COMPLETE CASES ONLY"""
        print("\n=== SURVEY LOGIC TESTS ===")
        
        # Test FINISHED values for complete cases (should all be 1)
        if 'FINISHED' in self.complete_df.columns:
            unique_finished = set(self.complete_df['FINISHED'].dropna())
            if unique_finished == {1.0}:
                self._log_success("FINISHED: All complete cases have FINISHED = 1")
            else:
                self._log_error(f"FINISHED: Complete cases have invalid values: {unique_finished}")
        
        # Test LASTPAGE should be 11 for all complete cases
        if 'LASTPAGE' in self.complete_df.columns:
            lastpage_values = self.complete_df['LASTPAGE'].dropna()
            if len(lastpage_values) > 0 and all(lastpage_values == 11):
                self._log_success("LASTPAGE: All complete cases finished page 11")
            else:
                unique_lastpage = set(lastpage_values)
                self._log_error(f"LASTPAGE: Complete cases have invalid values: {unique_lastpage}, should all be 11")
        
        # Test MAXPAGE should be 11 for all complete cases and equal to LASTPAGE
        if 'MAXPAGE' in self.complete_df.columns:
            maxpage_values = self.complete_df['MAXPAGE'].dropna()
            if len(maxpage_values) > 0:
                # Check MAXPAGE max value is 11
                if maxpage_values.max() <= 11:
                    self._log_success(f"MAXPAGE: Maximum value ≤ 11 ({maxpage_values.max()})")
                else:
                    self._log_error(f"MAXPAGE: Values exceed 11 (max: {maxpage_values.max()})")
                
                # Check MAXPAGE equals LASTPAGE for ALL cases (including incomplete)
                if 'LASTPAGE' in self.df.columns:  # Use full dataset, not just complete
                    # Check for all non-empty values
                    all_lastpage = self.df['LASTPAGE'].dropna()
                    all_maxpage = self.df['MAXPAGE'].dropna()
                    if len(all_lastpage) > 0 and len(all_maxpage) > 0:
                        # Compare only where both values exist
                        both_exist = self.df[['LASTPAGE', 'MAXPAGE']].dropna()
                        if len(both_exist) > 0:
                            pages_match = (both_exist['LASTPAGE'] == both_exist['MAXPAGE']).all()
                            if pages_match:
                                self._log_success("LASTPAGE = MAXPAGE: Always equal for all cases (complete & incomplete)")
                            else:
                                self._log_error("LASTPAGE ≠ MAXPAGE: Should be equal for all cases")
                        else:
                            self._log_success("LASTPAGE = MAXPAGE: No cases with both values to compare")
        
        # Test Q_VIEWER should always be 0
        if 'Q_VIEWER' in self.complete_df.columns:
            qviewer_values = self.complete_df['Q_VIEWER'].dropna()
            if len(qviewer_values) > 0 and all(qviewer_values == 0):
                self._log_success("Q_VIEWER: All values are 0")
            else:
                unique_qviewer = set(qviewer_values)
                self._log_error(f"Q_VIEWER: Contains non-zero values: {unique_qviewer}")
        
        # Test STATUS should always be empty
        if 'STATUS' in self.complete_df.columns:
            non_empty_status = self.complete_df['STATUS'].dropna()
            if len(non_empty_status) == 0:
                self._log_success("STATUS: Column is empty as required")
            else:
                self._log_error(f"STATUS: Contains {len(non_empty_status)} non-empty values")
        
        # Test REF should always be empty (using full dataset)
        if 'REF' in self.df.columns:
            non_empty_ref = self.df['REF'].dropna()
            if len(non_empty_ref) == 0:
                self._log_success("REF: Column is empty as required")
            else:
                self._log_error(f"REF: Contains {len(non_empty_ref)} non-empty values")
        
        # Test QUESTNME should always be "base" (using full dataset)
        if 'QUESTNME' in self.df.columns:
            questnme_values = self.df['QUESTNME'].dropna()
            if len(questnme_values) > 0:
                unique_questnme = set(questnme_values)
                if unique_questnme == {"base"}:
                    self._log_success("QUESTNME: All values are 'base'")
                else:
                    self._log_error(f"QUESTNME: Contains invalid values: {unique_questnme}, should only be 'base'")
            else:
                self._log_error("QUESTNME: Column is empty, should contain 'base'")
        
        # Test BV02 should always be 2 (using full dataset)
        if 'BV02' in self.df.columns:
            bv02_values = self.df['BV02'].dropna()
            if len(bv02_values) > 0:
                unique_bv02 = set(bv02_values)
                if unique_bv02 == {2}:
                    self._log_success("BV02: All values are 2")
                else:
                    self._log_error(f"BV02: Contains invalid values: {unique_bv02}, should only be 2")
            else:
                self._log_error("BV02: Column is empty, should contain 2")
    
    def test_skipped_cases_logic(self):
        """Test that skipped cases follow proper sequential filling logic"""
        # Filter incomplete cases from the main dataset
        incomplete_df = self.df[self.df['FINISHED'] == 0]
        if len(incomplete_df) == 0:
            self._log_error("No incomplete cases found for testing")
            return
            
        # Load page mapping from parameters
        import json
        try:
            with open('dataset_parameters.json', 'r', encoding='utf-8') as f:
                params = json.load(f)
        except Exception as e:
            self._log_error(f"Could not load dataset_parameters.json: {e}")
            return
            
        # Create variable to page mapping
        var_to_page = {}
        
        # Use explicit page mappings if available
        page_mappings_config = params.get('page_mappings', {})
        if page_mappings_config:
            for page_str, page_vars in page_mappings_config.items():
                page_num = int(page_str)
                for var in page_vars:
                    var_to_page[var] = page_num
        else:
            # Fallback to old method if page_mappings not found
            for var_group, config in params['variables'].items():
                page = config.get('page', 1)
                for item_key in config.get('items', {}).keys():
                    var_to_page[item_key] = page
                # Also map the group key itself if it exists as a column
                if var_group in self.df.columns:
                    var_to_page[var_group] = page
            
            # Add special system variables (early pages)
            system_vars = {
                'CASE': 1, 'SERIAL': 1, 'REF': 1, 'QUESTNNR': 1, 'MODE': 1, 'STARTED': 1,
                'AB01_CP': 1, 'AB01': 1, 'BV02': 1
            }
            var_to_page.update(system_vars)
        
        errors_found = 0
        
        for idx, row in incomplete_df.iterrows():
            case_id = row['CASE']
            lastpage = row['LASTPAGE']
            
            # Get all survey variables (exclude system variables at the end AND always-empty vars)
            survey_vars = [col for col in self.df.columns 
                          if col not in ['STATUS', 'FINISHED', 'Q_VIEWER', 'LASTPAGE', 'MAXPAGE', 'SERIAL', 'REF']]
            
            filled_vars = []
            empty_vars = []
            
            for var in survey_vars:
                value = row[var]
                # Check if variable is filled (not NaN, not empty string, not just quotes)
                is_filled = (pd.notna(value) and value != '' and 
                           value != '""' and str(value).strip() != '')
                
                if is_filled:
                    filled_vars.append(var)
                else:
                    empty_vars.append(var)
            
            # Test 1: No gaps in filled variables (left-to-right filling)
            # Variables should be filled sequentially from left to right in the dataset
            if filled_vars and empty_vars:
                # Find positions in the original column order
                filled_positions = [survey_vars.index(var) for var in filled_vars]
                empty_positions = [survey_vars.index(var) for var in empty_vars]
                
                max_filled_pos = max(filled_positions) if filled_positions else -1
                min_empty_pos = min(empty_positions) if empty_positions else len(survey_vars)
                
                if max_filled_pos > min_empty_pos:
                    self._log_error(f"Case {case_id}: Gap in variable filling - filled variables after empty ones")
                    errors_found += 1
            
            # Test 2: No partial filling within variable groups (same prefix)
            var_groups = {}
            for var in survey_vars:
                # Extract prefix (everything before the first number or underscore)
                prefix = var.split('_')[0] if '_' in var else var.rstrip('0123456789')
                if prefix not in var_groups:
                    var_groups[prefix] = []
                var_groups[prefix].append(var)
            
            for prefix, group_vars in var_groups.items():
                if len(group_vars) > 1:  # Only check groups with multiple variables
                    group_filled = [var for var in group_vars if var in filled_vars]
                    group_empty = [var for var in group_vars if var in empty_vars]
                    
                    if group_filled and group_empty:
                        # Check if it's a sequential pattern (all filled from start or all empty from some point)
                        group_positions = [(group_vars.index(var), var in filled_vars) for var in group_vars]
                        group_positions.sort()  # Sort by position in group
                        
                        # Should be: True, True, ..., True, False, False, ..., False
                        pattern_valid = True
                        found_false = False
                        for pos, is_filled in group_positions:
                            if found_false and is_filled:
                                pattern_valid = False
                                break
                            if not is_filled:
                                found_false = True
                        
                        if not pattern_valid:
                            self._log_error(f"Case {case_id}: Partial filling in group {prefix} - filled: {group_filled}, empty: {group_empty}")
                            errors_found += 1
            
            # Test 3: LASTPAGE corresponds to actual filling pattern
            # All variables on pages < LASTPAGE should be filled
            # All variables on pages >= LASTPAGE should be empty
            page_errors = 0
            
            for var in survey_vars:
                var_page = var_to_page.get(var, 11)  # Default to page 11 if not found
                is_filled = var in filled_vars
                
                if var_page < lastpage and not is_filled:
                    page_errors += 1
                elif var_page >= lastpage and is_filled and var not in system_vars:
                    # System vars are always filled, so exclude them
                    page_errors += 1
            
            if page_errors > 0:
                self._log_error(f"Case {case_id}: LASTPAGE {lastpage} doesn't match filling pattern ({page_errors} page violations)")
                errors_found += 1
        
        if errors_found == 0:
            self._log_success(f"Skipped cases logic: All {len(incomplete_df)} incomplete cases follow proper sequential filling")
        else:
            self._log_error(f"Skipped cases logic: {errors_found} cases have filling pattern violations")
        
        # Test MODE should always be "interview" (using full dataset)
        if 'MODE' in self.df.columns:
            mode_values = self.df['MODE'].dropna()
            if len(mode_values) > 0:
                unique_mode = set(mode_values)
                if unique_mode == {"interview"}:
                    self._log_success("MODE: All values are 'interview'")
                else:
                    self._log_error(f"MODE: Contains invalid values: {unique_mode}, should only be 'interview'")
            else:
                self._log_error("MODE: Column is empty, should contain 'interview'")
        
        # Test AB01 controls human/AI assignment and has only values 1 or 2
        if 'AB01' in self.complete_df.columns:
            ab01_values = self.complete_df['AB01'].dropna()
            if len(ab01_values) > 0:
                unique_ab01 = set(ab01_values)
                if unique_ab01.issubset({1.0, 2.0}):
                    self._log_success(f"AB01: Only values 1 and 2 present: {sorted(unique_ab01)}")
                else:
                    self._log_error(f"AB01: Contains invalid values: {unique_ab01}, should only be 1 or 2")
                
                # Check if AB01 corresponds inversely to MC01 (manipulation check)
                if 'MC01' in self.complete_df.columns:
                    # AB01=1 should correspond to MC01=2 (AI), AB01=2 to MC01=1 (Human) - inverse relationship
                    valid_df = self.complete_df[['AB01', 'MC01']].dropna()
                    correspondence_check = (
                        ((valid_df['AB01'] == 1) & (valid_df['MC01'] == 2)) |
                        ((valid_df['AB01'] == 2) & (valid_df['MC01'] == 1))
                    ).all()
                    if correspondence_check:
                        self._log_success("AB01 inverse MC01: Assignment correspondence correct (AB01=1→AI, AB01=2→Human)")
                    else:
                        self._log_error("AB01 inverse MC01: Assignment correspondence broken")
        
        # Test forced response design (only for complete cases)
        forced_response_columns = [col for col in self.complete_df.columns 
                                 if col not in ['KI03_01', 'SE04_01', 'SERIAL', 'STATUS', 'REF'] 
                                 and not col.startswith(('CASE', 'TIME'))]
        
        violations = []
        for col in forced_response_columns:
            if col in self.complete_df.columns:
                missing_count = self.complete_df[col].isna().sum()
                if missing_count > 0:
                    violations.append(f"{col}: {missing_count} missing")
        
        if not violations:
            self._log_success("Forced response: No unexpected missing values in complete cases")
        else:
            self._log_error(f"Forced response violations: {violations}")
    
    def test_date_ranges(self):
        """Test date range requirements"""
        print("\n=== DATE RANGE TESTS ===")
        
        if 'STARTED' in self.df.columns:
            try:
                # Convert to datetime
                dates = pd.to_datetime(self.df['STARTED'])
                min_date = dates.min()
                max_date = dates.max()
                
                # Expected range
                expected_start = datetime(2025, 7, 1)
                expected_end = datetime(2025, 7, 22, 23, 59, 59)
                
                if expected_start <= min_date and max_date <= expected_end:
                    self._log_success(f"STARTED: Dates in range ({min_date.date()} to {max_date.date()})")
                else:
                    self._log_error(f"STARTED: Dates outside range ({min_date} to {max_date})")
                    
            except Exception as e:
                self._log_error(f"STARTED: Date parsing failed: {e}")
    
    def test_statistical_properties(self):
        """Test statistical properties - COMPLETE CASES ONLY"""
        print("\n=== STATISTICAL TESTS ===")
        
        # Test Likert scale ranges (exclude non-Likert variables)
        likert_columns = []
        for col in self.complete_df.columns:
            # Include survey items but exclude specific non-Likert variables
            if (any(scale in col for scale in ['MC', 'MN', 'VS', 'EA', 'ID', 'KI', 'SE']) 
                and col not in ['SE04_01', 'KI03_01']  # Text/optional fields
                and not col.startswith('ID01')  # Participant IDs
                and not col.startswith('SE03_')  # Binary 0-1 variables (but include SE03)
                and col != 'CASE'):  # Case ID
                likert_columns.append(col)
        
        scale_violations = []
        for col in likert_columns:
            if col in self.complete_df.columns:
                values = self.complete_df[col].dropna()
                if len(values) > 0:
                    if values.min() < 1 or values.max() > 5:
                        scale_violations.append(f"{col}: {values.min()}-{values.max()}")
        
        if not scale_violations:
            self._log_success("Likert scales: All values in range 1-5")
        else:
            self._log_error(f"Likert scale violations: {scale_violations}")
        
        # Test binary variables (SE03_xx should be 0-1, but not SE03 itself)
        binary_columns = [col for col in self.complete_df.columns if col.startswith('SE03_')]
        binary_violations = []
        for col in binary_columns:
            if col in self.complete_df.columns:
                values = self.complete_df[col].dropna()
                if len(values) > 0:
                    if values.min() < 0 or values.max() > 1:
                        binary_violations.append(f"{col}: {values.min()}-{values.max()}")
        
        if not binary_violations:
            if binary_columns:
                self._log_success("Binary variables (SE03_xx): All values in range 0-1")
            else:
                self._log_success("Binary variables: No SE03_xx columns found")
        else:
            self._log_error(f"Binary variable violations: {binary_violations}")
        
        # Test SO01_01 should be realistic age
        if 'SO01_01' in self.complete_df.columns:
            age_values = self.complete_df['SO01_01'].dropna()
            if len(age_values) > 0:
                if 18 <= age_values.min() and age_values.max() <= 65:
                    self._log_success(f"Age (SO01_01): Realistic range ({age_values.min()}-{age_values.max()})")
                else:
                    self._log_error(f"Age (SO01_01): Unrealistic range ({age_values.min()}-{age_values.max()})")
        
        # Test demographic ranges
        if 'SO01' in self.complete_df.columns:  # Alternative age field
            age_values = self.complete_df['SO01'].dropna()
            if len(age_values) > 0:
                if 18 <= age_values.min() and age_values.max() <= 65:
                    self._log_success(f"Age (SO01): Realistic range ({age_values.min()}-{age_values.max()})")
                else:
                    self._log_error(f"Age (SO01): Unrealistic range ({age_values.min()}-{age_values.max()})")
        
        if 'SO02' in self.complete_df.columns:  # Gender
            gender_values = self.complete_df['SO02'].dropna()
            if len(gender_values) > 0:
                unique_genders = set(gender_values)
                if unique_genders.issubset({1, 2, 3}):
                    self._log_success(f"Gender (SO02): Valid values {sorted(unique_genders)}")
                else:
                    self._log_error(f"Gender (SO02): Invalid values {unique_genders}")
    
    def test_experimental_design(self):
        """Test experimental design integrity - COMPLETE CASES ONLY"""
        print("\n=== EXPERIMENTAL DESIGN TESTS ===")
        
        # Test manipulation check consistency
        mc_columns = [col for col in self.complete_df.columns if col.startswith('MC')]
        if mc_columns:
            mc_col = mc_columns[0]
            
            human_count = len(self.complete_df[self.complete_df[mc_col] == 1])
            ai_count = len(self.complete_df[self.complete_df[mc_col] == 2])
            
            # Check reasonable balance (within 20% difference is acceptable)
            total_complete = len(self.complete_df)
            expected_balance = total_complete / 2
            tolerance = total_complete * 0.2  # 20% tolerance
            
            if abs(human_count - expected_balance) <= tolerance and abs(ai_count - expected_balance) <= tolerance:
                self._log_success(f"Experimental balance: Human={human_count}, AI={ai_count} (acceptable)")
            else:
                self._log_error(f"Experimental imbalance: Human={human_count}, AI={ai_count} (difference > 20%)")
    
    def test_content_validation(self):
        """Test content-specific validation"""
        print("\n=== CONTENT VALIDATION TESTS ===")
        
        # Test CASE ID uniqueness (use full dataset for this)
        if 'CASE' in self.df.columns:
            case_ids = self.df['CASE'].dropna()
            if len(case_ids) == len(set(case_ids)):
                self._log_success("CASE IDs: All unique")
            else:
                self._log_error("CASE IDs: Duplicates found")
            
            # Test CASE ID sequence
            if list(case_ids.sort_values()) == list(range(1, len(case_ids) + 1)):
                self._log_success("CASE IDs: Sequential 1 to N")
            else:
                self._log_error("CASE IDs: Not sequential")
        
        # Test SERIAL column (should be empty) - use complete cases
        if 'SERIAL' in self.complete_df.columns:
            non_empty_serial = self.complete_df['SERIAL'].dropna()
            if len(non_empty_serial) == 0:
                self._log_success("SERIAL: Column is empty as required")
            else:
                self._log_error(f"SERIAL: Contains {len(non_empty_serial)} non-empty values")
        # All tests passed for content validation
        self._log_success("Content validation completed")
    
    def test_integer_display(self):
        """Test that numbers are displayed as integers without .0 suffixes"""
        print("\n=== INTEGER DISPLAY TESTS ===")
        
        # Read the raw CSV content to check formatting
        try:
            with open(self.dataset_path, 'r', encoding='utf-16') as f:
                csv_content = f.read()
            
            # Check for .0 suffixes in numeric fields (but exclude dates/times and text fields)
            float_pattern_found = False
            problematic_fields = []
            
            # Split into lines and check each line
            lines = csv_content.split('\n')
            if len(lines) > 1:  # Skip header
                header = lines[0].split('\t')
                # Sample a few lines to check for .0 patterns
                sample_lines = lines[1:6]  # Check first 5 data lines
                
                for line_num, line in enumerate(sample_lines, 2):
                    if line.strip():  # Skip empty lines
                        fields = line.split('\t')
                        for field_num, field in enumerate(fields):
                            # Skip date fields and text fields
                            if field_num < len(header):
                                col_name = header[field_num].strip('"')
                                # Skip date, text, and platform fields
                                if not any(skip in col_name.upper() for skip in ['STARTED', 'SE04', 'KI03']):
                                    # Check if field contains .0 pattern (but not in dates)
                                    if '.0' in field and not ':' in field and field.strip() != '':
                                        float_pattern_found = True
                                        if col_name not in problematic_fields:
                                            problematic_fields.append(col_name)
            
            if not float_pattern_found:
                self._log_success("All numeric values displayed as integers (no .0 suffixes)")
            else:
                self._log_error(f"Found .0 suffixes in numeric fields: {problematic_fields[:5]}")
                
        except Exception as e:
            self._log_error(f"Error checking number formatting: {e}")
    
    def _is_sequential(self, values):
        """Check if values are sequential"""
        sorted_vals = sorted(values)
        return all(sorted_vals[i] == sorted_vals[i-1] + 1 for i in range(1, len(sorted_vals)))
    
    def _log_success(self, message):
        """Log successful test"""
        print(f"  ✅ {message}")
        self.tests_passed += 1
    
    def _log_error(self, message):
        """Log failed test"""
        print(f"  ❌ {message}")
        self.tests_failed += 1
        self.errors.append(message)
    
    def run_all_tests(self):
        """Run all test suites"""
        print("=" * 60)
        print("🧪 DATASET VALIDATION TEST SUITE")
        print("=" * 60)
        
        if not self.load_dataset():
            return False
        
        # Run all test suites
        self.test_data_structure()
        self.test_data_types()
        self.test_survey_logic()
        self.test_date_ranges()
        self.test_statistical_properties()
        self.test_experimental_design()
        self.test_content_validation()
        self.test_skipped_cases_logic()
        self.test_integer_display()
        
        # Summary
        print("\n" + "=" * 60)
        print("📊 TEST SUMMARY")
        print("=" * 60)
        
        total_tests = self.tests_passed + self.tests_failed
        pass_rate = (self.tests_passed / total_tests * 100) if total_tests > 0 else 0
        
        print(f"Total Tests: {total_tests}")
        print(f"Passed: {self.tests_passed}")
        print(f"Failed: {self.tests_failed}")
        print(f"Pass Rate: {pass_rate:.1f}%")
        
        if self.tests_failed == 0:
            print("\n🎉 ALL TESTS PASSED! Dataset is ready for analysis.")
            return True
        else:
            print(f"\n⚠️  {self.tests_failed} TESTS FAILED:")
            for error in self.errors:
                print(f"   • {error}")
            print("\n🔧 Please fix these issues before proceeding with analysis.")
            return False

def main():
    """Main execution function"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Test generated dataset quality')
    parser.add_argument('--dataset', default='../Datensatz_neu.csv',
                        help='Path to dataset file (default: ../Datensatz_neu.csv)')
    parser.add_argument('--exit-on-fail', action='store_true',
                       help='Exit with error code if tests fail')
    
    args = parser.parse_args()
    
    tester = DatasetTester(args.dataset)
    success = tester.run_all_tests()
    
    if args.exit_on_fail and not success:
        sys.exit(1)
    
    return success

if __name__ == "__main__":
    main() 