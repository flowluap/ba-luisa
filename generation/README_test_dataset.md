# Dataset Testing Suite Documentation

## Overview

The `test_dataset.py` script is a comprehensive validation tool for the generated survey dataset (`Datensatz_neu.csv`). It performs **35 different tests** across 7 categories to ensure data quality, structural integrity, and survey logic compliance.

**Key Feature**: The script focuses on **complete cases only** (FINISHED=1) for most tests, ensuring that survey logic validation is accurate and doesn't include incomplete responses.

## Quick Start

```bash
# Test the default dataset
python test_dataset.py

# Test a specific dataset file
python test_dataset.py --dataset my_dataset.csv

# Exit with error code if tests fail (useful for CI/CD)
python test_dataset.py --exit-on-fail
```

## Test Categories

### 1. **Data Structure Tests** (5 tests)
- ✅ **Participant Count**: Verifies 145 total participants (131 complete + 14 incomplete)
- ✅ **Column Count**: Ensures 69 variables are present
- ✅ **Completion Distribution**: Validates correct complete/incomplete ratio
- ✅ **Complete Cases Filter**: Creates subset for focused testing

### 2. **Data Type Tests** (8 tests) - *Complete Cases Only*
- ✅ **ID Variables**: Checks ID01_01-04 have realistic participant IDs (not sequential 1-131)
- ✅ **KI03_01**: Validates integer values 1-5 with appropriate missing data
- ✅ **SE04_01**: Confirms platform names (Reddit, Pinterest, etc.) or empty values

### 3. **Survey Logic Tests** (8 tests) - *Complete Cases Only*
- ✅ **FINISHED Values**: All complete cases must have FINISHED = 1
- ✅ **LASTPAGE Logic**: All complete cases must have LASTPAGE = 11
- ✅ **MAXPAGE Range**: Maximum value ≤ 11 for complete cases
- ✅ **LASTPAGE = MAXPAGE**: Must be equal for all complete cases
- ✅ **Q_VIEWER**: All values must be 0
- ✅ **STATUS**: Column must be empty
- ✅ **AB01**: Only values 1 or 2 allowed (controls human/AI assignment)
- ✅ **AB01 = MC01**: Assignment correspondence must be correct
- ✅ **Forced Response**: No unexpected missing values in complete cases

### 4. **Date Range Tests** (1 test)
- ✅ **STARTED Dates**: Between 01.07.2025 and 22.07.2025

### 5. **Statistical Tests** (6 tests) - *Complete Cases Only*
- ✅ **Likert Scales**: All survey items in range 1-5
- ✅ **Binary Variables**: SE03_xx variables in range 0-1
- ✅ **Age (SO01_01)**: Realistic age range 18-65
- ✅ **Age (SO01)**: Alternative age field validation
- ✅ **Demographics**: Gender (SO02) values 1-3

### 6. **Experimental Design Tests** (1 test) - *Complete Cases Only*
- ✅ **Experimental Balance**: Human vs AI groups within 20% of each other

### 7. **Content Validation Tests** (3 tests)
- ✅ **CASE IDs**: Unique and sequential 1-N (uses full dataset)
- ✅ **SERIAL Column**: Empty as required (complete cases only)

## Key Changes in v2.0

### **Complete Cases Only Testing**
Most tests now focus exclusively on complete cases (FINISHED=1), ensuring:
- Survey logic validation is accurate
- No contamination from incomplete responses
- Focused testing on actual survey data

### **New Specific Tests**
- **MAXPAGE ≤ 11**: Survey has maximum 11 pages
- **LASTPAGE = MAXPAGE**: Must be equal for complete responses
- **Q_VIEWER = 0**: Technical field must be zero
- **STATUS = Empty**: Field must be empty for clean data
- **AB01 Controls Assignment**: Values 1/2 determine human/AI condition
- **AB01 = MC01**: Assignment consistency validation
- **SO01_01 Age**: Realistic participant ages

## Output Examples

### ✅ **All Tests Passed**
```
============================================================
📊 TEST SUMMARY
============================================================
Total Tests: 35
Passed: 35
Failed: 0
Pass Rate: 100.0%

🎉 ALL TESTS PASSED! Dataset is ready for analysis.
```

### ❌ **Tests Failed with Specific Issues**
```
============================================================
📊 TEST SUMMARY
============================================================
Total Tests: 35
Passed: 28
Failed: 7
Pass Rate: 80.0%

⚠️  7 TESTS FAILED:
   • MAXPAGE: Values exceed 11 (max: 12.0)
   • LASTPAGE ≠ MAXPAGE: Should be equal for complete cases
   • Q_VIEWER: Contains non-zero values: {1.0, 2.0, 3.0, 4.0, 5.0}
   • STATUS: Contains 131 non-empty values
   • AB01: Contains invalid values: {1.0, 2.0, 3.0, 4.0, 5.0}, should only be 1 or 2
   • AB01 ≠ MC01: Assignment correspondence broken
   • Age (SO01_01): Unrealistic range (1.0-5.0)

🔧 Please fix these issues before proceeding with analysis.
```

## Critical Survey Logic Requirements

### **Complete Cases Must Have:**
- `FINISHED = 1` (completion status)
- `LASTPAGE = 11` (finished all pages)
- `MAXPAGE ≤ 11` and `MAXPAGE = LASTPAGE`
- `Q_VIEWER = 0` (technical field)
- `STATUS = empty` (no status values)
- `AB01 ∈ {1, 2}` and `AB01 = MC01` (assignment control)

### **Realistic Data Requirements:**
- `SO01_01`: Age 18-65 years
- `ID01_xx`: Participant IDs 1000-999999 (not sequential)
- `SE04_01`: Platform names or 80-90% missing

## Command Line Options

| Option | Description | Example |
|--------|-------------|---------|
| `--dataset` | Specify dataset file path | `--dataset data/my_survey.csv` |
| `--exit-on-fail` | Exit with error code if tests fail | Useful for automated pipelines |
| `--help` | Show help message | `python test_dataset.py --help` |

## Integration Examples

### **Fix-Generate-Test Workflow**
```bash
#!/bin/bash
echo "🔧 Generating dataset..."
python generate_dataset.py

echo "🧪 Testing dataset quality..."
python test_dataset.py --exit-on-fail

if [ $? -eq 0 ]; then
    echo "✅ Dataset validated - proceeding with analysis"
    Rscript cronbach_alpha_analysis.R
else
    echo "❌ Dataset validation failed"
    echo "💡 Fix the issues in generate_dataset.py and regenerate"
    exit 1
fi
```

### **Development Loop**
```bash
# 1. Generate dataset
python generate_dataset.py

# 2. Test quality
python test_dataset.py

# 3. If failures, fix generation logic and repeat
# 4. When all tests pass, proceed with analysis
```

## Understanding Test Results

### **Survey Logic Failures (Critical)**
These indicate fundamental problems with the survey implementation:
- ❌ **FINISHED ≠ 1**: Complete cases not properly marked
- ❌ **LASTPAGE ≠ 11**: Survey progression logic broken
- ❌ **MAXPAGE > 11**: Survey structure incorrect
- ❌ **Q_VIEWER ≠ 0**: Technical fields populated incorrectly

### **Assignment Logic Failures (Critical)**
- ❌ **AB01 values**: Assignment control broken
- ❌ **AB01 ≠ MC01**: Experimental condition mismatch

### **Data Quality Issues (Important)**
- ⚠️ **Unrealistic Ages**: Demographic data incorrect
- ⚠️ **Sequential IDs**: Not realistic participant IDs
- ⚠️ **Missing Platform Data**: Platform field logic broken

### **Statistical Issues (Monitor)**
- ⚠️ **Experimental Imbalance**: Uneven group sizes
- ⚠️ **Scale Violations**: Values outside expected ranges

## Version History

- **v1.0**: Initial comprehensive testing suite
- **v1.1**: Added binary variable tests, improved balance tolerance
- **v1.2**: Enhanced Likert scale detection, fixed SE03 handling
- **v2.0**: **Complete cases only focus + specific survey logic tests**
  - Added complete cases filtering
  - New tests: MAXPAGE≤11, LASTPAGE=MAXPAGE, Q_VIEWER=0, STATUS=empty
  - AB01 assignment control validation
  - SO01_01 age validation
  - Enhanced survey logic compliance

---

💡 **Tip**: The script now catches survey implementation errors that would make the dataset unsuitable for analysis. Fix all failures before proceeding with statistical analysis. 