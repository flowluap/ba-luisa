# SoSci Survey Rücklauf Chart Generator

This PHP script generates a bar chart similar to SoSci Survey's "Rücklauf im Zeitverlauf" (Response Timeline) chart using the jpGraph library.

## 📋 Requirements

- **PHP** (version 7.0 or higher)
- **jpGraph library** (assumed to be located at `/jpgraph/src/`)
- **GD extension** for PHP (for image generation)

## 🎯 Features

### Chart Specifications
- **Dimensions**: 800x300 pixels
- **Date Range**: July 1, 2025 to July 22, 2025
- **X-axis**: Dates formatted as DD.MM
- **Y-axis**: Response counts (0 or 1)
- **Data**: 1 response on July 15 and 16, 0 responses on all other days

### Styling
- **Bar Color**: Gray (`#CCCCCC`)
- **X-axis Color**: Orange
- **X-axis Labels**: Rotated 90 degrees
- **Background**: White
- **No Title**: Clean, minimal design
- **No Legend**: Simplified appearance

## 🚀 Usage

### 1. Install jpGraph
Make sure jpGraph is installed at `/jpgraph/src/` or update the require_once paths in the script.

### 2. Run the Script
```bash
php ruecklauf_chart.php
```

### 3. Web Access
If running on a web server, access via:
```
http://your-domain.com/socivi/ruecklauf_chart.php
```

## 📊 Output

The script outputs a PNG image directly to the browser showing:
- 22 bars representing each day from July 1-22, 2025
- Two visible bars (height = 1) on July 15 and 16
- Zero-height bars for all other days
- Orange x-axis line
- Gray bars
- Rotated date labels

## 🔧 Customization

### Modify Date Range
Change the start and end dates:
```php
$startDate = new DateTime('2025-07-01');
$endDate = new DateTime('2025-07-22');
```

### Modify Response Data
Update the response logic:
```php
if ($day == 15 || $day == 16) {
    $responseCounts[] = 1;
} else {
    $responseCounts[] = 0;
}
```

### Change Colors
Modify bar and axis colors:
```php
$bplot->SetFillColor('#CCCCCC'); // Bar color
$graph->xaxis->SetColor('orange'); // X-axis color
```

### Adjust Dimensions
Change chart size:
```php
$graph = new Graph(800, 300); // Width, Height
```

## 📁 File Structure

```
socivi/
├── ruecklauf_chart.php    # Main chart generator script
└── README.md             # This documentation
```

## ⚠️ Notes

- The script assumes jpGraph is installed at `/jpgraph/src/`
- Make sure PHP has write permissions if saving to file
- The chart is output directly to browser as PNG
- Date formatting follows German convention (DD.MM)

## 🎨 SoSci Survey Similarity

This chart mimics the appearance and functionality of SoSci Survey's response timeline charts with:
- Clean, minimal design
- Date-based x-axis
- Response count visualization
- Professional styling suitable for research reports 