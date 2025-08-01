<?php
/**
 * SoSci Survey Rücklauf im Zeitverlauf Chart Generator
 * 
 * Generates a bar chart similar to SoSci Survey's response timeline chart
 * using jpGraph library with specific styling requirements.
 */

// Include Composer autoloader
require_once __DIR__ . '/vendor/autoload.php';

// Include jpGraph library
use Amenadiel\JpGraph\Graph\Graph;
use Amenadiel\JpGraph\Plot\BarPlot;

// Create the graph
$graph = new Graph(800, 300);

// Set up the graph
$graph->SetScale('textlin');
$graph->SetMargin(40, 20, 15, 40); // left, right, top, bottom - even tighter margins like the image

// Disable frame
$graph->SetFrame(false);

// Set background color to white
$graph->SetBackgroundGradient('white', 'white', GRAD_HOR, BGRAD_PLOT);

// Create date range from July 1, 2025 to July 21, 2025 (21 days as shown in image)
$startDate = new DateTime('2025-07-01');
$endDate = new DateTime('2025-07-21');
$interval = new DateInterval('P1D');
$datePeriod = new DatePeriod($startDate, $interval, $endDate);

// Initialize data arrays
$dates = [];
$responseCounts = [];

// Generate data for each day
foreach ($datePeriod as $date) {
    $dates[] = $date->format('d.m'); // Format as DD.MM
    $day = $date->format('j'); // Day without leading zero
    
    // Set response counts: 1 on July 15 and 17 (as shown in image), 0 elsewhere
    if ($day == 15 || $day == 17) {
        $responseCounts[] = 1;
    } else {
        $responseCounts[] = 0;
    }
}

// Create the bar plot
$bplot = new BarPlot($responseCounts);

// Set bar color to light gray (matching the image)
$bplot->SetFillColor('#D0D0D0');

// Set bar width - thinner bars as shown in image
$bplot->SetWidth(0.5);

// Add the bar plot to the graph
$graph->Add($bplot);

// Set x-axis labels (dates) - show every other day as in image
$tickLabels = [];
foreach ($dates as $i => $date) {
    if ($i % 2 == 0) { // Show every other date
        $tickLabels[] = $date;
    } else {
        $tickLabels[] = '';
    }
}
$graph->xaxis->SetTickLabels($tickLabels);

// Rotate x-axis labels to 90 degrees
$graph->xaxis->SetLabelAngle(90);

// Set x-axis line color to orange
$graph->xaxis->SetColor('orange');

// Set y-axis properties - only show 0 and 1
$graph->yaxis->SetColor('black');
$graph->yaxis->SetLabelFormat('%d');
$graph->yaxis->SetTickPositions([0, 1], [0, 1]);

// Remove grid for cleaner look
$graph->SetGridDepth(DEPTH_FRONT);
$graph->ygrid->Show(false);
$graph->xgrid->Show(false);

// Remove title and legend
$graph->title->Hide();
$graph->legend->Hide();

// Output the chart as PNG directly in the browser
$graph->Stroke();
?> 