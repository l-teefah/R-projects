# Automated Regression Analysis Script

## Overview
This R script automates the process of running regression analyses on a scheduled basis. It loads data from a specified location, performs regression modeling, and generates comprehensive HTML reports with visualizations and statistical summaries.

## Features
- **Automated Regression Modeling**: Runs linear regression models with configurable dependent and independent variables
- **Comprehensive HTML Reports**: Generates detailed reports with tables, statistics, and diagnostic plots
- **Scheduling Capability**: Can be scheduled to run automatically at specified intervals
- **Error Handling**: Includes robust error handling and logging
- **Customizable**: Easy configuration for data sources and model specifications

## Requirements
- R (version 3.6.0 or higher recommended)
- Required R packages:
  - tidyverse
  - rmarkdown
  - knitr
  - broom
  - ggplot2
  - lubridate
  - readxl (if working with Excel files)

## Installation
1. Save the script to your preferred location
2. Open the script in an R editor (RStudio recommended)
3. Modify the configuration settings (see below)
4. Set up scheduling as needed (see "Scheduling" section)

## Configuration
Edit the following variables in the [script](Automated_Regression_analysis.R) to match your environment:

```r
# ----- Configuration Settings (modify these) -----
data_path <- "C:/path/to/your/data/file.csv"  # Change to your data file path
output_dir <- "C:/path/to/reports"            # Change to your desired output directory
dependent_var <- "y"                          # Name of your dependent variable
independent_vars <- c("x1", "x2", "x3")       # Names of your independent variables
```

## Usage
### Manual Execution
To run the script manually:
1. Open R or RStudio
2. Source the script: `source("path/to/regression_analysis.R")`

### Scheduling
#### Windows (Task Scheduler)
1. Open Task Scheduler
2. Create a new task
3. Set the trigger (e.g., daily at 8 AM)
4. Action: Start a program
5. Program/script: `"C:\Program Files\R\R-4.x.x\bin\Rscript.exe"` (path to your Rscript.exe)
6. Add arguments: `C:\path\to\regression_analysis.R`

#### Mac/Linux (cron)
1. Open terminal
2. Type `crontab -e`
3. Add a line like: `0 8 * * * Rscript /path/to/regression_analysis.R`
4. Save and exit

#### Using R packages
For more complex scheduling needs, consider using packages like:
- `taskscheduleR` (Windows-focused)
- `cronR` (for Linux/Mac systems)

## Output
The script generates:
1. HTML report files with timestamps in the specified output directory
2. A log file tracking execution history and any errors

### HTML Report Contents
- Data summary statistics
- Regression model summary
- Coefficient table with confidence intervals
- Model fit statistics
- Diagnostic plots:
  - Residuals vs Fitted
  - Normal Q-Q Plot
  - Scale-Location Plot
  - Residuals vs Leverage
- Correlation matrix
- Scatter plot matrix

## Troubleshooting
- Check the log file (`regression_log.txt` in your output directory) for error messages
- Ensure all required packages are installed
- Verify that file paths are correct and accessible
- Make sure column names in your configuration match those in your data file
- For scheduling issues, check system logs for task execution errors

## Customization
The script can be customized further by:
- Modifying the R Markdown template in the `create_rmd_template()` function
- Adding additional diagnostic plots or analyses
- Changing the HTML theme or formatting options
- Adding email notifications upon completion or errors

## License
This script is provided as-is under an open-source license. Feel free to modify and distribute as needed.
