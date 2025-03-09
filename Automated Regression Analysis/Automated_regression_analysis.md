
# Automated Regression Analysis Scripts

This folder contains two R scripts for automating regression analysis workflows, both R scripts automates the process of running regression analyses on a scheduled basis. They load data from a specified location, performs regression modeling, and generates comprehensive HTML reports with visualizations and statistical summaries.

1. [**Automated_Regression_Analysis.R**](Automated_Regression_analysis.R) - Basic version that generates HTML reports.
2. [**Automated_Regression_Analysis_Script_with_Email_Notifications.R**](Automated_Regression_Analysis_Script_with_Email_Notifications.R) - Enhanced version that also sends email notifications.

## Overview
These scripts automate the process of:
- Loading data from specified file paths
- Running linear regression models
- Generating comprehensive HTML reports with diagnostic plots
- Logging all activities (with timestamps)
- Optionally sending email notifications with reports attached (email version only)

## Features
- **Automated Regression Modeling**: Runs linear regression models with configurable dependent and independent variables
- **Comprehensive HTML Reports**: Generates detailed reports with tables, statistics, and diagnostic plots
- **Scheduling Capability**: Can be scheduled to run automatically at specified intervals
- **Error Handling**: Includes robust error handling and logging
- **Customizable**: Easy configuration for data sources and model specifications
  
## Requirements

### Required R Packages
Both scripts require:
- R (version 3.6.0 or higher recommended)
- tidyverse (data manipulation)
- rmarkdown (report generation)
- knitr (formatting options)
- broom (model output formatting)
- ggplot2 (plotting)
- lubridate (date handling)
- readxl (for Excel files, used implicitly)

The email notification script additionally requires:
- emayili (alternative email package)

### Installation

```R
# Install base packages
install.packages(c("tidyverse", "rmarkdown", "knitr", "broom", "ggplot2", "lubridate", "readxl"))

# Install email packages (for email notification script only)
install.packages("emayili")
```

## Usage

### Basic Script - Automated_Regression_Analysis.R

1. Open the script in R or RStudio
2. Modify the configuration settings:
   
   ```R
   data_path <- "/path/to/your/data/file"  # Your data file path
   output_dir <- "/path/to/reports"            # Where reports should be saved
   dependent_var <- "y"                        # Your dependent variable
   independent_vars <- c("x1", "x2", "x3")     # Your independent variables
   ```
   
3. Run the script

### Email Notification Script - Automated_Regression_Analysis_Script_with_Email_Notifications.R

1. Open the script in R or RStudio
2. Modify all configuration settings including:

   ```R
   # Basic Settings
   data_path <- "/path/to/your/data/file.csv"  # Your data file path
   output_dir <- "/path/to/reports"            # Where reports should be saved
   dependent_var <- "y"                        # Your dependent variable
   independent_vars <- c("x1", "x2", "x3")     # Your independent variables
   
   # Email configuration
   send_email <- TRUE                         # Set to FALSE to disable email
   email_to <- "recipient@example.com"        # Recipient email
   email_from <- "your.email@example.com"     # Sender email
   email_subject <- "Automated Regression Analysis Report"
   
   # SMTP server configuration
   smtp_server <- "smtp.example.com"          # SMTP server address
   smtp_port <- 587                           # SMTP port (usually 587 for TLS, 465 for SSL)
   smtp_user <- "your.email@example.com"      # SMTP username
   smtp_password <- "your_password"           # SMTP password
   ```
   
3. Run the script

## Input Data Format

The scripts support loading data from:
- CSV files (.csv)
- Excel files (.xlsx)
- R data files (.rds)

Your data should contain:
- One column for the dependent variable
- One or more columns for independent variables
- Column names in the dataset should match the variables specified in the configuration

## Output

### Generated Files
Both scripts produce:
1. An R Markdown file (.Rmd) with analysis code
2. An HTML report with results, plots, and diagnostics
3. A log file (regression_log.txt) tracking script execution

The email script also creates:
- Email credentials file (if using Blastula)
- Error report (if failures occur)

### HTML Report Contents
The generated HTML report includes:
- Data summary statistics
- Regression model formula
- Model summary with coefficients
- Confidence intervals
- Model fit statistics (R², adjusted R², etc.)
- Diagnostic plots:
  - Residuals vs Fitted
  - Normal Q-Q Plot
  - Scale-Location Plot
  - Residuals vs Leverage
- Correlation matrix
- Scatter plot matrix
- Timestamp of generation
  
## Scheduling

### Windows (Task Scheduler)
1. Open Task Scheduler
2. Create a new task
3. Set the trigger (e.g., daily at 8 AM)
4. Action: Start a program
5. Program/script: `"C:\Program Files\R\R-4.x.x\bin\Rscript.exe"` (path to your Rscript.exe)
6. Add arguments: `C:\path\to\regression_analysis.R`
### Mac/Linux (cron)
1. Open terminal
2. Type `crontab -e`
3. Add a line like: `0 8 * * * Rscript /path/to/regression_analysis.R`
4. Save and exit
### Using R packages
For more complex scheduling needs, consider using packages like:
- `taskscheduleR` (Windows-focused)
- `cronR` (for Linux/Mac systems)
  
## Troubleshooting
- Check the log file (`regression_log.txt` in your output directory or email) for error messages
- Ensure all required packages are installed
- Verify that file paths are correct and accessible
- Make sure column names in your configuration match those in your data file
- For scheduling issues, check system logs for task execution errors
  
## Security Note
The email script requires storing SMTP credentials:
- For production use, implement proper credential storage
- Consider using environment variables or secure credential managers
- Do not commit the script with credentials to public repositories

## Customization
The scripts can be customized by:
- Modifying the R Markdown template in the create_rmd_template() function
- Adding additional analyses or plots
- Changing report styling by modifying the YAML header

## License
This script is provided as-is under an open-source license. Feel free to modify and distribute as needed.
