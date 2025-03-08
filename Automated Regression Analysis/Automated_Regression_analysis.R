# Automated Regression Analysis Script
# This script:
# 1. Loads data from a specified location
# 2. Runs a regression model
# 3. Generates an HTML report with results
# 4. Can be scheduled to run automatically

# ----- Required Packages -----
# Install packages if not already installed
#install.packages("tidyverse")
#install.packages("rmarkdown")
#install.packages("knitr")
#install.packages("broom")
#install.packages("ggplot2")
#install.packages("lubridate")

library(tidyverse)  # For data manipulation
library(rmarkdown)  # For report generation
library(knitr)      # For knitr options
library(broom)      # For converting model outputs to tidy data frames
library(ggplot2)    # For plotting
library(lubridate)  # For date handling

# ----- Configuration Settings (modify these, example shown below) ----
#data_path <- "/Users/lateefahyusuf/Library/Mobile Documents/com~apple~CloudDocs/Documents/Data Analytics/Firms.xlsx"  
#output_dir <- "/Users/lateefahyusuf/Library/Mobile Documents/com~apple~CloudDocs/Documents/Data Analytics"    
#dependent_var <- "avg_productivity"                        
#independent_vars <- c("CAPITAL", "FEES", "x3", .....)   

data_path <- "/path/to/your/data/file.csv"  # Change to your data file path
output_dir <- "/path/to/reports"            # Change to your desired output directory
dependent_var <- "y"                          # Name of your dependent variable
independent_vars <- c("x1", "x2", "x3")       # Names of your independent variables

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ----- Function to load data -----
load_data <- function(file_path) {
  # Determine file type and read accordingly
  file_ext <- tools::file_ext(file_path)
  
  data <- switch(file_ext,
                 "csv" = read.csv(file_path, stringsAsFactors = FALSE),
                 "xlsx" = readxl::read_excel(file_path),
                 "rds" = readRDS(file_path),
                 stop("Unsupported file format. Please use CSV, XLSX, or RDS.")
  )
  
  return(data)
}

# ----- Function to run regression model -----
run_regression <- function(data, dependent, independents) {
  # Create the formula for the regression
  formula_str <- paste(dependent, "~", paste(independents, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Run the regression model
  model <- lm(formula_obj, data = data)
  
  return(model)
}

# ----- Function to create R Markdown report template -----
create_rmd_template <- function(model, data, dependent, independents) {
  # Generate timestamp for report
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  report_file <- file.path(output_dir, paste0("regression_report_", timestamp, ".Rmd"))
  
  # Create R Markdown template
  rmd_content <- paste0("---
title: \"Automated Regression Analysis Report\"
author: \"Automated Script\"
date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\"
output: 
  html_document:
    toc: true
    toc_float: true
    theme: cosmo
    highlight: tango
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
library(tidyverse)
library(broom)
library(ggplot2)
```

## Summary of Data

```{r}
# Data summary
summary_data <- summary(data)
knitr::kable(as.data.frame(t(summary(data[, c('", paste(c(dependent, independents), collapse = "', '"), "')]))))
```

## Regression Model

The following regression model was fit:

$", dependent, " \\sim ", paste(independents, collapse = " + "), "$

### Model Summary

```{r}
# Model summary
summary(model)
```

### Coefficients Table

```{r}
# Tidy coefficients table
knitr::kable(tidy(model, conf.int = TRUE))
```

### Model Fit

```{r}
# Model fit statistics
glance_model <- glance(model)
knitr::kable(glance_model)
```

## Diagnostic Plots

### Residuals vs Fitted

```{r, fig.width=8, fig.height=6}
ggplot(augment(model), aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = \"dashed\", color = \"red\") +
  geom_smooth(se = FALSE, color = \"blue\") +
  labs(title = \"Residuals vs Fitted\",
       x = \"Fitted values\",
       y = \"Residuals\")
```

### Normal Q-Q Plot

```{r, fig.width=8, fig.height=6}
ggplot(augment(model), aes(sample = .std.resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = \"Normal Q-Q Plot\",
       x = \"Theoretical Quantiles\",
       y = \"Standardized Residuals\")
```

### Scale-Location Plot

```{r, fig.width=8, fig.height=6}
ggplot(augment(model), aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE, color = \"blue\") +
  labs(title = \"Scale-Location Plot\",
       x = \"Fitted values\",
       y = \"√|Standardized residuals|\")")
  
  # Add residuals vs leverage plot if sample size is sufficient
  rmd_content <- paste0(rmd_content, "
```

### Residuals vs Leverage

```{r, fig.width=8, fig.height=6}
ggplot(augment(model), aes(x = .hat, y = .std.resid)) +
  geom_point() +
  geom_smooth(se = FALSE, color = \"blue\") +
  labs(title = \"Residuals vs Leverage\",
       x = \"Leverage\",
       y = \"Standardized Residuals\")
```

## Variable Relationships

### Correlation Matrix

```{r}
cor_matrix <- cor(data[, c('", paste(c(dependent, independents), collapse = "', '"), "')])
knitr::kable(cor_matrix, digits = 3)
```

### Scatter Plot Matrix

```{r, fig.width=10, fig.height=10}
pairs(data[, c('", paste(c(dependent, independents), collapse = "', '"), "')], 
      main = \"Scatter Plot Matrix\")
```

## Conclusion

This report was automatically generated on ", format(Sys.time(), "%Y-%m-%d at %H:%M:%S"), ".
")
  
  # Write R Markdown template to file
  writeLines(rmd_content, report_file)
  
  return(report_file)
}

# ----- Main execution function -----
run_analysis <- function() {
  # Set up logging
  log_file <- file.path(output_dir, "regression_log.txt")
  cat(paste0("Analysis started at ", Sys.time(), "\n"), 
      file = log_file, append = TRUE)
  
  tryCatch({
    # Load data
    cat("Loading data...\n", file = log_file, append = TRUE)
    data <- load_data(data_path)
    
    # Run regression model
    cat("Running regression model...\n", file = log_file, append = TRUE)
    model <- run_regression(data, dependent_var, independent_vars)
    
    # Create R Markdown template
    cat("Creating R Markdown template...\n", file = log_file, append = TRUE)
    rmd_file <- create_rmd_template(model, data, dependent_var, independent_vars)
    
    # Render HTML report
    cat("Rendering HTML report...\n", file = log_file, append = TRUE)
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    html_file <- file.path(output_dir, paste0("regression_report_", timestamp, ".html"))
    
    render(rmd_file, output_file = html_file, envir = new.env())
    
    cat(paste0("Analysis completed at ", Sys.time(), 
               "\nHTML report saved to: ", html_file, "\n"), 
        file = log_file, append = TRUE)
    
    # Return path to the generated HTML file
    return(html_file)
    
  }, error = function(e) {
    error_msg <- paste0("ERROR at ", Sys.time(), ": ", e$message, "\n")
    cat(error_msg, file = log_file, append = TRUE)
    stop(error_msg)
  })
}

# ----- Execute the analysis -----
html_report <- run_analysis()
cat(paste0("Analysis completed. HTML report saved to: ", html_report, "\n"))