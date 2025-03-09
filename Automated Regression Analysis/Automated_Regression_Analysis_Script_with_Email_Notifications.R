# Automated Regression Analysis Script with Email Notifications
# This script:
# 1. Loads data from a specified location
# 2. Runs a regression model
# 3. Generates an HTML report with results
# 4. Sends an email notification with the report attached
# 5. Can be scheduled to run automatically

# ----- Required Packages -----
# Uncomment and install packages if not already installed
#install.packages("tidyverse")
#install.packages("rmarkdown")
#install.packages("knitr")
#install.packages("broom")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("keyring")

# Install email packages
#install.packages("emayili")

# Load the required packages
library(tidyverse)  # For data manipulation
library(rmarkdown)  # For report generation
library(knitr)      # For knitr options
library(broom)      # For converting model outputs to tidy data frames
library(ggplot2)    # For plotting
library(lubridate)  # For date handling
library(keyring)    # For blastula create_smtp_creds_key() function

# Load email package 
library(emayili)   # Email package 

# ----- Configuration Settings (modify these, example shown below) -----
#data_path <- "/Users/lateefahyusuf/Library/Mobile Documents/com~apple~CloudDocs/Documents/Data Analytics/Firms.xlsx"  
#output_dir <- "/Users/lateefahyusuf/Library/Mobile Documents/com~apple~CloudDocs/Documents/Data Analytics"    
#dependent_var <- "avg_productivity"                        
#independent_vars <- c("CAPITAL", "FEES", "x3", .....) 

data_path <- "/path/to/your/datafile"         # Change to your data file path
output_dir <- "/path/to/reports"              # Change to your desired output directory
dependent_var <- "y"                          # Name of your dependent variable
independent_vars <- c("x1", "x2", "x3")       # Names of your independent variables

# Email configuration
send_email <- TRUE                           # Set to FALSE to disable email notifications
email_to <- "recipient@example.com"          # Email address to send reports to
email_from <- "your.email@example.com"       # Your email address
email_subject <- "Automated Regression Analysis Report" # Email subject

# Email server configuration
# For Emayili 
email_server <- "smtp.example.com"           # SMTP server e.g., "smtp.gmail.com" or "smtp.mail.yahoo.com"
email_port <- 587                           # SMTP port (usually 587 for TLS, 465 for SSL but default is 587)
email_user <- "your.email@example.com"       # SMTP username
email_password <- "your_password"            # SMTP password

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
       y = \"Sqrt|Standardized residuals|\")
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

# ----- Function to send email with Emayili -----
send_email_emayili <- function(report_path) {
  tryCatch({
    # Create email message
    email <- emayili::envelope() %>%
      emayili::from(email_from) %>%
      emayili::to(email_to) %>%
      emayili::subject(email_subject) %>%
      emayili::text(paste0(
        "Automated Regression Analysis Complete\n\n",
        "The regression analysis job has completed at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ".\n\n",
        "Please find the HTML report attached for your review.\n\n",
        "Report Details:\n",
        "- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
        "- File: ", basename(report_path), "\n\n",
        "This is an automated message. Please do not reply."
      )) %>%
      emayili::attachment(report_path)
    
    # Create SMTP server connection
    smtp <- emayili::server(
      host = email_server,
      port = as.integer(email_port),
      username = email_user,
      password = email_password
    )
    
    # Send the email
    smtp(email)
    
    cat("Email sent successfully using Emayili!\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("Failed to send email using Emayili:", e$message, "\n")
    return(FALSE)
  })
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
    
    rmarkdown::render(input = rmd_file, output_file = basename(html_file), output_dir = dirname(html_file), envir = new.env())
    
    # Send email notification if enabled
    if (send_email) {
      cat("Sending email notification...\n", file = log_file, append = TRUE)
      email_sent <- send_email_emayili(html_file)
      
      if (email_sent) {
        cat("Email notification sent successfully.\n", file = log_file, append = TRUE)
      } else {
        cat("Failed to send email notification.\n", file = log_file, append = TRUE)
      }
    }
    
    cat(paste0("Analysis completed at ", Sys.time(), 
               "\nHTML report saved to: ", html_file, "\n"), 
        file = log_file, append = TRUE)
    
    # Return path to the generated HTML file
    return(html_file)
    
  }, error = function(e) {
    error_msg <- paste0("ERROR at ", Sys.time(), ": ", e$message, "\n")
    cat(error_msg, file = log_file, append = TRUE)
    
    # Try to send error notification email if email is enabled
    if (send_email) {
      tryCatch({
        # Create a simple error report
        error_report <- file.path(output_dir, "regression_error_report.txt")
        writeLines(c(
          "Regression Analysis Error Report",
          "===============================",
          paste0("Time: ", Sys.time()),
          paste0("Error: ", e$message),
          "\nPlease check the log file for more details."
        ), error_report)
        
        # Send error notification
        send_email_emayili(error_report)
        
      }, error = function(e2) {
        cat("Failed to send error notification email: ", e2$message, "\n", 
            file = log_file, append = TRUE)
      })
    }
    
    stop(error_msg)
  })
}

# ----- Execute the analysis -----
html_report <- run_analysis()
cat(paste0("Analysis completed. HTML report saved to: ", html_report, "\n"))