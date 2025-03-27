# Install and load the needed packages
#install.packages("writexl")
library(writexl)
library(tidyverse)
library(knitr)

# Load dataset 
df <- read.csv("Customer_support_data.csv", stringsAsFactors = FALSE)

# remove rows that didn't import properly
df <- df %>% 
  slice(-c(15437:15484, 45128, 45176))

# change CSAT to numeric and channel_name to factor
df <- df %>% mutate(CSAT.Score=as.numeric(CSAT.Score))
df <- df %>% mutate(Item_price=as.numeric(Item_price))

##########
#cleaning survey dates column
## survey response date has have wildly different lengths. Some are 9 characters (the expected "01-Aug-23" format), 
## but many are extremely long—up to 59765 characters which explains the "input string is too long" error when I try to convert it to date format
unique(df$Survey_response_Date)

# check date strings are the same length
all_lengths <- unique(nchar(df$Survey_response_Date))
print(all_lengths)

# Look at the actual byte representation of a sample string to reveal hidden characters
print(charToRaw(df$Survey_response_Date[1]))

# Let's try parsing a single value first as a test
test_date <- as.Date("01-Aug-23", format = "%d-%b-%y")
print(test_date)

# Try creating a new data frame with just a few rows to test
test_df <- data.frame(date_str = c("01-Aug-23", "02-Aug-23", "03-Aug-23"))
test_df$date_parsed <- as.Date(test_df$date_str, format = "%d-%b-%y")
print(test_df)

# Create new column for the cleaned dates
df$Clean_Survey_Date <- NA

# process strings that look like actual dates
date_pattern <- "^\\d{2}-[A-Za-z]{3}-\\d{2}$"
valid_dates <- grepl(date_pattern, df$Survey_response_Date) & nchar(df$Survey_response_Date) == 9

# Convert only the valid dates
df$Clean_Survey_Date[valid_dates] <- as.Date(df$Survey_response_Date[valid_dates], format = "%d-%b-%y")

# How many valid dates did we find?
sum(valid_dates)

# Look at the first few problematic entries
head(df$Survey_response_Date[!valid_dates])

# Count how many values are valid vs invalid - true(53133) & false(50)
table(valid_dates)

# Try to extract date patterns from longer strings
for (i in which(!valid_dates)) {
  # Look for date patterns in the text
  date_match <- regexpr("\\d{2}-[A-Za-z]{3}-\\d{2}", df$Survey_response_Date[i])
  if (date_match > 0) {
    extracted_date <- substr(df$Survey_response_Date[i], date_match, date_match + 8)
    df$Clean_Survey_Date[i] <- as.Date(extracted_date, format = "%d-%b-%y")
  }
}

# Create a new column for the cleaned dates
df$Clean_Survey_Date <- NA

# Convert only the valid dates 
date_pattern <- "^\\d{2}-[A-Za-z]{3}-\\d{2}$"
valid_dates <- grepl(date_pattern, df$Survey_response_Date) & nchar(df$Survey_response_Date) == 9
df$Clean_Survey_Date[valid_dates] <- as.Date(df$Survey_response_Date[valid_dates], format = "%d-%b-%y")

# Check the result
head(df$Clean_Survey_Date)

# class of Clean_Date column 
class(df$Clean_Survey_Date)

# date was stored in epoch format "19570"
# convert the numeric values to actual dates and save in new column while assuming those numbers are days since 1970-01-01 (the default for R)
df$survey_date <- as.Date(as.numeric(df$Clean_Survey_Date), origin = "1970-01-01")

# Check the result
head(df$survey_date)

######
# cleaning channel name column
## channel name also have wildly different lengths. 
unique(df$channel_name)

# Define the valid channel names
valid_channels <- c("Inbound", "Outcall", "Email")

# Function to extract channel names
extract_channel <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid channels
  matching_channels <- parts[parts %in% valid_channels]
  
  if (length(matching_channels) > 0) {
    return(matching_channels[1])  # Return the first match if multiple exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean channel column
df$clean_channel_name <- sapply(df$channel_name, extract_channel)

# Check the results
table(df$clean_channel_name, useNA = "ifany")

# Check how many records couldn't be mapped
missing_channels <- sum(is.na(df$clean_channel_name))
percent_missing <- (missing_channels / nrow(df)) * 100
cat("Records without a valid channel:", missing_channels, 
    "(", format(percent_missing, digits = 2), "%)", "\n")

# re-confirm if column parsed well
unique(df$clean_channel_name)

#######
# cleaning product category column
unique(df$Product_category)

# Define the valid product category names
valid_product_category <- c("Electronics", "LifeStyle", "Books & General merchandise", "Mobile", "Home", "Furniture", "Home Appliences")

# Function to extract product category names
extract_product_category <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid product category
  matching_product_category <- parts[parts %in% valid_product_category]
  
  if (length(matching_product_category) > 0) {
    return(matching_product_category[1])  # Return the first match if multiple exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean column
df$clean_product_category <- sapply(df$Product_category, extract_product_category)

# Check the results
table(df$clean_product_category, useNA = "ifany")

# Check how many records couldn't be mapped
missing_product_category <- sum(is.na(df$clean_product_category))
percent_missing_product <- (missing_product_category / nrow(df)) * 100
cat("Records without a valid product category:", missing_product_category, 
    "(", format(percent_missing_product, digits = 2), "%)", "\n")

# re-confirm in column parsed well
unique(df$clean_product_category)

# rename "home appliences" to "home appliances"
df$clean_product_category[df$clean_product_category == "Home Appliences"] <- "Home Appliances"

########
# cleaning agent shift column
unique(df$Agent.Shift)

# Define the valid agent shift names
valid_agent_shifts <- c("Morning", "Afternoon", "Evening", "Split", "Night")

# Function to extract agent shifts names
extract_shift <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid agent shift
  matching_shift <- parts[parts %in% valid_agent_shifts]
  
  if (length(matching_shift) > 0) {
    return(matching_shift[1])  # Return the first match if multiple exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean column
df$clean_agent_shifts <- sapply(df$Agent.Shift, extract_shift)

# Check the results
table(df$clean_agent_shifts, useNA = "ifany")

# Check how many shifts couldn't be mapped
missing_agent_shifts <- sum(is.na(df$clean_agent_shifts))
percent_missing_shift <- (missing_agent_shifts / nrow(df)) * 100
cat("Records without a valid agent shift value:", missing_agent_shifts, 
    "(", format(percent_missing_shift, digits = 2), "%)", "\n")

# re-confirm if column parsed well
unique(df$clean_agent_shifts) 

########
# cleaning tenure bucket column
unique(df$Tenure.Bucket)

# Define the valid tenure names
valid_tenure <- c("On Job Training", ">90", "0-30", "31-60", "61-90")

# Function to extract tenure bucket names
extract_tenure <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid tenure buckets
  matching_tenure <- parts[parts %in% valid_tenure]
  
  if (length(matching_tenure) > 0) {
    return(matching_tenure[1])  # Return the first match if multiple exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean column
df$clean_tenure <- sapply(df$Tenure.Bucket, extract_tenure)

# Check the results
table(df$clean_tenure, useNA = "ifany")

# Check how many shifts couldn't be mapped
missing_tenure <- sum(is.na(df$clean_tenure))
percent_missing_tenure <- (missing_tenure / nrow(df)) * 100
cat("Records without a valid tenure:", missing_tenure, 
    "(", format(percent_missing_tenure, digits = 2), "%)", "\n")

# re-confirm if column parsed well
unique(df$clean_tenure) 

############
# checking to see category and sub category columns imported properly
# both columns are corrupted
unique(df$category)
unique(df$Sub.category)

# cleaning category column
# Define the valid channel names
valid_category <- c("Product Queries", "Order Related", "Returns",  "Cancellation",  
                    "Shopzilla Related", "Payments related", "Refund Related", "Feedback", "Others", 
                    "App/website", "Offers & Cashback", "Onboarding related")

# Function to extract channel names
extract_category <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid channels
  matching_category <- parts[parts %in% valid_category]
  
  if (length(matching_category) > 0) {
    return(matching_category[1])  # Return the first match if multiple exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean channel column
df$clean_category <- sapply(df$category, extract_category)

# Check the results
table(df$clean_category, useNA = "ifany")

# Check how many records couldn't be mapped
missing_category <- sum(is.na(df$clean_category))
percent_missing_category <- (missing_category / nrow(df)) * 100
cat("Records without a valid category:", missing_category, 
    "(", format(percent_missing_category, digits = 2), "%)", "\n")

# re-confirm if column parsed well
unique(df$clean_category)

# cleaning sub category column
# Define the valid sub category names
valid_sub_category <- c("Life Insurance", "Product Specific Information", "Installation/demo", 
                        "Reverse Pickup Enquiry", "Not Needed", "Fraudulent User", "Product Specific Information", 
                        "Exchange / Replacement", "Missing", "General Enquiry", "Return request", "Delayed", 
                        "Service Centres Related", "Order status enquiry", "Return cancellation", "Unable to track", 
                        "Seller Cancelled Order", "Wrong", "Priority delivery", "Refund Related Issues", "Signup Issues", 
                        "Invoice request", "Online Payment Issues", "Technician Visit", "UnProfessional Behaviour", "Damaged", 
                        "Call back request", "Billing Related", "Payment related Queries", "Wrong", "Shopzilla Rewards", "COD Refund Details", 
                        "Wallet related", "PayLater related", "Customer Requested Modifications", "Refund Enquiry", "Account updation", 
                        "Priority delivery", "Issues with Shopzilla App", "Other Cashback", "Shopzila Premium Related", "Seller onboarding", 
                        "Affiliate Offers", "Service Center - Service Denial", "Product related Issues", "Instant discount", "Other Account Related Issues",
                        "Card/EMI", "Order Verification", "Call disconnected", "App/website Related", "Warranty related", "Others", "e-Gift Voucher",
                        "Unable to Login", "Non Order related", "Self-Help", "Payment pending", "Commission related", "Policy Related")

# Function to extract sub category names
extract_sub_category <- function(text) {
  if (is.na(text)) {
    return(NA)
  }
  
  # Split by commas
  parts <- strsplit(as.character(text), ",")[[1]]
  
  # Trim whitespace from each part
  parts <- trimws(parts)
  
  # Check if any part matches valid sub categories
  matching_sub_category <- parts[parts %in% valid_sub_category]
  
  if (length(matching_sub_category) > 0) {
    return(matching_sub_category[1])  # Return the first match if multiples exist
  } else {
    return(NA)
  }
}

# Apply the function to create a clean sub category column
df$clean_sub_category <- sapply(df$Sub.category, extract_sub_category)

# Check the results
table(df$clean_sub_category, useNA = "ifany")

# Check how many records couldn't be mapped
missing_sub_category <- sum(is.na(df$clean_sub_category))
percent_missing_sub_category <- (missing_sub_category / nrow(df)) * 100
cat("Records without a sub category:", missing_sub_category, 
    "(", format(percent_missing_sub_category, digits = 2), "%)", "\n")

# re-confirm if column parsed well
unique(df$clean_sub_category)

######
# Convert date columns to datetime format
datetime_columns <- c("order_date_time", "Issue_reported.at", "issue_responded")
for (col in datetime_columns) {
  if (col %in% colnames(df)) {
    df[[col]] <- dmy_hm(df[[col]], quiet = TRUE)
  }
}

# Calculate and create response time in minutes
if ("Issue_reported.at" %in% colnames(df) && "issue_responded" %in% colnames(df)) {
  df$Response_time_min <- as.numeric(difftime(df$issue_responded, df$Issue_reported.at, units = "mins"))
}

# Calculate and create response time in hours
if ("Issue_reported.at" %in% colnames(df) && "issue_responded" %in% colnames(df)) {
  df$response_time_hr <- as.numeric(difftime(df$issue_responded, df$Issue_reported.at, units = "hours"))
}

# Create day of week and hour features if not already done
if ("Issue_reported.at" %in% colnames(df)) {
  df$Day_of_week <- wday(df$Issue_reported.at, label = TRUE)
  df$Hour_of_day <- hour(df$Issue_reported.at)
  df$Week_of_month <- week(df$Issue_reported.at) - week(floor_date(df$Issue_reported.at, unit = "month")) + 1
} 


##########
# Extract specific columns
selected_columns <- df[, c("Unique.id", "survey_date", "clean_channel_name", "clean_category", "clean_sub_category", "CSAT.Score", "Supervisor",
                           "clean_product_category", "Customer_City", "clean_tenure", "clean_agent_shifts", "Agent_name",
                           "Item_price", "Response_time_min", "response_time_hr", "Day_of_week", "Hour_of_day", "Week_of_month", "Manager")]

# truncate long text (Unique.id)
selected_columns[] <- lapply(selected_columns, function(x) {
  if (is.character(x)) substr(x, 1, 32000) else x
})

# Save to an Excel file
write_xlsx(selected_columns, "customer_data.xlsx")
