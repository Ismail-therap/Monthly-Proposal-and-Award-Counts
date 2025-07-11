# ==============================================
# SCRIPT 0: PACKAGES AND DATA PATH CONFIGURATION 
# ==============================================

# 📌 Load Required Libraries
library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(readxl)     # For reading Excel files
library(lubridate)  # For handling date-time conversions
library(stringr)    # For string operations
library(tidyr)      # For reshaping data

# =====================================================
# STEP 1: Define Base Path for Data
# =====================================================

# 📌 Base Data Directory
base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Monthly Summary Analysis From Cayuse Data 04292025/Input"



# 📌 Define File Paths Using Base Path
# Cayuse Data
proposal_data_path <- file.path(base_path, "Cayuse Data/Proposal_Data_All_06302025.csv")
award_data_path <- file.path(base_path, "Cayuse Data/Award_Data_All_07082025.csv")


wyocloud_path <- file.path(base_path, "WyoCloud Data/")
project_financial_summary_path <- file.path(wyocloud_path, "Project Financial Summary_Results_04152025.xlsx")
project_information_path <- file.path(wyocloud_path, "Project Information_Results_04182025.xlsx")

# Award External Funding Data
college_name_data_path <- file.path(wyocloud_path, "Award External Funding By Dept_Details.xlsx")


Ashlee_path <- file.path(base_path, "Ashlee/")
college_name_data_path_Ashlee <- file.path(Ashlee_path, "College_Admin_Unit_Mapping.xlsx")

# =====================================================
# STEP 2: Define Output Paths
# =====================================================

# 📌 Define Base Output Directory
output_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Monthly Summary Analysis From Cayuse Data 04292025/Output/06302025/"

# 📌 Define Output File Names
# output_merged_fsu_award_path <- file.path(output_path, "filtered_award_non_award_with_FSU_031925.csv")
# output_no_submission_path <- file.path(output_path, "pi_without_any_submission_031925.csv")

# =====================================================
# STEP 3: Export Paths as Environment Variables
# =====================================================


Sys.setenv(PROJECT_FINANCIAL_SUMMARY_PATH = project_financial_summary_path)
Sys.setenv(PROJECT_INFORMATION_PATH = project_information_path)
Sys.setenv(COLLEGE_NAME_DATA_PATH = college_name_data_path)
Sys.setenv(COLLEGE_NAME_DATA_PATH_ASHLEE = college_name_data_path_Ashlee)
Sys.setenv(PROPOSAL_DATA_PATH = proposal_data_path)
Sys.setenv(AWARD_DATA_PATH = award_data_path)


#========================================================
# STEP 4: Required Functions
#========================================================

# To create the FY

library(lubridate)

get_fy <- function(date, fy_start_month = 7) {
  date <- as.Date(date)  # ensures POSIXct/t becomes Date
  ifelse(
    is.na(date),
    NA_character_,
    paste0("FY", year(date) + ifelse(month(date) >= fy_start_month, 1, 0))
  )
}


# Get the filtered data for selected month and year

# Function to filter by month name and year
filter_proposals_by_month_year <- function(data, month_name, year_val) {
  month_num <- match(tolower(month_name), tolower(month.name))  # Convert month name to number
  if (is.na(month_num)) {
    stop("Invalid month name. Please use full month name like 'March'.")
  }
  
  filtered_data <- data %>%
    filter(
      !is.na(Actual_Submission_Date),
      year(Actual_Submission_Date) == year_val,
      month(Actual_Submission_Date) == month_num
    )
  
  return(filtered_data)
}


# Get the proposal count by the correspondig status

create_fy_summary <- function(data) {
  library(dplyr)
  library(tidyr)
  
  # Step 1: Count per FY category
  fy_submission <- data %>%
    count(Actual_Submission_FY, name = "Submission")
  
  fy_consideration <- data %>%
    count(Actual_Udr_Consid_FY, name = "Consideration")
  
  fy_funding <- data %>%
    count(Actual_Funding_FY, name = "Funding")
  
  fy_not_funding <- data %>%
    count(Actual_Not_Funding_FY, name = "Not Funded")
  
  # Step 2: Merge on fiscal year
  fy_summary <- full_join(fy_submission, fy_consideration, 
                          by = c("Actual_Submission_FY" = "Actual_Udr_Consid_FY")) %>%
    full_join(fy_funding, by = c("Actual_Submission_FY" = "Actual_Funding_FY")) %>%
    full_join(fy_not_funding, by = c("Actual_Submission_FY" = "Actual_Not_Funding_FY")) %>%
    rename(FY = Actual_Submission_FY) %>%
    arrange(FY) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
    filter(!is.na(FY))
  
  return(fy_summary)
}



# Define a function for state fiscal quarters
get_fiscal_quarter <- function(date) {
  if (is.na(date)) return(NA)
  m <- month(date)
  y <- year(date)
  
  if (m %in% 7:9) {
    return(paste("Q1", paste0("FY", y + 1)))
  } else if (m %in% 10:12) {
    return(paste("Q2", paste0("FY", y + 1)))
  } else if (m %in% 1:3) {
    return(paste("Q3", paste0("FY", y)))
  } else if (m %in% 4:6) {
    return(paste("Q4", paste0("FY", y)))
  } else {
    return(NA)
  }
}