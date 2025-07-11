# ================================================
# SCRIPT 1: PROPOSAL DATA CLEANING & DATE CREATION
# ================================================

source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Monthly Summary Analysis From Cayuse Data 04292025/Code/0_Data_Path_Configuration.R")
# =====================================================
# STEP 1: Load and Clean Proposal Data
# =====================================================
options(scipen = 999)  # Turn off scientific notation globally
options(digits = 22)  # Max significant digits for printing


# Load Proposal Data
proposal_data <- read_csv(Sys.getenv("PROPOSAL_DATA_PATH"))


### Filtering by Proposal type (keeping only the new proposal)


# History Action Date and Comment column created:


proposal_history_action_comment <- proposal_data %>%
  filter(!is.na(`History Comment`), str_trim(`History Comment`) != "") %>%  # Remove NA or blank comments
  mutate(`History Combined` = paste0(`History Action Date`, ": ", `History Comment`)) %>%
  group_by(`Proposal #`) %>%
  summarise(`History Action Day and Comment` = paste(`History Combined`, collapse = " >> ")) %>%
  ungroup()



proposal_data <- left_join(proposal_data,proposal_history_action_comment,by = "Proposal #")




# Subset relevant columns
Created_date <- proposal_data[, c("Proposal #", "Created Date")]

# Clean and convert Created Date to fiscal year
cleaned_Created_date <- Created_date %>%
  filter(!is.na(`Created Date`)) %>%               # Remove NA Created Dates
  distinct(`Proposal #`, .keep_all = TRUE) %>%     # Keep first per Proposal #
  mutate(`Created Date` = mdy_hms(`Created Date`), # Convert string to datetime
         Proposal_Creation_FY = get_fy(`Created Date`))              # Apply FY conversion


# Update 'Status' Based on 'Resolution' Information

proposal_data <- proposal_data %>%
  mutate(Status = case_when(
    Resolution == "PI Abandoned" & Status == "Closed" ~ "Not Submitted to Sponsor",
    Resolution == "Not Funded" & Status == "Closed" ~ "Not Funded",
    `Proposal Type` == "Transfer" ~ "Transfer",
    TRUE ~ Status
  ))
# 


# =====================================================
# STEP 2: Extract Proposal History Data
# =====================================================

proposal_history_data <- proposal_data %>%
  select(`Proposal #`, Status,`sponsor type`, `History Action`, `History Action Date`,`History Comment`) %>%
  group_by(`Proposal #`) %>%
  fill(Status, .direction = "downup") %>%  # Fill missing values both down and up
  ungroup() 


# =====================================================
# STEP 3: Clean and Pivot 'History Action' Data
# =====================================================

# Clean 'History Action' Column
clean_proposal_history_data <- proposal_history_data %>%
  mutate(History_Action_Cleaned = gsub(".*: ", "", `History Action`)) %>%
  group_by(`Proposal #`, History_Action_Cleaned) %>%
  mutate(Action_Count = row_number(),
         History_Action_Final = ifelse(Action_Count > 1, 
                                       paste0(History_Action_Cleaned, "_", Action_Count), 
                                       History_Action_Cleaned)) %>%
  ungroup()


# Pivot History Data to Wide Format
proposal_wide <- clean_proposal_history_data %>%
  select(`Proposal #`, History_Action_Final, `History Action Date`) %>%
  pivot_wider(names_from = History_Action_Final, values_from = `History Action Date`)






# =====================================================
# STEP 4: Add Submission and Funding Dates
# =====================================================

# Add 'Status' Information
proposal_wide <- clean_proposal_history_data %>%
  select(`Proposal #`, Status,`sponsor type`) %>%
  distinct() %>%
  left_join(proposal_wide, by = "Proposal #")




# Selecting columns based on the Status:



# All Under Consideration Columns
Under_Consideration_cols <- c("Submitted to Sponsor to Under Consideration")

# All Not Funded Columns
Not_Funded_cols <- c("Not Funded","Not Funded_2","Not Funded_3")

# All Submission Columns
# Submission_cols <- c(
#   "Approved to Submitted to Sponsor", "In Development to Submitted to Sponsor",
#    "In Development to Submitted to Sponsor_2",
#   "Approved to Submitted to Sponsor_2", "Under Review to Submitted to Sponsor",
#   "Under Review to Submitted to Sponsor_2", "Under Consideration to Submitted to Sponsor",
#   "Form was edited in 'Submitted to Sponsor' status_2"
# )

Submission_cols <- c(
  "Approved to Submitted to Sponsor",
  "Approved to Submitted to Sponsor_2")

# All Funding Columns
funding_cols <- c(
  "Submitted to Sponsor to Funded", "Form was edited in 'Funded' status",
  "Under Consideration to Funded", "Funded (Project Complete)",
  "Submitted to Sponsor to Funded_2"
)





# Convert Dates to POSIXct Format
existing_submission_cols <- intersect(Submission_cols, names(proposal_wide))
existing_Udr_Consid_cols <- intersect(Under_Consideration_cols, names(proposal_wide))
existing_funding_cols <- intersect(funding_cols, names(proposal_wide))
existing_not_funding_cols <- intersect(Not_Funded_cols, names(proposal_wide))




proposal_wide[existing_submission_cols] <- lapply(proposal_wide[existing_submission_cols], function(x) mdy_hms(x, tz = "America/Denver"))
proposal_wide[existing_funding_cols] <- lapply(proposal_wide[existing_funding_cols], function(x) mdy_hms(x, tz = "America/Denver"))
proposal_wide[existing_Udr_Consid_cols] <- lapply(proposal_wide[existing_Udr_Consid_cols], function(x) mdy_hms(x, tz = "America/Denver"))
proposal_wide[existing_not_funding_cols] <- lapply(proposal_wide[existing_not_funding_cols], function(x) mdy_hms(x, tz = "America/Denver"))


# Create Actual Submission and Funding Dates
proposal_wide <- proposal_wide %>%
  mutate(
    Actual_Submission_Date = pmax(!!!syms(existing_submission_cols), na.rm = TRUE),
    Actual_Submission_Date = ifelse(is.infinite(Actual_Submission_Date), NA, Actual_Submission_Date),
    
    Actual_Funding_Date = pmax(!!!syms(existing_funding_cols), na.rm = TRUE),
    Actual_Funding_Date = ifelse(is.infinite(Actual_Funding_Date), NA, Actual_Funding_Date),
    
    Actual_Udr_Consid_Date = pmax(!!!syms(existing_Udr_Consid_cols), na.rm = TRUE),
    Actual_Udr_Consid_Date = ifelse(is.infinite(Actual_Udr_Consid_Date), NA, Actual_Udr_Consid_Date),
    
    Actual_Not_Funding_Date = pmax(!!!syms(existing_not_funding_cols), na.rm = TRUE),
    Actual_Not_Funding_Date = ifelse(is.infinite(Actual_Not_Funding_Date), NA, Actual_Not_Funding_Date)
    
  )

# Format Dates for Final Output
proposal_wide <- proposal_wide %>%
  mutate(
    Actual_Submission_Date = as.POSIXct(Actual_Submission_Date, origin = "1970-01-01", tz = "America/Denver"),
    Actual_Funding_Date = as.POSIXct(Actual_Funding_Date, origin = "1970-01-01", tz = "America/Denver"),
    Actual_Udr_Consid_Date = as.POSIXct(Actual_Udr_Consid_Date, origin = "1970-01-01", tz = "America/Denver"),
    Actual_Not_Funding_Date = as.POSIXct(Actual_Not_Funding_Date, origin = "1970-01-01", tz = "America/Denver")
  )




# Making Sure at least the Proposal Submitted
proposal_wide <- proposal_wide %>%
  filter(!is.na(Actual_Submission_Date))




# =====================================================
# STEP 5: Finalizing Cleaned Proposal Data
# =====================================================

# Select Submission and Funding Dates
proposal_data_submission_funding_date <- proposal_wide %>%
  select(`Proposal #`, Actual_Submission_Date,Actual_Udr_Consid_Date, Actual_Funding_Date,Actual_Not_Funding_Date)

# Merge Submission/Funding Dates with Main Proposal Data
proposal_data <- merge(proposal_data, proposal_data_submission_funding_date, by = "Proposal #")


#========================================================================
# STEP 6: Creating the FY columns from the Dates by the Status
#========================================================================

proposal_data <- proposal_data %>%
  mutate(
    `Created Date` = as.POSIXct(`Created Date`, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC"),
    Actual_Submission_Date = ymd_hms(Actual_Submission_Date, tz = "America/Denver"),
    Actual_Udr_Consid_Date = ymd_hms(Actual_Udr_Consid_Date, tz = "America/Denver"),
    Actual_Funding_Date = ymd_hms(Actual_Funding_Date, tz = "America/Denver"),
    Actual_Not_Funding_Date = ymd_hms(Actual_Not_Funding_Date, tz = "America/Denver"),
    
    Actual_Submission_FY = get_fy(Actual_Submission_Date),
    Actual_Udr_Consid_FY = get_fy(Actual_Udr_Consid_Date),
    Actual_Funding_FY = get_fy(Actual_Funding_Date),
    Actual_Not_Funding_FY = get_fy(Actual_Not_Funding_Date)
  )
# Adding the Proposal Creation date and creation FY
proposal_data <- merge(proposal_data,cleaned_Created_date,by = "Proposal #")


proposal_data_N <- proposal_data %>%
  filter(!is.na(Status)) %>%            # Remove rows where Status is NA
  distinct()                            # Then keep only distinct rows



# Calculate durations (in days)
proposal_data <- proposal_data %>%
  mutate(
    Days_to_Submission = ifelse(!is.na(Actual_Submission_Date) & !is.na(`Created Date.x`),
                                round(as.numeric(difftime(Actual_Submission_Date, `Created Date.x`, units = "days"))),
                                NA),
    
    Days_to_Funding_From_Submission = ifelse(!is.na(Actual_Funding_Date) & !is.na(Actual_Submission_Date),
                                             round(as.numeric(difftime(Actual_Funding_Date, Actual_Submission_Date, units = "days"))),
                                             NA),
    
    Days_to_Non_Funding_From_Submission = ifelse(!is.na(Actual_Not_Funding_Date) & !is.na(Actual_Submission_Date),
                                                 round(as.numeric(difftime(Actual_Not_Funding_Date, Actual_Submission_Date, units = "days"))),
                                                 NA)
  )


##################################
### Cleaning the Proposal Data ###
##################################

proposal_data <- proposal_data%>%
  distinct()

# Remove rows where both "Project Title" and "PI" 

proposal_data <- proposal_data %>%
  filter(!(is.na(`Project Title`) & is.na(PI)))


proposal_data <- proposal_data %>%
  filter(!( is.na(PI)))

proposal_data <- proposal_data %>%
  mutate(
    Actual_Submission_Month_Year = format(Actual_Submission_Date, "%B-%Y")  # Extract Month-Year
  )

# =====================================================
# STEP 5: Load and Merge College Name Data
# =====================================================



college_name_data_Ashlee <- read_excel(Sys.getenv("COLLEGE_NAME_DATA_PATH_ASHLEE"))



college_name_data_Ashlee <- college_name_data_Ashlee %>%
  na.omit() %>%
  rename(`Admin Unit` = `Department`)





# Merge College Data with Award Data
proposal_data <- left_join(proposal_data, 
                           college_name_data_Ashlee, 
                                by = "Admin Unit") %>%
  distinct()







#### Merging the same Admin Unit name together:


proposal_data$`Admin Unit` <- ifelse(proposal_data$`Admin Unit` == "Academic Affairs Division","Academic Affairs",
                                     ifelse(proposal_data$`Admin Unit` == "UW Extension Department","UW Extension",
                                            ifelse(proposal_data$`Admin Unit` == "Criminal Justice & Sociology Department","Criminal Justice & Sociology",
                                                   ifelse(proposal_data$`Admin Unit` == "Agricultural Experiment Station Department","Agricultural Experiment Station",
                                                          ifelse(proposal_data$`Admin Unit` == "Botany Department","Botany",
                                                                 ifelse(proposal_data$`Admin Unit` == "Zoology & Physiology Department","Zoology & Physiology",
                                                                        ifelse(proposal_data$`Admin Unit` == "Neltje Center for Excellence in Creativity and the Arts Department","Neltje Center for Excellence in Creativity and the Arts",
                                                                               ifelse(proposal_data$`Admin Unit` == "Music Department","Music",
                                                                                      ifelse(proposal_data$`Admin Unit` == "Psychology Department","Psychology",
                                                                                             ifelse(proposal_data$`Admin Unit` == "Chemistry Department","Chemistry",
                                                                                                    ifelse(proposal_data$`Admin Unit` == "Physics & Astronomy Department","Physics & Astronomy",
                                                                                                           ifelse(proposal_data$`Admin Unit` == "Haub School of Environment & Natural Resources Department","Haub School of Environment & Natural Resources",
                                                                                                                  ifelse(proposal_data$`Admin Unit` == "School of Pharmacy","Pharmacy",
                                                                                                                         ifelse(proposal_data$`Admin Unit` == "Research & Economic Development Department","Research & Economic Development",
                                                                                                                                ifelse(proposal_data$`Admin Unit` %in% c("School of Energy Resources Division",
                                                                                                                                                                         "School of Energy Resources Department"),"School of Energy Resources",proposal_data$`Admin Unit`)))))))))))))))




#############################################
#### Data Preparation based on request ######
#############################################


# Keeps only one row per Proposal #, favoring:
# 
# A row that has a funding date (i.e., awarded),
# 
# Or, if all are unfunded, the row with the most recent submission.


proposal_data <- proposal_data %>%
  group_by(`Proposal #`) %>%
  arrange(desc(!is.na(Actual_Funding_Date)), desc(Actual_Submission_Date)) %>%
  slice(1) %>%
  ungroup()







#Remove missing value for College/Division
proposal_data <- proposal_data %>%
  filter(!is.na(`College/Division`))


#Converting the Submission and Award Date to quarters:
# Apply the function to submission and funding dates
proposal_data <- proposal_data %>%
  mutate(
    Actual_Submission_Quarter = sapply(Actual_Submission_Date, get_fiscal_quarter),
    Actual_Funding_Quarter = sapply(Actual_Funding_Date, get_fiscal_quarter)
  )

# Ensure the column is numeric by removing non-numeric characters (e.g., "$", ",")
proposal_data <- proposal_data %>%
  mutate(`Total Sponsor Costs` = readr::parse_number(`Total Sponsor Costs`))


# Funding source categorization

proposal_data <- proposal_data %>%
  mutate(Sponsor_Type_Grouped = case_when(
    `sponsor type` == "U.S. Federal Government" ~ "Federal",
    
    `sponsor type` %in% c(
      "Wyoming State Governmental Entities",
      "Wyoming Local Governmental Entities",
      "Other State and Local Governmental Entities"
    ) ~ "State",
    
    `sponsor type` == "Industry" ~ "Business",
    
    `sponsor type` == "Non-Profit Organizations" ~ "Non-profit",
    
    TRUE ~ "Others"  # includes Tribal, Foreign, Universities, etc.
  ))



# Among federal agencies

proposal_data <- proposal_data %>%
  mutate(
    Sponsor_Category = if_else(
      `sponsor type` == "U.S. Federal Government",
      case_when(
        grepl("National Science Foundation", Sponsor, ignore.case = TRUE) ~ "NSF",
        grepl("U.S. Department of Energy", Sponsor, ignore.case = TRUE) |
          grepl("Department of Energy", Sponsor, ignore.case = TRUE) ~ "DOE",
        grepl("National Institute of Food and Agriculture", Sponsor, ignore.case = TRUE) ~ "USDA-NIFA",
        grepl("National Institutes of Health", Sponsor, ignore.case = TRUE) ~ "NIH",
        grepl("Department of Health and Human Services", Sponsor, ignore.case = TRUE) &
          !grepl("National Institutes of Health", Sponsor, ignore.case = TRUE) ~ "HHS (other than NIH)",
        TRUE ~ "Others"
      ),
      "Others"  # If sponsor type is not Federal
    )
  )


library(dplyr)

# Ensure numeric format for cost column
proposal_data <- proposal_data %>%
  mutate(`Total Sponsor Costs` = as.numeric(`Total Sponsor Costs`)) %>%
  filter(!is.na(`College/Division`) & `College/Division` != "")


# Funding category redefine based one Actual_Submission_Date and Actual_Funding_Date


proposal_data <- proposal_data %>%
  mutate(
    Submission_Status = case_when(
      !is.na(Actual_Funding_Date) ~ "Funded",
      !is.na(Actual_Submission_Date) ~ "Submitted",
      TRUE ~ NA_character_
    )
  )


proposal_data$Submission_Count <- ifelse(proposal_data$Submission_Status == "Submitted",1,0)
proposal_data$Award_Count <- ifelse(proposal_data$Submission_Status == "Funded",1,0)
proposal_data$Total_Submission_Count <- proposal_data$Submission_Count+proposal_data$Award_Count
proposal_data$Received_Total_Sponsor_Costs <- ifelse(proposal_data$Submission_Status == "Funded",proposal_data$`Total Sponsor Costs`,0)




proposal_data_subsetted <- proposal_data %>%
  select(
    `Proposal #`,`full proposal title`, `Project Title`, `PI`, `PI Unit`, `Status`, `Sponsor`, `Prime Sponsor`, `Admin Unit`,
    `Project Start Date`, `Project End Date`, `Proposal Type`, `Instrument Type`, `Project #`, `Created Date.x`,
    `Total Sponsor Costs`, `Sponsor Deadline`, `academic affairs`, `activity type`, `History Action Day and Comment`,
    `Actual_Submission_Date`, `Actual_Udr_Consid_Date`, `Actual_Funding_Date`, `Actual_Not_Funding_Date`,
    `Actual_Submission_FY`, `Actual_Udr_Consid_FY`, `Actual_Funding_FY`, `Actual_Not_Funding_FY`, `Created Date.y`,
    `Proposal_Creation_FY`, `Days_to_Submission`, `Days_to_Funding_From_Submission`, `Days_to_Non_Funding_From_Submission`,
    `Actual_Submission_Month_Year`, `College/Division`, `Actual_Submission_Quarter`, `Actual_Funding_Quarter`,
    `Sponsor_Type_Grouped`, `Sponsor_Category`, `Submission_Status`, `Submission_Count`, `Award_Count`,
    `Total_Submission_Count`, `Received_Total_Sponsor_Costs`
  )

output_path_prop <- file.path(output_path, "Processed_Proposal_Data_Subsetted.csv")

write.csv(proposal_data_subsetted,output_path_prop,row.names=F,na="")

