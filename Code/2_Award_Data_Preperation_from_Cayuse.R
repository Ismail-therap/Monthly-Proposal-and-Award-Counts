# ================================================
# SCRIPT 1: PROPOSAL DATA CLEANING & DATE CREATION
# ================================================

source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Monthly Summary Analysis From Cayuse Data 04292025/Code/0_Data_Path_Configuration.R")

library(dplyr)
library(lubridate)
library(readr)

options(scipen = 999)
options(digits = 22)

# STEP 1: Load the data
award_data <- read_csv(Sys.getenv("AWARD_DATA_PATH"))


# STEP 2: Remove rows where both Project Title and PI are missing (To remove unnecessary rows form the data)
award_data <- award_data %>%
  filter(!(is.na(`Project Title`) & is.na(PI)))



# STEP 3: Goal is to keep all the latest information about the award. So, we will keep the information from latest modification.

# Extract base award ID and version number
award_data <- award_data %>%
  mutate(
    # Check if Award # has a version (has a second hyphen after A0001)
    has_version = str_detect(`Award #`, "-\\d+$"),
    
    # Award_Base: remove version if present
    Award_Base = if_else(
      has_version,
      str_remove(`Award #`, "-\\d+$"),
      `Award #`
    ),
    
    # Award_Version: extract numeric version or assign 0 if missing
    Modification_Version = if_else(
      has_version,
      as.numeric(str_extract(`Award #`, "\\d+$")),
      0
    )
  ) %>%
  select(-has_version)  # optional: remove the helper column


# ======= This part of code will not work for new form. I can use the Creation date filter or condition to seperate the workflow
# If Form Creation Date < Aug 1. (Dummy Relese Date)
award_data_cleaned <- award_data %>%
  group_by(Award_Base) %>%
  filter(Modification_Version == max(Modification_Version, na.rm = TRUE)) %>%
  rename(`Latest Modification Type` = `Modification Type`)  %>%
  ungroup()

# === If Form Creation Date is >= Aug 1. (Dummy Relese Date)


# In next release if Increment Obligated Amount start reporting then I have to summed the value when Modification type is Original Award or New Funding Incremnet to
# get the total Obligated Amount. Currently it's reporting the cumulative summed value. 

# Then rbind old and new forms to go nex part!!!!


# STEP 4: Create a variable which show if it's a old project meaning legacy load or not.

# If there is a number in `infoed project number` then it's a legacy load (old project or not)
award_data_cleaned$legacy_load <- ifelse(!is.na(award_data_cleaned$`infoed project number`), "Yes", "No")




# STEP 5: Convert Award Start Date and add Month-Year column
award_data_cleaned <- award_data_cleaned %>%
  mutate(
    `Award Start Date` = as.Date(`Award Start Date`, format = "%m/%d/%Y"),
    Award_Start_Month_Year = paste0("'", format(`Award Start Date`, "%b-%y"))  # e.g., 'Jun-25
  )



# STEP 6: Adding the College/Division Information based on Admin Unit:

college_name_data_Ashlee <- read_excel(Sys.getenv("COLLEGE_NAME_DATA_PATH_ASHLEE"))



college_name_data_Ashlee <- college_name_data_Ashlee %>%
  na.omit() %>%
  rename(`Admin Unit` = `Department`)



# Merge College Data with Award Data
award_data_cleaned <- left_join(award_data_cleaned, 
                                college_name_data_Ashlee, 
                                by = "Admin Unit") %>%
  distinct()


award_data_cleaned$`Admin Unit` <- ifelse(award_data_cleaned$`Admin Unit` == "Academic Affairs Division", "Academic Affairs",
                                          ifelse(award_data_cleaned$`Admin Unit` == "UW Extension Department", "UW Extension",
                                                 ifelse(award_data_cleaned$`Admin Unit` == "Criminal Justice & Sociology Department", "Criminal Justice & Sociology",
                                                        ifelse(award_data_cleaned$`Admin Unit` == "Agricultural Experiment Station Department", "Agricultural Experiment Station",
                                                               ifelse(award_data_cleaned$`Admin Unit` == "Botany Department", "Botany",
                                                                      ifelse(award_data_cleaned$`Admin Unit` == "Zoology & Physiology Department", "Zoology & Physiology",
                                                                             ifelse(award_data_cleaned$`Admin Unit` == "Neltje Center for Excellence in Creativity and the Arts Department", "Neltje Center for Excellence in Creativity and the Arts",
                                                                                    ifelse(award_data_cleaned$`Admin Unit` == "Music Department", "Music",
                                                                                           ifelse(award_data_cleaned$`Admin Unit` == "Psychology Department", "Psychology",
                                                                                                  ifelse(award_data_cleaned$`Admin Unit` == "Chemistry Department", "Chemistry",
                                                                                                         ifelse(award_data_cleaned$`Admin Unit` == "Physics & Astronomy Department", "Physics & Astronomy",
                                                                                                                ifelse(award_data_cleaned$`Admin Unit` == "Haub School of Environment & Natural Resources Department", "Haub School of Environment & Natural Resources",
                                                                                                                       ifelse(award_data_cleaned$`Admin Unit` == "School of Pharmacy", "Pharmacy",
                                                                                                                              ifelse(award_data_cleaned$`Admin Unit` == "Research & Economic Development Department", "Research & Economic Development",
                                                                                                                                     ifelse(award_data_cleaned$`Admin Unit` %in% c("School of Energy Resources Division", "School of Energy Resources Department"), "School of Energy Resources",
                                                                                                                                            award_data_cleaned$`Admin Unit`)))))))))))))))




# Move "Award #" to be the first column
award_data_cleaned <- award_data_cleaned %>%
  select(`Award #`, everything())




award_data <- award_data_cleaned


subsetted_award_data <- award_data %>%
  select(
    `Award #`,`Award_Base`,`Project #`,`Project Title`,`PI`,`PI Unit`, 
    `Admin Unit`,`College/Division`,`Created Date`,`Award Start Date`,`Award_Start_Month_Year`, 
    `Award End Date`,`Award Notice Received`,`Sponsor`, `Prime Sponsor`, `sponsor type`,
     `Latest Modification Type`, `Modification Date`, `Modified By`, `activity type`, 
    `Status`,`Total Expected Amount`, `Obligated Amount`
)





subsetted_award_data <- subsetted_award_data %>%
  mutate(
    Comment = case_when(
      `Award #` == "23-0184-A0001-4" ~ "Release of $120,000 funding for this budget period",
      `Award #` == "23-0734-A0001-3" ~ "New funding increment in the amount of $325,125. TD Ticket:22565437",
      `Award #` == "24-0680-A0001-1" ~ "This continuation award releases $207,242 in previously approved funds for use during the current budget period.",
      `Award #` == "24-0712-A0002-1" ~ "This is a draft modification!",
      `Award #` == "24-0784-A0001-2" ~ "NIH added $65.08 to our awarded from the unobligated UF funds.",
      `Award #` == "23-1410-A0001-6" ~ "PO 2440825 has been lifted to the full LO amount of $260K.",
      `Award #` == "24-0956-A0001" ~ "Unfunded Collaboration agreement. Adding to ROAMWyo for agreement tracking purposes.",
      `Award #` == "25-0119-A0001-0" ~ "Unfunded Collaboration agreement. Adding to ROAMWyo for agreement tracking purposes.",
      `Award #` == "25-0119-A0002" ~ "Wyoming Integrated Test Center will provide $2,000,000... $55,000 in Phase 1.",
      `Award #` == "24-1027-A0001" ~ "Unfunded Collaboration agreement. Adding to ROAMWyo for agreement tracking purposes.",
      `Award #` == "24-1096-A0001" ~ "Unfunded Collaboration agreement. Adding to ROAMWyo for agreement tracking purposes.",
      `Award #` == "23-1409-A0001-4" ~ "Increase of $19,949. Amendment #4",
      `Award #` == "25-0377-A0001" ~ "Courtney Ray will be submitting checks to OSP. $240,000 expected. Award reflects $1 until first check.",
      `Award #` == "24-1093-A0001" ~ "Unfunded Collaboration agreement. Adding to ROAMWyo for agreement tracking purposes.",
      `Award #` == "25-0365-A0002" ~ "Unfunded Collaboration agreement. Adding to ROAMWyo for agreement tracking purposes.",
      `Award #` == "24-0896-A0001-0" ~ "After modification total expected and obligated amount is $34,999.58",
      `Award #` == "25-0292-A0001" ~ "Unfunded Collaboration agreement. Adding to ROAMWyo for agreement tracking purposes.",
      `Award #` == "25-0073-A0001" ~ "Unfunded Collaboration agreement. Adding to ROAMWyo for agreement tracking purposes.",
      `Award #` == "24-0281-A0001-4" ~ "Award Rec'd. $131,914",  # Only one comment per award; last one kept
      `Award #` == "24-0193-A0001-7" ~ "New funding increment of $47,282.00 (Corrected mistake from prior Modification)",
      `Award #` == "24-0060-A0001-3" ~ "This is a draft modification!",
      TRUE ~ NA_character_
    )
  )


output_path_prop <- file.path(output_path, "Processed_Award_Data_Subsetted_07082025.csv")
write.csv(subsetted_award_data,output_path_prop,row.names=F,na="")




