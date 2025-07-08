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

table(award_data$`Modification Type`)
### Fix admin Unit and PI unit changes:

# STEP 1: Extract base award ID and version number
award_data <- award_data %>%
  mutate(
    Award_Base = str_remove(`Award #`, "-\\d+$"),
    Award_Version = as.numeric(str_extract(`Award #`, "\\d+$"))
  )

# STEP 2: Fill latest PI and Admin Unit down to all related records per project
award_data <- award_data %>%
  group_by(Award_Base) %>%
  arrange(Award_Version, .by_group = TRUE) %>%
  mutate(
    Final_PI = coalesce(last(na.omit(PI)), first(PI)),
    Final_Admin_Unit = coalesce(last(na.omit(`Admin Unit`)), first(`Admin Unit`))
  ) %>%
  ungroup()

# STEP 3: If PI/Admin Unit differ from final value, update
award_data <- award_data %>%
  mutate(
    PI = ifelse(is.na(PI) | PI != Final_PI, Final_PI, PI),
    `Admin Unit` = ifelse(is.na(`Admin Unit`) | `Admin Unit` != Final_Admin_Unit, Final_Admin_Unit, `Admin Unit`)
  ) %>%
  select(-Award_Base, -Award_Version, -Final_PI, -Final_Admin_Unit)

class(award_data$`Award Start Date`)
class(award_data$`Modification Date`)
award_data <- award_data %>%
  mutate(
    `Award Start Date` = as.Date(`Award Start Date`, format = "%m/%d/%Y"),
    `Modification Date` = as.Date(`Modification Date`, format = "%m/%d/%Y")
  )


award_data <- award_data %>%
  mutate(
    `Award Start Date` = as.Date(ifelse(
      !is.na(`Modification Date`) &
        `Modification Type` == "New Funding Increment" &
        `Modification Date` > `Award Start Date`,
      `Modification Date`,
      `Award Start Date`
    ), origin = "1970-01-01")  # needed because dates are internally stored as numeric
  )



valid_mod_types <- c(
  "Original Award", "New Funding Increment","De-Obligation", 
  "Pre-Award Spending", "Pre-Award Spending Removal", 
  "Key Personnel Change"
)
# Remove: "Pre-Award Spending Removal"

Increment_amount_data <- award_data %>%
  filter(`Modification Type` %in% valid_mod_types) %>%
  select(`Award #`, `Obligated Amount`, `Modification Type`) %>%
  filter(!is.na(`Obligated Amount`)) %>%
  mutate(
    `Obligated Amount` = gsub("[^0-9.]", "", `Obligated Amount`),
    `Obligated Amount` = as.numeric(`Obligated Amount`),
    Base_Award = str_remove(`Award #`, "-\\d+$")
  ) %>%
  arrange(Base_Award, `Award #`) %>%
  group_by(Base_Award) %>%
  # Define types that use lag-based increment calculation
  mutate(
    is_increment_type = `Modification Type` %in% c("Original Award", "New Funding Increment", "De-Obligation"),
    incr_sequence = ifelse(is_increment_type, `Obligated Amount`, NA_real_)
  ) %>%
  mutate(
    last_increment = zoo::na.locf(incr_sequence, na.rm = FALSE),
    last_increment = ifelse(is.na(last_increment), 0, last_increment),
    Increment_Amount_Obliged = case_when(
      is_increment_type ~ `Obligated Amount` - lag(last_increment, default = 0),
      TRUE ~ `Obligated Amount`
    )
  ) %>%
  ungroup() %>%
  mutate(
    Increment_Amount_Obliged = case_when(
      Increment_Amount_Obliged < 0 & !(`Modification Type` %in% c("De-Obligation", "Sponsor Decrease")) ~ `Obligated Amount`,
      TRUE ~ Increment_Amount_Obliged
    ),
    Increment_Amount_Obliged = format(round(Increment_Amount_Obliged, 2), nsmall = 2)
  ) %>%
  select(`Award #`, Increment_Amount_Obliged)

# Merge back to award data
award_data <- merge(award_data, Increment_amount_data, by = "Award #")

# Convert History Action Date to datetime first
award_data <- award_data %>%
  mutate(`History Action Date Parsed` = mdy_hms(`History Action Date`, quiet = TRUE))



# STEP 2: Select last 16 columns to move them forward (as in your original script)
award_data <- award_data %>%
  select(-(1:16), everything()[1:16])

# STEP 3: Remove rows where both Project Title and PI are missing
award_data <- award_data %>%
  filter(!(is.na(`Project Title`) & is.na(PI)))

# STEP 4: Separate versioned and non-versioned award rows
award_data <- award_data %>%
  mutate(
    is_versioned = grepl("-\\d+$", `Award #`)
  )


award_data_versioned <- award_data %>%
  filter(is_versioned) %>%
  mutate(
    Award_Base = sub("-\\d+$", "", `Award #`),
    Award_Version = as.numeric(sub(".*-(\\d+)$", "\\1", `Award #`))
  ) %>%
  select(-Award_Base, -Award_Version, -is_versioned)




# ---- KEEP NON-VERSIONED AWARDS AS-IS ---- #
award_data_nonversioned <- award_data %>%
  filter(!is_versioned) %>%
  select(-is_versioned)

# ---- COMBINE CLEANED DATA ---- #
award_data_cleaned <- bind_rows(award_data_versioned, award_data_nonversioned)


class(award_data_cleaned$`Award Start Date`)


# STEP 5: Convert Award Start Date and add Month-Year column
award_data_cleaned <- award_data_cleaned %>%
  mutate(
    Award_Start_Month_Year = paste0("'", format(`Award Start Date`, "%b-%y"))  # e.g., 'Jun-25
  )




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

# Final filtering
award_data_cleaned <- award_data_cleaned %>%
  filter(`Modification Type` %in% c("Original Award", "New Funding Increment"))




award_data <- award_data_cleaned


subsetted_award_data <- award_data %>%
  select(
    `Award #`,`Project Title`,`Award Title`,`PI`,`PI Unit`, `Award Amount`, `Award Id`, `Award Notice Received`, `Increment Amount`,
    `Modification Number`, `Modification Type`, `Modification Date`, `Modified By`, `sponsor type`, `activity type`,
    `Increment_Amount_Obliged`, `History Action Date Parsed` , `Sponsor`, `Prime Sponsor`,
    `Instrument Type`, `Award Start Date`, `Award End Date`, `Admin Unit`, `Obligated Amount`, `Project #`,
    `Total Expected Amount`, `Anticipated Amount`, `Status`, `Created Date`, `Award Type`, `Award_Start_Month_Year`,
    `College/Division`
  )




library(dplyr)
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

output_path_prop <- file.path(output_path, "Processed_Award_Data_Subsetted.csv")
write.csv(subsetted_award_data,output_path_prop,row.names=F,na="")
