####################################################################################
# If there is any issue in Obliged amount number we handle that using this script ##
####################################################################################


code_base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Monthly Summary Analysis From Cayuse Data 04292025/Code"
source(file.path(code_base_path, "0_Data_Path_Configuration.R"))
source(file.path(code_base_path, "2_Award_Data_Preperation_from_Cayuse.R"))


##### Keeping the Increment Obleiged Amount when it's > 0, 
#### If the value is = 0, Use script "Manual_Award_data_cleaning.R" to fix the values and merge with the clean data.

award_data_check <- award_data %>%
  filter(Award_Start_Month_Year %in% c("'Jun-25"))

problematic_expected_amount <- award_data_check %>%
  filter(is.na(as.numeric(`Total Expected Amount`)) | as.numeric(`Total Expected Amount`) == 0)

problematic_Increment_Amount <- award_data_check %>%
  filter(as.numeric(Increment_Amount_Obliged)==0 | as.numeric(Increment_Amount_Obliged) < 100)


intersect(problematic_expected_amount$`Award #`,problematic_Increment_Amount$`Award #`)

setdiff(problematic_expected_amount$`Award #`,problematic_Increment_Amount$`Award #`)
setdiff(problematic_Increment_Amount$`Award #`,problematic_expected_amount$`Award #`)


# Problematic Award # for manual check in Cayuse:
print(problematic_expected_amount$`Award #`)


# Problematic Award # for manual check in Cayuse:
print(problematic_Increment_Amount$`Award #`)

# Showing the important columns in problematic data:
View(problematic_expected_amount[,c("Award #","College/Division","Admin Unit","Award_Start_Month_Year","Total Expected Amount","Obligated Amount","Increment_Amount_Obliged")])

View(problematic_Increment_Amount[,c("Award #","College/Division","Admin Unit","Award_Start_Month_Year","Total Expected Amount","Obligated Amount","Increment_Amount_Obliged")])

#### Next step #####

# After find the comment for the problematic Award # we need 
# to Go back to 2_Award_Data_Preperation_from_Cayuse.R script to add those comment in very last part and then save the data for reporting in pivot or PowerBI.

