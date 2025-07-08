# ================================================
# SCRIPT 2: PROJECT FINANCIAL DATA CLEANING & MERGING
# ================================================

code_base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Monthly Summary Analysis From Cayuse Data 04292025/Code"
source(file.path(code_base_path, "0_Data_Path_Configuration.R"))
source(file.path(code_base_path, "1_Updated_Proposal_data_load_and_clean_from_Cayuse.R"))
source(file.path(code_base_path, "2_Award_Data_Preperation_from_Cayuse.R"))

# 1. The number of submitted federal grant proposals and how many are funded.

############################# Data request: Jami 04182025 ##

output_path_specific <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Total Fedral Grant Funded Through TIG 04152025/Output/"

award_non_award_subset <- proposal_data %>%
  filter(!is.na(PI) & PI != "") %>%
  select(
    `Proposal #`,
    `Admin Unit`,
    `Instrument Type`,
    `PI`,
    `sponsor type`,
    `Prime Sponsor`,
    `Project End Date`,
    `Project Start Date`,
    `Project Title`,
    `Proposal Type`,
    `Sponsor`,
    `Status`,
    `Actual_Submission_Date`,
    `Actual_Udr_Consid_Date`,
    `Actual_Funding_Date`,
    `Actual_Not_Funding_Date`,
    `Actual_Submission_FY`,
    `Actual_Udr_Consid_FY`,
    `Actual_Funding_FY`,
    `Actual_Not_Funding_FY`,
    `Created Date.y`,
    `Proposal_Creation_FY`,
    `Days_to_Submission`,
    `Days_to_Funding_From_Submission`,
    `Days_to_Non_Funding_From_Submission`,
    `College/Division`
  )

output_path_raw_processed <- file.path(output_path_specific, "Processed_Raw_Data_06062025.csv")
write.csv(award_non_award_subset,output_path_raw_processed,row.names=F,na="")


################# Testing ###########
library(readxl)
TIG_data <- read_excel("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Total Fedral Grant Funded Through TIG 04152025/Input/Nick Data/UWyo_UtilizationReport_1.1.23 - 5.1.25_TIG_5.1.25.xlsx", 
                       sheet = "ProposalDevelopment")




# Add proposal status column
TIG_data$`Proposal #` <- trimws(TIG_data$`Proposal #`)
proposal_data_subset <- proposal_data %>%
  select("Proposal #","Status")

colnames(proposal_data_subset)[2] <- "Proposal_Status" 


TIG_data <- left_join(TIG_data, proposal_data_subset,by="Proposal #")




TIG_data <- TIG_data %>%
  mutate(`Award #` = gsub("P", "A", `Proposal #`))

TIG_data$`Award #` <- trimws(TIG_data$`Award #`)
TIG_data_merged <- left_join(TIG_data,award_data,by="Award #")

colnames(TIG_data_merged)[4] <- "PI"

TIG_data_merged_subsetted <- TIG_data_merged[,c(colnames(TIG_data),"Status","sponsor type","College/Division","Admin Unit","PI.y","Total Expected Amount","Increment_Amount_Obliged","Award_Start_Month_Year")]


write.csv(TIG_data_merged_subsetted,"C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Total Fedral Grant Funded Through TIG 04152025/Output/TIG_merged.csv")
