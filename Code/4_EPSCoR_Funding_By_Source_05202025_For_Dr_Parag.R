# ================================================
# SCRIPT 2: PROJECT FINANCIAL DATA CLEANING & MERGING
# ================================================

code_base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Monthly Summary Analysis From Cayuse Data 04292025/Code"
source(file.path(code_base_path, "0_Data_Path_Configuration.R"))
source(file.path(code_base_path, "2_Award_Data_Preperation_from_Cayuse.R"))

##################################################################################################################
# USDA-NIFA has an EPSCoR-like program, embedded in its FASE (Food and Agricultural Science Enhancement) Grants.##
##################################################################################################################



award_data_NIFA_24 <- award_data %>%
  filter( Sponsor == "National Institute of Food and Agriculture/Department of Agriculture",
         str_ends(Award_Start_Month_Year, "24"))



USDA_NIFA <- aggregate(as.numeric(award_data_NIFA_24$Increment_Amount_Obliged),
          by = list(FAIN = award_data_NIFA_24$fain),
          FUN = sum)

# Total NIFA
sum(USDA_NIFA$x)





award_data_AFRI_24 <- award_data %>%
  filter(`cfda aln number` == "10.310" & Sponsor == "National Institute of Food and Agriculture/Department of Agriculture",
          str_ends(Award_Start_Month_Year, "24"))

USDA_AFRI <- aggregate(as.numeric(award_data_AFRI_24$Increment_Amount_Obliged),
                       by = list(FAIN = award_data_AFRI_24$fain),
                       FUN = sum)
# Total AFRI
sum(USDA_AFRI$x)

# 15% of AFRI is considered as FASE.

# % of FASE among all NIFA or USDA 
((sum(USDA_AFRI$x)*0.15)/sum(USDA_NIFA$x))*100

############
##### NSF###
############





##############
### NIH ######
##############




##############
### DOE ######
##############



##############
### NASA #####
##############


