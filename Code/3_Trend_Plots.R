# ================================================
# SCRIPT 2: PROJECT FINANCIAL DATA CLEANING & MERGING
# ================================================

code_base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Monthly Summary Analysis From Cayuse Data 04292025/Code"
source(file.path(code_base_path, "0_Data_Path_Configuration.R"))
source(file.path(code_base_path, "1_Updated_Proposal_data_load_and_clean_from_Cayuse.R"))
source(file.path(code_base_path, "2_Award_Data_Preperation_from_Cayuse.R"))


#### Monthly Proposal Submission counts:

library(dplyr)
library(ggplot2)
library(lubridate)

# Ensure date column is Date
proposal_data <- proposal_data %>%
  mutate(Actual_Submission_Date = as.Date(Actual_Submission_Date))

# Filter for July 2024 – April 2025 and count by month
monthly_counts <- proposal_data %>%
  filter(Actual_Submission_Date >= as.Date("2024-07-01"),
         Actual_Submission_Date <= as.Date("2025-06-30")) %>%
  mutate(Month_Year = floor_date(Actual_Submission_Date, "month")) %>%
  group_by(Month_Year) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Month_Year)

# Plot
ggplot(monthly_counts, aes(x = Month_Year, y = Count)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  labs(
    title = "Proposal Submissions by Month-Year",
    x = "Month-Year",
    y = "Number of Submissions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





##### Award Count by month:

# Filter for Jan–Apr 2020
award_monthly_counts <- award_data %>%
  filter(`Award Start Date` >= as.Date("2024-07-01"),
         `Award Start Date` <= as.Date("2025-06-30")) %>%
  mutate(Month_Year = floor_date(`Award Start Date`, "month")) %>%
  group_by(Month_Year) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Month_Year)

# Plot
ggplot(award_monthly_counts, aes(x = Month_Year, y = Count)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  geom_text(aes(label = Count), vjust = -0.5, size = 3) +
  scale_x_date(date_labels = "%b-%Y", date_breaks = "1 month") +
  labs(
    title = "Award Counts by Month-Year",
    x = "Month-Year",
    y = "Number of Awards"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





## Quarterly Proposal Submission:


library(dplyr)
library(ggplot2)
library(lubridate)

# Define custom fiscal quarter based on submission month
proposal_data <- proposal_data %>%
  filter(Actual_Submission_Date >= as.Date("2024-07-01"),
         Actual_Submission_Date <= as.Date("2025-06-30")) %>%
  mutate(
    Month = month(Actual_Submission_Date),
    Year = year(Actual_Submission_Date),
    
    # Fiscal Year for logic
    FY = ifelse(Month >= 7, Year + 1, Year),
    
    # Quarter Label should be FY - 1
    Label_Year = FY - 1,
    
    # Define Custom Quarter Label
    Custom_Quarter = case_when(
      Month %in% 7:9   ~ paste0("Q1-", Label_Year),
      Month %in% 10:12 ~ paste0("Q2-", Label_Year),
      Month %in% 1:3   ~ paste0("Q3-", Label_Year),
      Month %in% 4:6   ~ paste0("Q4-", Label_Year),
      TRUE ~ NA_character_
    )
  )

# Aggregate by Custom Quarter and sort
quarterly_counts <- proposal_data %>%
  filter(!is.na(Custom_Quarter)) %>%
  group_by(Custom_Quarter) %>%
  summarise(Total_Submissions = n(), .groups = "drop") %>%
  mutate(
    Year = as.numeric(sub(".*-(\\d+)", "\\1", Custom_Quarter)),
    Q_Num = as.numeric(sub("Q(\\d+)-.*", "\\1", Custom_Quarter)),
    Sort_Key = Year * 10 + Q_Num
  ) %>%
  arrange(Sort_Key)

# Ensure proper order
quarterly_counts$Custom_Quarter <- factor(quarterly_counts$Custom_Quarter, levels = quarterly_counts$Custom_Quarter)

# Plot
ggplot(quarterly_counts, aes(x = Custom_Quarter, y = Total_Submissions, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  geom_text(aes(label = Total_Submissions), vjust = -0.5, size = 4) +
  labs(
    title = "Proposal Submissions by Custom Quarter (FY-Based)",
    x = "Quarter",
    y = "Total Submissions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))



#### Award counts quarterly


library(dplyr)
library(ggplot2)
library(lubridate)

# Step 1: Filter and assign custom fiscal quarters
award_quarterly_counts <- award_data %>%
  filter(`Award Start Date` >= as.Date("2024-07-01"),
         `Award Start Date` <= as.Date("2025-04-30")) %>%
  mutate(
    Month = month(`Award Start Date`),
    Year = year(`Award Start Date`),
    
    # Assign fiscal year
    FY = ifelse(Month >= 7, Year + 1, Year),
    
    # Use FY - 1 in label to match start year labeling style
    Label_Year = FY - 1,
    
    # Define custom quarter
    Custom_Quarter = case_when(
      Month %in% 7:9   ~ paste0("Q1-", Label_Year),
      Month %in% 10:12 ~ paste0("Q2-", Label_Year),
      Month %in% 1:3   ~ paste0("Q3-", Label_Year),
      Month %in% 4:6   ~ paste0("Q4-", Label_Year),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Custom_Quarter))  # Remove unmatched quarters (like Apr–Jun)

# Step 2: Aggregate by quarter and sort
award_quarterly_counts <- award_quarterly_counts %>%
  group_by(Custom_Quarter) %>%
  summarise(Total_Awards = n(), .groups = "drop") %>%
  mutate(
    Year = as.numeric(sub(".*-(\\d+)", "\\1", Custom_Quarter)),
    Q_Num = as.numeric(sub("Q(\\d+)-.*", "\\1", Custom_Quarter)),
    Sort_Key = Year * 10 + Q_Num
  ) %>%
  arrange(Sort_Key)

# Fix x-axis order
award_quarterly_counts$Custom_Quarter <- factor(award_quarterly_counts$Custom_Quarter, levels = award_quarterly_counts$Custom_Quarter)

# Step 3: Plot
ggplot(award_quarterly_counts, aes(x = Custom_Quarter, y = Total_Awards, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  geom_text(aes(label = Total_Awards), vjust = -0.5, size = 4) +
  labs(
    title = "Award Counts by Custom Quarter (FY-Based)",
    x = "Quarter",
    y = "Number of Awards"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))





