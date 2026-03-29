## Research Question:
# Which types of healthcare data breaches result in the largest impact on individuals?

## Hypothesis:
# Digital breaches (e.g., servers, computers) will affect more individuals than paper-based breaches.


# Load libraries 
library(tidyverse) 


# load data

security <- read.csv("https://raw.githubusercontent.com/jossalene/CodingCult/refs/heads/master/security.csv")


# This displays all variable names in the data set
security %>% 
  colnames()


security_clean <- security %>% 
  select(Individuals_Affected , Type_of_Breach , Location_of_Breached_Information)


# Data Cleaning

security_clean %>% 
  head()


# Displays a frequency table of the variable Type_of_Breach. Each row represents a different breach type and how many times it happened. 

table(security_clean$Type_of_Breach)


# Overall distribution of individuals affected across all breaches
security_clean %>% 
  summarise(M_affected = mean(Individuals_Affected) ,
            min_affected = min (Individuals_Affected) ,
            P50_affected = median(Individuals_Affected),
            max_affected = max(Individuals_Affected) )


# Which breach types result in the largest impact?
security_clean %>%
  group_by(Type_of_Breach) %>%
  summarise(
    avg_affected = mean(Individuals_Affected),
    median_affected = median(Individuals_Affected),
    max_affected = max(Individuals_Affected),
    count = n()
  ) %>%
  arrange(desc(avg_affected))


## Interpretation: 
# Breaches categorized as "Loss" and "Unauthorized Access" show the highest average impact.
# However, "Theft" remains the most frequent type of breach, indicating that while it occurs often,
# it does not always result in the largest individual impact.


# Shows all the locations were the data was breached and combines duplicates

unique(security_clean$Location_of_Breached_Information)


# Simplifying breach categories for analysis
security_clean <- security_clean %>%
  mutate(Breach_Simple = case_when(
    str_detect(Type_of_Breach, "Theft") ~ "Theft",
    str_detect(Type_of_Breach, "Hacking") ~ "Hacking",
    str_detect(Type_of_Breach, "Loss") ~ "Loss",
    str_detect(Type_of_Breach, "Unauthorized") ~ "Unauthorized Access",
    TRUE ~ "Other" ))


# Comparing breach types by impact on individuals
security_clean %>%
  group_by(Breach_Simple) %>%
  summarise(
    avg_affected = mean(Individuals_Affected),
    median_affected = median(Individuals_Affected),
    max_affected = max(Individuals_Affected),
    count = n()
  ) %>% 
arrange(desc(avg_affected))


## Conclusion:
# The hypothesis is partially supported. Digital-related breaches tend to affect more individuals on average,
# but high-impact incidents also occur in other categories such as loss and unauthorized access.
# Overall, breach impact is highly variable and driven by a small number of large incidents.

