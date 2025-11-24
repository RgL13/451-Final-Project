library(dplyr)

raw <- read.csv("/Users/zia/Desktop/UW/Quarter/26AUTU/stat451/project/part3/IHME-RAW.csv")


clean <- raw %>%
  filter(
    measure_name == "Prevalence",
    metric_name  == "Percent",
    age_name     == "All ages",
    year == 2023
  ) %>%
  select(location_name, cause_name, sex_name, val, year)

write.csv(clean, "IHME-DATA.csv", row.names = FALSE)

