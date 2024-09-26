library(tidyverse)
library(DT)
setwd("C:/Users/MT/Desktop/R Coding/STA9750-2024-FALL")

FARES <- readxl::read_xlsx("2022_fare_revenue.xlsx") |>
    select(-`State/Parent NTD ID`, 
           -`Reporter Type`,
           -`Reporting Module`,
           -`TOS`,
           -`Passenger Paid Fares`,
           -`Organization Paid Fares`) |>
    filter(`Expense Type` == "Funds Earned During Period") |>
    select(-`Expense Type`) |>
    group_by(`NTD ID`,       # Sum over different `TOS` for the same `Mode`
             `Agency Name`,  # These are direct operated and sub-contracted 
             `Mode`) |>      # of the same transit modality
                             # Not a big effect in most munis (significant DO
                             # tends to get rid of sub-contractors), but we'll sum
                             # to unify different passenger experiences
    summarize(`Total Fares` = sum(`Total Fares`)) |>
    ungroup()

EXPENSES <- readr::read_csv("2022_expenses.csv") |>
    select(`NTD ID`, 
           `Agency`,
           `Total`, 
           `Mode`) |>
    mutate(`NTD ID` = as.integer(`NTD ID`)) |>
    rename(Expenses = Total) |>
    group_by(`NTD ID`, `Mode`) |>
    summarize(Expenses = sum(Expenses)) |>
    ungroup()

FINANCIALS <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))

TRIPS <- readxl::read_xlsx("ridership.xlsx", sheet = "UPT") |>
            filter(`Mode/Type of Service Status` == "Active") |>
            select(-`Legacy NTD ID`, 
                   -`Reporter Type`, 
                   -`Mode/Type of Service Status`, 
                   -`UACE CD`, 
                   -`TOS`) |>
            pivot_longer(-c(`NTD ID`:`3 Mode`), 
                            names_to="month", 
                            values_to="UPT") |>
            drop_na() |>
            mutate(month=my(month)) # Parse _m_onth _y_ear date specs
MILES <- readxl::read_xlsx("ridership.xlsx", sheet="VRM") |>
            filter(`Mode/Type of Service Status` == "Active") |>
            select(-`Legacy NTD ID`, 
                   -`Reporter Type`, 
                   -`Mode/Type of Service Status`, 
                   -`UACE CD`, 
                   -`TOS`) |>
            pivot_longer(-c(`NTD ID`:`3 Mode`), 
                            names_to="month", 
                            values_to="VRM") |>
            drop_na() |>
            group_by(`NTD ID`, `Agency`, `UZA Name`, 
                     `Mode`, `3 Mode`, month) |>
            summarize(VRM = sum(VRM)) |>
            ungroup() |>
            mutate(month=my(month)) # Parse _m_onth _y_ear date specs

USAGE <- inner_join(TRIPS, MILES) |>
    mutate(`NTD ID` = as.integer(`NTD ID`))

sample_n(USAGE, 1000) |> 
  mutate(month=as.character(month)) |> 
  DT::datatable()

# Task 1 - Creating Syntatic Names

USAGE <- USAGE |> 
  rename(metro_area = `UZA Name`)

# Task 2: Recoding the Mode column
# Details obtained from: https://www.transit.dot.gov/ntd/national-transit-database-ntd-glossary#D
USAGE <- USAGE |>
  mutate(Mode = case_when(
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "CC" ~ "Cable Car",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferryboat",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "LR" ~ "Light Rail",
    Mode == "MB" ~ "Bus",
    Mode == "MG" ~ "Monorail Automated Guideway",
    Mode == "PB" ~ "Publico",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "TR" ~ "Aerial Tramway",
    Mode == "VP" ~ "Vanpool",
    Mode == "YR" ~ "Hybrid Rail",
    TRUE ~ "Unknown"
))

# Creating table with cleaned data

if(!require("DT")) install.packages("DT")
library(DT)

USAGE <- USAGE |> 
  select(-`3 Mode`)

sample_n(USAGE, 1000) |> 
  mutate(month = as.character(month)) |> 
  DT::datatable()

# Task 3: Answering Instructor Specified Questions with dplyr

# Q1.What transit agency had the most total VRM in our data set?

USAGE |>
  group_by(Agency) |>
  summarise(UPT_Total = sum(UPT)) |>
  arrange(desc(UPT_Total)) |>
  head(1) |>
  ungroup()

# Q2. What transit mode had the most total VRM in our data set?

USAGE |>
  group_by(Mode) |>
  summarise(VRM_Total = sum(VRM)) |>
  arrange(desc(VRM_Total)) |>
  head(1) |>
  ungroup()
# The data shows bus transportation had the most vehicle revenue miles "VRM"
# which is "The miles that vehicles are scheduled to or actually travel while in revenue service"
# https://www.transit.dot.gov/ntd/national-transit-database-ntd-glossary#V

# Q3. How many trips were taken on the NYC Subway (Heavy Rail) in May 2024?

colnames(USAGE)
nyc_filter_q3 <- USAGE |>
  filter(metro_area == "New York--Jersey City--Newark, NY--NJ", Mode == "Heavy Rail", month == "2024-05-01") |>
  summarise(May_2024_Trips = sum(UPT, na.rm = TRUE))
print(nyc_filter_q3)

# Q4. Can not be answered with the current data available

# Q5. How much did NYC subway ridership fall between April 2019 and April 2020?

nyc_filter_q5_part1 <- USAGE |>
  filter(metro_area == "New York--Jersey City--Newark, NY--NJ", Mode == "Heavy Rail", month == "2019-04-01") |>
  summarise(nyc_sub_2019 = sum(UPT, na.rm = TRUE))
nyc_filter_q5_part2 <- USAGE |>
  filter(metro_area == "New York--Jersey City--Newark, NY--NJ", Mode == "Heavy Rail", month == "2020-04-01") |>
  summarise(nyc_sub_2020 = sum(UPT, na.rm = TRUE))
nyc_filter_q5_part3=((nyc_filter_q5_part2-nyc_filter_q5_part1)/(nyc_filter_q5_part1))*100
print(nyc_filter_q5_part3)

# Task 4: Explore and Analyze: Find three more interesting transit facts in this data other than those above.

# Which Agency has the most VRM per UPT

colnames(USAGE)

vrm_per_upt <- USAGE |>
  group_by(Agency) |>
  summarise(UPT_Total = sum(UPT), VRM_Total = sum(VRM), VRM_per_UPT_ratio = (VRM_Total/UPT_Total)) |>
  ungroup() |>
  arrange(VRM_per_UPT_ratio) |>
  head(5)

print(vrm_per_upt)
# Most used mode per metro area

most_used_mode_per_metro_area <- USAGE |>
  group_by(metro_area, Mode) |>
  summarise(total_upt = sum(UPT, na.rm = TRUE), .groups = "drop") |>
  group_by(metro_area) |>
  slice(which.max(total_upt)) |>
  select(metro_area, Mode, total_upt)
print(most_used_mode_per_metro_area)
  
  
# Top metro areas by mode

top_metro_areas_by_mode <- USAGE |>
  group_by(Mode, metro_area) |>
  summarise(total_upt = sum(UPT, na.rm = TRUE)) |>
  group_by(Mode) |>
  slice(which.max(total_upt)) |>
  arrange(desc(total_upt))

print(top_metro_areas_by_mode)


# Task 5: Table Summarization

USAGE_2022_ANNUAL <- USAGE |>
  filter(year(month) == "2022") |>
  group_by(`NTD ID`, Agency, metro_area, Mode) |>
  summarise(UPT_Total = sum(UPT, na.rm = TRUE), VRM_Total = sum(VRM, na.rm = TRUE)) |>
  ungroup()

FINANCIALS <- FINANCIALS |>
  mutate(Mode = case_when(
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "CC" ~ "Cable Car",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferryboat",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "LR" ~ "Light Rail",
    Mode == "MB" ~ "Bus",
    Mode == "MG" ~ "Monorail Automated Guideway",
    Mode == "PB" ~ "Publico",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "TR" ~ "Aerial Tramway",
    Mode == "VP" ~ "Vanpool",
    Mode == "YR" ~ "Hybrid Rail",
    TRUE ~ "Unknown"
  ))


USAGE_AND_FINANCIALS <- left_join(USAGE_2022_ANNUAL, FINANCIALS, 
  join_by(`NTD ID`, "Mode")) |>
  drop_na()

colnames(USAGE_2022_ANNUAL)

sample_n(USAGE_2022_ANNUAL, 1141) |> 
    DT::datatable()

# Task 6: Farebox Recovery Among Major Systems
# Q1. Which transit system (agency and mode) had the most UPT in 2022?

USAGE_2022_ANNUAL |>
  group_by(Agency, Mode) |>
  arrange(desc(UPT_Total)) |>
  head(5) |>
  ungroup()


# Q2.Which transit system (agency and mode) had the highest farebox recovery, defined as 
# the highest ratio of Total Fares to Expenses?


highest_farebox_recovery <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarise(ratio = (sum(`Total Fares`, na.rm = TRUE)/sum(Expenses, na.rm = TRUE))) |>
  ungroup() |>
  arrange(desc(ratio)) |>
  head(5)

print(highest_farebox_recovery)

# Q3. Which transit system (agency and mode) has the lowest expenses per UPT?

lowest_expenses_per_UPT <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarise(ratio = (sum(Expenses, na.rm = TRUE)/sum(UPT_Total, na.rm = TRUE)),.groups = "drop") |>
  arrange(ratio) |>
  head(5)
print(lowest_expenses_per_UPT)

# Q4. Which transit system (agency and mode) has the highest total fares per UPT?

highest_total_fares_per_UPT <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarise(ratio = (sum(`Total Fares`, na.rm = TRUE)/sum(UPT_Total, na.rm = TRUE)),.groups = "drop") |>
  arrange(desc(ratio)) |>
  head(5)
print(highest_total_fares_per_UPT) 
 
# Q5. Which transit system (agency and mode) has the lowest expenses per VRM?

lowest_expenses_per_VRM <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarise(ratio = (sum(Expenses, na.rm = TRUE)/sum(VRM_Total, na.rm = TRUE)),.groups = "drop") |>
  arrange(ratio) |>
  head(5)
print(lowest_expenses_per_VRM)

# Q6. Which transit system (agency and mode) has the highest total fares per VRM?

highest_fares_per_VRM <- USAGE_AND_FINANCIALS |>
  group_by(Agency, Mode) |>
  summarise(ratio = (sum(`Total Fares`, na.rm = TRUE)/sum(VRM_Total, na.rm = TRUE)),.groups = "drop") |>
  arrange(desc(ratio)) |>
  head(5)
print(highest_fares_per_VRM)

# You may wish to restrict your answer to major transit systems, which you can define as those with 400,000 UPT per annum.

