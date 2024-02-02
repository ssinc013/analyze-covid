
# Create a file path vector to every data set in the data_CDC_raw folder
dataPaths_char <- list.files(
  path = "data_CDC_raw",
  pattern = "Community",
  # keeps the full path name
  full.names = TRUE
)


# create a vector of the desired counties for analysis
counties = c("Miami-Dade County, FL", "Broward County, FL", "Palm Beach County, FL")

# create a function to read in excel files for the CDC data
ImportCDC <- function(filename) {
  # Convert xlsx sheet to list
  read_xlsx(filename, sheet = "Counties", skip = 1) %>% 
    
    # Filter to our counties of interest
    filter(
      County %in% counties
    ) %>% 
    
    # Select variables of interest
    select(
      any_of(
        c(
          "County",
          "Cases - last 7 days",
          "Viral (RT-PCR) lab test positivity rate - last 7 days (may be an underestimate due to delayed reporting)", 
          "NAAT positivity rate - last 7 days (may be an underestimate due to delayed reporting)",
          "Confirmed COVID-19 admissions - last 7 days",
          "People who are fully vaccinated",
          "People who are fully vaccinated - ages 65+"
        )
      )
    ) %>% 
    
    # Add variable for date of report
    mutate(
      report_date = ymd(str_extract(filename, "\\d{8}"))
    )
}

# TESTING
ImportCDC(dataPaths_char[[100]])

# CREATING COVID-19 DATASET
CDC_data_df <-
  map(
    .x = dataPaths_char,
    .f = ImportCDC,
    .progress = TRUE
  ) %>% 
  
  # Combine list of tibbles into single tibble,
  # padding NA for missing columns
  bind_rows()



# create a vector of the desired counties for analysis
counties1 = c("Miami-Dade", "Broward", "Palm Beach")

# read in the population data to calculate proportions for analysis
pop_total <- read_xlsx(
  path = "/Users/shellyfsinclair/masters/23_spring/adv_r/analyze_COVID_delta/county_population_20210515.xlsx", 
  skip = 6,
  col_names = c(
    "County",
    rep("trash", times = 15),
    "ages_65_74", "ages_75_84", "ages_85", "population_total",
    rep("trash", times = 20)
  )) %>% 
  # remove columns we don't want
  select(!starts_with("trash"))


# FINAL DATASET
# Join report and population data
reportData_df <-
  CDC_data_df %>%
  
  # Tidy county names
  mutate(across("County", ~ str_remove(., " County, FL"))) %>%
  
  # Join on County
  left_join(pop_total, by = "County") %>%
  
  # Rename variables to not be tedious
  rename(
    county = County,
    cases_7day = `Cases - last 7 days`,
    pcr_pos_rate = `Viral (RT-PCR) lab test positivity rate - last 7 days (may be an underestimate due to delayed reporting)`, 
    naat_pos_rate = `NAAT positivity rate - last 7 days (may be an underestimate due to delayed reporting)`,
    admissions_7day = `Confirmed COVID-19 admissions - last 7 days`,
    vacc_total = `People who are fully vaccinated`,
    vacc_65_total = `People who are fully vaccinated - ages 65+`
  ) %>% 
  
  # Convert chr population variables to numeric
  mutate(
    across(
      c(ages_65_74, ages_75_84, ages_85, population_total),
      ~ str_remove_all(., "[:punct:]") %>% 
        as.numeric
    )
  ) %>% 
  
  # Create proportion variables
  mutate(
    # Test data goes from RT-PCR to NAAT reporting,
    #   so we must account for both for positive test proportion
    proportion_positive = 
      if_else(
        is.na(pcr_pos_rate), 
        naat_pos_rate, 
        pcr_pos_rate
      ),
    vacc_proportion = vacc_total / population_total,
    vacc_65_proportion = vacc_65_total / (ages_65_74 + ages_75_84 + ages_85),
    cases_7dayAve = cases_7day
  ) %>% 
  
  # Keep only variables we need
  select(
    c(report_date, county, cases_7dayAve, proportion_positive, 
      admissions_7day, vacc_proportion, vacc_65_proportion)
  ) %>% 
  
  # Written to csv file in data_clean folder
  write_csv(file = "data_clean/CDC_COVID_wrangled.csv")




