if(!require(pacman)) install.packages("pacman")

p_load(
  tidyverse,
  here,
  summarytools,
  janitor,
  sf, 
  tmap,
  OpenStreetMap,
  flextable,
  rio, 
  gtsummary, 
  epikit, 
  gt,
  dplyr,
  knitr, 
  writexl
)

ppt_data <- read.csv("C:/Users/Dell/Documents/Personal/Independent_Projects/MOH/PPT/PPT_study.csv")

# Define the breaks 
age_breaks <- c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)

# Define the labels for the age groups
age_labels <- c(
  "18-24", "25-29", "30-34", "35-39", "40-44", 
  "45-49", "50-54", "55-59", "60-64", "65-69", 
  "70-74", "75-79", "80-84", "85 above"
)

# Create Age_Group variable
ppt_data <- ppt_data %>%
  mutate(
    age_group = cut(
      x = age,
      breaks = age_breaks,
      labels = age_labels,
      include.lowest = TRUE, 
      right = FALSE          
    )
  )

#17. I would be willing to participate in digital disease surveillance programs

join_program_df<- ppt_data |> 
  select(
    join_program, 
    gender, 
    age_group, 
    edu, 
    locat, 
    district
  ) |> 
  mutate(
    across(where(is.character), as.factor)
  )

summary_table <- join_program_df |>
  tbl_summary(
    by = join_program,  
    missing = "no",

    digits = list(
      gender ~ c(0, 2),
      age_group ~ c(0, 2),
      edu ~ c(0, 2), 
      locat ~ c(0, 2), 
      district ~ c(0, 2)
    ), 
    label = list(
      gender ~ "Gender",
      age_group ~ "Age Group", 
      edu ~ "Education",
      locat ~ "Location",
      district ~ "District"
    )
  ) 
summary_table  <- summary_table  |>
  modify_header(label ~ "Willing to participate in digital disease surveillance programs")

# Print table
summary_table  %>% kable()
# Save table to Excel
export_df <- summary_table |> 
  gtsummary::as_tibble()
write_xlsx(export_df, "C:/Users/Dell/Documents/Personal/Independent_Projects/MOH/PPT/Willing_to_participate.xlsx")


#18.	I trust that digital systems can help detect disease outbreaks early.
digital_detect_df<- ppt_data |> 
  select(
    digital_detect, 
    gender, 
    age_group, 
    edu, 
    locat, 
    district
  ) |> 
  mutate(
    across(where(is.character), as.factor)
  )

summary_table <- digital_detect_df |>
  tbl_summary(
    by = digital_detect,  
    missing = "no",
     digits = list(
      gender ~ c(0, 2),
      age_group ~ c(0, 2),
      edu ~ c(0, 2), 
      locat ~ c(0, 2), 
      district ~ c(0, 2)
    ), 
    label = list(
      gender ~ "Gender",
      age_group ~ "Age Group", 
      edu ~ "Education",
      locat ~ "Location",
      district ~ "District"
    )
  ) 
summary_table  <- summary_table  |>
  modify_header(label ~ "Trust that digital systems can help detect disease outbreaks early")

# Print table
summary_table  %>% kable()
# Save table to Excel
export_df <- summary_table |> 
  gtsummary::as_tibble()
write_xlsx(export_df, "C:/Users/Dell/Documents/Personal/Independent_Projects/MOH/PPT/trust_digital_systems.xlsx")

#19. I think people in my community would accept digital surveillance tools if properly explained
accept_digitaltool_df<- ppt_data |> 
  select(
    accept_digitaltool, 
    gender, 
    age_group, 
    edu, 
    locat, 
    district
  ) |> 
  mutate(
    across(where(is.character), as.factor)
  )

summary_table <- accept_digitaltool_df |>
  tbl_summary(
    by = accept_digitaltool,  
    missing = "no",
    digits = list(
      gender ~ c(0, 2),
      age_group ~ c(0, 2),
      edu ~ c(0, 2), 
      locat ~ c(0, 2), 
      district ~ c(0, 2)
    ), 
    label = list(
      gender ~ "Gender",
      age_group ~ "Age Group", 
      edu ~ "Education",
      locat ~ "Location",
      district ~ "District"
    )
  ) 
summary_table  <- summary_table  |>
  modify_header(label ~ "People in my community would accept digital surveillance tools if properly explained")

# Print table
summary_table  %>% kable()
# Save table to Excel
export_df <- summary_table |> 
  gtsummary::as_tibble()
write_xlsx(export_df, "C:/Users/Dell/Documents/Personal/Independent_Projects/MOH/PPT/accept_digitaltools.xlsx")

#20.	In your own words, what would make you feel more comfortable using your phone to report health problems or symptoms?
# comfort_via_phone_df<- ppt_data |> 
#   select(
#     comfort_via_phone, 
#     gender, 
#     age_group, 
#     edu, 
#     locat, 
#     district
#   ) |> 
#   mutate(
#     across(where(is.character), as.factor)
#   )
# 
# summary_table <- comfort_via_phone_df |>
#   tbl_summary(
#     by = comfort_via_phone,  
#     missing = "no",
#     digits = list(
#       gender ~ c(0, 2),
#       age_group ~ c(0, 2),
#       edu ~ c(0, 2), 
#       locat ~ c(0, 2), 
#       district ~ c(0, 2)
#     ), 
#     label = list(
#       gender ~ "Gender",
#       age_group ~ "Age Group", 
#       edu ~ "Education",
#       locat ~ "Location",
#       district ~ "District"
#     )
#   ) 
# summary_table  <- summary_table  |>
#   modify_header(label ~ "what would make you feel more comfortable using your phone to report health problems or symptoms")
# 
# # Print the final table
# summary_table  %>% kable()
# # Save the full table to Excel
# export_df <- summary_table |> 
#   gtsummary::as_tibble()
# write_xlsx(export_df, "C:/Users/Dell/Documents/Personal/Independent_Projects/MOH/PPT/comfort_via_phone.xlsx")

#21.	Have you ever used a mobile phone or app to receive or report health information?
rece_repo_healthinfo_df<- ppt_data |> 
  select(
    rece_repo_healthinfo, 
    gender, 
    age_group, 
    edu, 
    locat, 
    district
  ) |> 
  mutate(
    across(where(is.character), as.factor)
  )

summary_table <- rece_repo_healthinfo_df |>
  tbl_summary(
    by = rece_repo_healthinfo,  
    missing = "no",
    digits = list(
      gender ~ c(0, 2),
      age_group ~ c(0, 2),
      edu ~ c(0, 2), 
      locat ~ c(0, 2), 
      district ~ c(0, 2)
    ), 
    label = list(
      gender ~ "Gender",
      age_group ~ "Age Group", 
      edu ~ "Education",
      locat ~ "Location",
      district ~ "District"
    )
  ) 
summary_table  <- summary_table  |>
  modify_header(label ~ "Have you ever used a mobile phone or app to receive or report health information")

# Print table
summary_table  %>% kable()
# Save table to Excel
export_df <- summary_table |> 
  gtsummary::as_tibble()
write_xlsx(export_df, "C:/Users/Dell/Documents/Personal/Independent_Projects/MOH/PPT/rece_repo_healthinfo.xlsx")

#22.	Which method would you prefer for reporting symptoms or receiving alerts?
rece_repo_alert_df<- ppt_data |> 
  select(
    rece_repo_alert, 
    gender, 
    age_group, 
    edu, 
    locat, 
    district
  ) |> 
  mutate(
    across(where(is.character), as.factor)
  )

summary_table <- rece_repo_alert_df |>
  tbl_summary(
    by = rece_repo_alert,  
    missing = "no",
    digits = list(
      gender ~ c(0, 2),
      age_group ~ c(0, 2),
      edu ~ c(0, 2), 
      locat ~ c(0, 2), 
      district ~ c(0, 2)
    ), 
    label = list(
      gender ~ "Gender",
      age_group ~ "Age Group", 
      edu ~ "Education",
      locat ~ "Location",
      district ~ "District"
    )
  ) 
summary_table  <- summary_table  |>
  modify_header(label ~ "Which method would you prefer for reporting symptoms or receiving alerts")

# Print table
summary_table  %>% kable()
# Save table to Excel
export_df <- summary_table |> 
  gtsummary::as_tibble()
write_xlsx(export_df, "C:/Users/Dell/Documents/Personal/Independent_Projects/MOH/PPT/prefered_method.xlsx")

