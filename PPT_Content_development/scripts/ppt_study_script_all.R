# === 1. Load libraries ===
library(readxl)
library(dplyr)
library(tidyr)
library(flextable)
library(officer)
library(stringr)

# === 2. Load data ===
setwd("C:/Users/HAMDAN SAIDI/Desktop/PPT_study")
PPT_study6 <- read_excel("PPT_dataset3.xls")

# === 3. Keep only needed columns ===
PPT_clean1 <- PPT_study6[, c("gender_cut", "age_group", "education", "locat_cut", "district_cut", 
                             "comfort_report_cut", "believe_report_ill_cut", "use_digitalsystem_cut", 
                             "digital_safe_cut", "join_program_cut", "digital_detect_cut", "accept_digitaltool_cut")]

# === 4. Clean and standardize outcome variables ===
response_levels <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

cols_to_clean <- c("comfort_report_cut", "believe_report_ill_cut", "use_digitalsystem_cut",
                   "digital_safe_cut", "join_program_cut", "digital_detect_cut", "accept_digitaltool_cut")

for(col in cols_to_clean){
  PPT_clean1[[col]] <- PPT_clean1[[col]] %>%
    as.character() %>%
    trimws() %>%
    str_to_title() %>%
    factor(levels = response_levels)
}

# === 5. Crosstab function ===
create_survey_table <- function(data, var, outcome) {
  data %>%
    filter(!is.na(.data[[var]]), !is.na(.data[[outcome]])) %>%
    group_by(Category = .data[[var]], Response = .data[[outcome]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    complete(Response = factor(response_levels, levels = response_levels), fill = list(n = 0)) %>%
    group_by(Category) %>%
    mutate(percent = round(n / sum(n) * 100, 1),
           label = paste0(n, " (", percent, "%)")) %>%
    select(Category, Response, label) %>%
    pivot_wider(names_from = Response, values_from = label, values_fill = "0 (0%)") %>%
    ungroup()
}

# === 6. Create Word document ===
doc <- read_docx()

# Demographic variables
demographics <- c("gender_cut", "age_group", "education", "locat_cut", "district_cut")

# Outcome variables with descriptions
outcomes <- list(
  believe_report_ill_cut = "Belief that phone reporting improves healthcare",
  comfort_report_cut = "Comfort reporting symptoms via SMS/Mobile apps",
  use_digitalsystem_cut = "Acceptance of using a digital system approved by Ministry of Health",
  digital_safe_cut = "Belief that personal health data is safe via digital tools",
  join_program_cut = "Willingness to participate in digital programs",
  digital_detect_cut = "Trust in digital systems for early outbreak detection",
  accept_digitaltool_cut = "Community acceptance of digital tools if explained"
)

# === 7. Loop through outcomes and demographics ===
for(outcome_var in names(outcomes)){
  for(demo_var in demographics){
    tbl <- create_survey_table(PPT_clean1, demo_var, outcome_var)
    
    ft <- flextable(tbl) %>%
      autofit() %>%
      bold(part = "header") %>%
      align(align = "center", part = "all")
    
    doc <- doc %>%
      body_add_par(paste("Table:", outcomes[[outcome_var]], "by", demo_var),
                   style = "heading 2") %>%
      body_add_flextable(ft) %>%
      body_add_par("", style = "Normal")
  }
}

# === 8. Save Word file ===
print(doc, target = "C:/Users/HAMDAN SAIDI/Desktop/PPT_study/Belief_and_Comfort_Crosstabs_forall.docx")






