library(tidyverse)
library(janitor)
library(gt)
library(clipr)

##### Importing ######

orig <- read_csv("recession_major_qualtrics.csv") %>%
  clean_names()

###### Cleaning ######

# Qualtrics outputs the data with 2 extraneous rows: a repetition of the
# variable names and an additional level of descriptions for each variable

orig <- orig[-c(1:2), ] %>% 
  
  # Renaming and paring down variable to be used in regressions and
  # visualizations. Note that I drop any personally identifiable information
  # (PII) and use response_id as a unique identifier for privary reasons
  
  select(progress,
         duration_secs = duration_in_seconds,
         finished,
         response_id,
         race = q1,
         anticip_grad = q4,
         gender = q5,
         family_income = q6,
         legacy_status = q7,
         intl_status = q8,
         conc_certainty = q9,
         anticip_conc = q48,
         declared_conc = q30,
         anticip_sectors = q39,
         career_prep = q15,
         career_track = q16,
         salary_imp = q17,
         current_econ_str = q40,
         one_year_econ_str = q11,
         recess_change = q12,
         new_conc = q13,
         reason_change = q14,
         ideal_anticip_salary = q20)

# Dummy variables for fixed effects 

orig <- orig %>% 
  
  # 1 indicates that the respondent would change their concentration, 0 indicates that they would not 
  
  mutate(recess_change_d = ifelse(recess_change == "Yes", 1, ifelse(recess_change == "No", 0, NA)),
         
         # 1 indicates that the respondent self-identified as female, 0
         # indicates that they self-identified as male
         
         gender_d = ifelse(gender == "Female", 1, ifelse(gender == "Male", 0, NA))) 
  
  # Converting string numeric values to integers. Ranges recoded as means and
  # boundless values recoded as floors
  
  orig$ideal_anticip_salary <- orig$ideal_anticip_salary %>% 
    recode(`$20k-$40k` = 30000, 
         `$61k-80k` = 70000, 
         `$101k-150k` = 125000, 
         `$151k-200k` = 175000, 
         `$200k+` = 200000, 
         `$41k-60k` = 50000)
  
  orig$family_income <- orig$family_income %>% 
    recode(`Over $500k` = 500000,
           `Between $80k and $125k` = 102500,
           `Below $40k` = 40000,
           `Between $250k and $500k` = 375000,
           `Between $125k and $250k` = 187500,
           `Between $40k and $80k` = 60000)
  
###### Analysis ######
  
# Regressing change of concentration in response to recession on ideal
# anticipated salary after graduation
  
change_salary <- orig %>% 
    select(gender_d,
           recess_change_d,
           ideal_anticip_salary) %>% 
    filter(!is.na(gender_d),
           !is.na(recess_change_d),
           !is.na(ideal_anticip_salary))

linear_mod <- lm(recess_change_d ~ ideal_anticip_salary * gender_d, data = change_salary)

summary(linear_mod)

# Graduation year breakdown

orig %>% 
  group_by(anticip_grad) %>% 
  count() %>% 
  filter(!is.na(anticip_grad)) %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% 
  mutate(freq = n / total) %>% 
  select(`Graduation Year` = anticip_grad,
         `Proportion of Respondents` = freq) %>% 
  gt() %>% 
  tab_header(
    title = "Proportion of Respondents by Expected Graduation Year",
    subtitle = "First-Years and Fourth-Years Overrepresented"
  ) %>% 
  fmt_percent(vars(`Proportion of Respondents`))

# Gender Breakdown

orig %>% 
  group_by(gender) %>% 
  count() %>% 
  filter(!is.na(gender)) %>% 
  ungroup() %>% 
  mutate(total = sum(n)) %>% 
  mutate(freq = n / total) %>% 
  select(`Gender` = gender,
         `Proportion of Respondents` = freq) %>% 
  gt() %>% 
  tab_header(
    title = "Proportion of Respondents by Gender Identification",
    subtitle = "Women Overrepresented"
  ) %>% 
  fmt_percent(vars(`Proportion of Respondents`))




