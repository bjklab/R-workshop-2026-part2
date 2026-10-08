#' #################################################################
#' load the libraries that will give us extra functions for our work
#' #################################################################

library(tidyverse)
library(gtsummary)
library(gt)



#' #################################################################
#' load two (mock) data tables from REDCap exports
#' #################################################################

read_csv("data/mock_redcap_subjects.csv")

read_csv("data/mock_redcap_visits.csv")


redcap_subjects <- read_csv("data/mock_redcap_subjects.csv")
redcap_subjects %>%
  View()


redcap_visits <- read_csv("data/mock_redcap_visits.csv")
redcap_visits %>%
  View()



#' #################################################################
#' explore outcome variable with a plot
#' #################################################################

redcap_visits %>%
  ggplot(data = .) +
  geom_histogram(aes(x = measured_QOL), binwidth = 1, fill = "white", color = "red", alpha = 0.5)



#' #################################################################
#' describe exposure variables in a table
#' #################################################################

redcap_subjects %>%
  mutate(age_years = lubridate::time_length(interval(enrollment_date, birth_date), "years")) %>%
  select(rural_residence, age_years, smoker, diabetes_mellitus, cad, copd) %>%
  gtsummary::tbl_summary(data = ., by = rural_residence) %>%
  gtsummary::add_overall() %>%
  gtsummary::add_p() %>%
  gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ "**Rural Residence**")


mock_redcap_table1 <- redcap_subjects %>%
  mutate(age_years = lubridate::time_length(interval(enrollment_date, birth_date), "years")) %>%
  select(rural_residence, age_years, smoker, diabetes_mellitus, cad, copd) %>%
  gtsummary::tbl_summary(data = ., by = rural_residence) %>%
  gtsummary::add_overall() %>%
  gtsummary::add_p() %>%
  gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ "**Rural Residence**")

mock_redcap_table1

mock_redcap_table1 %>%
  gtsummary::as_gt() %>%
  gtsave(filename = "tables/mock_redcap_table1.png")

mock_redcap_table1 %>%
  gtsummary::as_gt() %>%
  gtsave(filename = "tables/mock_redcap_table1.html") 
# note: html table can be opened in Excel

mock_redcap_table1 %>%
  gtsummary::save_flex_docx(path = "tables/mock_redcap_table1.docx")


#' #################################################################
#' join subject and visit data & explore outcome ~ exposure associations
#' #################################################################

redcap_subjects %>%
  left_join(redcap_visits, by = "subject_id") %>% 
  glimpse()


redcap_subjects %>%
  left_join(redcap_visits, by = "subject_id") %>% 
  ggplot(data = .) +
  geom_boxplot(aes(x = rural_residence, y = measured_QOL, fill = rural_residence))


redcap_subjects %>%
  left_join(redcap_visits, by = "subject_id") %>% 
  ggplot(data = .) +
  geom_boxplot(aes(x = rural_residence, y = measured_QOL, fill = rural_residence)) +
  facet_wrap(facets = ~ smoker, labeller = label_both)


redcap_subjects %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  group_by(subject_id) %>%
  summarise(n_visits = n(),
            median_QOL = median(measured_QOL, na.rm = TRUE),
            rural_residence = unique(rural_residence)) %>%
  ggplot(data = .) +
  geom_point(aes(x = median_QOL, y = n_visits, color = rural_residence))

redcap_subjects %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  group_by(subject_id) %>%
  summarise(n_visits = n(),
            median_QOL = median(measured_QOL, na.rm = TRUE),
            rural_residence = unique(rural_residence),
            smoker = unique(smoker)) %>%
  ggplot(data = .) +
  geom_point(aes(x = median_QOL, y = n_visits, color = rural_residence)) +
  geom_smooth(method = "lm", aes(x = median_QOL, y = n_visits, color = rural_residence)) +
  facet_wrap(facets = ~ smoker, labeller = label_both)

