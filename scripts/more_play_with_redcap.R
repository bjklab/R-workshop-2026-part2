#' #################################################################
#' load the libraries that will give us extra functions for our work
#' #################################################################

library(tidyverse)
library(gtsummary)
library(gt)

library(haven)
library(readxl)
library(googledrive)
library(googlesheets4)


#' #################################################################
#' load two (mock) data tables from REDCap exports
#' #################################################################

# our first pass:
redcap_subjects <- read_csv("data/mock_redcap_subjects.csv")
redcap_subjects %>%
  View()

redcap_visits <- read_csv("data/mock_redcap_visits.csv")
redcap_visits %>%
  View()

#' lots of options for data import:
#' Microsoft Excel files: .xlsx
redcap_subjects <- readxl::read_xlsx(path = "data/mock_redcap_subjects.xlsx", sheet = 1)
redcap_subjects %>%
  View()

#' Stata files: .dta
redcap_visits <- haven::read_dta("data/mock_redcap_visits.dta")
redcap_visits %>%
  View()


#' Google Sheets (or other files posted on Google Drive)
# public Google Sheets data with expanded visit data: https://docs.google.com/spreadsheets/d/1UaPXuOauNRwzBC3l9PFsurqZXWVthvtjaf_VdNXE654

# because data are public, you can tell googledrive not to expect login credentials
drive_deauth()
gs4_deauth()

# use the public link
public_sheet_url <- "https://docs.google.com/spreadsheets/d/1UaPXuOauNRwzBC3l9PFsurqZXWVthvtjaf_VdNXE654/edit?usp=sharing"

# use googlesheets4 to read it
local_copy <- read_sheet(public_sheet_url)
local_copy %>%
  View()

redcap_visits <- local_copy
redcap_visits %>%
  View()


#' #################################################################
#' explore outcome variable with a plot
#' #################################################################

#' outcome variable histogram, faceted by season
redcap_visits %>%
  ggplot(data = .) +
  geom_histogram(aes(x = measured_QOL, fill = season), binwidth = 1, color = "white") +
  facet_wrap(facets = ~ season) +
  scale_fill_brewer(palette = "Spectral") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Measured QOL (32-item survey)", y = "Visit Count")

# outcome variable histogram, stacked
redcap_visits %>%
  ggplot(data = .) +
  geom_histogram(aes(x = measured_QOL, fill = season), binwidth = 1, color = "white") +
  #facet_wrap(facets = ~ season) +
  scale_fill_brewer(palette = "Spectral") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Measured QOL (32-item survey)", y = "Visit Count", fill = "")


#' #################################################################
#' name and save to make R objects durable
#' #################################################################

# name it!

qol_histogram_stacked_by_season <- redcap_visits %>%
  ggplot(data = .) +
  geom_histogram(aes(x = measured_QOL, fill = season), binwidth = 1, color = "white") +
  #facet_wrap(facets = ~ season) +
  scale_fill_brewer(palette = "Spectral") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Measured QOL (32-item survey)", y = "Visit Count", fill = "")

# save it!

qol_histogram_stacked_by_season %>%
  ggsave(plot = ., "figures/qol_histogram_stacked_by_season.png", height = 6, width = 8, units = "in", dpi = 600)

qol_histogram_stacked_by_season %>%
  ggsave(plot = ., "figures/qol_histogram_stacked_by_season.pdf", height = 6, width = 8, units = "in")


#' #################################################################
#' Manipulating Data in R: Columns
#' #################################################################

#' #################################################################
#' describe exposure variables in a table 1
#' #################################################################

# operations on rows include `filter` & `slice`
# operations on columns include `select`, `mutate`, & `summarize`

redcap_subjects %>%
  mutate(age_years = lubridate::time_length(interval(enrollment_date, birth_date), "years")) %>%
  rename_all(.funs = ~ stringr::str_to_title(string = gsub(pattern = "_", replacement = " ", .x))) %>%
  rename(`Age (years)` = `Age Years`, `CAD` = `Cad`, `COPD` = `Copd`) %>%
  select(`Rural Residence`, `Age (years)`, `Smoker`, `Diabetes Mellitus`, `CAD`, `COPD`) %>%
  gtsummary::tbl_summary(data = ., by = `Rural Residence`) %>%
  gtsummary::add_overall() %>%
  gtsummary::add_p() %>%
  gtsummary::modify_spanning_header(gtsummary::all_stat_cols() ~ "**Rural Residence**")


mock_redcap_table1 <- redcap_subjects %>%
  mutate(age_years = lubridate::time_length(interval(enrollment_date, birth_date), "years")) %>%
  rename_all(.funs = ~ stringr::str_to_title(string = gsub(pattern = "_", replacement = " ", .x))) %>%
  rename(`Age (years)` = `Age Years`, `CAD` = `Cad`, `COPD` = `Copd`) %>%
  select(`Rural Residence`, `Age (years)`, `Smoker`, `Diabetes Mellitus`, `CAD`, `COPD`) %>%
  gtsummary::tbl_summary(data = ., by = `Rural Residence`) %>%
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

#' commonly-used joining functions
redcap_subjects %>%
  left_join(redcap_visits, by = "subject_id") %>% 
  glimpse()

redcap_subjects %>%
  right_join(redcap_visits, by = "subject_id") %>% 
  glimpse()

redcap_subjects %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  glimpse()


#' what if subjects are missing from one side?
redcap_subjects %>%
  slice(1:50) %>%
  left_join(redcap_visits, by = "subject_id") %>% 
  glimpse()

redcap_subjects %>%
  slice(1:50) %>%
  right_join(redcap_visits, by = "subject_id") %>% 
  glimpse()

redcap_subjects %>%
  slice(1:50) %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  glimpse()



#' #################################################################
#' join subject and visit data & explore outcome ~ exposure associations
#' #################################################################

redcap_subjects %>%
  full_join(redcap_visits, by = "subject_id") %>% 
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

