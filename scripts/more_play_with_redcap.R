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
#' manipulating data structure
#' #################################################################

redcap_visits %>% 
  group_by(season) %>% 
  summarise(unique_subject_count = n_distinct(subject_id),
            visit_count = n(),
            positive_scv2_test_count = sum(scv2_positive, na.rm = TRUE))

redcap_visits %>% 
  group_by(season) %>% 
  summarise(unique_subject_count = n_distinct(subject_id),
            visit_count = n(),
            positive_scv2_test_count = sum(scv2_positive, na.rm = TRUE)) %>% 
  pivot_longer(cols = -season, names_to = "season_feature", values_to = "count")

redcap_visits %>% 
  group_by(season) %>% 
  summarise(unique_subject_count = n_distinct(subject_id),
            visit_count = n(),
            positive_scv2_test_count = sum(scv2_positive, na.rm = TRUE)) %>% 
  pivot_longer(cols = -season, names_to = "season_feature", values_to = "count") %>%
  ggplot(data = .) +
  geom_col(aes(x = season, y = count, fill = season_feature)) +
  facet_wrap(facets = ~ season_feature, labeller = as_labeller(c("positive_scv2_test_count" = "SCV2 Count", "unique_subject_count" = "Subject Count", "visit_count" = "Visit Count"))) +
  scale_fill_brewer(palette = "Spectral", labels = c("positive_scv2_test_count" = "SCV2 Count", "unique_subject_count" = "Subject Count", "visit_count" = "Visit Count")) +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90)) +
  labs(x = "", y = "Count", fill = "") +
  guides(fill = guide_legend(nrow = 2))

redcap_visits %>% 
  group_by(season) %>% 
  summarise(unique_subject_count = n_distinct(subject_id),
            visit_count = n(),
            positive_scv2_test_count = sum(scv2_positive, na.rm = TRUE)) %>% 
  pivot_longer(cols = -season, names_to = "season_feature", values_to = "count") %>%
  pivot_wider(id_cols = season_feature, names_from = season, values_from = count)


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
#' plots to explore outcome ~ exposure associations
#' #################################################################

# rural plots
qol_v_rural_box <- redcap_subjects %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  ggplot(data = .) +
  geom_boxplot(aes(x = rural_residence, y = measured_QOL, fill = rural_residence))
qol_v_rural_box

qol_v_rural_box_by_smoker <- redcap_subjects %>%
  left_join(redcap_visits, by = "subject_id") %>% 
  ggplot(data = .) +
  geom_boxplot(aes(x = rural_residence, y = measured_QOL, fill = rural_residence)) +
  facet_wrap(facets = ~ smoker, labeller = label_both)
qol_v_rural_box_by_smoker

qol_v_rural_box_by_smoker_copd <- redcap_subjects %>%
  left_join(redcap_visits, by = "subject_id") %>% 
  ggplot(data = .) +
  geom_boxplot(aes(x = rural_residence, y = measured_QOL, fill = rural_residence)) +
  facet_wrap(facets = ~ smoker + copd, labeller = label_both)
qol_v_rural_box_by_smoker_copd


# age plots
qol_v_age_point <- redcap_subjects %>%
  mutate(age_years = lubridate::time_length(interval(birth_date, enrollment_date), "years")) %>%
  left_join(redcap_visits, by = "subject_id") %>% 
  ggplot(data = .) +
  geom_point(aes(x = age_years, y = measured_QOL)) +
  geom_smooth(aes(x = age_years, y = measured_QOL), method = "lm")
qol_v_age_point

qol_v_age_point_rural <- redcap_subjects %>%
  mutate(age_years = lubridate::time_length(interval(birth_date, enrollment_date), "years")) %>%
  left_join(redcap_visits, by = "subject_id") %>% 
  ggplot(data = .) +
  geom_point(aes(x = age_years, y = measured_QOL, color = rural_residence)) +
  geom_smooth(aes(x = age_years, y = measured_QOL, color = rural_residence), method = "lm")
qol_v_age_point_rural

qol_v_age_point_smoker_copd <- redcap_subjects %>%
  mutate(age_years = lubridate::time_length(interval(birth_date, enrollment_date), "years")) %>%
  left_join(redcap_visits, by = "subject_id") %>% 
  ggplot(data = .) +
  geom_point(aes(x = age_years, y = measured_QOL)) +
  geom_smooth(aes(x = age_years, y = measured_QOL), method = "lm") +
  facet_wrap(facets = ~ smoker, labeller = label_both)
qol_v_age_point_smoker_copd


#' #################################################################
#' tests and models of outcome ~ exposure associations
#' #################################################################

# QOL outcome, rural residence exposure
redcap_subjects %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  t.test(measured_QOL ~ rural_residence, data = ., alternative = "two.sided")
  
t_test_qol_v_rural <- redcap_subjects %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  t.test(measured_QOL ~ rural_residence, data = ., alternative = "two.sided")

t_test_qol_v_rural

t_test_qol_v_rural %>%
  broom::tidy()

kw_test_qol_v_rural <- redcap_subjects %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  kruskal.test(measured_QOL ~ rural_residence, data = .)

kw_test_qol_v_rural

kw_test_qol_v_rural %>%
  broom::tidy()

lm_qol_v_rural <- redcap_subjects %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  lm(measured_QOL ~ rural_residence, data = .)

lm_qol_v_rural

lm_qol_v_rural %>%
  summary()

lm_qol_v_rural %>%
  broom::tidy()

lm_qol_v_rural %>%
  gtsummary::tbl_regression()


lm_qol_v_rural_smoker <- redcap_subjects %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  lm(measured_QOL ~ rural_residence + smoker, data = .)

lm_qol_v_rural_smoker %>%
  broom::tidy()

lm_qol_v_rural_smoker %>%
  gtsummary::tbl_regression()




# QOL outcome, age exposure

lm_qol_v_age <- redcap_subjects %>%
  mutate(age_years = lubridate::time_length(interval(birth_date, enrollment_date), "years")) %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  lm(measured_QOL ~ age_years, data = .)

lm_qol_v_age

lm_qol_v_age %>%
  summary()

lm_qol_v_age %>%
  broom::tidy()

lm_qol_v_age %>%
  gtsummary::tbl_regression()


lm_qol_v_age_smoker_copd <- redcap_subjects %>%
  mutate(age_years = lubridate::time_length(interval(birth_date, enrollment_date), "years")) %>%
  full_join(redcap_visits, by = "subject_id") %>% 
  lm(measured_QOL ~ age_years + smoker + copd, data = .)

lm_qol_v_age_smoker_copd %>%
  broom::tidy()

gt_lm_qol_v_age_smoker_copd <- lm_qol_v_age_smoker_copd %>%
  gtsummary::tbl_regression() %>%
  gtsummary::as_gt()
gt_lm_qol_v_age_smoker_copd

gt_lm_qol_v_age_smoker_copd %>%
  gtsave(filename = "tables/gt_lm_qol_v_age_smoker_copd.png")


#' #################################################################
#' use patchwork to create a figure comprising multiple plots/tables
#' https://patchwork.data-imaginist.com/articles/patchwork.html
#' #################################################################

library(patchwork)

# combine multiple plots
combined_figure1 <- qol_v_rural_box + (qol_histogram_stacked_by_season / qol_v_age_point) + plot_layout(heights = c(2,1,1), widths = c(1,2,2)) + plot_annotation(tag_levels = 'A')
combined_figure1

combined_figure1 %>%
  ggsave(plot = ., filename = "figures/combined_figure1.pdf", height = 8, width = 12, units = "in")

combined_figure1 %>%
  ggsave(plot = ., filename = "figures/combined_figure1.png", height = 8, width = 12, units = "in", dpi = 600)



# combine tables and plots
combined_figure2 <- qol_v_age_point_smoker_copd + gt_lm_qol_v_age_smoker_copd + plot_layout(heights = c(1,1), widths = c(2,1))
combined_figure2

combined_figure2 %>%
  ggsave(plot = ., filename = "figures/combined_figure2.pdf", height = 8, width = 12, units = "in")

combined_figure2 %>%
  ggsave(plot = ., filename = "figures/combined_figure2.png", height = 8, width = 12, units = "in", dpi = 600)



#' #################################################################
#' revisiting model objects
#' #################################################################

lm_qol_v_age %>%
  summary()

lm_qol_v_age %>%
  broom::tidy()

lm_qol_v_age %>%
  broom::glance()

lm_qol_v_age %>%
  broom::augment()

lm_qol_v_age %>%
  broom::augment(newdata = tibble(age_years = seq(10,99,1)), interval = "confidence", conf.level = 0.95)

lm_qol_v_age %>%
  broom::augment(newdata = tibble(age_years = seq(10,99,1)), interval = "prediction", conf.level = 0.95)

ggplot(lm_qol_v_age, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Residual vs. Fitted Values Plot",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_bw()


