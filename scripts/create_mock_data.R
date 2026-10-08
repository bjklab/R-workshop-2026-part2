library(tidyverse)
library(scales)
library(babynames)
data(babynames)

set.seed(16)

# mock subject data

mock_redcap_subjects <- tibble(record_id = 1:100) %>%
  mutate(subject_id = paste0("MOCK_REDCAP_SUBJECT_", stringr::str_pad(string = 1:100, pad = "0", side = "left", width = 4))) %>%
  rowwise() %>%
  mutate(last_name = sample(babynames$name, size = 1, replace = TRUE)) %>%
  mutate(first_name = sample(babynames$name, size = 1, replace = TRUE)) %>%
  mutate(birth_date = sample(seq.Date(from = as.Date("1944-04-24"), to = as.Date("2001-01-16"), by = 1), size = 1, replace = TRUE)) %>%
  ungroup() %>%
  mutate(enrollment_date = sort(sample(seq.Date(from = as.Date("2025-11-18"), to = as.Date("2026-06-03"), by = 1), size = 100, replace = TRUE))) %>%
  mutate(smoker = rbernoulli(n = 100, p = 0.15)) %>%
  mutate(rural_residence = rbernoulli(n = 100, p = 0.40)) %>%
  mutate(diabetes_mellitus = rbernoulli(n = 100, p = 0.30)) %>%
  mutate(copd = map_lgl(.x = smoker, .f = ~ rbernoulli(n = 1, p = 0.50 * .x + 0.1))) %>%
  mutate(cad = map2_lgl(.x = smoker, .y = diabetes_mellitus, .f = ~ rbernoulli(n = 1, p = (0.30 * .y) + (0.30 * .x) + 0.1)))

mock_redcap_subjects

mock_redcap_subjects %>%
  xtabs(data = ., formula = ~ smoker + copd)

mock_redcap_subjects %>%
  xtabs(data = ., formula = ~ cad + diabetes_mellitus)

mock_redcap_subjects %>%
  write_csv("data/mock_redcap_subjects.csv")


# mock visit data

mock_redcap_visits <- mock_redcap_subjects %>%
  select(subject_id, enrollment_date, smoker, rural_residence, diabetes_mellitus, copd, cad) %>%
  mutate(risk_score = smoker + diabetes_mellitus + copd + cad - rural_residence) %>%
  mutate(risk_score = scales::rescale(risk_score, to = c(1,12))) %>%
  select(subject_id, enrollment_date, risk_score) %>%
  mutate(visit_count = round(risk_score)) %>%
  #uncount(weights = visit_count) %>%
  mutate(visit_date = map(.x = enrollment_date, .f = ~ seq.Date(from = .x, to = Sys.Date(), by = 1))) %>%
  mutate(visit_date = map2(.x = visit_date, .y = visit_count, .f = ~ sample(.x, size = .y, replace = FALSE))) %>%
  unnest(cols = visit_date) %>%
  select(subject_id, visit_date, risk_score) %>%
  arrange(subject_id, visit_date) %>%
  mutate(measured_QOL = rescale(risk_score, to = c(0,1))) %>%
  mutate(measured_QOL = map_dbl(.x = measured_QOL, .f = ~ 5 + rbinom(n = 1, size = 27, prob = 1 - .x))) %>%
  select(-risk_score)

mock_redcap_visits

mock_redcap_visits %>%
  write_csv("data/mock_redcap_visits.csv")


# add seasonal risk of SCV2

library(lubridate)

get_season <- function(input_date) {
  # Convert date to a numeric MMDD format (e.g., April 1st becomes 401)
  mmdd <- month(input_date) * 100 + day(input_date)
  
  # Cut into seasonal bins based on calendar days
  # Winter crosses years, so we set it as the default fallback
  cut(mmdd,
      breaks = c(0, 319, 620, 921, 1220, 1300),
      labels = c("Winter", "Spring", "Summer", "Autumn", "Winter"),
      right = TRUE)
}

# Example Usage:
test_dates <- as.Date(c("2026-01-15", "2026-04-10", "2026-07-22", "2026-10-31"))
get_season(test_dates)
get_season(test_dates) %>%
  as.numeric()

mock_redcap_visits_scv2 <- mock_redcap_visits %>%
  mutate(season = get_season(visit_date)) %>%
  mutate(scv2_positive = map_lgl(.x = season, .f = ~ rbernoulli(n = 1, p = 0.2 * (as.numeric(.x) %% 2) + 0.01))) %>%
  #count(scv2_positive)
  # seasonal effect
  rowwise() %>%
  mutate(measured_QOL = ifelse(season == "Winter" & measured_QOL >= 2, measured_QOL - 1, measured_QOL)) %>%
  # SCV2 effect
  mutate(measured_QOL = ifelse(scv2_positive == TRUE & measured_QOL >= 2, measured_QOL - 2, measured_QOL))

mock_redcap_visits_scv2 %>%
  write_csv("data/mock_redcap_visits_scv2.csv")


