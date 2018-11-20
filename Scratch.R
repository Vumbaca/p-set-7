library(tidyverse)
library(fs)

download.file(url = "https://goo.gl/ZRCBda", destfile = "polling.zip", mode = "wb")

unzip("polling.zip")

file_names <- dir_ls("2018-live-poll-results-master/data")

x <- map_dfr(file_names, read_csv, .id = "source")

file_delete(c("polling.zip", "2018-live-poll-results-master"))

x <- x %>%
  select(source, response, age_combined, educ4, race_eth, gender_combined, partyid, final_weight) %>%
  mutate(state = toupper(str_sub(source, start = 51, end = 52))) %>%
  mutate(district = case_when(
    source %in% str_subset(x$source, pattern = "sen") ~ "sen",
    source %in% str_subset(x$source, pattern = "gov") ~ "gov",
    TRUE ~ str_sub(source, start = 53, end = 54))) %>%
  mutate(wave = paste("wave", str_sub(source, start = 56, end = 56), sep = "_")) %>%
  mutate(district_id = paste(state, district, sep = "-"))

multiwave <- x %>%
  select(district_id, wave) %>%
  distinct() %>%
  group_by(district_id) %>%
  tally() %>%
  filter(n > 1)

x <- x %>%
  filter(!district_id %in% multiwave$district_id | wave == "wave_3")

adv <- x %>%
  group_by(district_id, response) %>%
  mutate(weight = sum(final_weight)) %>%
  group_by(district_id) %>%
  mutate(total = sum(final_weight)) %>%
  select(district_id, response, weight, total) %>%
  distinct() %>%
  filter(response == "Dem" | response == "Rep") %>%
  spread(response, weight) %>%
  mutate("rep_adv" = 100 * (Rep - Dem) / total) %>%
  select(district_id, rep_adv)

x <- left_join(x, adv, by = "district_id")

y <- read_csv("mt_2_results.csv")

y <- y %>%
  mutate(district = ifelse(district == "AL", 01, district)) %>%
  mutate(district_id = paste(state, district, sep = "-")) %>%
  mutate(rep_adv = 100 * (rep_votes - dem_votes) / (rep_votes + dem_votes + other_votes))

midterm <- left_join(x, y, by = "district_id")