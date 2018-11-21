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

x_age <- x %>%
  group_by(district_id) %>%
  count(age_combined) %>%
  spread(age_combined, n) %>%
  mutate(total = `18 to 29` + `30 to 44` + `45 to 64` + `65 and older`) %>%
  mutate(p_18 = `18 to 29` / total) %>%
  mutate(p_30 = `30 to 44` / total) %>%
  mutate(p_45 = `45 to 64` / total) %>%
  mutate(p_65 = `65 and older` / total)

x <- left_join(x, x_age, by = "district_id")
  
x_educ <- x %>%
  group_by(district_id) %>%
  count(educ4) %>%
  spread(educ4, n) %>%
  mutate(total = `[DO NOT READ] Don't know/Refused` +
                 `4-year College Grad.` +
                 `High School Grad. or Less` +
                 `Postgraduate Degree` +
                 `Some College Educ.`) %>%
  mutate(p_high = `High School Grad. or Less` / total) %>%
  mutate(p_some = `Some College Educ.` / total) %>%
  mutate(p_coll = `4-year College Grad.` / total) %>%
  mutate(p_post = `Postgraduate Degree` / total)

x <- left_join(x, x_educ, by = "district_id")

x_race <- x %>%
  group_by(district_id) %>%
  count(race_eth) %>%
  spread(race_eth, n) %>%
  mutate(total = ifelse(is.na(Asian), `[DO NOT READ] Don't know/Refused` + 0 + Black + Hispanic + Other + White,
                        `[DO NOT READ] Don't know/Refused` + Asian + Black + Hispanic + Other + White)) %>%
  mutate(p_asian = ifelse(is.na(Asian), 0, Asian / total)) %>%
  mutate(p_black = Black / total) %>%
  mutate(p_hispanic = Hispanic / total) %>%
  mutate(p_white = White / total)

x <- left_join(x, x_race, by = "district_id")

x_gender <- x %>%
  group_by(district_id) %>%
  count(gender_combined) %>%
  spread(gender_combined, n) %>%
  mutate(total = Female + Male) %>%
  mutate(p_female = Female / total) %>%
  mutate(p_male = Male / total)

x <- left_join(x, x_gender, by = "district_id")

x_party <- x %>%
  group_by(district_id) %>%
  count(partyid) %>%
  spread(partyid, n) %>%
  mutate(total = `[DO NOT READ] Refused` +
                 Democrat +
                 `Independent (No party)` +
                 `or as a member of another political party` +
                 Republican) %>%
  mutate(p_dem = Democrat / total) %>%
  mutate(p_indep = `Independent (No party)` / total) %>%
  mutate(p_rep = Republican / total)

x <- left_join(x, x_party, by = "district_id")

y <- read_csv("mt_2_results.csv")

y <- y %>%
  mutate(district = ifelse(district == "AL", 01, district)) %>%
  mutate(district_id = paste(state, district, sep = "-")) %>%
  mutate(rep_adv = 100 * (rep_votes - dem_votes) / (rep_votes + dem_votes + other_votes))

shiny_data <- left_join(x, y, by = "district_id")

shiny_data <- shiny_data %>%
  mutate(perc_error =  (rep_adv.y - rep_adv.x) / rep_adv.x) %>%
  select(district_id,
         perc_error,
         p_18, p_30, p_45, p_65,
         p_high, p_some, p_coll, p_post,
         p_asian, p_black, p_hispanic, p_white,
         p_female, p_male,
         p_dem, p_indep, p_rep) %>%
  distinct()

write_rds(shiny_data, "Errors/shiny_data.rds", compress = "gz")