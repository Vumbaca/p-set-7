library(tidyverse)

library(fs)

# download file
download.file(url = "https://goo.gl/ZRCBda", destfile = "polling.zip", mode = "wb")

# unzip file
unzip("polling.zip")

# save all file names in unzipped data folder
file_names <- dir_ls("2018-live-poll-results-master/data")

# read all files into dataframe saving their names as source
x <- map_dfr(file_names, read_csv, .id = "source")

# delete both downloaded files
file_delete(c("polling.zip", "2018-live-poll-results-master"))

x <- x %>%
  # keep only useful variables
  select(source, response, age_combined, educ4, race_eth, gender_combined, partyid, final_weight) %>%
  # find and format state from source
  mutate(state = toupper(str_sub(source, start = 51, end = 52))) %>%
  # extract district number or else senator or governor indicator from source
  mutate(district = case_when(
    source %in% str_subset(x$source, pattern = "sen") ~ "sen",
    source %in% str_subset(x$source, pattern = "gov") ~ "gov",
    TRUE ~ str_sub(source, start = 53, end = 54))) %>%
  # find wave number and save as character string
  mutate(wave = paste("wave", str_sub(source, start = 56, end = 56), sep = "_")) %>%
  # create recognizable district id from state and district
  mutate(district_id = paste(state, district, sep = "-"))

multiwave <- x %>%
  # isolate district and wave
  select(district_id, wave) %>%
  # eliminate copies from same district and wave combination
  distinct() %>%
  # group rows from same district together
  group_by(district_id) %>%
  # count number of rows in each district group
  tally() %>%
  # keep any districts with more than one row which indicates more than one wave
  filter(n > 1)

x <- x %>%
  # keep all rows of districts with only one wave and rows only from most recent wave for any district with multiple waves
  filter(!district_id %in% multiwave$district_id | wave == "wave_3")

adv <- x %>%
  # categorize rows by unique district and response pairings
  group_by(district_id, response) %>%
  # calculate weighted sum of all rows of each response in each district
  mutate(weight = sum(final_weight)) %>%
  # categorize rows by district
  group_by(district_id) %>%
  # calculate weighted sum of all rows of any response in each district
  mutate(total = sum(final_weight)) %>%
  # keep only district and response and recent calculations
  select(district_id, response, weight, total) %>%
  # remove duplicates
  distinct() %>%
  # filter out responses not from two major parties
  filter(response == "Dem" | response == "Rep") %>%
  # reorganize data
  spread(response, weight) %>%
  # calculate republican advantage for each district
  mutate("rep_adv" = round(100 * (Rep - Dem) / total, 2)) %>%
  # keep only district and its corresponding republican advantage
  select(district_id, rep_adv)

# supplement base data with republican advantage value for each district
x <- left_join(x, adv, by = "district_id")

x_age <- x %>%
  # categorize according to district
  group_by(district_id) %>%
  # count rows in each age range
  count(age_combined) %>%
  # reorganize data
  spread(age_combined, n) %>%
  # calculate total number of rows included
  mutate(total = `18 to 29` + `30 to 44` + `45 to 64` + `65 and older`) %>%
  # calculate percentage of rows in 18 to 29 age range
  mutate(p_18 = round(100 * `18 to 29` / total, 2)) %>%
  # calculate percentage of rows in 30 to 44 age range
  mutate(p_30 = round(100 * `30 to 44` / total, 2)) %>%
  # calculate percentage of rows in 45 to 64 age range
  mutate(p_45 = round(100 * `45 to 64` / total, 2)) %>%
  # calculate percentage of rows in 65 and older age range
  mutate(p_65 = round(100 * `65 and older` / total, 2))

# supplement base data with age range percentages for each district
x <- left_join(x, x_age, by = "district_id")
  
# repeat age procedure with education levels
x_educ <- x %>%
  group_by(district_id) %>%
  count(educ4) %>%
  spread(educ4, n) %>%
  mutate(total = `[DO NOT READ] Don't know/Refused` +
                 `4-year College Grad.` +
                 `High School Grad. or Less` +
                 `Postgraduate Degree` +
                 `Some College Educ.`) %>%
  mutate(p_high = round(100 * `High School Grad. or Less` / total, 2)) %>%
  mutate(p_some = round(100 * `Some College Educ.` / total, 2)) %>%
  mutate(p_coll = round(100 * `4-year College Grad.` / total, 2)) %>%
  mutate(p_post = round(100 * `Postgraduate Degree` / total, 2))

# add educational level percentages
x <- left_join(x, x_educ, by = "district_id")

# repeat age procedure with race ethnicity
x_race <- x %>%
  group_by(district_id) %>%
  count(race_eth) %>%
  spread(race_eth, n) %>%
  # take into account one WV district for which asian count is NA
  mutate(total = ifelse(is.na(Asian), `[DO NOT READ] Don't know/Refused` + 0 + Black + Hispanic + Other + White,
                        `[DO NOT READ] Don't know/Refused` + Asian + Black + Hispanic + Other + White)) %>%
  # take into account one WV district for which asian count is NA
  mutate(p_asian = ifelse(is.na(Asian), 0, round(100 * Asian / total, 2))) %>%
  mutate(p_black = round(100 * Black / total, 2)) %>%
  mutate(p_hispanic = round(100 * Hispanic / total, 2)) %>%
  mutate(p_white = round(100 * White / total, 2))

# add race ethnicity data
x <- left_join(x, x_race, by = "district_id")

# repeat age procedure with gender
x_gender <- x %>%
  group_by(district_id) %>%
  count(gender_combined) %>%
  spread(gender_combined, n) %>%
  mutate(total = Female + Male) %>%
  mutate(p_female = round(100 * Female / total, 2)) %>%
  mutate(p_male = round(100 * Male / total, 2))

# add gender data
x <- left_join(x, x_gender, by = "district_id")

# repeat age procedure with political party
x_party <- x %>%
  group_by(district_id) %>%
  count(partyid) %>%
  spread(partyid, n) %>%
  mutate(total = `[DO NOT READ] Refused` +
                 Democrat +
                 `Independent (No party)` +
                 `or as a member of another political party` +
                 Republican) %>%
  mutate(p_dem = round(100 * Democrat / total, 2)) %>%
  mutate(p_other = round(100 * `or as a member of another political party` / total, 2)) %>%
  mutate(p_indep = round(100 * `Independent (No party)` / total, 2)) %>%
  mutate(p_rep = round(100 * Republican / total, 2))

# add political party
x <- left_join(x, x_party, by = "district_id")

# read actual results data from file in repo to dataframe
y <- read_csv("mt_2_results.csv")

y <- y %>%
  # change at large only district in a state districts to district one
  mutate(district = ifelse(district == "AL", 01, district)) %>%
  # create familiar district id from state and district
  mutate(district_id = paste(state, district, sep = "-")) %>%
  # compute actual republican advantage to compare to predicted
  mutate(rep_adv = round(100 * (rep_votes - dem_votes) / (rep_votes + dem_votes + other_votes), 2))

# join prediction data to results data while matching by district
shiny_data <- left_join(x, y, by = "district_id")

shiny_data <- shiny_data %>%
  # keep variables of interest and remove all others
  select(district_id,
         rep_adv_act = rep_adv.y,
         rep_adv_est = rep_adv.x,
         p_18, p_30, p_45, p_65,
         p_high, p_some, p_coll, p_post,
         p_asian, p_black, p_hispanic, p_white,
         p_female, p_male,
         p_dem, p_rep, p_indep, p_other) %>%
  # compute error as difference between actual and predicted republican advantage
  mutate(error = round(rep_adv_act - rep_adv_est, 2)) %>%
  # remove duplicates so as to have one row per district
  distinct()

# save data to file in repo for app
write_rds(shiny_data, "Errors/shiny_data.rds", compress = "gz")