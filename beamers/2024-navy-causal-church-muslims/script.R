# script
# joseph bulbulia
# talk to navy march 2024


# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/libs2.R")

# WARNING:  COMMENT THIS OUT. JB DOES THIS FOR WORKING WITHOUT WIFI
source("/Users/joseph/GIT/templates/functions/funs.R")

# ALERT: UNCOMMENT THIS AND DOWNLOAD THE FUNCTIONS FROM JB's GITHUB
# source(
#   "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
# )

# source(
#   "https://raw.githubusercontent.com/go-bayes/templates/main/functions/experimental_funs.R"
# )


## WARNING SET THIS PATH TO YOUR DATA ON YOUR SECURE MACHINE.
pull_path <-
  fs::path_expand(
    #"/Users/joseph/v-project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs_refactor/nzavs_data_23"
    "/Users/joseph/Library/CloudStorage/Dropbox-v-project/Joseph\ Bulbulia/00Bulbulia\ Pubs/DATA/nzavs-current/r-data/nzavs_data"
  )

# path for talk figs
path_talk <- '/Users/joseph/GIT/bulbeamers/beamers/2024-navy-causal-church-muslims'


# read data
dat <- arrow::read_parquet(pull_path)



# timeline ----------------------------------------------------------------

# timeline to show responses
library(dplyr)
library(lubridate)
library(ggplot2)

# Set the base date
base_date <- as.Date("2009-06-30")


#  ensure  data has a 'day' column that correctly represents dates.

df_timeline <- dat %>%
  mutate(year = as.numeric(as.character(wave))) %>%
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) %>%
  dplyr:::count(day = floor_date(timeline, "day")) |>
  mutate(nzavs_wave = factor(
    case_when(
      day >= as.Date("2009-08-30") & day < as.Date("2010-10-15") ~ "time 1",
      day >= as.Date("2010-10-15") & day < as.Date("2011-10-15") ~ "time 2",
      day >= as.Date("2011-10-15") & day < as.Date("2012-10-15") ~ "time 3",
      day >= as.Date("2012-10-15") & day < as.Date("2013-10-15") ~ "time 4",
      day >= as.Date("2013-10-15") & day < as.Date("2014-10-15") ~ "time 5",
      day >= as.Date("2014-10-15") & day < as.Date("2015-10-15") ~ "time 6",
      day >= as.Date("2015-10-15") & day < as.Date("2016-10-15") ~ "time 7",
      day >= as.Date("2016-10-15") & day < as.Date("2017-10-15") ~ "time 8",
      day >= as.Date("2017-10-15") & day < as.Date("2018-10-15") ~ "time 9",
      day >= as.Date("2018-10-15") & day < as.Date("2019-10-15") ~ "time 10",
      day >= as.Date("2019-10-15") & day < as.Date("2020-10-15") ~ "time 11",
      day >= as.Date("2020-10-15") & day < as.Date("2021-10-15") ~ "time 12",
      day >= as.Date("2021-10-15") & day < as.Date("2022-10-15") ~ "time 13",
      day >= as.Date("2022-10-15") & day < as.Date("2023-10-15") ~ "time 14",
      TRUE ~ NA_character_  # For days outside the defined waves
    ))
  ) |>
  arrange(day, nzavs_wave)

# check
str(df_timeline)

head(df_timeline)

# check n
n_unique(dat$id)

# make timeline
timeline_histgram_2009_2024 <-  ggplot(df_timeline, aes(x = day, y = n, fill = nzavs_wave)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(
    title = "New Zealand Attitudes and Values Study (panel)",
    subtitle = "N = 72,910; years 2012-2022",
    x = "NZAVS years 2012- 2022 cohort (N = 72,910): daily counts by condition",
    y = "Count of Responses"
  ) +
  theme_classic() +
  scale_fill_viridis_d() +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )


#  plot
print(timeline_histgram_2009_2024)

# save
ggsave(
  timeline_histgram_2009_2024,
  path = path_talk,
  width = 12,
  height = 8,
  units = "in",
  filename = "timeline_histgram_2009_2024.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 400
)



# muslim warmth after attacks ---------------------------------------------

tl <- dat %>%
  select(warm_muslims, wave,tscore, sample_frame_opt_in,  id) %>%
  mutate(year = as.numeric(as.character(wave))) |>
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) %>%
  dplyr::filter(timeline > "2012-06-06") %>%
  # dplyr:::count(day = floor_date(timeline, "day"))%>%
  dplyr::mutate(attack_condition = factor(
    ifelse(timeline < "2019-03-15", 0, 1)
  )) %>%
  arrange(timeline, attack_condition)

# check n
length(unique(tl$id))


# discontinuity plot
muslim_attack_discontinuity_2012_2022_include_opt_in <-
  ggplot(tl, aes(x = timeline, y = warm_muslims, color = attack_condition)) +
  geom_jitter(alpha = .03, width = 1) +
  stat_smooth(method = "gam") +
  theme(legend.position = "bottom") +
  labs(
    title = "Discontinuity at attacks (GAM)",
    subtitle = "Boost to Warmth increase in the years following the attacks: FULL SAMPLE",
    y = "Muslim Warmth",
    x = "NZAVS Time 4 - 14 Cohort (2012-2023), (N = 71,128)"
  ) +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

# save
ggsave(
  muslim_attack_discontinuity_2012_2022_include_opt_in,
 # path = here::here("figures"),
  width = 12,
  height = 8,
  units = "in",
  filename ="muslim_attack_discontinuity_2012_2022_include_opt_in.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)


# check opt-ins
t2 <- tl |>  filter(sample_frame_opt_in!=1)

# no diff
# muslim_attack_discontinuity_2012_2022_include_opt_in_NOT <-
#   ggplot(t2, aes(x = timeline, y = warm_muslims, color = attack_condition)) +
#   geom_jitter(alpha = .03, width = 1) +
#   stat_smooth(method = "gam") +
#   theme(legend.position = "bottom") +
#   labs(
#     title = "Discontinuity at attacks (GAM)",
#     subtitle = "Boost to Warmth increase in the years following the attacks",
#     y = "Muslim Warmth",
#     x = "NZAVS Time 4 - 13 Cohort (2012-2022), (N = 60,742)"
#   ) +
#   scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 12)
#   )
#
# muslim_attack_discontinuity_2012_2022_include_opt_in_NOT
#
#
# # save graph
# ggsave(
#   muslim_attack_discontinuity_2012_2022_include_opt_in_NOT,
#   path = path_talk,
#   width = 12,
#   height = 8,
#   units = "in",
#   filename = "muslim_attack_discontinuity_2012_2022_include_opt_in_NOT.png",
#   device = 'png',
#   limitsize = FALSE,
#   dpi = 400
# )



# trust in science --------------------------------------------------------





tl <- dat %>%
  select(warm_muslims, wave,tscore, sample_frame_opt_in,  id) %>%
  mutate(year = as.numeric(as.character(wave))) |>
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) %>%
  dplyr::filter(timeline > "2012-06-06") %>%
  # dplyr:::count(day = floor_date(timeline, "day"))%>%
  dplyr::mutate(attack_condition = factor(
    ifelse(timeline < "2019-03-15", 0, 1)
  )) %>%
  arrange(timeline, attack_condition)

# check n
length(unique(tl$id))


# discontinuity plot
muslim_attack_discontinuity_2012_2022_include_opt_in <-
  ggplot(tl, aes(x = timeline, y = warm_muslims, color = attack_condition)) +
  geom_jitter(alpha = .03, width = 1) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 4)) +
  theme(legend.position = "bottom") +
  labs(
    title = "Muslim Warmth Pre/Post Mosque Attacks (GAM)",
    y = "Muslim Warmth",
    x = "NZAVS Time 4 - 14 Cohort (2012-2023), (N = 71,128)"
  ) +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

muslim_attack_discontinuity_2012_2022_include_opt_in

# save
ggsave(
  muslim_attack_discontinuity_2012_2022_include_opt_in,
  path = here::here("figures"),
  width = 16,
  height = 9,
  units = "in",
  filename ="muslim_attack_discontinuity_2012_2022_include_opt_in.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)



# covid new ---------------------------------------------------------------

colnames(dat)

df_covid  <- dat %>%
  select(conspiracy_beliefs, police_trust, science_trust, pol_politician_trust,
         alert_level_combined, wave,tscore, sample_frame_opt_in,  id) %>%
  mutate(year = as.numeric(as.character(wave))) |>
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) %>%
  dplyr::filter(timeline > "2017-10-01") %>%
  # dplyr:::count(day = floor_date(timeline, "day"))%>%
  dplyr::mutate(covid_19_attack = factor(
    ifelse(timeline < "2020-01-06", 0, 1)
  )) %>%
  arrange(timeline, alert_level_combined)

# check n
length(unique(df_covid$id))


# discontinuity plot
plot_rdd_covid_trust_police <-
  ggplot(df_covid, aes(x = timeline, y = police_trust, color = covid_19_attack)) +
  geom_jitter(alpha = .03, width = 1) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 4)) +
  theme(legend.position = "bottom") +
  labs(
    title = "Trust in Police: Pre/Post Covid-19 Attack",
    y = "Trust in Police",
    x = "NZAVS Time 9 - 14 Cohort (2017-2023), (N = 64287)"
  ) +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

plot_rdd_covid_trust_police

# save
ggsave(
  plot_rdd_covid_trust_police,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename ="plot_rdd_covid_trust_police.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)



# discontinuity plot
plot_rdd_covid_trust_politicians <-
  ggplot(df_covid, aes(x = timeline, y = pol_politician_trust, color = covid_19_attack)) +
  geom_jitter(alpha = .03, width = 1) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 4)) +
  theme(legend.position = "bottom") +
  labs(
    title = "Trust in Politicians:  Pre/Post Covid-19 Attack",
    y = "Trust in Politicians",
    x = "NZAVS Time 9 - 14 Cohort (2017-2023), (N = 64287)"
  ) +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )
plot_rdd_covid_trust_politicians

# save
ggsave(
  plot_rdd_covid_trust_politicians,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename ="plot_rdd_covid_trust_politicians.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)



# discontinuity plot
# science measure started in t11
df_covid_science  <- dat %>%
  select(conspiracy_beliefs, police_trust, science_trust, pol_politician_trust,
         alert_level_combined, wave,tscore, sample_frame_opt_in,  id) %>%
  mutate(year = as.numeric(as.character(wave))) |>
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) %>%
  dplyr::filter(timeline > "2019-10-01") %>%
  # dplyr:::count(day = floor_date(timeline, "day"))%>%
  dplyr::mutate(covid_19_attack = factor(
    ifelse(timeline < "2020-01-06", 0, 1)
  )) %>%
  arrange(timeline, alert_level_combined)

n_unique(df_covid_science$id)

plot_rdd_covid_trust_science <-
  ggplot(df_covid_science, aes(x = timeline, y = science_trust, color = covid_19_attack)) +
  geom_jitter(alpha = .03, width = 1) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
  theme(legend.position = "bottom") +
  labs(
    title = "Trust in Science: Pre/Post Covid-19 Attacks",
    y = "Trust in Science",
    x = "NZAVS Time 11 - 14 Cohort (2012-2023), (N = 55297)"
  ) +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )
plot_rdd_covid_trust_science

# save
ggsave(
  plot_rdd_covid_trust_science,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename ="plot_rdd_covid_trust_science.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)

# conspiracy beliefs (also started in time 11)


plot_rdd_covid_trust_conspiracy <-
  ggplot(df_covid_science, aes(x = timeline, y = conspiracy_beliefs, color = covid_19_attack)) +
  geom_jitter(alpha = .03, width = 1) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
  theme(legend.position = "bottom") +
  labs(
    title = "Conspiracy Beliefs: Pre/Post Covid-19 Attacks",
    y = "Conspiracy Beliefs",
    x = "NZAVS Time 11 - 14 Cohort (2012-2023), (N = 55297)"
  ) +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )
plot_rdd_covid_trust_conspiracy

# save
ggsave(
  plot_rdd_covid_trust_conspiracy,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename ="plot_rdd_covid_trust_conspiracy.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)


# ai ----------------------------------------------------------------------

# get ids
ids_2018 <- dat %>%
  filter(year_measured == 1, wave == 2018) |>
  pull(id)


# # Filter the dataset for waves 2018 and 2022, including only those IDs from 2018
dat_2018_2022 <- dat %>%
  filter(id %in% ids_2018 & wave %in% c(2018, 2022))



n_unique(dat_2018_2022$id)



ids_in_both_waves <- dat_2018_2022 %>%
  group_by(id) %>%
  filter(all(c(2018, 2022) %in% wave)) %>%
  summarise() %>%
  pull(id)



# Filter the original dataset for these IDs and the two waves
dat_ai <- nzavs_data %>%
  filter(id %in% ids_in_both_waves, wave %in% c(2018, 2022), year_measured == 1)|>
  droplevels()

# Optional: filter to include only baseline_ids, if required
# dat_ai <- dat_ai %>% filter(id %in% baseline_ids)






graph_ai_change <- dat_2018_2022 %>%
  ggplot(aes(x = wave, y = issue_regulate_ai, fill = wave)) +
  labs(title = "Attitudes to AI regulation in 2018 and 2022",
       subtitle = "`Strict regulation limiting the development and use of Artificial Intelligence`") +
  geom_boxplot(size = .05, notch = TRUE) +
  scale_fill_okabe_ito() +
  # geom_jitter(width = 0.2, alpha = 0.02, size = 1.5) +  # Adding jittered points with weak alpha
  facet_grid(. ~ wave, scales = "free_x", space = "free_x") +
  theme(legend.position = "none") +
  theme_classic()


graph_ai_change


ggsave(
  graph_ai_change,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename ="graph_ai_change.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)



graph_issue_govt_surveillance_change <- dat_2018_2022 %>%
  ggplot(aes(x = wave, y = issue_govt_surveillance, fill = wave)) +
  labs(title = "Attitudes to Gov Surveillance in 2018 and 2022",
       subtitle = "`Collection of telephone and internet data by the New Zealand Government as part of anti-terrorism efforts`") +
  geom_boxplot(size = .05, notch = TRUE) +
  scale_fill_okabe_ito() +
  # geom_jitter(width = 0.2, alpha = 0.02, size = 1.5) +  # Adding jittered points with weak alpha
  facet_grid(. ~ wave, scales = "free_x", space = "free_x") +
  theme(legend.position = "none") +
  theme_classic()

graph_issue_govt_surveillance_change + graph_ai_change



ggsave(
  graph_issue_govt_surveillance_change,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename ="graph_issue_govt_surveillance_change.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)


## BETTER GRAPHS


library(mgcv)

dat_2022 <- dat %>%
  filter(wave %in% 2022, year_measured == 1) |>
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) |>
  dplyr::filter(timeline > "2022-09-21") # nzavs codebook



n_unique(dat_2022$id)


# spare
dat_2022_error  <- dat %>%
  filter(wave %in% 2022, year_measured == 1) |>
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) |>
  dplyr::filter(timeline < "2022-09-22") # nzavs codebook


n_unique(dat_2022_error$id)


dat_2022_error$id

## MODEL WITH WEIGHTS

gam_model_issue_govt_surveillance  <-
  gam(issue_govt_surveillance ~ bs(timeline, 3),
      weights = w_gend_age_ethnic,
      data = dat_2022)
dat_2022$timeline

# Prepare a new data frame for predictions
timeline_range <- range(dat_2022$timeline, na.rm = TRUE)
timeline_range
timeline_grid <-
  seq.Date(from = timeline_range[1],
           to = timeline_range[2],
           length.out = 200)

predictions <- data.frame(timeline = timeline_grid)

# Predict using the gam model
# Predict using the gam model with confidence intervals
preds <-
  predict(gam_model_issue_govt_surveillance,
          newdata = predictions,
          se.fit = TRUE)
predictions$issue_govt_surveillance <- preds$fit
predictions$lower <-
  preds$fit - 1.96 * preds$se.fit  # Lower bound of 95% CI
predictions$upper <-
  preds$fit + 1.96 * preds$se.fit  # Upper bound of 95% CI

# Create the plot with uncertainty
graph_issue_govt_surveillance <-
  ggplot(dat_2022, aes(x = timeline, y = issue_govt_surveillance)) +
  geom_jitter(alpha = .1, width = 1) +
  geom_ribbon(data = predictions,
              aes(ymin = lower, ymax = upper, x = timeline),
              alpha = 0.7) +
  geom_line(
    data = predictions,
    aes(x = timeline, y = issue_govt_surveillance),
    color = "blue"
  ) +
  labs(title = "Collection of telephone and internet data by\nthe New Zealand Government as part of anti-terrorism efforts",
       y = "Approve Government Surveillance",
       x = "NZAVS T14: n = 33643 (missing = 2001)") +
  theme_classic() +
  scale_okabe_ito(alpha = .1, aesthetics = "colour") +
  theme(legend.position = "none")

graph_issue_govt_surveillance

ggsave(
  graph_issue_govt_surveillance,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename ="better_graph_issue_govt_surveillance.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)


# REGULATE AI
gam_model_issue_regulate_ai  <-
  gam(issue_regulate_ai ~ bs(timeline, 3),
      weights = w_gend_age_ethnic,
      data = dat_2022)

timeline_range
# Prepare a new data frame for predictions
timeline_range <- range(dat_2022$timeline, na.rm = TRUE)
timeline_grid <-
  seq(from = timeline_range[1],
      to = timeline_range[2],
      length.out = 200)
predictions_ai <- data.frame(timeline = timeline_grid)

# Predict using the gam model
# Predict using the gam model with confidence intervals
preds_ai  <-
  predict(gam_model_issue_regulate_ai,
          newdata = predictions_ai,
          se.fit = TRUE)
predictions_ai$issue_regulate_ai <- preds_ai$fit
predictions_ai$lower <-
  preds_ai$fit - 1.96 * preds_ai$se.fit  # Lower bound of 95% CI
predictions_ai$upper <-
  preds_ai$fit + 1.96 * preds_ai$se.fit  # Upper bound of 95% CI

# Create the plot with uncertainty
graph_issue_regulate_ai <-
  ggplot(dat_2022, aes(x = timeline, y = issue_regulate_ai)) +
  geom_jitter(alpha = .1, width = 1) +
  geom_ribbon(data = predictions_ai,
              aes(ymin = lower, ymax = upper, x = timeline),
              alpha = 0.7) +
  geom_line(data = predictions_ai,
            aes(x = timeline, y = issue_regulate_ai),
            color = "red") +
  labs(title =  "Strict regulation limiting the development and use of Artificial Intelligence",
       y = "Approve AI Regulation",
       x = "NZAVS T14: n = 33643 (missing = 2150)") +
  theme_classic() +
  scale_okabe_ito(alpha = 1, aesthetics = "colour") +
  theme(legend.position = "none")

graph_issue_regulate_ai

combo_weighted_gam_3 <-
  graph_issue_govt_surveillance + graph_issue_regulate_ai + plot_annotation(title = "Comparison of Attitudes to New Zealand Government Interventions from 2022-SEP-22 to 2023-OCT-10",
                                                                            subtitle = "Generalised Additive Model: 3-knot splines weighted to NZ Census Age/Gender/Ethnicity, NZAVS (n=33643)",
                                                                            tag_levels = "A")


combo_weighted_gam_3

# save graph
ggsave(
  combo_weighted_gam_3,
    path = path_talk,
    width = 16,
    height = 9,
    units = "in",
    filename ="better_combo_graph_govt_surveillance_ai.png",
    device = 'png',
    limitsize = TRUE,
    dpi = 400
  )





# BEST GRAPHS -------------------------------------------------------------


#  month by month comparison ----------------------------------------------



dat_2022$month <- month(dat_2022$timeline)

dat_2022$month

table1::table1(
  ~ issue_regulate_ai + issue_govt_surveillance |
    as.factor(month),
  data = dat_2022,
  transpose = TRUE
)



library(ggplot2)
library(lubridate)

# Assuming dat_2022$timeline is a Date column
dat_2022 <- dat_2022 %>%
  mutate(month_year = format(timeline, "%b %Y"))

# Convert month_year to a factor with levels sorted chronologically
# Extract year and month number for sorting
dat_2022 <- dat_2022 %>%
  mutate(
    year = year(timeline),
    month_num = month(timeline),
    month_year = factor(month_year, levels = format(sort(unique(
      make_date(year, month_num, 1)
    )), "%b %Y"))
  )

# Check the structure of month_year
str(dat_2022$month_year)

# Now you can proceed to create your boxplots with dat_2022$month_year



# Now you can proceed to create your boxplots with dat_2022$month_year


# Creating boxplots for issue_regulate_ai by month


# Creating boxplots for issue_regulate_ai with color, notches, and custom labels
graph_ai_month <-
  ggplot(dat_2022,
         aes(x = month_year, y = issue_regulate_ai, fill = month_year)) +
  geom_boxplot(notch = TRUE) +
  #  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.15,
              alpha = 0.1,
              size = 1) +
  scale_fill_viridis_d() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title =  "Strict regulation limiting the development and use of Artificial Intelligence",
       x = "Month and Year",
       y = "Issue Regulate AI") +   theme(legend.position = "none")

graph_ai_month


# Creating boxplots for issue_govt_surveillance by month
graph_govt_surveillance_month <-
  ggplot(dat_2022,
         aes(x = month_year, y = issue_govt_surveillance, fill = month_year)) +
  geom_boxplot(notch = TRUE) +
  #  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.15,
              alpha = 0.1,
              size = 1) +
  labs(title = "Collection of telephone and internet data by the New Zealand Government as part of anti-terrorism efforts",
       x = "Month and Year",
       y = "Issue Govt Surveillance") +
  theme_classic() +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +   theme(legend.position = "none")

# Display the plots
compare_boxplots <-
  graph_govt_surveillance_month + graph_ai_month + plot_annotation(title = "Boxplots Comparisons of Attitudes to New Zealand Government Interventions from 2022-SEP-22 to 2023-OCT-10",
                                                                   subtitle = "NZAVS Time 14 (n=33643)",
                                                                   tag_levels = "A") +  plot_layout(guides = 'collect')

compare_boxplots

# weighted means by month -------------------------------------------------

# Function to calculate weighted mean and its standard error
weighted_mean_se <- function(x, w) {
  sum_w <- sum(w, na.rm = TRUE)
  sum_w_sq <- sum(w ^ 2, na.rm = TRUE)
  n <- length(w)
  weighted_mean <- weighted.mean(x, w, na.rm = TRUE)
  se <-
    sqrt(sum(w * (x - weighted_mean) ^ 2, na.rm = TRUE) / (sum_w ^ 2 - sum_w_sq / n))
  list(mean = weighted_mean, se = se)
}

# Calculate weighted means, standard errors, and confidence intervals
weighted_stats <- dat_2022 %>%
  group_by(month_year) %>%
  summarise(
    regulate_ai_stats = list(weighted_mean_se(issue_regulate_ai, w_gend_age_ethnic)),
    govt_surveillance_stats = list(
      weighted_mean_se(issue_govt_surveillance, w_gend_age_ethnic)
    )
  ) %>%
  mutate(
    regulate_ai_mean = sapply(regulate_ai_stats, function(x)
      x$mean),
    regulate_ai_se = sapply(regulate_ai_stats, function(x)
      x$se),
    regulate_ai_lower_ci = regulate_ai_mean - 1.96 * regulate_ai_se,
    regulate_ai_upper_ci = regulate_ai_mean + 1.96 * regulate_ai_se,
    govt_surveillance_mean = sapply(govt_surveillance_stats, function(x)
      x$mean),
    govt_surveillance_se = sapply(govt_surveillance_stats, function(x)
      x$se),
    govt_surveillance_lower_ci = govt_surveillance_mean - 1.96 * govt_surveillance_se,
    govt_surveillance_upper_ci = govt_surveillance_mean + 1.96 * govt_surveillance_se
  )

# Now, weighted_stats contains the means, standard errors, and confidence intervals


# Calculate weighted means for each month
# weighted_means <- dat_2022 %>%
#   group_by(month_year) %>%
#   summarise(issue_regulate_ai_mean = weighted.mean(issue_regulate_ai, w_gend_age_ethnic, na.rm = TRUE),
#             issue_govt_surveillance_mean = weighted.mean(issue_govt_surveillance, w_gend_age_ethnic, na.rm = TRUE))


# Plot for issue_regulate_ai with confidence intervals
plot_regulate_ai <-
  ggplot(weighted_stats, aes(x = month_year, y = regulate_ai_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = regulate_ai_lower_ci, ymax = regulate_ai_upper_ci),
                width = 0.2) +
  labs(title = "Strict regulation limiting the development and use of Artificial Intelligence",
       x = "Month and Year",
       y = "Weighted Mean") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(limits = c(3.5, 6))
plot_regulate_ai
# Plot for issue_govt_surveillance with confidence intervals
plot_govt_surveillance <-
  ggplot(weighted_stats, aes(x = month_year, y = govt_surveillance_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = govt_surveillance_lower_ci, ymax = govt_surveillance_upper_ci),
                width = 0.2) +
  labs(title = "Collection of telephone and internet data by the New Zealand Government as part of anti-terrorism efforts",
       x = "Month and Year",
       y = "Weighted Mean") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(limits = c(3.5, 6))
plot_govt_surveillance
# Display the plots

compare_monthly_weighted_means <-
  plot_govt_surveillance + plot_regulate_ai + plot_annotation(title = "Weighted Monthly Marginal Means: Comparisons of Attitudes to New Zealand Government Interventions",
                                                              subtitle = "NZAVS Time 14 (n=33643)",
                                                              tag_levels = "A") +  plot_layout(guides = 'collect')

compare_monthly_weighted_means


ggsave(
  compare_monthly_weighted_means,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename ="plot_compare_monthly_weighted_means_govt_surveillance_ai.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)


## unweighted
library(dplyr)
library(ggplot2)

# Calculate unweighted means and confidence intervals for each month
unweighted_stats <- dat_2022 %>%
  group_by(month_year) %>%
  summarise(
    issue_regulate_ai_mean = mean(issue_regulate_ai, na.rm = TRUE),
    issue_regulate_ai_se = sd(issue_regulate_ai, na.rm = TRUE) / sqrt(n()),
    issue_govt_surveillance_mean = mean(issue_govt_surveillance, na.rm = TRUE),
    issue_govt_surveillance_se = sd(issue_govt_surveillance, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(
    regulate_ai_lower_ci = issue_regulate_ai_mean - 1.96 * issue_regulate_ai_se,
    regulate_ai_upper_ci = issue_regulate_ai_mean + 1.96 * issue_regulate_ai_se,
    govt_surveillance_lower_ci = issue_govt_surveillance_mean - 1.96 * issue_govt_surveillance_se,
    govt_surveillance_upper_ci = issue_govt_surveillance_mean + 1.96 * issue_govt_surveillance_se
  )

# Plot for issue_regulate_ai
plot_regulate_ai_unweighted <-
  ggplot(unweighted_stats,
         aes(x = month_year, y = issue_regulate_ai_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = regulate_ai_lower_ci, ymax = regulate_ai_upper_ci),
                width = 0.2) +
  labs(title = "Strict regulation limiting the development and use of Artificial Intelligence",
       x = "Month and Year",
       y = "Mean") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(limits = c(3.5, 6))

# Plot for issue_govt_surveillance
plot_govt_surveillance_unweighted <-
  ggplot(unweighted_stats,
         aes(x = month_year, y = issue_govt_surveillance_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = govt_surveillance_lower_ci, ymax = govt_surveillance_upper_ci),
                width = 0.2) +
  labs(title = "Collection of telephone and internet data by the New Zealand Government as part of anti-terrorism efforts",
       x = "Month and Year",
       y = "Mean") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(limits = c(3.5, 6))

# Display the plots
compare_monthly_unweighted_monthly_means  <-
  plot_govt_surveillance_unweighted + plot_regulate_ai_unweighted

compare_weighted_unweighted_monthly_means <-
  compare_monthly_unweighted_monthly_means / compare_monthly_weighted_means +
  plot_annotation(title = "Compare unweighted (top) and weighted (bottom) Monthly Marginal Means: Attitudes to New Zealand Government Interventions", tag_levels = "A") +  plot_layout(guides = 'collect')

compare_weighted_unweighted_monthly_means

ggsave(
  compare_weighted_unweighted_monthly_means,
  path = here::here(here::here("figs")),
  width = 20,
  height = 10,
  units = "in",
  filename = "compare_weighted_unweighted_monthly_means.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 300
)

compare_weighted_unweighted_monthly_means_only_ai <-
  plot_regulate_ai + plot_regulate_ai_unweighted +
  plot_annotation(title = "Compare unweighted (left) and weighted (right) Monthly Marginal Means: Attitudes to New Zealand Government Interventions", tag_levels = "A") +  plot_layout(guides = 'collect')

compare_weighted_unweighted_monthly_means_only_ai

ggsave(
  compare_weighted_unweighted_monthly_means_only_ai,
  path = here::here(here::here("figs")),
  width = 20,
  height = 10,
  units = "in",
  filename = "compare_weighted_unweighted_monthly_means_only_ai.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 300
)




# RELIGIOUS CHANGE OVER TIME ----------------------------------------------

df_religion  <- dat %>%
  select(religion_religious,
         alert_level_combined, wave,tscore, religion_church_binary,
         sample_frame_opt_in, w_gend_age_euro, id) %>%
  mutate(year = as.numeric(as.character(wave))) |>
  dplyr::mutate(timeline = make_date(year = 2009, month = 6, day = 30) + tscore) %>%
  dplyr::filter(timeline > "2009-06-30") %>%
  # dplyr:::count(day = floor_date(timeline, "day"))%>%
  dplyr::mutate(covid_19_attack = factor(
    ifelse(timeline < "2020-01-06", 0, 1)
  )) %>%
 # dplyr::mutate(religion_religious_factor = as.factor(religion_religious)) |>
  dplyr::mutate(religion_church_binary_factor = as.factor(religion_church_binary)) |>
  arrange(timeline, alert_level_combined)

# check n
length(unique(df_religion$id))
table(is.na(df_religion$religion_religious))

mean(df_religion$religion_religious, na.rm = TRUE)

dat$w_gend_age_ethnic
# discontinuity plot
plot_religious_change <-
  ggplot(df_religion, aes(x = timeline, y = religion_church_binary,
                          #weight = w_gend_age_euro
                          )) + # Include weight in aes
  geom_point(alpha =.05) + # Plot the raw data points
  stat_smooth(method = "glm", method.args = list(family = "binomial"), formula = y ~ x) +
  labs(y = "Probability", x = "Predictor") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Probability of Church Attendance in New Zealand: Years 2009 - 2023",
    y = "Probability Church Attendance",
    x = "NZAVS Time 1 - 14 (2009-2023), (N = 72854)"
  ) +
#  scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

plot_religious_change

# save
ggsave(
  plot_religious_change,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename ="plot_prob_church_attendance_nz_2009to2024.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)


#
# # discontinuity plot
# plot_rdd_covid_trust_politicians <-
#   ggplot(df_covid, aes(x = timeline, y = pol_politician_trust, color = covid_19_attack)) +
#   geom_jitter(alpha = .03, width = 1) +
#   stat_smooth(method = "gam", formula = y ~ s(x, k = 4)) +
#   theme(legend.position = "bottom") +
#   labs(
#     title = "Trust in Politicians:  Pre/Post Covid-19 Attack",
#     y = "Trust in Politicians",
#     x = "NZAVS Time 9 - 14 Cohort (2017-2023), (N = 64287)"
#   ) +
#   scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 12)
#   )
# plot_rdd_covid_trust_politicians
#
# # save
# ggsave(
#   plot_rdd_covid_trust_politicians,
#   path = path_talk,
#   width = 16,
#   height = 9,
#   units = "in",
#   filename ="plot_rdd_covid_trust_politicians.png",
#   device = 'png',
#   limitsize = TRUE,
#   dpi = 400
# )
#
#
# # simple ------------------------------------------------------------------
#
#
#
#
#
#
# graph_issue_govt_surveillance <-
#   ggplot(dat_2022, aes(x = timeline, y = issue_govt_surveillance)) +
#   geom_jitter(alpha = .25, width = 1) +
#   stat_smooth(method = "gam", formula = y ~ s(x, k = 4)) +
#   labs(title =  "Collection of telephone and internet data by the New Zealand Government as part of anti-terrorism efforts",
#        #  subtitle = "Boost to Warmth increase in the years following the attacks",
#        y = "Approve Gov't Surveillance",
#        x = "NZAVS T14: n = 33723") + theme(legend.position = "none") +
#   # #scale_fill_discrete(name=NULL) + # not working
#   scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 12)
#   ) #+ scale_y_continuous(limits = c(3.75, 5))
#
#
# graph_issue_govt_surveillance
#
#
# graph_issue_regulate_ai <-
#   ggplot(dat_2022, aes(x = timeline, y = issue_regulate_ai)) +
#   geom_jitter(alpha = .25, width = 1) +
#   stat_smooth(method = "gam", formula = y ~ s(x, k = 4)) +
#
#   labs(title =  "Strict regulation limiting the development and use of Artificial Intelligence",
#        #  subtitle = "Boost to Warmth increase in the years following the attacks",
#        y = "Approve Gov't AI Regulation",
#        x = "NZAVS T14: n = 33723") + theme(legend.position = "none") +
#   # #scale_fill_discrete(name=NULL) + # not working
#   scale_okabe_ito(alpha = 1, aesthetics = "colour") + theme_classic() +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 12),
#     legend.title = element_text(size = 12)
#   )
#
# graph_issue_regulate_ai + graph_issue_govt_surveillance
#
#
# combo_graph <-
#   graph_issue_govt_surveillance + graph_issue_regulate_ai + plot_annotation(title = "Comparison of Attitudes to New Zealand Government Interventions in from 2022-09-22 to 2023-10-10",
#                                                                             #  subtitle = "Panel A = Government Surveillance for Anti-Terrorism; Panel B = Strict Regulation of AI development",
#                                                                             tag_levels = "A")
#
# combo_graph
#
# # save graph
# ggsave(
#   combo_graph,
#   path = here::here(here::here("figs")),
#   width = 20,
#   height = 10,
#   units = "in",
#   filename = "combo_graph_.jpg",
#   device = 'jpeg',
#   limitsize = FALSE,
#   dpi = 300
# )
#
#
#
# #
# RELIGION  ---------------------------------------------------------------


## Read in data from analysis
# imports
n_participants <-  here_read("n_participants")
n_participants
# read standard deviations units of outcome (volunteering/donations) in 2020
sd_donations <- here_read("sd_donations")
sd_volunteer <- here_read("sd_volunteer")


# verify positivity
#church
transition_table <- here_read("transition_table")
# binary
transition_table_2 <- here_read("transition_table_out_church_2")



# socialising
transition_table_socialising <- here_read("transition_table_socialising")


# binary
transition_table_socialising_shift<- here_read("transition_table_socialising_shift")
transition_table_socialising_shift


# ordinary regressions
fit_church_on_donate <-here_read("fit_church_on_donate")
lm_coef_church_on_donate <- tbl_regression(fit_church_on_donate)
b_cross_church_on_donate <-inline_text(lm_coef_church_on_donate, variable = religion_church_round, pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")

# use
b_cross_church_on_donate


fit_church_on_volunteer <-here_read("fit_church_on_volunteer")
lm_coef_fit_church_on_volunteer <- tbl_regression(fit_church_on_volunteer)
b_cross_church_on_volunteer <-inline_text(lm_coef_fit_church_on_volunteer, variable = religion_church_round, pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")

# use
b_cross_church_on_volunteer


fit_socialising_on_donate<-here_read("fit_socialising_on_donate")
fit_socialising_on_donate <-here_read("fit_socialising_on_donate")
lm_coef_fit_socialising_on_donate <- tbl_regression(fit_socialising_on_donate)
b_cross_socialising_on_donate <-inline_text(lm_coef_fit_socialising_on_donate, variable = hours_community_sqrt_round, pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")

# use
b_cross_socialising_on_donate


fit_socialising_on_volunteer <-here_read("fit_socialising_on_volunteer")
fit_socialising_on_volunteer <-here_read("fit_socialising_on_volunteer")
lm_coef_fit_socialising_on_volunteer <- tbl_regression(fit_socialising_on_volunteer)
b_cross_socialising_on_volunteer <-inline_text(lm_coef_fit_socialising_on_volunteer, variable = hours_community_sqrt_round, pattern = "b = {estimate}; (95% CI {conf.low}, {conf.high})")

# use
b_cross_socialising_on_volunteer

library(glue)
# works but not used
# testlm_coef_church_on_donate <- glue("b = {fit_church_on_donate_x[2, 1]}, SE = {fit_church_on_donate_x[2, 3]}")



# not this table is too long in most instance
# table_baseline<- here_read("table_baseline")

# personality
table_baseline_personality <- here_read("table_baseline_personality")

# demographic vars
table_demographic_vars <- here_read("table_demographic_vars")

# self reported behaviours
table_virtue_vars <- here_read("table_virtue_vars")

# prejudice
table_acceptance_vars<- here_read("table_acceptance_vars")

# help received
table_selected_sorted_names_help_received_vars <- here_read("table_selected_sorted_names_help_received_vars")

# density of the exposure
graph_density_of_exposure<- here_read("graph_density_of_exposure")
graph_density_of_exposure_socialising<- here_read("graph_density_of_exposure_socialising")

# graphs
# graph_density_of_exposure_socialising + ggtitle("Weekly hours socialing")
#graph_density_of_exposure + ggtitle("Monthly religious service")

#
#
# #### USE THIS #####
# # gain
# tab_compare_church_prosocial_behaviour_raw<-here_read("tab_compare_church_prosocial_behaviour_raw")
# tab_compare_church_prosocial_behaviour_z<- here_read("tab_compare_church_prosocial_behaviour_z")
#
#
# group_tab_compare_church_prosocial_behaviour_z<- here_read("group_tab_compare_church_prosocial_behaviour_z")
# group_tab_compare_church_prosocial_behaviour_raw<- here_read("group_tab_compare_church_prosocial_behaviour_raw")
#
# #Loss
# tab_compare_church_prosocial_behaviour_raw<- here_read("tab_compare_church_prosocial_behaviour_raw")
# tab_compare_church_prosocial_behaviour_z_loss<- here_read("tab_compare_church_prosocial_behaviour_z_loss")
#
# group_tab_compare_church_prosocial_behaviour_z_loss<- here_read("group_tab_compare_church_prosocial_behaviour_z_loss")
# group_tab_compare_church_prosocial_behaviour_loss_raw <- group_tab(tab_compare_church_prosocial_behaviour_loss_raw, type = "RD")
#


#### GAIN CHURCH PROSOCIAL

# for reference
tab_compare_church_prosocial_behaviour_raw<- here_read("tab_compare_church_prosocial_behaviour_raw")
group_tab_compare_church_prosocial_behaviour_raw<- here_read("group_tab_compare_church_prosocial_behaviour_raw")

tab_compare_church_prosocial_behaviour_raw
group_tab_compare_church_prosocial_behaviour_raw

tab_compare_church_prosocial_behaviour_z<- here_read("tab_compare_church_prosocial_behaviour_z")
group_tab_compare_church_prosocial_behaviour_z<- here_read("group_tab_compare_church_prosocial_behaviour_z")

tab_compare_church_prosocial_behaviour_z
group_tab_compare_church_prosocial_behaviour_z


plot_gain_church_prosocial_z <- margot_plot(
  group_tab_compare_church_prosocial_behaviour_z,
  type = "RD",
  title = "Religious service effect on reported donations and volunteering",
  subtitle = ">= 1 x weekly religious service attendance",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_gain_church_prosocial_z

ggsave(
  plot_gain_church_prosocial_z,
  path = path_talk,
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_gain_church_prosocial_z.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



### LOSS CHURCH PROSOCIAL

tab_compare_church_prosocial_behaviour_loss_raw<- here_read("tab_compare_church_prosocial_behaviour_loss_raw")
group_tab_compare_church_prosocial_behaviour_loss_raw<- here_read("group_tab_compare_church_prosocial_behaviour_loss_raw")

tab_compare_church_prosocial_behaviour_loss_raw
group_tab_compare_church_prosocial_behaviour_loss_raw

tab_compare_church_prosocial_behaviour_z_loss<- here_read("tab_compare_church_prosocial_behaviour_z_loss")
group_tab_compare_church_prosocial_behaviour_z_loss<- here_read("group_tab_compare_church_prosocial_behaviour_z_loss")
tab_compare_church_prosocial_behaviour_z_loss
group_tab_compare_church_prosocial_behaviour_z_loss

plot_loss_church_prosocial_z <- margot_plot(
  group_tab_compare_church_prosocial_behaviour_z_loss,
  type = "RD",
  title = "Religious service effect on reported donations and volunteering",
  subtitle = "Lose any religious service",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)



# combo graph
plot_gain_church_prosocial_z / plot_loss_church_prosocial_z + plot_annotation(tag_levels = "A")

#### GAIN CHURCH PREJUDICE

tab_warm_church <- here_read("tab_warm_church")
group_tab_warm_church <- here_read("group_tab_warm_church")

tab_warm_church
group_tab_warm_church

plot_prejudice_church <- margot_plot(
  group_tab_warm_church,
  type = "RD",
  title = "Religious service effect on prejudice/acceptance",
  subtitle = ">= 1 x weekly service attendance",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_prejudice_church



### LOSS CHURCH PREJUDICE

tab_warm_church_loss <- here_read("tab_warm_church_loss")
group_tab_warm_church_loss <- here_read("group_tab_warm_church_loss")
tab_warm_church_loss
group_tab_warm_church_loss

plot_prejudice_church_loss <- margot_plot(
  group_tab_warm_church_loss,
  type = "RD",
  title = "Religious service effect on prejudice/acceptance",
  subtitle = "Lose any religious service",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)


# combo graph
plot_prejudice_church / plot_prejudice_church_loss + plot_annotation(tag_levels = "A")
#
# ggsave(
#   plot_prejudice_church_loss,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 8,
#   height = 6,
#   units = "in",
#   filename ="plot_prejudice_church_loss.png",
#   device = 'png',
#   limitsize = FALSE,
#   dpi = 600
# )


#### GAIN CHURCH HELP RECEIVED

tab_church_help_received <- here_read('tab_church_help_received')
group_tab_church_help_received <- here_read("group_tab_church_help_received")
tab_church_help_received
group_tab_church_help_received

# graph
plot_church_help_received <- margot_plot(
  group_tab_church_help_received,
  type = "RR",
  title = "Religious service effect on help received",
  subtitle = ">= 1 x weekly service attendance",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = 0,
  x_lim_lo = 0,
  x_lim_hi =  2
)

plot_church_help_received




ggsave(
  plot_group_tab_time_church,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_church_help_received.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)

### LOSS CHURCH HELP RECEIVED
tab_church_help_received_loss <- here_read('tab_church_help_received_loss')
group_tab_church_help_received_loss <- here_read("group_tab_church_help_received_loss")
group_tab_church_help_received_loss
tab_church_help_received_loss

plot_group_tab_time_church_loss <- margot_plot(
  group_tab_church_help_received_loss,
  type = "RR",
  title = "Religious service effect on help received",
  subtitle = "Lose any religious service",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = 0,
  x_lim_lo = 0,
  x_lim_hi =  2
)

plot_group_tab_time_church / plot_group_tab_time_church_loss + plot_annotation(tag_levels = "A")

#
# ggsave(
#   plot_group_tab_time_church_loss,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 8,
#   height = 6,
#   units = "in",
#   filename = "plot_group_tab_time_church_loss.png",
#   device = 'png',
#   limitsize = FALSE,
#   dpi = 600
# )


### SOCIALISING GAIN PROSOCIAL
tab_socializing_prosocial_behaviour_z<- here_read("tab_socializing_prosocial_behaviour_z")
group_tab_socializing_prosocial_behaviour_z<- here_read("group_tab_socializing_prosocial_behaviour_z")
tab_socializing_prosocial_behaviour_z
group_tab_socializing_prosocial_behaviour_z

plot_socializing_prosocial<- margot_plot(
  group_tab_socializing_prosocial_behaviour_z,
  type = "RD",
  title = "Socialising effect on charity",
  subtitle = ">= 1.4  x weekly hours socialising",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_socializing_prosocial


ggsave(
  plot_socializing_prosocial,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_socializing_prosocial.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 400
)



### LOSS SOCIALISING RAW
tab_socializing_prosocial_behaviour_loss_raw<- here_read("tab_socializing_prosocial_behaviour_loss_raw")
group_tab_socializing_prosocial_behaviour_loss_raw<- here_read("group_tab_socializing_prosocial_behaviour_loss_raw")
tab_socializing_prosocial_behaviour_loss_raw

### LOSS SOCIALISING  prosocial Z

tab_socializing_prosocial_behaviour_loss_z<- here_read("tab_socializing_prosocial_behaviour_loss_z")
group_tab_socializing_prosocial_behaviour_loss_z<- here_read("group_tab_socializing_prosocial_behaviour_loss_z")
group_tab_socializing_prosocial_behaviour_loss_z
plot_socializing_prosocial_loss_z<- margot_plot(
  group_tab_socializing_prosocial_behaviour_loss_z,
  type = "RD",
  title = "Socialising effect on charity",
  subtitle = "any community socialising lost",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_socializing_prosocial/ plot_socializing_prosocial_loss_z +  plot_annotation(tag_levels = "A")



######################
### SOCIALISING
######################

## SELF REPORTED PROSOCIAL

tab_socializing_prosocial_behaviour_raw<- here_read("tab_socializing_prosocial_behaviour_raw")


group_tab_socializing_prosocial_behaviour_raw<- here_read("group_tab_socializing_prosocial_behaviour_raw")

tab_socializing_prosocial_behaviour_z<- here_read("tab_socializing_prosocial_behaviour_z")
group_tab_socializing_prosocial_behaviour_z<- here_read("group_tab_socializing_prosocial_behaviour_z")


plot_socializing_prosocial<- margot_plot(
  group_tab_socializing_prosocial_behaviour_z,
  type = "RD",
  title = "Socialising effect on charity",
  subtitle = ">= 1.4  x weekly hours socialising",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_socializing_prosocial




## Prejudice
tab_socialising_prejudice_z  <- here_read("tab_warm_socialising")
group_table_socialising_prejudice_z  <- here_read("group_tab_warm_socialising")

plot_warm_socialising <- margot_plot(
  group_table_socialising_prejudice_z,
  type = "RD",
  title = "Socialing effect on prejudice/acceptance",
  subtitle = ">= 1.4  x weekly hours socialising",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_warm_socialising


ggsave(
  plot_warm_socialising,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_warm_socialising.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)


tab_warm_socialising_loss<- here_read("tab_warm_socialising_loss")
group_tab_warm_socialising_loss<- here_read("group_tab_warm_socialising_loss")

plot_warm_socialising_loss <- margot_plot(
  group_tab_warm_socialising_loss,
  type = "RD",
  title = "Socialing effect on prejudice/acceptance",
  subtitle = "Any socialising lost",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_warm_socialising_loss
#
# ggsave(
#   plot_warm_socialising_loss,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 8,
#   height = 6,
#   units = "in",
#   filename = "plot_warm_socialising_loss.png",
#   device = 'png',
#   limitsize = FALSE,
#   dpi = 600
# )

### LOSS SOCIALISING PREJUDICE
tab_warm_socialising_loss<- here_read("tab_warm_socialising_loss")
group_tab_warm_socialising_loss<- here_read("group_tab_warm_socialising_loss")
plot_warm_socialising_loss <- margot_plot(
  group_tab_warm_socialising_loss,
  type = "RD",
  title = "Socialing effect on prejudice/acceptance",
  subtitle = "Any socialising lost",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_warm_socialising_loss

# ggsave(
#   plot_warm_socialising_loss,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 8,
#   height = 6,
#   units = "in",
#   filename = "plot_warm_socialising_loss.png",
#   device = 'png',
#   limitsize = FALSE,
#   dpi = 600
# )


plot_warm_socialising +  plot_warm_socialising_loss /  plot_annotation(tag_levels = "A")



# GAIN SOCIALISING HELP RECEIVED
tab_socialising_help_received <- here_read("tab_socialising_help_received")
group_tab_socialising_help_received <- here_read("group_tab_socialising_help_received")

plot_socialising_gain_help_received<- margot_plot(
  group_tab_socialising_help_received,
  type = "RR",
  title = "Socialising effect on help recieved",
  subtitle = ">= 1.4  x weekly hours socialising",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = 0,
  x_lim_lo = 0,
  x_lim_hi =  2
)

plot_socialising_gain_help_received

ggsave(
  plot_socialising_gain_help_received,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_socialising_gain_help_received.png",
  device = 'png',
  limitsize = TRUE,
  dpi = 400
)



# ggsave(
#   plot_socialising_gain_help_received,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 8,
#   height = 6,
#   units = "in",
#   filename = "plot_socialising_gain_help_received.png",
#   device = 'png',
#   limitsize = FALSE,
#   dpi = 600
# )


## LOSS SOCIALISING HELP RECIEVED


tab_socialising_help_received_loss<- here_read("tab_socialising_help_received_loss")
group_tab_socialising_help_received_loss <- here_read("group_tab_socialising_help_received_loss")

plot_socialising_loss_help_received <- margot_plot(
  group_tab_socialising_help_received_loss,
  type = "RR",
  title = "Socialising effect on help recieved",
  subtitle = "any community socialising lost",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = 0,
  x_lim_lo = 0,
  x_lim_hi =  2
)

plot_socialising_loss_help_received

############### STRONG INTERVENTIONS #######################


##############################################
### STRONG CONTRASTS  CHURCH ################
################################################

tab_strong_church_gain_prosocial<- here_read("tab_strong_church_gain_prosocial")
group_tab_strong_church_gain_prosocial<- here_read("group_tab_strong_church_gain_prosocial")

plot_gain_church_prosocial_strong <- margot_plot(
  group_tab_strong_church_gain_prosocial,
  type = "RD",
  title = "Religious service effect on reported donations and volunteering",
  subtitle = "Weekly vs. No Religious Service",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_gain_church_prosocial_strong

## WARM
tab_warm_church_strong<- here_read("tab_warm_church_strong")
group_tab_warm_church_strong <- here_read("group_tab_warm_church_strong")


plot_prejudice_church_strong <- margot_plot(
  group_tab_warm_church_strong,
  type = "RD",
  title = "Religious service effect on prejudice/acceptance",
  subtitle = "Weekly vs. None",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_prejudice_church_strong

ggsave(
  plot_prejudice_church_strong,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 6,
  units = "in",
  filename ="plot_prejudice_church_strong.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
)



# graph strong church help received
tab_church_help_received_strong <- here_read('tab_church_help_received_strong')
group_tab_church_help_received_strong <- here_read("group_tab_church_help_received_strong")


plot_group_tab_time_church_strong <- margot_plot(
  group_tab_church_help_received_strong,
  type = "RR",
  title = "Religious service effect on help received_strong",
  subtitle = "Weekly vs. None",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = 0,
  x_lim_lo = 0,
  x_lim_hi =  2
)

plot_group_tab_time_church_strong


#  SOCIALISING PROSOCIAL STRONG

# for reference
tab_socializing_prosocial_behaviour_loss_raw_strong<- here_read("tab_socializing_prosocial_behaviour_loss_raw_strong")
group_tab_socializing_prosocial_behaviour_loss_raw_strong<- here_read("group_tab_socializing_prosocial_behaviour_loss_raw_strong")

# view
tab_socializing_prosocial_behaviour_loss_raw_strong
group_tab_socializing_prosocial_behaviour_loss_raw_strong

tab_socializing_prosocial_behaviour_loss_z_strong<- here_read("tab_socializing_prosocial_behaviour_loss_z_strong")
group_tab_socializing_prosocial_behaviour_loss_z_strong<- here_read("group_tab_socializing_prosocial_behaviour_loss_z_strong")
group_tab_socializing_prosocial_behaviour_loss_z_strong
plot_group_tab_socializing_prosocial_behaviour_loss_z_strong<- margot_plot(
  group_tab_socializing_prosocial_behaviour_loss_z_strong,
  type = "RD",
  title = "Socialising effect on charity",
  subtitle = ">= 1.4 Weekly Hours vs None",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_group_tab_socializing_prosocial_behaviour_loss_z_strong



### Socialising and Warmth

tab_warm_socialising_strong<- here_read("tab_warm_socialising_strong")
group_tab_warm_warm_socialising_strong <- here_read("group_tab_warm_socialising_strong")
tab_warm_socialising_strong
group_tab_warm_warm_socialising_strong
plot_warm_socialising_strong <- margot_plot(
  group_tab_warm_warm_socialising_strong,
  type = "RD",
  title = "Socialing effect on prejudice/acceptance",
  subtitle = ">= 1.4 weekly hours socialising vs None",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)

plot_warm_socialising_strong

# ggsave(
#   plot_warm_socialising_strong,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 8,
#   height = 6,
#   units = "in",
#   filename = "plot_warm_socialising_strong.png",
#   device = 'png',
#   limitsize = FALSE,
#   dpi = 600
# )

## Socialising and Friends

tab_socialising_help_received_strong <- here_read("tab_socialising_help_received_strong")
group_tab_socialising_help_received_strong <- here_read("group_tab_socialising_help_received_strong")

plot_help_time_strong <- margot_plot(
  group_tab_socialising_help_received_strong,
  type = "RR",
  title = "Socialising effect on help recieved",
  subtitle = ">= 1.4 weekly hours socialising vs None",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = 0,
  x_lim_lo = 0,
  x_lim_hi =  2
)

plot_help_time_strong
# ggsave(
#   plot_help_time_strong,
#   path = here::here(here::here(push_mods, "figs")),
#   width = 8,
#   height = 6,
#   units = "in",
#   filename = "plot_help_time_strong.png",
#   device = 'png',
#   limitsize = FALSE,
#   dpi = 600
# )



# GRAPHS ------------------------------------------------------------------





n_participants
group_tab_health
title_church = "Weekly Religious Service N = 32058"
# set title
subtitle_health = "Prejudice/Acceptance"


## loss and gain combo on volunteering

plot_loss_gain_volunteering <- plot_gain_church_prosocial_z / plot_loss_church_prosocial_z + plot_annotation(tag_levels = "A")
plot_loss_gain_volunteering
#
ggsave(
  plot_loss_gain_volunteering,
  path = path_talk,
  width = 12,
  height = 8,
  units = "in",
  filename = "plot_loss_gain_volunteering.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
 )



plot_church_prejudice<- margot_plot(
  group_tab_warm_church,
  type = "RD",
  title = "Religious service effect on minority-group attitudes",
  subtitle = ">= 1 x weekly religious service attendance",
  xlab = "",
  ylab = "",
  estimate_scale = 1,
  base_size = 8,
  text_size = 2.5,
  point_size = .5,
  title_size = 12,
  subtitle_size = 11,
  legend_text_size = 8,
  legend_title_size = 10,
  x_offset = -1,
  x_lim_lo = -1,
  x_lim_hi =  .5
)


#check
plot_church_prejudice
#
ggsave(
  plot_church_prejudice,
  path = path_talk,
  width = 16,
  height = 9,
  units = "in",
  filename = "plot_church_prejudice.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 400
 )



## Dollar value


#read estimates of unstandardised models
m_charity_donate_raw <- here_read("m_charity_donate")
m_charity_donate_null_raw <- here_read("m_charity_donate_null")

# obtain contrast
contrast_donate_full_raw <- lmtp_contrast(m_charity_donate_raw,
                                          ref = m_charity_donate_null_raw,
                                          type = "additive")

# contrast
contrast_donate_full_raw


# counterfactual giving
counterfactual_charity_donate <- contrast_donate_full_raw$vals$shift

# actual giving
estimate_charity_donate <- contrast_donate_full_raw$vals$ref

# nz_adult_population in 2021
nz_adult_population = 3989000


factual_charity_estimate = nz_adult_population * estimate_charity_donate
counterfactual_factual_charity_estimate = nz_adult_population * counterfactual_charity_donate

# difference
counterfactual_difference_charity_donate = counterfactual_factual_charity_estimate - factual_charity_estimate

counterfactual_difference_charity_donate

# almost 3 billion NZD
# 2,806,882,916

# nz annual budget in 2021
 nz_annual_budget = 14494000000 * 4
# nz_annual_budget
#
 counterfactual_difference_charity_donate/ nz_annual_budget
#
# counterfactual_difference_charity_donate
#
# options(scipen=999)
# nz_annual_budget
#
# #
# 0.04841457
#
# counterfactual giving
counterfactual_charity_donate <- contrast_donate_full_raw$vals$shift

# actual giving
estimate_charity_donate <- contrast_donate_full_raw$vals$ref

# nz_adult_population in 2021
nz_adult_population = 3989000


factual_charity_estimate = nz_adult_population * estimate_charity_donate
counterfactual_factual_charity_estimate = nz_adult_population * counterfactual_charity_donate

# difference
counterfactual_difference_charity_donate = counterfactual_factual_charity_estimate - factual_charity_estimate

counterfactual_difference_charity_donate

# almost 3 billion NZD
# 2,806,882,916

# nz annual budget in 2021
# nz_annual_budget = 14494000000 * 4
# nz_annual_budget
#
# counterfactual_difference_charity_donate/ nz_annual_budget
#
# counterfactual_difference_charity_donate
#
# options(scipen=999)
# nz_annual_budget
#
# #
# 0.04841457
#



