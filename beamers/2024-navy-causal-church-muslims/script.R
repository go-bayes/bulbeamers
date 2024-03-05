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

library(dplyr)
library(lubridate)
library(ggplot2)

# Assuming 'dat' is your original dataset and has a date column or a way to create it.
# First, ensure your data has a 'day' column that correctly represents dates.

# For demonstration, let's assume 'wave' can be used directly as 'year',
# and there's some way to determine 'day'. You might need to adjust this part.
rarep_all <- dat %>%
  filter(wave %in% 2009:2023) %>%
  filter(year_measured == 1) %>%
  mutate(year = as.numeric(as.character(wave))) %>%
  # Assuming 'day' needs to be created or is already present. Add that step here if necessary.
  # For now, let's pretend 'day' is directly usable.
  # Your data preparation steps here...
  summarise(n = n(), .groups = 'drop') # Count the number of rows in each group


# Now 'rarep_all' includes a column 'n' with counts.

# Plotting
lds_all <- ggplot(rarep_all, aes(x = day, y = n, fill = condition)) +
  geom_col() +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "1 year", limits = c(as.Date("2012-09-10"), as.Date("2022-10-16"))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  geom_vline(xintercept = as.numeric(as.Date("2019-03-15")), col = "red", linetype = "dashed") +
  labs(title = "New Zealand Attitudes and Values Study (panel)", subtitle = "N = 67,409; years 2012-2021", x = "NZAVS years 2012- 2022 cohort (N = 67,409): daily counts by condition", y = "Count of Responses") +
  theme_classic() +
  scale_fill_okabe_ito() +
  theme(legend.position = "top", legend.text = element_text(size = 12), legend.title = element_text(size = 12))

print(lds_all)



library(dplyr)
library(lubridate)
library(ggplot2)


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


length(unique(tl$id))



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

n_unique(tl$id)

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


t2 <- tl |>  filter(sample_frame_opt_in!=1)

t2

library(here)
library(skimr)
n_unique(t2$id)

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
plot_group_tab_time_church <- margot_plot(
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

plot_group_tab_time_church


ggsave(
  plot_group_tab_time_church,
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 6,
  units = "in",
  filename = "plot_group_tab_time_church.png",
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
  path = here::here(here::here(push_mods, "figs")),
  width = 8,
  height = 6,
  units = "in",
  filename = "plot_socializing_prosocial.png",
  device = 'png',
  limitsize = FALSE,
  dpi = 600
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
