# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")

# Pre-registered hypothesis H6: If respondents are exposed to the vignette evaluation task before (vs. after) they get to answer questions about governance of speech, they are more willing to have speech restricted, and see more responsibility for action against hate speech in the hands of online services, lawmakers, the justice system, and employers.


# load data ---------------------------

load("data/cooked/data_survey_combined.RData")
load("data/cooked/data_survey_resp.RData")


# balance table of respondent covariates by treatment status -----------

dat_summary <- ungroup(data_survey_resp) %>% dplyr::select(all_of(c(resp_demographics_covars, "prefs_after_vignette")))
names(dat_summary) <- map_chr(dat_summary, var_label)

fmla <- "~`Vignette_task_placement`"

"Vignette task placement" %in% names(dat_summary)
names(dat_summary)[names(dat_summary) == "Vignette task placement"] <- "Vignette_task_placement"

datasummary_balance(as.formula(fmla),
                    data = dat_summary, 
                    fmt = function(x) sprintf("%.1f%%", 100 * x), 
                    title = "Descriptive statistics of respondent characteristics, by treatment group status (exposure experiment)\\label{tab:balance-table-exposure-experiment}",
                    stars = TRUE,
                    escape = FALSE,
                    output = "figures/balance-table-exposure-experiment.tex")



# Exposure experiment -----------------------

## From the PRP:
# The hypothesis on the question order experiment (H6) will be tested by regressing the outcomes on the governance of speech preference questions (a) only on the pre/post treatment indicator (sparse model) and (b) on the pre/post treatment indicator and a covariate-adjusted model using Lin’s (2013) saturated regression approach including all pre-treatment covariates.


# OLS with and without respondent characteristics -----------------

# prepare covariates
resp_covars <- filter(resp_covars_df, regmodel == TRUE) %>% pull(resp_variable)

# prepare outcome variables

data_survey_resp$tradeoffs_speech_num[data_survey_resp$tradeoffs_speech == "Speak freely"] <- 1
data_survey_resp$tradeoffs_speech_num[data_survey_resp$tradeoffs_speech == "Welcome and safe"] <- 0
data_survey_resp$tradeoffs_platforms_num[data_survey_resp$tradeoffs_platforms == "Not responsible"] <- 1
data_survey_resp$tradeoffs_platforms_num[data_survey_resp$tradeoffs_platforms == "Responsible"] <- 0
data_survey_resp$tradeoffs_govments_num[data_survey_resp$tradeoffs_govments == "Speak freely"] <- 1
data_survey_resp$tradeoffs_govments_num[data_survey_resp$tradeoffs_govments == "Prevent hate"] <- 0

data_survey_resp <- data_survey_resp %>% 
  mutate(respsblty_platforms_num = case_when(
    respsblty_platforms == "No responsibility at all" ~ 0,
    respsblty_platforms == "Rather no responsibility" ~ 1,
    respsblty_platforms == "Some responsibility" ~ 2,
    respsblty_platforms == "Full responsibility" ~ 3,
    TRUE ~ NA_real_
  )) %>%
  mutate(respsblty_victims_num = case_when(
    respsblty_victims == "No responsibility at all" ~ 0,
    respsblty_victims == "Rather no responsibility" ~ 1,
    respsblty_victims == "Some responsibility" ~ 2,
    respsblty_victims == "Full responsibility" ~ 3,
    TRUE ~ NA_real_
  )) %>%
  mutate(respsblty_witnesses_num = case_when(
    respsblty_witnesses == "No responsibility at all" ~ 0,
    respsblty_witnesses == "Rather no responsibility" ~ 1,
    respsblty_witnesses == "Some responsibility" ~ 2,
    respsblty_witnesses == "Full responsibility" ~ 3,
    TRUE ~ NA_real_
  )) %>%
  mutate(respsblty_lawmakers_num = case_when(
    respsblty_lawmakers == "No responsibility at all" ~ 0,
    respsblty_lawmakers == "Rather no responsibility" ~ 1,
    respsblty_lawmakers == "Some responsibility" ~ 2,
    respsblty_lawmakers == "Full responsibility" ~ 3,
    TRUE ~ NA_real_
  )) %>%
  mutate(respsblty_justice_num = case_when(
    respsblty_justice == "No responsibility at all" ~ 0,
    respsblty_justice == "Rather no responsibility" ~ 1,
    respsblty_justice == "Some responsibility" ~ 2,
    respsblty_justice == "Full responsibility" ~ 3,
    TRUE ~ NA_real_
  )) %>%
  mutate(respsblty_employers_num = case_when(
    respsblty_employers == "No responsibility at all" ~ 0,
    respsblty_employers == "Rather no responsibility" ~ 1,
    respsblty_employers == "Some responsibility" ~ 2,
    respsblty_employers == "Full responsibility" ~ 3,
    TRUE ~ NA_real_
  ))
    
exposure_items_vec <- c("tradeoffs_speech_num", "tradeoffs_platforms_num", "tradeoffs_govments_num",
                        "respsblty_platforms_num", "respsblty_victims_num", "respsblty_witnesses_num", 
                        "respsblty_lawmakers_num", "respsblty_justice_num", "respsblty_employers_num")

# build formulas

fmlas <- paste0(exposure_items_vec, 
                " ~ prefs_after_vignette") %>% map(as.formula)

fmla_covars <- paste0("~", paste0(resp_covars, collapse = " + ")) %>% as.formula()

# run models, pooled

exposure_models_pooled <- map(fmlas, lm_robust, data = data_survey_resp)
exposure_models_pooled_covars <- map(fmlas, lm_lin, covariates = fmla_covars, data = data_survey_resp)

# run models, by country

exposure_models_country <- list()
exposure_models_country_covars <- list()

for (i in country_codes_chr) {
  exposure_models_country[[i]] <- map(fmlas, lm_robust, data = filter(data_survey_resp, resp_country2 == i))
  exposure_models_country_covars[[i]] <- map(fmlas, lm_lin, covariates = fmla_covars, data = filter(data_survey_resp, resp_country2 == i))
}

# extract estimates

exposure_models_out_pooled <- bind_rows(
  map_df(exposure_models_pooled, tidy_extend3, covars_filter = "prefs_after_vignette", covars = "none", data = "pooled"),
  map_df(exposure_models_pooled_covars, tidy_extend3, covars_filter = "prefs_after_vignette", covars = "lin", data = "pooled")
)

exposure_models_out_country_list <- list()
for (i in country_codes_chr) {
  exposure_models_out_country_list[[i]] <- bind_rows(
    map_df(exposure_models_country[[i]], tidy_extend3, covars_filter = "prefs_after_vignette", covars = "none", data = i),
    map_df(exposure_models_country_covars[[i]], tidy_extend3, covars_filter = "prefs_after_vignette", covars = "lin", data = i)
  )
}
exposure_models_out_country <- bind_rows(exposure_models_out_country_list)
exposure_models_out <- bind_rows(exposure_models_out_pooled, exposure_models_out_country)

# add labels, colors

dat <- exposure_models_out %>% 
  left_join(resp_covars_df, by = c("outcome" = "resp_variable")) %>%
  left_join(country_codes_df, by = c("data" = "code")) %>%
  # Outcome category
  mutate(resp_category = ifelse(str_detect(dat$resp_varlabel, "Tradeoffs"), "Trade-offs", "Responsibility")) %>%
  # Setting color by sample
  mutate(color_custom = ifelse(country == "Pooled", "black", "darkgrey"),
         alpha_custom = ifelse(country == "Pooled", 1, .95)) %>% 
  # Adjusting label width
  mutate(resp_varlabel_ref = str_wrap(dat$resp_varlabel_ref, width = 25)) %>%  
  # Compute adjusted p.value for multiple comparisons
  group_by(country) %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"),
         p_adj_below_05 = p_adj < 0.05) %>%
  ungroup() %>%
  mutate(point_shape = ifelse(p_adj_below_05, 19, 21))

dodge = .8

# plot results

ggplot(filter(dat, covars == "none"), aes(x = resp_varlabel_ref, y = estimate, group = estimate, color = color_custom, alpha = alpha_custom)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = resp_varlabel_ref, 
                     ymin = conf.low,
                     ymax = conf.high),
                 lwd = 1/2,
                 position = position_dodge2(width = dodge),
                 show.legend = FALSE) +
  geom_point(aes(x = resp_varlabel_ref, 
                 y = estimate,
                 shape = point_shape),
             fill = "white",
             position = position_dodge2(width = dodge),
             show.legend = FALSE) + 
  geom_text(aes(label = country, x = resp_varlabel_ref, y = conf.high),
            hjust = -0.2,  # Push labels slightly to the right
            size = 2, 
            position = position_dodge2(width = dodge),
            show.legend = FALSE) +
  coord_flip() + 
  facet_grid2(resp_category ~ ., 
              scales = "free_y", 
              space = "free_y") + 
scale_color_manual(values = c("black" = "black", "gray" = "gray")) +  # Custom colors
  scale_alpha_identity() +  # Directly use alpha values
  scale_y_continuous(breaks = seq(-1, 1, .1), labels = seq(-1, 1, .1), expand = expansion(mult = c(0.1, 0.2))) +  # Increase right-side space
  scale_x_discrete() + 
  scale_shape_identity() + 
  theme_bw() + 
  ylab("Estimated effect of hate speech vignettes exposure") + xlab("") + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position= "bottom",
        legend.title=element_text(size = 9),
        legend.margin=margin(-10,0,0,0),
        legend.spacing.x = unit(.25, 'cm'),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line("grey", size = 0.1),
        plot.margin=unit(c(0.1,0.1,0.5,0.1),"cm"))
ggsave("figures/effects-exposure-none.png", width = 6, height = 9, dpi = 300)


# plot results

ggplot(filter(dat, covars == "lin"), aes(x = resp_varlabel_ref, y = estimate, group = estimate, color = color_custom, alpha = alpha_custom)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = resp_varlabel_ref, 
                     ymin = conf.low,
                     ymax = conf.high),
                 lwd = 1/2,
                 position = position_dodge2(width = dodge),
                 show.legend = FALSE) +
  geom_point(aes(x = resp_varlabel_ref, 
                 y = estimate,
                 shape = point_shape),
             fill = "white",
             position = position_dodge2(width = dodge),
             show.legend = FALSE) + 
  geom_text(aes(label = country, x = resp_varlabel_ref, y = conf.high),
            hjust = -0.2,  # Push labels slightly to the right
            size = 2, 
            position = position_dodge2(width = dodge),
            show.legend = FALSE) +
  coord_flip() + 
  facet_grid2(resp_category ~ ., 
              scales = "free_y", 
              space = "free_y") + 
  scale_color_manual(values = c("black" = "black", "gray" = "gray")) +  # Custom colors
  scale_alpha_identity() +  # Directly use alpha values
  scale_y_continuous(breaks = seq(-1, 1, .1), labels = seq(-1, 1, .1), expand = expansion(mult = c(0.1, 0.2))) +  # Increase right-side space
  scale_x_discrete() + 
  scale_shape_identity() + 
  theme_bw() + 
  ylab("Estimated effect of hate speech vignettes exposure") + xlab("") + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position= "bottom",
        legend.title=element_text(size = 9),
        legend.margin=margin(-10,0,0,0),
        legend.spacing.x = unit(.25, 'cm'),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line("grey", size = 0.1),
        plot.margin=unit(c(0.1,0.1,0.5,0.1),"cm"))
ggsave("figures/effects-exposure-covars.png", width = 6, height = 9, dpi = 300)







