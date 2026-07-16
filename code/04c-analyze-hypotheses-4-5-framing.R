# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")

# Pre-registered H4: If the vignette evaluation is framed as a moderation task for a platform that promotes free speech and takes a critical stance against any form of censorship (vs. a neutral frame), respondents are less likely to prefer platform action against the message sender but are as likely to perceive messages as being hate speech and offensive speech, and to prefer legal action against the message/sender.

# Pre-registered H5: If the vignette evaluation is framed as a moderation task for a platform that vows to protect vulnerable users from hate speech and takes an aggressive stance against hateful content on their pages (vs. a neutral frame), respondents are more likely to prefer platform action against the message sender but are as likely to perceive messages as being hate speech and offensive speech, and to prefer legal action against the message/sender.


# load data ---------------------------

load("data/cooked/data_survey_combined.RData")
load("data/cooked/data_survey_resp.RData")


# balance table of respondent covariates by treatment status -----------

dat_summary <- ungroup(data_survey_resp) %>% dplyr::select(all_of(c(resp_demographics_covars, "frame_vig")))
names(dat_summary) <- map_chr(dat_summary, var_label)

fmla <- "~`Vignette_frame`"

"Vignette frame" %in% names(dat_summary)
names(dat_summary)[names(dat_summary) == "Vignette frame"] <- "Vignette_frame"

datasummary_balance(as.formula(fmla),
                    data = dat_summary, 
                    fmt = function(x) sprintf("%.1f%%", 100 * x), 
                    title = "Descriptive statistics of respondent characteristics, by treatment group status (framing experiment)\\label{tab:balance-table-framing-experiment}",
                    stars = TRUE,
                    escape = FALSE,
                    output = "results/balance-table-framing-experiment.tex")


# subset data for various robustness checks -----------

# only with respondents who confirmed "I have read and understood those instructions"
data_survey_check1 <- filter(data_survey_combined, frame_vig_na == 0)
# only with respondents who passed frame manipulation check
data_survey_check2 <- filter(data_survey_combined, manipcheck_frame_passed == TRUE)
# only with respondents who passed behavioral attention check (t_vig_intro_Page_Submit > 10)
data_survey_check3 <- filter(data_survey_combined, attcheck_passed == TRUE)
# only with respondents who passed behavioral attention check (t_vig_intro_Page_Submit > 25)
data_survey_check4 <- filter(data_survey_combined, t_vig_intro_Page_Submit > 25)

# check labels for figure
check_df <- data.frame(data = c("none", "check1", "check2", "check3", "check4"),
                       check_lab = c("None", "Read instructions", "**Manip. check**", "Att. check 10s", "Att. check 25s"))


# estimate models for framing experiment (H4/H5) -----------------------

# generate model formulas

fmlas <- paste0(outcome_vars, 
                " ~ 1 + frame_vig + ", 
                paste0(vig_covars_df_main$vig_attribute_varnames, collapse = " + "), " + ",
                "(1|resp_id) + (1|deck_id)")

# run mixed-effects models, pooled

frame_models_pooled <- map(paste(fmlas, "+ (1|resp_country2)"), lmer, data = data_survey_combined)
frame_models_pooled_check1 <- map(paste(fmlas, "+ (1|resp_country2)"), lmer, data = data_survey_check1)
frame_models_pooled_check2 <- map(paste(fmlas, "+ (1|resp_country2)"), lmer, data = data_survey_check2)
frame_models_pooled_check3 <- map(paste(fmlas, "+ (1|resp_country2)"), lmer, data = data_survey_check3)
frame_models_pooled_check4 <- map(paste(fmlas, "+ (1|resp_country2)"), lmer, data = data_survey_check4)

# run mixed-effects models, by country

frame_models_country <- list()
frame_models_country_check1 <- list()
frame_models_country_check2 <- list()
frame_models_country_check3 <- list()
frame_models_country_check4 <- list()

for (i in country_codes_chr) {
  frame_models_country[[i]] <- map(fmlas, lmer, data = filter(data_survey_combined, resp_country2 == i))
  frame_models_country_check1[[i]] <- map(fmlas, lmer, data = filter(data_survey_check1, resp_country2 == i))
  frame_models_country_check2[[i]] <- map(fmlas, lmer, data = filter(data_survey_check2, resp_country2 == i))
  frame_models_country_check3[[i]] <- map(fmlas, lmer, data = filter(data_survey_check3, resp_country2 == i))
  frame_models_country_check4[[i]] <- map(fmlas, lmer, data = filter(data_survey_check4, resp_country2 == i))
}

# extract estimates, pooled models

frame_models_pooled_out <- 
  bind_rows(
    map_df(frame_models_pooled, tidy_extend2, data = "pooled", covars = "none"),
    map_df(frame_models_pooled_check1, tidy_extend2, data = "pooled", covars = "check1"),
    map_df(frame_models_pooled_check2, tidy_extend2, data = "pooled", covars = "check2"),
    map_df(frame_models_pooled_check3, tidy_extend2, data = "pooled", covars = "check3"),
    map_df(frame_models_pooled_check4, tidy_extend2, data = "pooled", covars = "check4")
  ) 


# extract estimates, by country

frame_models_country_list <- list()
for(i in country_codes_chr) {
  frame_models_country_list[[i]] <- 
    bind_rows(
      map_df(frame_models_country[[i]], tidy_extend2, data = i, covars = "none"),
      map_df(frame_models_country_check1[[i]], tidy_extend2, data = i, covars = "check1"),
      map_df(frame_models_country_check2[[i]], tidy_extend2, data = i, covars = "check2"),
      map_df(frame_models_country_check3[[i]], tidy_extend2, data = i, covars = "check3"),
      map_df(frame_models_country_check4[[i]], tidy_extend2, data = i, covars = "check4")
    ) 
}
frame_models_country_out <- bind_rows(frame_models_country_list)



# prepare labels 

dat_pooled <- 
frame_models_pooled_out %>%
  filter(variable == "frame_vig", reference_row == FALSE) %>%
  left_join(vig_outcomes_df, by = c("response" = "outcome_vars")) %>%
  left_join(country_codes_df, by = c("data" = "code")) %>%
  mutate(outcome_label = factor(outcome_vars_labels2, levels = vig_outcomes_df$outcome_vars_labels2)) %>%
  mutate(p.value = 2 * (1 - pnorm(abs(estimate / std.error)))) %>% 
  group_by(label) %>% 
  mutate(p_adj = p.adjust(p.value, method = "BH"),
         p_adj_below_05 = p_adj < 0.01) %>%
  ungroup() %>%
  mutate(point_shape = ifelse(p_adj_below_05, 19, 21))

dat_country <- 
  frame_models_country_out %>%
  filter(variable == "frame_vig", reference_row == FALSE) %>%
  left_join(vig_outcomes_df, by = c("response" = "outcome_vars")) %>%
  left_join(country_codes_df, by = c("data" = "code")) %>%
  mutate(outcome_label = factor(outcome_vars_labels2, levels = vig_outcomes_df$outcome_vars_labels2))  %>%
  mutate(p.value = 2 * (1 - pnorm(abs(estimate / std.error)))) %>% 
  group_by(label) %>% 
  mutate(p_adj = p.adjust(p.value, method = "BH"),
         p_adj_below_05 = p_adj < 0.01) %>%
  ungroup() %>%
  mutate(point_shape = ifelse(p_adj_below_05, 19, 21))


# plot estimates: by country

dat_sub <- rbind(filter(dat_country, covars == "none"), filter(dat_pooled, covars == "none"))
dat_sub$country <- factor(dat_sub$country, levels = c("Pooled", unique(country_codes_df$country[country_codes_df$code != "pooled"])))

dat_sub <- dat_sub %>%
  mutate(color_custom = ifelse(country == "Pooled", "black", "darkgrey"),
         alpha_custom = ifelse(country == "Pooled", 1, .95))  # 1 for Pooled, 0.5 for others

dodge <- .75
ggplot(dat_sub, aes(x = outcome_label, y = estimate, group = estimate, color = color_custom, alpha = alpha_custom)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = outcome_label, 
                     ymin = conf.low,
                     ymax = conf.high),
                 lwd = 1/2,
                 position = position_dodge2(width = dodge, reverse = FALSE),
                 show.legend = FALSE) +
  geom_point(aes(x = outcome_label, 
                 y = estimate,
                 shape = point_shape),
             position = position_dodge2(width = dodge, reverse = FALSE),
             fill = "white",
             show.legend = FALSE) + 
  geom_text(aes(label = country, x = outcome_label, y = conf.high),
            hjust = -0.2,  # Push labels slightly to the right
            size = 2, 
            position = position_dodge2(width = dodge),
            show.legend = FALSE) +
  coord_flip() + 
  facet_grid(outcome_label ~ label, scales = "free_y", space = "free_y", switch = "y") +
  theme_bw() + 
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +  # Increase right-side space
  scale_x_discrete(expand = c(0.3,0.1)) +  # Increase right-side space
  scale_shape_identity() +
  scale_color_manual(values = c("black" = "black", "gray" = "gray")) +  # Custom colors
  scale_alpha_identity() +  # Directly use alpha values
  ylab("Estimate") + xlab("") + 
  theme(text = element_text(family = "Fira Sans"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position= "bottom",
        legend.title=element_text(size = 9),
        legend.margin=margin(-10,0,0,0),
        legend.spacing.x = unit(.25, 'cm'),
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line("black", size = 0.1),
        plot.margin=unit(c(0.1,0.1,0.5,0.1),"cm"))
ggsave("results/effects-frame-by-country.png", width = 6, height = 8, dpi = 300)



# plot estimates: by attention check

dat_sub <- dat_pooled
dat_sub$color_custom <- "black"
dat_sub$alpha_custom <- 1
dat_sub <- left_join(dat_sub, check_df, by = c("covars" = "data"))
dat_sub$hjust_var <- ifelse(dat_sub$label == "Free speech", 1.01, -.01)
dat_sub$text_lab_pos <- ifelse(dat_sub$label == "Free speech", dat_sub$conf.low, dat_sub$conf.high)

# dat_sub$hjust_var[dat_sub$label == "Free speech" & dat_sub$covars == "check2"] <- -.03
# dat_sub$hjust_var[dat_sub$label == "Protect users" & dat_sub$covars == "check2"] <- 1.03
# 
# dat_sub$text_lab_pos[dat_sub$label == "Free speech" & dat_sub$covars == "check2"] <- dat_sub$conf.high[dat_sub$label == "Free speech" & dat_sub$covars == "check2"]
# dat_sub$text_lab_pos[dat_sub$label == "Protect users" & dat_sub$covars == "check2"] <- dat_sub$conf.low[dat_sub$label == "Protect users" & dat_sub$covars == "check2"]

dodge <- .75

ggplot(dat_sub, aes(x = outcome_label, y = estimate, group = covars, color = color_custom, alpha = alpha_custom)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = outcome_label, 
                     ymin = conf.low,
                     ymax = conf.high),
                 lwd = 1/2,
                 position = position_dodge2(width = dodge, reverse = FALSE),
                 show.legend = FALSE) +
  geom_point(aes(x = outcome_label, 
                 y = estimate,
                 shape = point_shape),
             position = position_dodge2(width = dodge, reverse = FALSE),
             fill = "white",
             show.legend = FALSE) + 
  geom_richtext(aes(label = check_lab, x = outcome_label, y = text_lab_pos,  hjust = hjust_var),
            position = position_dodge2(width = dodge),
            show.legend = FALSE,
            family = "Fira Sans",
            size = 3,
            fill = NA,
            label.color = NA) +
  coord_flip() + 
  facet_grid(outcome_label ~ label, scales = "free", space = "free_y", switch = "y") +
  theme_bw() + 
  scale_y_continuous(expand = expansion(mult = c(0.7, 0.7))) +  # Increase right-side space
  scale_x_discrete(expand = c(0.3,0.1)) +  # Increase right-side space
  scale_shape_identity() +
  scale_color_manual(values = c("black" = "black", "gray" = "gray")) +  # Custom colors
  scale_alpha_identity() +  # Directly use alpha values
  ylab("Estimate") + xlab("") + 
  theme(text = element_text(family = "Fira Sans"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position= "bottom",
        legend.title=element_text(size = 9),
        legend.margin=margin(-10,0,0,0),
        legend.spacing.x = unit(.25, 'cm'),
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line("black", size = 0.1),
        plot.margin=unit(c(0.1,0.1,0.5,0.1),"cm"))
ggsave("results/effects-frame-by-check.png", width = 6, height = 8, dpi = 300)




