# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")
emm_options(lmerTest.limit = 2000)
emm_options(pbkrtest.limit = 2000)


# load data ---------------------------

load("data/cooked/data_survey_combined.RData")

# random-effects structures used throughout the paper
re_pooled  <- "(1|resp_id) + (1|deck_id) + (1|resp_country2)"
re_country <- "(1|resp_id) + (1|deck_id)"   # within a single country (drops country RE)


# helper functions ---------------------------

# significance stars from a two-sided asymptotic z-test on estimate / SE
star_from_p <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
}
est_stars <- function(est, se) {
  p <- 2 * pnorm(-abs(est / se))
  paste0(sprintf("%+.2f", est * 100), star_from_p(p))
}

# myside effect (Agree vs. Disagree), pooled and by country, tagged with a column label
compute_myside_columns <- function(data, col_label) {
  fmla_pooled  <- paste0(outcome_vars, " ~ 1 + target_resp_issue_agreement + ", re_pooled)
  fmla_country <- paste0(outcome_vars, " ~ 1 + target_resp_issue_agreement + ", re_country)

  m_pooled <- map(fmla_pooled, lmer, data = data)

  m_country <- list()
  for (i in country_codes_chr) {
    m_country[[i]] <- map(fmla_country, lmer, data = filter(data, resp_country2 == i))
  }

  tp <- models_tidy_contrast_fun(m_pooled, "target_resp_issue_agreement", by_vars = NULL,
                                 outcome_vars = outcome_vars, labels_df = exp_covars_df,
                                 label_var = "exp_variable")
  tc <- models_tidy_contrast_fun(m_country, "target_resp_issue_agreement", by_vars = country_codes_chr,
                                 outcome_vars = outcome_vars, labels_df = exp_covars_df,
                                 label_var = "exp_variable")

  bind_rows(tp, tc) %>% mutate(column = col_label)
}

# myside effect (Agree vs. Disagree), pooled only (lighter)
pooled_myside_tidy <- function(data, sample_label) {
  m <- map(paste0(outcome_vars, " ~ 1 + target_resp_issue_agreement + ", re_pooled), lmer, data = data)
  models_tidy_contrast_fun(m, "target_resp_issue_agreement", by_vars = NULL,
                           outcome_vars = outcome_vars, labels_df = exp_covars_df,
                           label_var = "exp_variable") %>%
    mutate(sample = sample_label)
}

# omnibus joint LRT of an alignment x moderator interaction (ML models)
omnibus_one <- function(outcome, moderator, data) {
  re <- if (moderator == "resp_country2") re_country else re_pooled
  f0 <- as.formula(paste0(outcome, " ~ 1 + target_resp_issue_agreement + ", moderator, " + ", re))
  f1 <- as.formula(paste0(outcome, " ~ 1 + target_resp_issue_agreement * ", moderator, " + ", re))
  m0 <- lmer(f0, data = data, REML = FALSE)
  m1 <- lmer(f1, data = data, REML = FALSE)
  a  <- anova(m0, m1)
  tibble(outcome = outcome, moderator = moderator,
         df = a$Df[2], chisq = a$Chisq[2], p.value = a$`Pr(>Chisq)`[2])
}

# turn a stack of compute_myside_columns() outputs into plot-ready data
assemble_plot_tidy <- function(raw, column_levels) {
  raw %>%
    left_join(country_codes_df, by = c("data" = "code")) %>%
    mutate(country = if_else(data == "pooled", "Pooled", country)) %>%
    left_join(vig_outcomes_df, by = c("response" = "outcome_vars")) %>%
    add_confints(levels = c(.95, .99)) %>%
    mutate(
      estimate_fmt = sprintf("%+.1f", estimate * 100),
      country = factor(country, levels = country_sorted),
      country = fct_recode(country, "**Pooled**" = "Pooled"),
      column  = factor(column, levels = column_levels),
      # row facet strips are rotated: element_markdown() breaks these on \n, not <br>
      outcome_row = factor(
        str_replace_all(as.character(outcome_vars_labels_regtable), "<br>", "\n"),
        levels = str_replace_all(as.character(vig_outcomes_df$outcome_vars_labels_regtable), "<br>", "\n"))
    )
}

# flipped coefficient plot (outcomes as rows, column as columns), styled like Figure 4
plot_myside_flipped <- function(plot_tidy, file, width = 8, height = 10) {
  ggplot(plot_tidy, aes(x = estimate, y = country)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = .5, ymax = 1.5), fill = "#dfdfdf") +
    geom_vline(xintercept = seq(-.1, .15, .025), linewidth = 0.3, color = "#bfbfbf") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_point(aes(color = estimate), size = 1.5, alpha = 1) +
    geom_errorbarh(aes(xmin = ci99_lo, xmax = ci99_hi, color = estimate), height = 0, alpha = 0.8, linewidth = .5) +
    geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi, color = estimate), height = 0, alpha = 0.8, linewidth = 1) +
    geom_text(aes(x = ci99_hi, label = estimate_fmt, color = estimate),
              family = "Fira Sans", hjust = -0.2, size = 2.8) +
    facet_grid(outcome_row ~ column, switch = "y") +
    scale_y_discrete(limits = rev, position = "right") +
    scale_color_gradientn(colors = c("#2c7bb6", "#4c4c4c", "#d7191c"),
                          values = scales::rescale(c(-1, 0, 1), from = c(-1, 1)),
                          limits = c(-.2, .2)) +
    scale_x_continuous(breaks = c(-.10, -.05, 0, 0.05, 0.10, 0.15),
                       labels = label_percent(accuracy = 1, suffix = ""),
                       expand = expansion(mult = c(0.00, .15))) +
    labs(x = "Estimated myside bias effect (agree vs. disagree with target's issue position) in percentage points",
         y = NULL, title = NULL, subtitle = NULL) +
    guides(color = "none") +
    theme_minimal() +
    theme(text = element_text(family = "Fira Sans"),
          title = element_text(size = 10, face = "bold"),
          axis.title = element_text(size = 10, face = "bold"),
          axis.text.y = element_markdown(),
          axis.text.y.right = element_markdown(),
          axis.title.x = element_markdown(face = "plain"),
          panel.ontop = TRUE,
          panel.grid = element_blank(),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0),
          panel.spacing.x = unit(5, "pt"),
          panel.spacing.y = unit(5, "pt"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          strip.text = element_markdown(face = "bold"))
  ggsave(file, width = width, height = height, bg = "white", dpi = 300)
}

# wide latex table of pooled myside estimates: rows = sample, cols = outcomes
make_est_table <- function(tidy_pooled, row_var, row_levels, caption, label, file,
                           row_col_name = "Sample") {
  wide <- tidy_pooled %>%
    mutate(cell = est_stars(estimate, std.error)) %>%
    dplyr::select(all_of(row_var), response, cell) %>%
    pivot_wider(names_from = response, values_from = cell) %>%
    mutate(!!row_var := factor(.data[[row_var]], levels = row_levels)) %>%
    arrange(.data[[row_var]]) %>%
    dplyr::select(all_of(row_var), all_of(outcome_vars))

  k <- kable(wide, format = "latex", booktabs = TRUE, linesep = "",
             caption = caption, label = label,
             col.names = c(row_col_name, outcome_vars_labels2),
             align = c("l", rep("r", length(outcome_vars))), escape = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE)
  writeLines(k, file)
}

# wide latex table of omnibus LRTs: rows = moderator, cols = outcomes (chi-square)
make_omnibus_table <- function(omni, row_levels, caption, label, file,
                               row_col_name = "Subgroup moderator") {
  wide <- omni %>%
    mutate(cell = paste0(sprintf("%.2f", chisq), star_from_p(p.value))) %>%
    dplyr::select(moderator_label, df, outcome, cell) %>%
    pivot_wider(names_from = outcome, values_from = cell) %>%
    mutate(moderator_label = factor(moderator_label, levels = row_levels)) %>%
    arrange(moderator_label) %>%
    dplyr::select(moderator_label, df, all_of(outcome_vars))

  k <- kable(wide, format = "latex", booktabs = TRUE, linesep = "",
             caption = caption, label = label,
             col.names = c(row_col_name, "df", outcome_vars_labels2),
             align = c("l", "r", rep("r", length(outcome_vars))), escape = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE)
  writeLines(k, file)
}

sig_note <- "Cells show percentage-point estimates. Significance from two-sided z-tests: *** p<0.001, ** p<0.01, * p<0.05."
lrt_note <- "Cells show likelihood-ratio $\\chi^2$ statistics for the joint alignment$\\times$moderator interaction. *** p<0.001, ** p<0.01, * p<0.05."


# baseline myside effect and country ordering ---------------------------

# full-sample myside effect; country order (by offensive effect) reused across all plots
myside_full <- compute_myside_columns(data_survey_combined, "All")

country_code_order <- myside_full %>%
  filter(response == "vig_perc_offensive2", data != "pooled") %>%
  arrange(desc(estimate)) %>% pull(data)
country_sorted <- c(country_codes_df$country[match(country_code_order, country_codes_df$code)], "Pooled")


# cross-pressure: full vs. ideologically discordant vs. concordant ---------------------------

dat_discordant <- filter(data_survey_combined, target_resp_ideology_match == "Mismatch")
dat_concordant <- filter(data_survey_combined, target_resp_ideology_match == "Match")

cp_discordant <- compute_myside_columns(dat_discordant, "Ideologically<br>discordant")
cp_concordant <- compute_myside_columns(dat_concordant, "Ideologically<br>concordant")

cp_levels <- c("All<br>judgments", "Ideologically<br>discordant", "Ideologically<br>concordant")
cp_plot_tidy <- assemble_plot_tidy(
  bind_rows(mutate(myside_full, column = "All<br>judgments"), cp_discordant, cp_concordant),
  cp_levels
)
plot_myside_flipped(cp_plot_tidy, "results/robustness-crosspressure-coefplot.png", width = 7, height = 10)

# latex table, pooled estimates only
cp_table <- bind_rows(
  mutate(filter(myside_full, data == "pooled"), sample = "All judgments"),
  mutate(filter(cp_discordant, data == "pooled"), sample = "Ideologically discordant"),
  mutate(filter(cp_concordant, data == "pooled"), sample = "Ideologically concordant")
)
make_est_table(
  cp_table, "sample",
  row_levels = c("All judgments", "Ideologically discordant", "Ideologically concordant"),
  caption = paste("Myside bias by ideological cross-pressure (pooled).",
                  "Ideologically discordant = respondent's left--right leaning does not match the target's issue-stance direction.",
                  sig_note),
  label = "robustness-crosspressure",
  file = "results/robustness-crosspressure.tex",
  row_col_name = "Judgments"
)


# desensitization: full vs. first 3 vs. last 3 vignettes ---------------------------

dat_first3 <- filter(data_survey_combined, vig_pos <= 3)
dat_last3  <- filter(data_survey_combined, vig_pos >= 6)   # positions 6,7,8 of 8

ord_first3 <- compute_myside_columns(dat_first3, "First 3<br>vignettes")
ord_last3  <- compute_myside_columns(dat_last3,  "Last 3<br>vignettes")

ord_levels <- c("All<br>vignettes", "First 3<br>vignettes", "Last 3<br>vignettes")
ord_plot_tidy <- assemble_plot_tidy(
  bind_rows(mutate(myside_full, column = "All<br>vignettes"), ord_first3, ord_last3),
  ord_levels
)
plot_myside_flipped(ord_plot_tidy, "results/robustness-vignette-order-coefplot.png", width = 7, height = 10)

# latex table, pooled estimates only
ord_table <- bind_rows(
  mutate(filter(myside_full, data == "pooled"), sample = "All vignettes"),
  mutate(filter(ord_first3, data == "pooled"), sample = "First 3 vignettes"),
  mutate(filter(ord_last3, data == "pooled"), sample = "Last 3 vignettes")
)
make_est_table(
  ord_table, "sample",
  row_levels = c("All vignettes", "First 3 vignettes", "Last 3 vignettes"),
  caption = paste("Myside bias by position in the vignette sequence (pooled), testing for desensitization.",
                  sig_note),
  label = "robustness-vignette-order",
  file = "results/robustness-vignette-order.tex",
  row_col_name = "Vignettes"
)


# omnibus subgroup tests (Leeper et al.) ---------------------------

subgroup_mods <- tibble(
  moderator_label = c("Country", "Political ideology", "Respondent gender",
                      "Silencing belief", "Message type"),
  moderator       = c("resp_country2", "leftright_cat", "gender",
                      "silencing_score_cat", "sender_category")
)

grid_subgroup <- expand_grid(outcome = outcome_vars, moderator = subgroup_mods$moderator)
omni_subgroup <- pmap_dfr(
  list(grid_subgroup$outcome, grid_subgroup$moderator),
  function(o, m) omnibus_one(o, m, data_survey_combined)
) %>%
  left_join(subgroup_mods, by = "moderator")

make_omnibus_table(
  omni_subgroup, row_levels = subgroup_mods$moderator_label,
  caption = paste("Omnibus tests of subgroup heterogeneity in myside bias.", lrt_note),
  label = "robustness-omnibus-subgroup",
  file = "results/robustness-omnibus-subgroup.tex"
)


# treatment-arm invariance: framing and exposure placement ---------------------------

arm_mods <- tibble(
  moderator_label = c("Platform framing", "Exposure placement (question order)"),
  moderator       = c("frame_vig", "prefs_after_vignette")
)

grid_arms <- expand_grid(outcome = outcome_vars, moderator = arm_mods$moderator)
omni_arms <- pmap_dfr(
  list(grid_arms$outcome, grid_arms$moderator),
  function(o, m) omnibus_one(o, m, data_survey_combined)
) %>%
  left_join(arm_mods, by = "moderator")

make_omnibus_table(
  omni_arms, row_levels = arm_mods$moderator_label,
  caption = paste("Treatment-arm invariance: framing and exposure-placement assignments do not moderate myside bias.",
                  "Non-significant interactions justify pooling across arms.", lrt_note),
  label = "robustness-treatment-arm",
  file = "results/robustness-treatment-arm.tex",
  row_col_name = "Experimental arm"
)


# recruitment ad-type sensitivity ---------------------------

dat_topic   <- filter(data_survey_combined, grepl("topic",   resp_adtype))
dat_neutral <- filter(data_survey_combined, grepl("neutral", resp_adtype))

adtype_table <- bind_rows(
  pooled_myside_tidy(dat_topic,   "Topic ad (speech-primed)"),
  pooled_myside_tidy(dat_neutral, "Neutral ad")
)
make_est_table(
  adtype_table, "sample",
  row_levels = c("Topic ad (speech-primed)", "Neutral ad"),
  caption = paste("Myside bias by recruitment-ad type (pooled).",
                  "82\\% of respondents were recruited via the topic (speech-primed) ad.", sig_note),
  label = "robustness-adtype",
  file = "results/robustness-adtype.tex",
  row_col_name = "Recruitment ad"
)


# within-person identification (Mundlak decomposition) ---------------------------

# the Mundlak model adds each respondent's mean alignment as a covariate, so the
# coefficient on agreement_num becomes the pure within-person effect; stability
# across the two models shows the effect is not driven by between-person confounds

dat_within <- data_survey_combined %>%
  filter(!is.na(target_resp_issue_agreement)) %>%
  mutate(agreement_num = as.numeric(target_resp_issue_agreement == "Agree")) %>%
  group_by(resp_id) %>%
  mutate(agreement_btw = mean(agreement_num, na.rm = TRUE)) %>%
  ungroup()

mundlak_table <- map_dfr(outcome_vars, function(o) {
  m_std <- lmer(as.formula(paste0(o, " ~ 1 + agreement_num + ", re_pooled)),
                data = dat_within, REML = TRUE)
  m_wth <- lmer(as.formula(paste0(o, " ~ 1 + agreement_num + agreement_btw + ", re_pooled)),
                data = dat_within, REML = TRUE)
  bind_rows(
    tibble(response = o, sample = "Standard (between + within)",
           estimate = fixef(m_std)[["agreement_num"]],
           std.error = sqrt(diag(vcov(m_std)))[["agreement_num"]]),
    tibble(response = o, sample = "Within-person (Mundlak)",
           estimate = fixef(m_wth)[["agreement_num"]],
           std.error = sqrt(diag(vcov(m_wth)))[["agreement_num"]])
  )
})

make_est_table(
  mundlak_table, "sample",
  row_levels = c("Standard (between + within)", "Within-person (Mundlak)"),
  caption = paste("Within-person identification of myside bias (Mundlak within--between decomposition, pooled).",
                  "The Mundlak model controls for each respondent's mean alignment, isolating the within-person effect.",
                  sig_note),
  label = "robustness-mundlak",
  file = "results/robustness-mundlak.tex",
  row_col_name = "Model"
)
