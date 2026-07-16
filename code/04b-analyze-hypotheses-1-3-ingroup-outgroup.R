# load packages, functions, and helper objects --------

# This script holds many mixed-effects models in memory. Each section extracts
# and saves its tidy estimates to data/cooked/, then frees the model objects
# with rm() + gc(), so sections can be run one at a time (re-run this preamble
# after restarting R). The combined plots read the saved tidy tables from disk.

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")
emm_options(lmerTest.limit = 2000)
emm_options(pbkrtest.limit = 2000)


# load data ---------------------------

load("data/cooked/data_survey_combined.RData")
load("data/cooked/data_survey_resp.RData")


# helper: latex table of plotted effects with BH-FDR p-values ---------------------------

# tabulates the focal effects shown in a coefficient figure (not control variables),
# with estimate, p, and Benjamini-Hochberg-adjusted p (FDR applied within each outcome).
# df must contain: response (outcome var), estimate, std.error, and the id columns in id_cols.
# writes a tabularray longtblr (breaks across pages, booktabs rules) to file.
effects_bh_table <- function(df, id_cols, id_names, caption, label, file, group_by_outcome = TRUE) {
  fmt_p <- function(p) ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p))

  tab <- df %>%
    dplyr::select(response, all_of(id_cols), estimate, std.error) %>%
    group_by(response) %>%
    mutate(p    = 2 * pnorm(-abs(estimate / std.error)),
           p_bh = p.adjust(p, method = "BH")) %>%
    ungroup() %>%
    left_join(dplyr::select(vig_outcomes_df, outcome_vars, outcome_vars_labels2),
              by = c("response" = "outcome_vars")) %>%
    mutate(outcome = factor(outcome_vars_labels2, levels = vig_outcomes_df$outcome_vars_labels2)) %>%
    arrange(outcome)

  # with group_by_outcome, outcomes are inline \SetCell headers; otherwise the
  # outcome becomes a leading column filled on every row (no group headers).
  head_names <- if (group_by_outcome) c(id_names, "Estimate (pp)", "$p$", "$p$ (BH)")
                else c("Outcome", id_names, "Estimate (pp)", "$p$", "$p$ (BH)")
  n_l       <- if (group_by_outcome) length(id_cols) else length(id_cols) + 1L
  ncol_body <- length(head_names)

  lines <- c(
    "\\begin{longtblr}[",
    paste0("  caption = {", caption, "},"),
    paste0("  label = {tab:", label, "},"),
    "]{",
    paste0("  colspec = {", paste0(strrep("l", n_l), "rrr"), "},"),
    "  rowhead = 1,",
    "  row{1} = {font=\\bfseries},",
    "}",
    "\\toprule",
    paste0(paste(head_names, collapse = " & "), " \\\\"),
    "\\midrule"
  )

  row_line <- function(d, i, lead = NULL) {
    idvals <- vapply(id_cols, function(cc) as.character(d[[cc]][i]), character(1))
    cells  <- c(lead, idvals,
                sprintf("%+.2f", d$estimate[i] * 100),
                fmt_p(d$p[i]), fmt_p(d$p_bh[i]))
    paste0(paste(cells, collapse = " & "), " \\\\")
  }

  if (group_by_outcome) {
    for (oc in levels(tab$outcome)) {
      sub <- filter(tab, outcome == oc)
      if (nrow(sub) == 0) next
      lines <- c(lines, paste0("\\SetCell[c=", ncol_body, "]{l} \\textit{", oc, "} \\\\"))
      for (i in seq_len(nrow(sub))) lines <- c(lines, row_line(sub, i))
      lines <- c(lines, "\\midrule")
    }
    lines[length(lines)] <- "\\bottomrule"
  } else {
    prev <- NULL
    for (i in seq_len(nrow(tab))) {
      oc <- as.character(tab$outcome[i])
      if (!is.null(prev) && oc != prev) lines <- c(lines, "\\midrule")
      lines <- c(lines, row_line(tab, i, lead = oc))
      prev <- oc
    }
    lines <- c(lines, "\\bottomrule")
  }
  lines <- c(lines, "\\end{longtblr}")
  writeLines(lines, file)
}


# H1: issue agreement moderates vignette attribute effects ---------------

# Pre-registered H1: Respondents who agree (vs. disagree) with the issue position of the target are more likely to perceive messages as being hate speech and offensive speech, and to prefer platform, legal, and employer action against the message/sender.

# generate model formulas

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ",
                "(1|resp_id) + (1|deck_id)")

# run mixed-effects models

h1_models_pooled <- map(paste(fmlas, "+ target_resp_issue_agreement + (1|resp_country2)"), lmer, data = data_survey_combined)

h1_models_country <- list()
for (i in country_codes_chr) {
  h1_models_country[[i]] <- map(paste(fmlas, "+ target_resp_issue_agreement"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}

# run mixed-effects models, by message category

h1_models_by_message_category_pooled <- map(paste(fmlas, "+ target_resp_issue_agreement*sender_category + (1|resp_country2)"), lmer, data = data_survey_combined)

h1_models_by_message_category_country <- list()
for (i in country_codes_chr) {
  h1_models_by_message_category_country[[i]] <- map(paste(fmlas, "+ target_resp_issue_agreement*sender_category"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}

# run mixed-effects models, by issue topic

h1_models_by_message_topic_pooled <- map(paste(fmlas, "+ target_resp_issue_agreement*target_topic + (1|resp_country2)"), lmer, data = data_survey_combined)

h1_models_by_message_topic_country <- list()
for (i in country_codes_chr) {
  h1_models_by_message_topic_country[[i]] <- map(paste(fmlas, "+ target_resp_issue_agreement*target_topic"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}

# run mixed-effects models, by respondent ideology

h1_models_by_resp_leftright_pooled <- map(paste(fmlas, "+ target_resp_issue_agreement*leftright_cat + (1|resp_country2)"), lmer, data = data_survey_combined)

h1_models_by_resp_leftright_country <- list()
for (i in country_codes_chr) {
  h1_models_by_resp_leftright_country[[i]] <- map(paste(fmlas, "+ target_resp_issue_agreement*leftright_cat"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}


# run mixed-effects models, by respondent gender

h1_models_by_resp_gender_pooled <- map(paste(fmlas, "+ target_resp_issue_agreement*gender + (1|resp_country2)"), lmer, data = data_survey_combined)

h1_models_by_resp_gender_country <- list()
for (i in country_codes_chr) {
  h1_models_by_resp_gender_country[[i]] <- map(paste(fmlas, "+ target_resp_issue_agreement*gender"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}

# run mixed-effects models, by silencing speech score

h1_models_by_resp_silencing_pooled <- map(paste(fmlas, "+ target_resp_issue_agreement*silencing_score_cat + (1|resp_country2)"), lmer, data = data_survey_combined)

h1_models_by_resp_silencing_country <- list()
for (i in country_codes_chr) {
  h1_models_by_resp_silencing_country[[i]] <- map(paste(fmlas, "+ target_resp_issue_agreement*silencing_score_cat"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}



# extract model estimates, pooled models

h1_models_pooled_tidy <- models_tidy_contrast_fun(h1_models_pooled, "target_resp_issue_agreement", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1_models_by_message_category_pooled_tidy <- models_tidy_contrast_fun(h1_models_by_message_category_pooled, var_x = "target_resp_issue_agreement", var_z = "sender_category", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1_models_by_message_topic_pooled_tidy <- models_tidy_contrast_fun(h1_models_by_message_topic_pooled, var_x = "target_resp_issue_agreement", var_z = "target_topic", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1_models_by_resp_leftright_pooled_tidy <- models_tidy_contrast_fun(h1_models_by_resp_leftright_pooled, var_x = "target_resp_issue_agreement", var_z = "leftright_cat", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1_models_by_resp_gender_pooled_tidy <- models_tidy_contrast_fun(h1_models_by_resp_gender_pooled, var_x = "target_resp_issue_agreement", var_z = "gender", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1_models_by_resp_silencing_pooled_tidy <- models_tidy_contrast_fun(h1_models_by_resp_silencing_pooled, var_x = "target_resp_issue_agreement", var_z = "silencing_score_cat", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")

save(h1_models_by_message_category_pooled_tidy, file = "data/cooked/h1_models_by_message_category_pooled_tidy.RData")
save(h1_models_by_message_topic_pooled_tidy, file = "data/cooked/h1_models_by_message_topic_pooled_tidy.RData")
save(h1_models_by_resp_leftright_pooled_tidy, file = "data/cooked/h1_models_by_resp_leftright_pooled_tidy.RData")
save(h1_models_by_resp_gender_pooled_tidy, file = "data/cooked/h1_models_by_resp_gender_pooled_tidy.RData")
save(h1_models_by_resp_silencing_pooled_tidy, file = "data/cooked/h1_models_by_resp_silencing_pooled_tidy.RData")

h1_models_pooled_tidy_comb <- bind_rows(
  h1_models_pooled_tidy, 
  h1_models_by_message_category_pooled_tidy,
  h1_models_by_message_topic_pooled_tidy,
  h1_models_by_resp_leftright_pooled_tidy,
  h1_models_by_resp_gender_pooled_tidy,
  h1_models_by_resp_silencing_pooled_tidy
  )

models_df <- gt_prep_fun(h1_models_pooled_tidy_comb, by = "response", arrange_by = "estimate_vig_perc_offensive2", varlabels_ref = "exp_varlabel_ref")
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))

# generate the gt table

gt_table_fun(models_df, 
             glance_df = NULL, 
             stats_df, 
             outcome_vars = vig_outcomes_df$outcome_vars,
             outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
             sample = "Pooled",
             varlabels_ref = "exp_varlabel_ref") %>% 
  gtsave_auto("results/table-h1-models-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)


tidy_mm_df <- models_tidy_mms_fun(
  models     = h1_models_pooled,                       
  predictors = "target_resp_issue_agreement",
  data       = data_survey_combined,      
  labels_df = resp_covars_df,
  label_var = "resp_variable"
)
models_mm_df <- gt_prep_fun(tidy_mm_df, by = "response", arrange_by = NULL, varlabels_ref = "resp_varlabel")
rel_diff <- function(a, b) {
  (a - b) / b * 100
}

rel_diff(
  models_mm_df$estimate_vig_remove[models_mm_df$label == "Agree"], 
  models_mm_df$estimate_vig_remove[models_mm_df$label == "Disagree"]
)



# extract model estimates, by country

h1_models_country_tidy <- models_tidy_contrast_fun(h1_models_country, "target_resp_issue_agreement", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1_models_by_message_category_country_tidy <- models_tidy_contrast_fun(h1_models_by_message_category_country, var_x = "target_resp_issue_agreement", var_z = "sender_category", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1_models_by_message_topic_country_tidy <- models_tidy_contrast_fun(h1_models_by_message_topic_country, var_x = "target_resp_issue_agreement", var_z = "target_topic", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1_models_by_resp_leftright_country_tidy <- models_tidy_contrast_fun(h1_models_by_resp_leftright_country, var_x = "target_resp_issue_agreement", var_z = "leftright_cat", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1_models_by_resp_gender_country_tidy <- models_tidy_contrast_fun(h1_models_by_resp_gender_country, var_x = "target_resp_issue_agreement", var_z = "gender", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1_models_by_resp_silencing_country_tidy <- models_tidy_contrast_fun(h1_models_by_resp_silencing_country, var_x = "target_resp_issue_agreement", var_z = "silencing_score_cat", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")

h1_models_country_tidy_comb <- bind_rows(
  h1_models_country_tidy, 
  h1_models_by_message_category_country_tidy,
  h1_models_by_message_topic_country_tidy,
  h1_models_by_resp_leftright_country_tidy,
  h1_models_by_resp_gender_country_tidy,
  h1_models_by_resp_silencing_country_tidy
)

# country tables, by outcome

for(i in country_codes_chr) {
  models_df <- h1_models_country_tidy_comb %>% filter(data == i) %>% gt_prep_fun(by = "response", arrange_by = NULL, varlabels_ref = "exp_varlabel_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df,
               glance_df = NULL,
               stats_df,
               outcome_vars = vig_outcomes_df$outcome_vars,
               outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
               sample = country_codes_df$country[country_codes_df$code == i],
               varlabels_ref = "exp_varlabel_ref") %>% 
    gtsave_auto(paste0("results/table-h1-models-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}

# outcome tables, by country

for(i in outcome_vars) {
  models_df <- h1_models_country_tidy_comb %>% filter(response == i) %>% gt_prep_fun(by = "data", arrange_by = NULL, varlabels_ref = "exp_varlabel_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df, 
               glance_df = NULL, 
               stats_df, 
               outcome_vars = country_codes_df$code[country_codes_df$sepcountry == TRUE],
               outcome_vars_labels = country_codes_df$country[country_codes_df$sepcountry == TRUE],
               sample = vig_outcomes_df$outcome_vars_labels_regtable[vig_outcomes_df$outcome_vars == i],
               spanners = FALSE,
               varlabels_ref = "exp_varlabel_ref") %>% 
    gtsave_auto(paste0("results/table-h1-models-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}


## Effects plot, pooled and by country

# order effects by estimate size

country_code_order <- 
  filter(h1_models_country_tidy, response == "vig_perc_offensive2") %>%
  arrange(desc(estimate)) %>% pull(data)
country_sorted <- country_codes_df %>%
  slice(match(country_code_order, code)) %>%
  pull(country)
country_sorted <- c(country_sorted, "Pooled")

# combine pooled and country-specific estimates

h1_models_plot_tidy <- bind_rows(
  h1_models_pooled_tidy,
  h1_models_country_tidy
) %>%
  left_join(country_codes_df, 
            by = c("data" = "code")) %>%
  left_join(vig_outcomes_df,
            by = c("response" = "outcome_vars")) %>%
  mutate(country = factor(country, levels = country_sorted),
         country = fct_relevel(country, "Pooled", after = Inf),
         outcome_vars_labels_regtable = factor(outcome_vars_labels_regtable, 
                                               levels = vig_outcomes_df$outcome_vars_labels_regtable)
  ) %>% 
  add_confints(levels = c(.80, .95, .99)) %>% # add confidence intervals
  mutate(estimate_fmt = sprintf("%+.1f", estimate * 100)) %>% # format estimates
  mutate(country = fct_recode(country,
                              "**Pooled**" = "Pooled"))

save(h1_models_plot_tidy, file = "data/cooked/h1_models_plot_tidy.RData")

# plot pooled and by country estimates

h1_models_plot_tidy %>%
  ggplot(aes(x = estimate, y = country)) +
  geom_rect(
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = .5,
      ymax = 1.5
    ),
    fill = "#dfdfdf"
  ) +
  geom_vline(xintercept = seq(-.025, .15, .025), linewidth = 0.3, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(color = estimate), size = 1.5, alpha = 1) +
  geom_errorbarh(aes(xmin = ci99_lo, xmax = ci99_hi, color = estimate), height = 0, alpha = 0.8, size = .75) +
  geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi, color = estimate), height = 0, alpha = 0.8, size = 1) +
  geom_errorbarh(aes(xmin = ci80_lo, xmax = ci80_hi, color = estimate), height = 0, alpha = 0.8, size = 1.5) +
  geom_text(
    aes(
      x = ci99_hi,   # position at the upper CI bound
      label = estimate_fmt,
      color = estimate
    ),
    family = "Fira Sans",
    hjust = -0.2,    # nudge a bit further right
    size = 2.8
  ) +
  facet_grid(~ outcome_vars_labels_regtable) +
  scale_y_discrete(limits = rev) +
  scale_color_viridis_c(direction = -1, begin = 0.2, end = 0.8) +
  scale_x_continuous(
    breaks = c(0, 0.05, 0.10, 0.15),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0.05, .25)) # add more room on the right
  ) + 
  labs(x = "Effect of agreeing with target issue position (vs. disagreeing)", 
       y = NULL,
       title = NULL,
       subtitle = NULL) +
  guides(color = "none") + 
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_markdown(),
        panel.ontop = TRUE,
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        panel.spacing.x = unit(0, "pt"),        # remove facet gaps horizontally
        panel.spacing.y = unit(0, "pt"),        # (in case of multiple rows)
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.text = element_markdown(face = "bold"))
ggsave("results/effects-h1-coefplot.png", width = 10, height = 3,   bg = "white", dpi = 300)


# free H1 model objects (tidy tables saved to disk)

rm(list = c(
  "h1_models_pooled", "h1_models_country",
  "h1_models_by_message_category_pooled", "h1_models_by_message_category_country",
  "h1_models_by_message_topic_pooled", "h1_models_by_message_topic_country",
  "h1_models_by_resp_leftright_pooled", "h1_models_by_resp_leftright_country",
  "h1_models_by_resp_gender_pooled", "h1_models_by_resp_gender_country",
  "h1_models_by_resp_silencing_pooled", "h1_models_by_resp_silencing_country"
))
gc()


# H2: gender/ethnicity alignment with target moderate vignette attribute effects ---------------

# Pre-registered H2: Respondents who share gender or ethnicity with the target are more likely to perceive messages as being hate speech and offensive speech, and to prefer platform, legal, and employer action against the message/sender.


# generate model formulas

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ",
                "(1|resp_id) + (1|deck_id)")

# run mixed-effects models

h2gender_models_pooled <- map(paste(fmlas, "+ target_gender_match + (1|resp_country2)"), lmer, data = data_survey_combined)

h2gender_models_country <- list()
for (i in country_codes_chr) {
  h2gender_models_country[[i]] <- map(paste(fmlas, "+ target_gender_match"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}

h2white_models_pooled <- map(paste(fmlas, "+ target_white_match + (1|resp_country2)"), lmer, data = data_survey_combined)

h2white_models_country <- list()
for (i in country_codes_chr) {
  h2white_models_country[[i]] <- map(paste(fmlas, "+ target_white_match"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}


# run mixed-effects models, by message category

h2gender_models_by_message_category_pooled <- map(paste(fmlas, "+ target_gender_match*sender_category + (1|resp_country2)"), lmer, data = data_survey_combined)

h2gender_models_by_message_category_country <- list()
for (i in country_codes_chr) {
  h2gender_models_by_message_category_country[[i]] <- map(paste(fmlas, "+ target_gender_match*sender_category"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}


h2white_models_by_message_category_pooled <- map(paste(fmlas, "+ target_white_match*sender_category + (1|resp_country2)"), lmer, data = data_survey_combined)

h2white_models_by_message_category_country <- list()
for (i in country_codes_chr) {
  h2white_models_by_message_category_country[[i]] <- map(paste(fmlas, "+ target_white_match*sender_category"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}

# run mixed-effects models, by respondent gender/white

h2gender_models_by_resp_gender_pooled <- map(paste(fmlas, "+ target_gender_match*gender + (1|resp_country2)"), lmer, data = data_survey_combined)

h2gender_models_by_resp_gender_country <- list()
for (i in country_codes_chr) {
  h2gender_models_by_resp_gender_country[[i]] <- map(paste(fmlas, "+ target_gender_match*gender"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}


h2white_models_by_resp_white_pooled <- map(paste(fmlas, "+ target_white_match*white_cat + (1|resp_country2)"), lmer, data = data_survey_combined)

h2white_models_by_resp_white_country <- list()
for (i in country_codes_chr) {
  h2white_models_by_resp_white_country[[i]] <- map(paste(fmlas, "+ target_white_match*white_cat"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}


# extract model estimates, pooled models

h2gender_models_pooled_tidy <- models_tidy_contrast_fun(h2gender_models_pooled, "target_gender_match", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h2gender_models_by_message_category_pooled_tidy <- models_tidy_contrast_fun(h2gender_models_by_message_category_pooled, var_x = "target_gender_match", var_z = "sender_category", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h2gender_models_by_resp_gender_pooled_tidy <- models_tidy_contrast_fun(h2gender_models_by_resp_gender_pooled, var_x = "target_gender_match", var_z = "gender", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")

h2white_models_pooled_tidy <- models_tidy_contrast_fun(h2white_models_pooled, "target_white_match", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h2white_models_by_message_category_pooled_tidy <- models_tidy_contrast_fun(h2white_models_by_message_category_pooled, var_x = "target_white_match", var_z = "sender_category", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h2white_models_by_resp_white_pooled_tidy <- models_tidy_contrast_fun(h2white_models_by_resp_white_pooled, var_x = "target_white_match", var_z = "white_cat", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")


h2_models_pooled_tidy_comb <- bind_rows(
  h2gender_models_pooled_tidy, 
  h2gender_models_by_message_category_pooled_tidy,
  h2gender_models_by_resp_gender_pooled_tidy,
  h2white_models_pooled_tidy, 
  h2white_models_by_message_category_pooled_tidy,
  h2white_models_by_resp_white_pooled_tidy,
)

models_df <- gt_prep_fun(h2_models_pooled_tidy_comb, by = "response", arrange_by = NULL, varlabels_ref = "exp_varlabel_ref")
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))

# generate the gt table
gt_table_fun(models_df, 
             glance_df = NULL, 
             stats_df, 
             outcome_vars = vig_outcomes_df$outcome_vars,
             outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
             sample = "Pooled",
             varlabels_ref = "exp_varlabel_ref") %>% 
  gtsave_auto("results/table-h2target-models-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)

# extract model estimates, by country

h2gender_models_country_tidy <- models_tidy_contrast_fun(h2gender_models_country, "target_gender_match", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h2gender_models_by_message_category_country_tidy <- models_tidy_contrast_fun(h2gender_models_by_message_category_country, var_x = "target_gender_match", var_z = "sender_category", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h2gender_models_by_resp_gender_country_tidy <- models_tidy_contrast_fun(h2gender_models_by_resp_gender_country, var_x = "target_gender_match", var_z = "gender", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")

h2white_models_country_tidy <- models_tidy_contrast_fun(h2white_models_country, "target_white_match", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h2white_models_by_message_category_country_tidy <- models_tidy_contrast_fun(h2white_models_by_message_category_country, var_x = "target_white_match", var_z = "sender_category", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h2white_models_by_resp_white_country_tidy <- models_tidy_contrast_fun(h2white_models_by_resp_white_country, var_x = "target_white_match", var_z = "white_cat", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")

h2_models_country_tidy_comb <- bind_rows(
  h2gender_models_country_tidy, 
  h2gender_models_by_message_category_country_tidy,
  h2gender_models_by_resp_gender_country_tidy,
  h2white_models_country_tidy, 
  h2white_models_by_message_category_country_tidy,
  h2white_models_by_resp_white_country_tidy,
)

# country tables, by outcome

for(i in country_codes_chr) {
  models_df <- h2_models_country_tidy_comb %>% filter(data == i) %>% gt_prep_fun(by = "response", arrange_by = NULL, varlabels_ref = "exp_varlabel_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df,
               glance_df = NULL,
               stats_df,
               outcome_vars = vig_outcomes_df$outcome_vars,
               outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
               sample = country_codes_df$country[country_codes_df$code == i],
               varlabels_ref = "exp_varlabel_ref") %>% 
    gtsave_auto(paste0("results/table-h2target-models-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}


# outcome tables, by country

for(i in outcome_vars) {
  models_df <- h2_models_country_tidy_comb %>% filter(response == i) %>% gt_prep_fun(by = "data", arrange_by = NULL, varlabels_ref = "exp_varlabel_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df, 
               glance_df = NULL, 
               stats_df, 
               outcome_vars = country_codes_df$code[country_codes_df$sepcountry == TRUE],
               outcome_vars_labels = country_codes_df$country[country_codes_df$sepcountry == TRUE],
               sample = vig_outcomes_df$outcome_vars_labels_regtable[vig_outcomes_df$outcome_vars == i],
               spanners = FALSE,
               varlabels_ref = "exp_varlabel_ref") %>% 
    gtsave_auto(paste0("results/table-h2target-models-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}



## Effects plot, pooled and by country

# order effects by estimate size

country_code_order <- 
  filter(h2gender_models_country_tidy, response == "vig_perc_offensive2") %>%
  arrange(desc(estimate)) %>% pull(data)
country_sorted <- country_codes_df %>%
  slice(match(country_code_order, code)) %>%
  pull(country)
country_sorted <- c(country_sorted, "Pooled")

# combine pooled and country-specific estimates

h2gender_models_plot_tidy <- bind_rows(
  h2gender_models_pooled_tidy,
  h2gender_models_country_tidy
) %>%
  left_join(country_codes_df, 
            by = c("data" = "code")) %>%
  left_join(vig_outcomes_df,
            by = c("response" = "outcome_vars")) %>%
  mutate(country = factor(country, levels = country_sorted),
         country = fct_relevel(country, "Pooled", after = Inf),
         outcome_vars_labels_regtable = factor(outcome_vars_labels_regtable, 
                                               levels = vig_outcomes_df$outcome_vars_labels_regtable)
  ) %>% 
  add_confints(levels = c(.80, .95, .99)) %>% # add confidence intervals
  mutate(estimate_fmt = sprintf("%+.1f", estimate * 100)) %>% # format estimates
  mutate(country = fct_recode(country,
                              "**Pooled**" = "Pooled"))

save(h2gender_models_plot_tidy, file = "data/cooked/h2gender_models_plot_tidy.RData")

# plot pooled and by country estimates

h2gender_models_plot_tidy %>%
  ggplot(aes(x = estimate, y = country)) +
  geom_rect(
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = .5,
      ymax = 1.5
    ),
    fill = "#dfdfdf"
  ) +
  geom_vline(xintercept = seq(-.025, .15, .025), linewidth = 0.3, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(color = estimate), size = 1.5, alpha = 1) +
  geom_errorbarh(aes(xmin = ci99_lo, xmax = ci99_hi, color = estimate), height = 0, alpha = 0.8, size = .75) +
  geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi, color = estimate), height = 0, alpha = 0.8, size = 1) +
  geom_errorbarh(aes(xmin = ci80_lo, xmax = ci80_hi, color = estimate), height = 0, alpha = 0.8, size = 1.5) +
  geom_text(
    aes(
      x = ci99_hi,   # position at the upper CI bound
      label = estimate_fmt,
      color = estimate
    ),
    family = "Fira Sans",
    hjust = -0.2,    # nudge a bit further right
    size = 2.8
  ) +
  facet_grid(~ outcome_vars_labels_regtable) +
  scale_y_discrete(limits = rev) +
  scale_color_viridis_c(direction = -1, begin = 0.2, end = 0.8) +
  scale_x_continuous(
    breaks = c(0, 0.05, 0.10, 0.15),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0.05, .25)) # add more room on the right
  ) + 
  labs(x = "Effect of sharing target gender (vs. not sharing)", 
       y = NULL,
       title = NULL,
       subtitle = NULL) +
  guides(color = "none") + 
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_markdown(),
        panel.ontop = TRUE,
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        panel.spacing.x = unit(0, "pt"),        # remove facet gaps horizontally
        panel.spacing.y = unit(0, "pt"),        # (in case of multiple rows)
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.text = element_markdown(face = "bold"))
ggsave("results/effects-h2targetgender-coefplot.png", width = 10, height = 3,   bg = "white", dpi = 300)



## Effects plot, pooled and by country

# order effects by estimate size

country_code_order <- 
  filter(h2white_models_country_tidy, response == "vig_perc_offensive2") %>%
  arrange(desc(estimate)) %>% pull(data)
country_sorted <- country_codes_df %>%
  slice(match(country_code_order, code)) %>%
  pull(country)
country_sorted <- c(country_sorted, "Pooled")

# combine pooled and country-specific estimates

h2white_models_plot_tidy <- bind_rows(
  h2white_models_pooled_tidy,
  h2white_models_country_tidy
) %>%
  left_join(country_codes_df, 
            by = c("data" = "code")) %>%
  left_join(vig_outcomes_df,
            by = c("response" = "outcome_vars")) %>%
  mutate(country = factor(country, levels = country_sorted),
         country = fct_relevel(country, "Pooled", after = Inf),
         outcome_vars_labels_regtable = factor(outcome_vars_labels_regtable, 
                                               levels = vig_outcomes_df$outcome_vars_labels_regtable)
  ) %>% 
  add_confints(levels = c(.80, .95, .99)) %>% # add confidence intervals
  mutate(estimate_fmt = sprintf("%+.1f", estimate * 100)) %>% # format estimates
  mutate(country = fct_recode(country,
                              "**Pooled**" = "Pooled"))

save(h2white_models_plot_tidy, file = "data/cooked/h2white_models_plot_tidy.RData")


# plot pooled and by country estimates

h2white_models_plot_tidy %>%
  ggplot(aes(x = estimate, y = country)) +
  geom_rect(
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = .5,
      ymax = 1.5
    ),
    fill = "#dfdfdf"
  ) +
  geom_vline(xintercept = seq(-.025, .15, .025), linewidth = 0.3, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(color = estimate), size = 1.5, alpha = 1) +
  geom_errorbarh(aes(xmin = ci99_lo, xmax = ci99_hi, color = estimate), height = 0, alpha = 0.8, size = .75) +
  geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi, color = estimate), height = 0, alpha = 0.8, size = 1) +
  geom_errorbarh(aes(xmin = ci80_lo, xmax = ci80_hi, color = estimate), height = 0, alpha = 0.8, size = 1.5) +
  geom_text(
    aes(
      x = ci99_hi,   # position at the upper CI bound
      label = estimate_fmt,
      color = estimate
    ),
    family = "Fira Sans",
    hjust = -0.2,    # nudge a bit further right
    size = 2.8
  ) +
  facet_grid(~ outcome_vars_labels_regtable) +
  scale_y_discrete(limits = rev) +
  scale_color_viridis_c(direction = -1, begin = 0.2, end = 0.8) +
  scale_x_continuous(
    breaks = c(0, 0.05, 0.10, 0.15),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0.05, .25)) # add more room on the right
  ) + 
  labs(x = "Effect of sharing target skin tone (vs. not sharing)", 
       y = NULL,
       title = NULL,
       subtitle = NULL) +
  guides(color = "none") + 
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_markdown(),
        panel.ontop = TRUE,
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        panel.spacing.x = unit(0, "pt"),        # remove facet gaps horizontally
        panel.spacing.y = unit(0, "pt"),        # (in case of multiple rows)
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.text = element_markdown(face = "bold"))
ggsave("results/effects-h2targetwhite-coefplot.png", width = 10, height = 3,   bg = "white", dpi = 300)


# free H2 model objects (plot tidy tables saved to disk)

rm(list = c(
  "h2gender_models_pooled", "h2gender_models_country",
  "h2white_models_pooled", "h2white_models_country",
  "h2gender_models_by_message_category_pooled", "h2gender_models_by_message_category_country",
  "h2white_models_by_message_category_pooled", "h2white_models_by_message_category_country",
  "h2gender_models_by_resp_gender_pooled", "h2gender_models_by_resp_gender_country",
  "h2white_models_by_resp_white_pooled", "h2white_models_by_resp_white_country"
))
gc()




# H3: gender/ethnicity alignment with sender moderate vignette attribute effects ---------------

# Pre-registered H3: Respondents who share gender or ethnicity with the sender are more likely to perceive messages as being hate speech and offensive speech, and to prefer platform, legal, and employer action against the message/sender.


# generate model formulas

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ",
                "(1|resp_id) + (1|deck_id)")

# run mixed-effects models

h3gender_models_pooled <- map(paste(fmlas, "+ sender_gender_match + (1|resp_country2)"), lmer, data = data_survey_combined)

h3gender_models_country <- list()
for (i in country_codes_chr) {
  h3gender_models_country[[i]] <- map(paste(fmlas, "+ sender_gender_match"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}

h3white_models_pooled <- map(paste(fmlas, "+ sender_white_match + (1|resp_country2)"), lmer, data = data_survey_combined)

h3white_models_country <- list()
for (i in country_codes_chr) {
  h3white_models_country[[i]] <- map(paste(fmlas, "+ sender_white_match"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}


# run mixed-effects models, by message category

h3gender_models_by_message_category_pooled <- map(paste(fmlas, "+ sender_gender_match*sender_category + (1|resp_country2)"), lmer, data = data_survey_combined)

h3gender_models_by_message_category_country <- list()
for (i in country_codes_chr) {
  h3gender_models_by_message_category_country[[i]] <- map(paste(fmlas, "+ sender_gender_match*sender_category"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}


h3white_models_by_message_category_pooled <- map(paste(fmlas, "+ sender_white_match*sender_category + (1|resp_country2)"), lmer, data = data_survey_combined)

h3white_models_by_message_category_country <- list()
for (i in country_codes_chr) {
  h3white_models_by_message_category_country[[i]] <- map(paste(fmlas, "+ sender_white_match*sender_category"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}

# run mixed-effects models, by respondent gender/white

h3gender_models_by_resp_gender_pooled <- map(paste(fmlas, "+ sender_gender_match*gender + (1|resp_country2)"), lmer, data = data_survey_combined)

h3gender_models_by_resp_gender_country <- list()
for (i in country_codes_chr) {
  h3gender_models_by_resp_gender_country[[i]] <- map(paste(fmlas, "+ sender_gender_match*gender"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}


h3white_models_by_resp_white_pooled <- map(paste(fmlas, "+ sender_white_match*white_cat + (1|resp_country2)"), lmer, data = data_survey_combined)

h3white_models_by_resp_white_country <- list()
for (i in country_codes_chr) {
  h3white_models_by_resp_white_country[[i]] <- map(paste(fmlas, "+ sender_white_match*white_cat"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}


# extract model estimates, pooled models

h3gender_models_pooled_tidy <- models_tidy_contrast_fun(h3gender_models_pooled, "sender_gender_match", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h3gender_models_by_message_category_pooled_tidy <- models_tidy_contrast_fun(h3gender_models_by_message_category_pooled, var_x = "sender_gender_match", var_z = "sender_category", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h3gender_models_by_resp_gender_pooled_tidy <- models_tidy_contrast_fun(h3gender_models_by_resp_gender_pooled, var_x = "sender_gender_match", var_z = "gender", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")

h3white_models_pooled_tidy <- models_tidy_contrast_fun(h3white_models_pooled, "sender_white_match", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h3white_models_by_message_category_pooled_tidy <- models_tidy_contrast_fun(h3white_models_by_message_category_pooled, var_x = "sender_white_match", var_z = "sender_category", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h3white_models_by_resp_white_pooled_tidy <- models_tidy_contrast_fun(h3white_models_by_resp_white_pooled, var_x = "sender_white_match", var_z = "white_cat", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")


h3_models_pooled_tidy_comb <- bind_rows(
  h3gender_models_pooled_tidy, 
  h3gender_models_by_message_category_pooled_tidy,
  h3gender_models_by_resp_gender_pooled_tidy,
  h3white_models_pooled_tidy, 
  h3white_models_by_message_category_pooled_tidy,
  h3white_models_by_resp_white_pooled_tidy,
)

models_df <- gt_prep_fun(h3_models_pooled_tidy_comb, by = "response", arrange_by = NULL, varlabels_ref = "exp_varlabel_ref")
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))

# generate the gt table
gt_table_fun(models_df, 
             glance_df = NULL, 
             stats_df, 
             outcome_vars = vig_outcomes_df$outcome_vars,
             outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
             sample = "Pooled",
             varlabels_ref = "exp_varlabel_ref") %>% 
  gtsave_auto("results/table-h3sender-models-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)



# extract model estimates, by country

h3gender_models_country_tidy <- models_tidy_contrast_fun(h3gender_models_country, "sender_gender_match", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h3gender_models_by_message_category_country_tidy <- models_tidy_contrast_fun(h3gender_models_by_message_category_country, var_x = "sender_gender_match", var_z = "sender_category", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h3gender_models_by_resp_gender_country_tidy <- models_tidy_contrast_fun(h3gender_models_by_resp_gender_country, var_x = "sender_gender_match", var_z = "gender", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")

h3white_models_country_tidy <- models_tidy_contrast_fun(h3white_models_country, "sender_white_match", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h3white_models_by_message_category_country_tidy <- models_tidy_contrast_fun(h3white_models_by_message_category_country, var_x = "sender_white_match", var_z = "sender_category", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h3white_models_by_resp_white_country_tidy <- models_tidy_contrast_fun(h3white_models_by_resp_white_country, var_x = "sender_white_match", var_z = "white_cat", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")

h3_models_country_tidy_comb <- bind_rows(
  h3gender_models_country_tidy, 
  h3gender_models_by_message_category_country_tidy,
  h3gender_models_by_resp_gender_country_tidy,
  h3white_models_country_tidy, 
  h3white_models_by_message_category_country_tidy,
  h3white_models_by_resp_white_country_tidy,
)

# country tables, by outcome

for(i in country_codes_chr) {
  models_df <- h3_models_country_tidy_comb %>% filter(data == i) %>% gt_prep_fun(by = "response", arrange_by = NULL, varlabels_ref = "exp_varlabel_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df,
               glance_df = NULL,
               stats_df,
               outcome_vars = vig_outcomes_df$outcome_vars,
               outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
               sample = country_codes_df$country[country_codes_df$code == i],
               varlabels_ref = "exp_varlabel_ref") %>% 
    gtsave_auto(paste0("results/table-h3sender-models-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}


# outcome tables, by country

for(i in outcome_vars) {
  models_df <- h3_models_country_tidy_comb %>% filter(response == i) %>% gt_prep_fun(by = "data", arrange_by = NULL, varlabels_ref = "exp_varlabel_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df, 
               glance_df = NULL, 
               stats_df, 
               outcome_vars = country_codes_df$code[country_codes_df$sepcountry == TRUE],
               outcome_vars_labels = country_codes_df$country[country_codes_df$sepcountry == TRUE],
               sample = vig_outcomes_df$outcome_vars_labels_regtable[vig_outcomes_df$outcome_vars == i],
               spanners = FALSE,
               varlabels_ref = "exp_varlabel_ref") %>% 
    gtsave_auto(paste0("results/table-h3sender-models-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}




## Effects plot, pooled and by country

# order effects by estimate size

country_code_order <- 
  filter(h3gender_models_country_tidy, response == "vig_perc_offensive2") %>%
  arrange(desc(estimate)) %>% pull(data)
country_sorted <- country_codes_df %>%
  slice(match(country_code_order, code)) %>%
  pull(country)
country_sorted <- c(country_sorted, "Pooled")

# combine pooled and country-specific estimates

h3gender_models_plot_tidy <- bind_rows(
  h3gender_models_pooled_tidy,
  h3gender_models_country_tidy
) %>%
  left_join(country_codes_df, 
            by = c("data" = "code")) %>%
  left_join(vig_outcomes_df,
            by = c("response" = "outcome_vars")) %>%
  mutate(country = factor(country, levels = country_sorted),
         country = fct_relevel(country, "Pooled", after = Inf),
         outcome_vars_labels_regtable = factor(outcome_vars_labels_regtable, 
                                               levels = vig_outcomes_df$outcome_vars_labels_regtable)
  ) %>% 
  add_confints(levels = c(.80, .95, .99)) %>% # add confidence intervals
  mutate(estimate_fmt = sprintf("%+.1f", estimate * 100)) %>% # format estimates
  mutate(country = fct_recode(country,
                              "**Pooled**" = "Pooled"))

save(h3gender_models_plot_tidy, file = "data/cooked/h3gender_models_plot_tidy.RData")


# plot pooled and by country estimates

h3gender_models_plot_tidy %>%
  ggplot(aes(x = estimate, y = country)) +
  geom_rect(
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = .5,
      ymax = 1.5
    ),
    fill = "#dfdfdf"
  ) +
  geom_vline(xintercept = seq(-.025, .15, .025), linewidth = 0.3, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(color = estimate), size = 1.5, alpha = 1) +
  geom_errorbarh(aes(xmin = ci99_lo, xmax = ci99_hi, color = estimate), height = 0, alpha = 0.8, size = .75) +
  geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi, color = estimate), height = 0, alpha = 0.8, size = 1) +
  geom_errorbarh(aes(xmin = ci80_lo, xmax = ci80_hi, color = estimate), height = 0, alpha = 0.8, size = 1.5) +
  geom_text(
    aes(
      x = ci99_hi,   # position at the upper CI bound
      label = estimate_fmt,
      color = estimate
    ),
    family = "Fira Sans",
    hjust = -0.2,    # nudge a bit further right
    size = 2.8
  ) +
  facet_grid(~ outcome_vars_labels_regtable) +
  scale_y_discrete(limits = rev) +
  scale_color_viridis_c(direction = -1, begin = 0.2, end = 0.8) +
  scale_x_continuous(
    breaks = c(0, 0.05, 0.10, 0.15),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0.05, .25)) # add more room on the right
  ) + 
  labs(x = "Effect of sharing sender gender (vs. not sharing)", 
       y = NULL,
       title = NULL,
       subtitle = NULL) +
  guides(color = "none") + 
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_markdown(),
        panel.ontop = TRUE,
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        panel.spacing.x = unit(0, "pt"),        # remove facet gaps horizontally
        panel.spacing.y = unit(0, "pt"),        # (in case of multiple rows)
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.text = element_markdown(face = "bold"))
ggsave("results/effects-h3sendergender-coefplot.png", width = 10, height = 3,   bg = "white", dpi = 300)



## Effects plot, pooled and by country

# order effects by estimate size

country_code_order <- 
  filter(h3white_models_country_tidy, response == "vig_perc_offensive2") %>%
  arrange(desc(estimate)) %>% pull(data)
country_sorted <- country_codes_df %>%
  slice(match(country_code_order, code)) %>%
  pull(country)
country_sorted <- c(country_sorted, "Pooled")

# combine pooled and country-specific estimates

h3white_models_plot_tidy <- bind_rows(
  h3white_models_pooled_tidy,
  h3white_models_country_tidy
) %>%
  left_join(country_codes_df, 
            by = c("data" = "code")) %>%
  left_join(vig_outcomes_df,
            by = c("response" = "outcome_vars")) %>%
  mutate(country = factor(country, levels = country_sorted),
         country = fct_relevel(country, "Pooled", after = Inf),
         outcome_vars_labels_regtable = factor(outcome_vars_labels_regtable, 
                                               levels = vig_outcomes_df$outcome_vars_labels_regtable)
  ) %>% 
  add_confints(levels = c(.80, .95, .99)) %>% # add confidence intervals
  mutate(estimate_fmt = sprintf("%+.1f", estimate * 100)) %>% # format estimates
  mutate(country = fct_recode(country,
                              "**Pooled**" = "Pooled"))

save(h3white_models_plot_tidy, file = "data/cooked/h3white_models_plot_tidy.RData")


# plot pooled and by country estimates

h3white_models_plot_tidy %>%
  ggplot(aes(x = estimate, y = country)) +
  geom_rect(
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = .5,
      ymax = 1.5
    ),
    fill = "#dfdfdf"
  ) +
  geom_vline(xintercept = seq(-.025, .15, .025), linewidth = 0.3, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(color = estimate), size = 1.5, alpha = 1) +
  geom_errorbarh(aes(xmin = ci99_lo, xmax = ci99_hi, color = estimate), height = 0, alpha = 0.8, size = .75) +
  geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi, color = estimate), height = 0, alpha = 0.8, size = 1) +
  geom_errorbarh(aes(xmin = ci80_lo, xmax = ci80_hi, color = estimate), height = 0, alpha = 0.8, size = 1.5) +
  geom_text(
    aes(
      x = ci99_hi,   # position at the upper CI bound
      label = estimate_fmt,
      color = estimate
    ),
    family = "Fira Sans",
    hjust = -0.2,    # nudge a bit further right
    size = 2.8
  ) +
  facet_grid(~ outcome_vars_labels_regtable) +
  scale_y_discrete(limits = rev) +
  scale_color_viridis_c(direction = -1, begin = 0.2, end = 0.8) +
  scale_x_continuous(
    breaks = c(0, 0.05, 0.10, 0.15),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0.05, .25)) # add more room on the right
  ) + 
  labs(x = "Effect of sharing sender skin tone (vs. not sharing)", 
       y = NULL,
       title = NULL,
       subtitle = NULL) +
  guides(color = "none") + 
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_markdown(),
        panel.ontop = TRUE,
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        panel.spacing.x = unit(0, "pt"),        # remove facet gaps horizontally
        panel.spacing.y = unit(0, "pt"),        # (in case of multiple rows)
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.text = element_markdown(face = "bold"))
ggsave("results/effects-h3senderwhite-coefplot.png", width = 10, height = 3,   bg = "white", dpi = 300)


# free H3 model objects (plot tidy tables saved to disk)

rm(list = c(
  "h3gender_models_pooled", "h3gender_models_country",
  "h3white_models_pooled", "h3white_models_country",
  "h3gender_models_by_message_category_pooled", "h3gender_models_by_message_category_country",
  "h3white_models_by_message_category_pooled", "h3white_models_by_message_category_country",
  "h3gender_models_by_resp_gender_pooled", "h3gender_models_by_resp_gender_country",
  "h3white_models_by_resp_white_pooled", "h3white_models_by_resp_white_country"
))
gc()




# H1* (not pre-registered): ideology match -----------

# generate model formulas

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ",
                "(1|resp_id) + (1|deck_id)")

# run mixed-effects models

h1ideology_models_pooled <- map(paste(fmlas, "+ target_resp_ideology_match + (1|resp_country2)"), lmer, data = data_survey_combined)

h1ideology_models_country <- list()
for (i in country_codes_chr) {
  h1ideology_models_country[[i]] <- map(paste(fmlas, "+ target_resp_ideology_match"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}

# run mixed-effects models, by message category

h1ideology_models_by_message_category_pooled <- map(paste(fmlas, "+ target_resp_ideology_match*sender_category + (1|resp_country2)"), lmer, data = data_survey_combined)

h1ideology_models_by_message_category_country <- list()
for (i in country_codes_chr) {
  h1ideology_models_by_message_category_country[[i]] <- map(paste(fmlas, "+ target_resp_ideology_match*sender_category"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}

# run mixed-effects models, by respondent ideology

h1ideology_models_by_resp_leftright_pooled <- map(paste(fmlas, "+ target_resp_ideology_match*leftright_cat + (1|resp_country2)"), lmer, data = data_survey_combined)

h1ideology_models_by_resp_leftright_country <- list()
for (i in country_codes_chr) {
  h1ideology_models_by_resp_leftright_country[[i]] <- map(paste(fmlas, "+ target_resp_ideology_match*leftright_cat"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}


# extract model estimates, pooled models

h1ideology_models_pooled_tidy <- models_tidy_contrast_fun(h1ideology_models_pooled, "target_resp_ideology_match", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1ideology_models_by_message_category_pooled_tidy <- models_tidy_contrast_fun(h1ideology_models_by_message_category_pooled, var_x = "target_resp_ideology_match", var_z = "sender_category", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1ideology_models_by_resp_leftright_pooled_tidy <- models_tidy_contrast_fun(h1ideology_models_by_resp_leftright_pooled, var_x = "target_resp_ideology_match", var_z = "leftright_cat", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")

h1ideology_models_pooled_tidy_comb <- bind_rows(
  h1ideology_models_pooled_tidy, 
  h1ideology_models_by_message_category_pooled_tidy,
  h1ideology_models_by_resp_leftright_pooled_tidy
)

models_df <- gt_prep_fun(h1ideology_models_pooled_tidy_comb, by = "response", arrange_by = NULL, varlabels_ref = "exp_varlabel_ref")
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))

# generate the gt table

gt_table_fun(models_df, 
             glance_df = NULL, 
             stats_df, 
             outcome_vars = vig_outcomes_df$outcome_vars,
             outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
             sample = "Pooled",
             varlabels_ref = "exp_varlabel_ref") %>% 
  gtsave_auto("results/table-h1ideology-models-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)


# extract model estimates, by country

h1ideology_models_country_tidy <- models_tidy_contrast_fun(h1ideology_models_country, "target_resp_ideology_match", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1ideology_models_by_message_category_country_tidy <- models_tidy_contrast_fun(h1ideology_models_by_message_category_country, var_x = "target_resp_ideology_match", var_z = "sender_category", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1ideology_models_by_resp_leftright_country_tidy <- models_tidy_contrast_fun(h1ideology_models_by_resp_leftright_country, var_x = "target_resp_ideology_match", var_z = "leftright_cat", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")

h1ideology_models_country_tidy_comb <- bind_rows(
  h1ideology_models_country_tidy, 
  h1ideology_models_by_message_category_country_tidy,
  h1ideology_models_by_resp_leftright_country_tidy
)

# country tables, by outcome

for(i in country_codes_chr) {
  models_df <- h1ideology_models_country_tidy_comb %>% filter(data == i) %>% gt_prep_fun(by = "response", arrange_by = NULL, varlabels_ref = "exp_varlabel_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df,
               glance_df = NULL,
               stats_df,
               outcome_vars = vig_outcomes_df$outcome_vars,
               outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
               sample = country_codes_df$country[country_codes_df$code == i],
               varlabels_ref = "exp_varlabel_ref") %>% 
    gtsave_auto(paste0("results/table-h1ideology-models-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}

# outcome tables, by country

for(i in outcome_vars) {
  models_df <- h1ideology_models_country_tidy_comb %>% filter(response == i) %>% gt_prep_fun(by = "data", arrange_by = NULL, varlabels_ref = "exp_varlabel_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df, 
               glance_df = NULL, 
               stats_df, 
               outcome_vars = country_codes_df$code[country_codes_df$sepcountry == TRUE],
               outcome_vars_labels = country_codes_df$country[country_codes_df$sepcountry == TRUE],
               sample = vig_outcomes_df$outcome_vars_labels_regtable[vig_outcomes_df$outcome_vars == i],
               spanners = FALSE,
               varlabels_ref = "exp_varlabel_ref") %>% 
    gtsave_auto(paste0("results/table-h1ideology-models-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}


## Effects plot, pooled and by country

# order effects by estimate size

country_code_order <- 
  filter(h1ideology_models_country_tidy, response == "vig_perc_offensive2") %>%
  arrange(desc(estimate)) %>% pull(data)
country_sorted <- country_codes_df %>%
  slice(match(country_code_order, code)) %>%
  pull(country)
country_sorted <- c(country_sorted, "Pooled")

# combine pooled and country-specific estimates

h1ideology_models_plot_tidy <- bind_rows(
  h1ideology_models_pooled_tidy,
  h1ideology_models_country_tidy
) %>%
  left_join(country_codes_df, 
            by = c("data" = "code")) %>%
  left_join(vig_outcomes_df,
            by = c("response" = "outcome_vars")) %>%
  mutate(country = factor(country, levels = country_sorted),
         country = fct_relevel(country, "Pooled", after = Inf),
         outcome_vars_labels_regtable = factor(outcome_vars_labels_regtable, 
                                               levels = vig_outcomes_df$outcome_vars_labels_regtable)
  ) %>% 
  add_confints(levels = c(.80, .95, .99)) %>% # add confidence intervals
  mutate(estimate_fmt = sprintf("%+.1f", estimate * 100)) %>% # format estimates
  mutate(country = fct_recode(country,
                              "**Pooled**" = "Pooled"))


# plot pooled and by country estimates

h1ideology_models_plot_tidy %>%
  ggplot(aes(x = estimate, y = country)) +
  geom_rect(
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = .5,
      ymax = 1.5
    ),
    fill = "#dfdfdf"
  ) +
  geom_vline(xintercept = seq(-.025, .15, .025), linewidth = 0.3, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(color = estimate), size = 1.5, alpha = 1) +
  geom_errorbarh(aes(xmin = ci99_lo, xmax = ci99_hi, color = estimate), height = 0, alpha = 0.8, size = .75) +
  geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi, color = estimate), height = 0, alpha = 0.8, size = 1) +
  geom_errorbarh(aes(xmin = ci80_lo, xmax = ci80_hi, color = estimate), height = 0, alpha = 0.8, size = 1.5) +
  geom_text(
    aes(
      x = ci99_hi,   # position at the upper CI bound
      label = estimate_fmt,
      color = estimate
    ),
    family = "Fira Sans",
    hjust = -0.2,    # nudge a bit further right
    size = 2.8
  ) +
  facet_grid(~ outcome_vars_labels_regtable) +
  scale_y_discrete(limits = rev) +
  scale_color_viridis_c(direction = -1, begin = 0.2, end = 0.8) +
  scale_x_continuous(
    breaks = c(0, 0.05, 0.10, 0.15),
    labels = label_percent(accuracy = 1),
    expand = expansion(mult = c(0.05, .25)) # add more room on the right
  ) + 
  labs(x = "Effect of ideological match with target issue position (vs. mismatch)", 
       y = NULL,
       title = NULL,
       subtitle = NULL) +
  guides(color = "none") + 
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_markdown(),
        panel.ontop = TRUE,
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        panel.spacing.x = unit(0, "pt"),        # remove facet gaps horizontally
        panel.spacing.y = unit(0, "pt"),        # (in case of multiple rows)
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.text = element_markdown(face = "bold"))
ggsave("results/effects-h1ideology-coefplot.png", width = 10, height = 3,   bg = "white", dpi = 300)






# Myside-vs-ideology horse-race column for Figure 4 (revision; exploratory, not pre-registered) ----
# Issue-stance agreement coefficient estimated jointly with ideology match, i.e. myside bias net of left-right ideology. Reuses the same pipeline as h1_models_plot_tidy so it forms a second column of the combined coefficient plot.

fmlas <- paste0(outcome_vars, " ~ 1 + (1|resp_id) + (1|deck_id)")

h1hr_models_pooled <- map(paste(fmlas, "+ target_resp_issue_agreement + target_resp_ideology_match + (1|resp_country2)"), lmer, data = data_survey_combined)

h1hr_models_country <- list()
for (i in country_codes_chr) {
  h1hr_models_country[[i]] <- map(paste(fmlas, "+ target_resp_issue_agreement + target_resp_ideology_match"), lmer, data = filter(data_survey_combined, resp_country2 == i))
}

h1hr_models_pooled_tidy  <- models_tidy_contrast_fun(h1hr_models_pooled, "target_resp_issue_agreement", by_vars = NULL, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")
h1hr_models_country_tidy <- models_tidy_contrast_fun(h1hr_models_country, "target_resp_issue_agreement", by_vars = country_codes_chr, outcome_vars = outcome_vars, labels_df = exp_covars_df, label_var = "exp_variable")

h1hr_models_plot_tidy <- bind_rows(h1hr_models_pooled_tidy, h1hr_models_country_tidy) %>%
  left_join(country_codes_df, by = c("data" = "code")) %>%
  left_join(vig_outcomes_df, by = c("response" = "outcome_vars")) %>%
  mutate(country = factor(country, levels = country_sorted),
         country = fct_relevel(country, "Pooled", after = Inf),
         outcome_vars_labels_regtable = factor(outcome_vars_labels_regtable,
                                               levels = vig_outcomes_df$outcome_vars_labels_regtable)) %>%
  add_confints(levels = c(.80, .95, .99)) %>%
  mutate(estimate_fmt = sprintf("%+.1f", estimate * 100)) %>%
  mutate(country = fct_recode(country, "**Pooled**" = "Pooled")) %>%
  mutate(is_horserace = TRUE)

save(h1hr_models_plot_tidy, file = "data/cooked/h1hr_models_plot_tidy.RData")


# free ideology and horse-race model objects (plot tidy tables saved to disk)

rm(list = c(
  "h1ideology_models_pooled", "h1ideology_models_country",
  "h1ideology_models_by_message_category_pooled", "h1ideology_models_by_message_category_country",
  "h1ideology_models_by_resp_leftright_pooled", "h1ideology_models_by_resp_leftright_country",
  "h1hr_models_pooled", "h1hr_models_country"
))
gc()


# combined H1-H2-H3 effects plot ------------

# load and combine the saved plot tidy tables (so this figure can be built independently)
load("data/cooked/h1_models_plot_tidy.RData")     # h1_models_plot_tidy
load("data/cooked/h1hr_models_plot_tidy.RData")   # h1hr_models_plot_tidy
load("data/cooked/h2gender_models_plot_tidy.RData")
load("data/cooked/h2white_models_plot_tidy.RData")
load("data/cooked/h3gender_models_plot_tidy.RData")
load("data/cooked/h3white_models_plot_tidy.RData")

h1_3_models_plot_tidy <-
  bind_rows(
    h1_models_plot_tidy,
    h1hr_models_plot_tidy,
    h2gender_models_plot_tidy,
    h2white_models_plot_tidy,
    h3gender_models_plot_tidy,
    h3white_models_plot_tidy
  )
# column facet labels (horizontal strips): use <br> for line breaks
h1_3_models_plot_tidy$exp_varlabel_short <- h1_3_models_plot_tidy$exp_varlabel %>% str_replace("Alignment with ", "") %>% str_replace(" ", "<br>") %>% firstup()
h1_3_models_plot_tidy <- h1_3_models_plot_tidy %>%
  mutate(
    exp_varlabel_short = if_else(!is.na(is_horserace) & is_horserace,
                                 "Target issue position<br>(adj. for ideology match)",
                                 exp_varlabel_short),
    exp_varlabel_short = factor(exp_varlabel_short,
      levels = c("Target<br>issue position",
                 "Target issue position<br>(adj. for ideology match)",
                 "Target<br>gender", "Target<br>skin tone",
                 "Sender<br>gender", "Sender<br>skin tone"))
  )


# flipped layout: outcomes as rows, predictor categories as columns

h1_3_models_plot_tidy %>%
  # row facet labels (rotated strips): element_markdown breaks these on \n, not <br>
  mutate(outcome_vars_labels_regtable = factor(
           str_replace_all(as.character(outcome_vars_labels_regtable), "<br>", "\n"),
           levels = str_replace_all(as.character(vig_outcomes_df$outcome_vars_labels_regtable), "<br>", "\n"))) %>%
  ggplot(aes(x = estimate, y = country)) +
  geom_rect(
    aes(
      xmin = -Inf, xmax = Inf,
      ymin = .5,
      ymax = 1.5
    ),
    fill = "#dfdfdf"
  ) +
  geom_vline(xintercept = seq(-.1, .15, .025), linewidth = 0.3, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(color = estimate), size = 1.5, alpha = 1) +
  geom_errorbarh(aes(xmin = ci99_lo, xmax = ci99_hi, color = estimate), height = 0, alpha = 0.8, size = .5) +
  geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi, color = estimate), height = 0, alpha = 0.8, size = 1) +
  geom_text(
    aes(
      x = ci99_hi,   # position at the upper CI bound
      label = estimate_fmt,
      color = estimate
    ),
    family = "Fira Sans",
    hjust = -0.2,    # nudge a bit further right
    size = 2.8
  ) +
  facet_grid(outcome_vars_labels_regtable ~ exp_varlabel_short, switch = "y") +
  scale_y_discrete(limits = rev, position = "right") +
  scale_color_gradientn(
    colors = c("#2c7bb6", "#4c4c4c", "#d7191c"),
    values = scales::rescale(c(-1, 0, 1), from = c(-1, 1)), # fixed anchors
    limits = c(-.2, .2)
  ) +
  scale_x_continuous(
    breaks = c(-.10, -.05, 0, 0.05, 0.10, 0.15),
    labels = label_percent(accuracy = 1, suffix = ""),
    expand = expansion(mult = c(0.00, .15)) # add more room on the right
  ) + 
  labs(x = "Estimated effect of match with column category on row outcome in percentage points", 
       y = NULL,
       title = NULL, # Effect of respondent match with...
       subtitle = NULL) +
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
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.text = element_markdown(face = "bold"))
ggsave("results/effects-h1-3-coefplot-flipped.png", width = 10, height = 10, bg = "white", dpi = 300)


# latex table of Figure 4 effects with BH-FDR p-values ------------

# report only the issue-stance alignment effect (Figure 4, column 1), with outcome as a leading column
fig4_country_levels <- str_replace_all(levels(h1_3_models_plot_tidy$country), "\\*\\*", "")
fig4_sample_levels  <- c("Pooled", setdiff(fig4_country_levels, "Pooled"))

fig4_tab_df <- h1_3_models_plot_tidy %>%
  mutate(effect = str_replace_all(as.character(exp_varlabel_short), "<br>", " "),
         sample = factor(str_replace_all(as.character(country), "\\*\\*", ""),
                         levels = fig4_sample_levels)) %>%
  filter(effect == "Target issue position") %>%
  arrange(sample) %>%
  dplyr::select(response, sample, estimate, std.error)

effects_bh_table(
  fig4_tab_df, id_cols = c("sample"), id_names = c("Sample"),
  caption = paste0("Model estimates underlying Figure~\\ref{fig:effects-h1-3-coefplot-flipped} (issue-stance alignment): ",
                   "effect of respondent--target issue-stance alignment (myside bias) on each outcome, ",
                   "pooled and by country. Estimates in percentage points; ",
                   "Benjamini--Hochberg FDR-adjusted $p$-values computed within each outcome."),
  label = "effects-h1-3-bh",
  file = "results/table-effects-h1-3-bh.tex",
  group_by_outcome = FALSE
)




# H1 heterogeneity plot ----

# load the saved subgroup tidy tables (so this plot can be made independently)
load("data/cooked/h1_models_by_message_category_pooled_tidy.RData")
load("data/cooked/h1_models_by_message_topic_pooled_tidy.RData")
load("data/cooked/h1_models_by_resp_leftright_pooled_tidy.RData")
load("data/cooked/h1_models_by_resp_gender_pooled_tidy.RData")
load("data/cooked/h1_models_by_resp_silencing_pooled_tidy.RData")

# collect data
h1_models_pooled_tidy_comb <- bind_rows(
  h1_models_by_message_category_pooled_tidy,
  h1_models_by_message_topic_pooled_tidy,
  h1_models_by_resp_leftright_pooled_tidy,
  h1_models_by_resp_gender_pooled_tidy,
  h1_models_by_resp_silencing_pooled_tidy
) %>%
  left_join(vig_outcomes_df,
            by = c("response" = "outcome_vars")) %>% 
  add_confints(levels = c(.80, .95, .99)) %>% 
  mutate(estimate_fmt = sprintf("%+.1f", estimate * 100))

# fix labels
h1_models_pooled_tidy_comb$subgroup <- str_extract(h1_models_pooled_tidy_comb$exp_varlabel, ", .*") %>% str_replace(", by ", "") %>% firstup() %>% str_replace(" ", "\n") %>% str_replace("Respondent", "Resp.") %>% str_replace(" score", "")
h1_models_pooled_tidy_comb$subgroup_value <- str_extract(h1_models_pooled_tidy_comb$label, "\\| .*") %>% str_replace("\\| ", "") %>% firstup()
h1_models_pooled_tidy_comb$outcome_vars_labels_regtable <- factor(h1_models_pooled_tidy_comb$outcome_vars_labels_regtable, 
                                               levels = vig_outcomes_df$outcome_vars_labels_regtable)
h1_models_pooled_tidy_comb$subgroup_value

# order effects by estimate size

subgroup_order <- 
  filter(h1_models_pooled_tidy_comb, response == "vig_perc_offensive2") %>%
  arrange(desc(estimate)) %>% pull(subgroup_value)
h1_models_pooled_tidy_comb$subgroup_value <- factor(h1_models_pooled_tidy_comb$subgroup_value, levels = subgroup_order)


# plot
h1_models_pooled_tidy_comb %>%
  ggplot(aes(x = estimate, y = subgroup_value)) +
  geom_vline(xintercept = seq(-.05, .20, .025), linewidth = 0.3, color = "#bfbfbf") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_point(aes(color = estimate), size = 1.5, alpha = 1) +
  geom_errorbarh(aes(xmin = ci99_lo, xmax = ci99_hi, color = estimate), height = 0, alpha = 0.8, size = .5) +
  geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi, color = estimate), height = 0, alpha = 0.8, size = 1) +
  geom_text(
    aes(
      x = ci99_hi,   # position at the upper CI bound
      label = estimate_fmt,
      color = estimate
    ),
    family = "Fira Sans",
    hjust = -0.2,    # nudge a bit further right
    size = 2.8
  ) +
  facet_grid(subgroup ~ outcome_vars_labels_regtable, scales = "free_y", space = "free_y", switch = "y") +
  scale_y_discrete(limits = rev, position = "right") +
  scale_color_gradientn(
    colors = c("#2c7bb6", "#4c4c4c", "#d7191c"),
    values = scales::rescale(c(-1, 0, 1), from = c(-1, 1)), # fixed anchors
    limits = c(-.2, .2)
    ) +
  scale_x_continuous(
    breaks = c(-.05, 0, 0.05, 0.10, 0.15, 0.20),
    labels = label_percent(accuracy = 1, suffix = ""),
    expand = expansion(mult = c(0.00, .2)) # add more room on the right
  ) + 
  labs(x = "Estimated myside bias effect in percentage points", 
       y = NULL,
       title = NULL, # "Effect of match with target issue position (myside bias), by message/respondent subgroup"
       subtitle = NULL) + 
  guides(color = "none") + 
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        title = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_markdown(),
        axis.title.x = element_markdown(face = "plain"),
        panel.ontop = TRUE,
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        panel.spacing.x = unit(5, "pt"),
        panel.spacing.y = unit(5, "pt"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.text = element_markdown(face = "bold"))
ggsave("results/effects-h1-coefplot-heterogeneity.png", width = 10, height = 6, bg = "white", dpi = 300)


# latex table of Figure 5 effects with BH-FDR p-values ------------

fig5_tab_df <- h1_models_pooled_tidy_comb %>%
  mutate(subgroup_clean = str_replace_all(as.character(subgroup), "\n", " "),
         level = as.character(subgroup_value)) %>%
  arrange(subgroup_clean, subgroup_value) %>%
  dplyr::select(response, subgroup_clean, level, estimate, std.error)

effects_bh_table(
  fig5_tab_df, id_cols = c("subgroup_clean", "level"), id_names = c("Subgroup", "Level"),
  caption = paste0("Model estimates underlying Figure~\\ref{fig:effects-h1-coefplot-heterogeneity}: ",
                   "myside bias (respondent--target issue-stance alignment) estimated within each message and ",
                   "respondent subgroup. Estimates in percentage points; ",
                   "Benjamini--Hochberg FDR-adjusted $p$-values computed within each outcome."),
  label = "effects-h1-heterogeneity-bh",
  file = "results/table-effects-h1-heterogeneity-bh.tex"
)




