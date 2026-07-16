# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")


# load data ---------------------------

load("data/cooked/data_survey_combined.RData")
vig_covars_df_main <- filter(vig_covars_df, main_model == TRUE)


# Vignette attributes, pooled ---------------

# generate model formulas

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ", 
                paste0(vig_covars_df_main$vig_attribute_varnames, collapse = " + "), " + vig_pos_cat", " + ",
                "(1|resp_id) + (1|deck_id)")

# run mixed-effects models, pooled

acme_models_pooled <- map(paste(fmlas, "+ (1|resp_country2)"), lmer, data = data_survey_combined)
names(acme_models_pooled) <- outcome_vars


# extract model AMCE estimates

tidy_df <- models_tidy_fun(acme_models_pooled, vig_covars_df_main, "vig_attribute_varnames")
tidy_df <- filter(tidy_df, variable != "vig_pos_cat")
models_df <- gt_prep_fun(tidy_df, by = "response", arrange_by = "estimate_vig_perc_offensive2", varlabels_ref = "vig_attribute_varlabels_ref")
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
glance_df <- glance_parse_fun(acme_models_pooled, by = outcome_vars, varlabels_ref = "vig_attribute_varlabels_ref") 

# generate the gt table

gt_table_fun(models_df, 
         glance_df, 
         stats_df, 
         outcome_vars = vig_outcomes_df$outcome_vars,
         outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
         sample = "Pooled",
         varlabels_ref = "vig_attribute_varlabels_ref") %>% 
  gtsave_auto("results/table-acme-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)


# extract MMs

tidy_mm_df <- models_tidy_mms_fun(
  models     = acme_models_pooled,                       
  predictors = c(vig_covars_df_main$vig_attribute_varnames, "vig_pos_cat"),
  data       = data_survey_combined,      
  labels_df = vig_covars_df_main,
  label_var = "vig_attribute_varnames"
)
models_mm_df <- gt_prep_fun(tidy_mm_df, by = "response", arrange_by = "estimate_vig_perc_offensive2", varlabels_ref = "vig_attribute_varlabels")

# generate the gt table for MMs

gt_table_fun(models_mm_df, 
             rename(glance_df, vig_attribute_varlabels = vig_attribute_varlabels_ref), 
             stats_df, 
             outcome_vars = vig_outcomes_df$outcome_vars,
             outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
             sample = "Pooled",
             varlabels_ref = "vig_attribute_varlabels",
             estimates = "mm") %>% 
  gtsave_auto("results/table-mm-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)




# Vignette attributes, by country ---------------

# run mixed-effects models, by country (fmlas from pooled section)

acme_models_country <- list()
for (i in country_codes_chr) {
  acme_models_country[[i]] <- map(fmlas, lmer, data = filter(data_survey_combined, resp_country2 == i))
  names(acme_models_country[[i]]) <- outcome_vars
}

# extract model AMCE estimates

tidy_df <- models_tidy_fun(acme_models_country, vig_covars_df_main, "vig_attribute_varnames", by_vars = country_codes_chr, outcome_vars = outcome_vars)

# country tables, by outcome

for(i in country_codes_chr) {
  models_df <- tidy_df %>% filter(data == i) %>% gt_prep_fun(by = "response", arrange_by = "estimate_vig_perc_offensive2", varlabels_ref = "vig_attribute_varlabels_ref")
  glance_df <- glance_parse_fun(acme_models_country[[i]], country_re = FALSE, by = outcome_vars, varlabels_ref = "vig_attribute_varlabels_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df,
           glance_df,
           stats_df,
           outcome_vars = vig_outcomes_df$outcome_vars,
           outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
           sample = country_codes_df$country[country_codes_df$code == i],
           varlabels_ref = "vig_attribute_varlabels_ref") %>% 
    gtsave_auto(paste0("results/table-acme-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}


# outcome tables, by country

for(i in outcome_vars) {
  models_df <- tidy_df %>% filter(response == i) %>% gt_prep_fun(by = "data", arrange_by = "estimate_bra", varlabels_ref = "vig_attribute_varlabels_ref")
  glance_df <- glance_parse_fun(transpose(acme_models_country)[[which(outcome_vars == i)]], country_re = FALSE, by = country_codes_chr, varlabels_ref = "vig_attribute_varlabels_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df, 
           glance_df, 
           stats_df, 
           outcome_vars = country_codes_df$code[country_codes_df$sepcountry == TRUE],
           outcome_vars_labels = country_codes_df$country[country_codes_df$sepcountry == TRUE],
           sample = vig_outcomes_df$outcome_vars_labels_regtable[vig_outcomes_df$outcome_vars == i],
           spanners = FALSE,
           varlabels_ref = "vig_attribute_varlabels_ref") %>% 
    gtsave_auto(paste0("results/table-acme-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}


# extract MMs

tidy_mm_list <- list()
for (i in country_codes_chr) {
tidy_mm_list[[i]] <- models_tidy_mms_fun(
  models     = acme_models_country[[i]],                       
  predictors = c(vig_covars_df_main$vig_attribute_varnames, "vig_pos_cat"),
  data       = data_survey_combined,      
  labels_df = vig_covars_df_main,
  label_var = "vig_attribute_varnames"
)
tidy_mm_list[[i]]$data <- i
}
tidy_mm_df <- bind_rows(tidy_mm_list)

# generate the gt table for MMs

for(i in country_codes_chr) {
  models_mm_df <- tidy_mm_df %>% filter(data == i) %>% gt_prep_fun(by = "response", arrange_by = "estimate_vig_perc_offensive2", varlabels_ref = "vig_attribute_varlabels")
  glance_df <- glance_parse_fun(acme_models_country[[i]], country_re = FALSE, by = outcome_vars, varlabels_ref = "vig_attribute_varlabels_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_mm_df, 
               rename(glance_df, vig_attribute_varlabels = vig_attribute_varlabels_ref), 
               stats_df, 
               outcome_vars = vig_outcomes_df$outcome_vars,
               outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
               sample = country_codes_df$country[country_codes_df$code == i],
               varlabels_ref = "vig_attribute_varlabels",
               estimates = "mm") %>% 
    gtsave_auto(paste0("results/table-mm-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}




# Respondent characteristics, pooled ---------------

# generate model formulas

resp_covars <- filter(resp_covars_df, regmodel == TRUE) %>% pull(resp_variable)

fmlas <- paste0(outcome_vars, 
                        " ~ 1 + ", 
                        paste0(resp_covars, collapse = " + "), " + ", 
                paste0(vig_covars_df_main$vig_attribute_varnames, collapse = " + "), " + ",
                        "(1|resp_id) + (1|deck_id)")



# run mixed-effects models, pooled

respondent_models_pooled <- map(paste(fmlas, "+ (1|resp_country2)"), lmer, data = data_survey_combined)
names(respondent_models_pooled) <- outcome_vars

# extract model AMCE estimates

tidy_df <- models_tidy_fun(respondent_models_pooled, resp_covars_df, "resp_variable", drop_vig_vars = TRUE)
models_df <- gt_prep_fun(tidy_df, by = "response", arrange_by = "estimate_vig_perc_offensive2", varlabels_ref = "resp_varlabel_ref")
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
glance_df <- glance_parse_fun(respondent_models_pooled, by = outcome_vars, varlabels_ref = "resp_varlabel_ref") 

# generate the gt table

gt_table_fun(models_df, 
         glance_df, 
         stats_df, 
         outcome_vars = vig_outcomes_df$outcome_vars,
         outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
         sample = "Pooled",
         varlabels_ref = "resp_varlabel_ref") %>% 
  gtsave_auto("results/table-respmodels-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)


# extract MMs

tidy_mm_df <- models_tidy_mms_fun(
  models     = respondent_models_pooled,                       
  predictors = resp_covars,
  data       = data_survey_combined,      
  labels_df = resp_covars_df,
  label_var = "resp_variable"
)
models_mm_df <- gt_prep_fun(tidy_mm_df, by = "response", arrange_by = NULL, varlabels_ref = "resp_varlabel")

# generate the gt table for MMs

gt_table_fun(models_mm_df, 
             rename(glance_df, resp_varlabel = resp_varlabel_ref), 
             stats_df, 
             outcome_vars = vig_outcomes_df$outcome_vars,
             outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
             sample = "Pooled",
             varlabels_ref = "resp_varlabel",
             estimates = "mm") %>% 
  gtsave_auto("results/table-respmodels-mm-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)



# Respondent characteristics, by country ---------------

# run mixed-effects models, by country (fmlas from pooled section)

respondent_models_country <- list()
for (i in country_codes_chr) {
  respondent_models_country[[i]] <- map(fmlas, lmer, data = filter(data_survey_combined, resp_country2 == i))
  names(respondent_models_country[[i]]) <- outcome_vars
}

# extract model AMCE estimates

tidy_df <- models_tidy_fun(respondent_models_country, resp_covars_df, "resp_variable", by_vars = country_codes_chr, outcome_vars = outcome_vars, drop_vig_vars = TRUE)

# country tables, by outcome

for(i in country_codes_chr) {
  models_df <- tidy_df %>% filter(data == i) %>% gt_prep_fun(by = "response", arrange_by = NULL, varlabels_ref = "resp_varlabel_ref")
  glance_df <- glance_parse_fun(respondent_models_country[[i]], country_re = FALSE, by = outcome_vars, varlabels_ref = "resp_varlabel_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df,
           glance_df,
           stats_df,
           outcome_vars = vig_outcomes_df$outcome_vars,
           outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
           sample = country_codes_df$country[country_codes_df$code == i],
           varlabels_ref = "resp_varlabel_ref") %>% 
    gtsave_auto(paste0("results/table-respmodels-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}

# outcome tables, by country

for(i in outcome_vars) {
  models_df <- tidy_df %>% filter(response == i) %>% gt_prep_fun(by = "data", arrange_by = NULL, varlabels_ref = "resp_varlabel_ref")
  glance_df <- glance_parse_fun(transpose(respondent_models_country)[[which(outcome_vars == i)]], country_re = FALSE, by = country_codes_chr, varlabels_ref = "resp_varlabel_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df, 
           glance_df, 
           stats_df, 
           outcome_vars = country_codes_df$code[country_codes_df$sepcountry == TRUE],
           outcome_vars_labels = country_codes_df$country[country_codes_df$sepcountry == TRUE],
           sample = vig_outcomes_df$outcome_vars_labels_regtable[vig_outcomes_df$outcome_vars == i],
           spanners = FALSE,
           varlabels_ref = "resp_varlabel_ref") %>% 
    gtsave_auto(paste0("results/table-respmodels-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}


# extract MMs

tidy_mm_list <- list()
for (i in country_codes_chr) {
  tidy_mm_list[[i]] <- models_tidy_mms_fun(
    models     = respondent_models_country[[i]],                       
    predictors = resp_covars,
    data       = data_survey_combined,      
    labels_df = resp_covars_df,
    label_var = "resp_variable"
  )
  tidy_mm_list[[i]]$data <- i
}
tidy_mm_df <- bind_rows(tidy_mm_list)

# generate the gt table for MMs

for(i in country_codes_chr) {
  models_mm_df <- tidy_mm_df %>% filter(data == i) %>% gt_prep_fun(by = "response", arrange_by = NULL, varlabels_ref = "resp_varlabel")
  glance_df <- glance_parse_fun(respondent_models_country[[i]], country_re = FALSE, by = outcome_vars, varlabels_ref = "resp_varlabel")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_mm_df, 
               glance_df, 
               stats_df, 
               outcome_vars = vig_outcomes_df$outcome_vars,
               outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
               sample = country_codes_df$country[country_codes_df$code == i],
               varlabels_ref = "resp_varlabel",
               estimates = "mm") %>% 
    gtsave_auto(paste0("results/table-respmodels-mm-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}





# Vignette position, pooled ---------------

# generate model formulas

fmlas <- paste0(outcome_vars, 
                " ~ 1 + vig_pos_cat + sender_category + (1|resp_id) + (1|deck_id)")

# run mixed-effects models, pooled

vignette_position_models_pooled <- map(paste(fmlas, "+ (1|resp_country2)"), lmer, data = data_survey_combined)
names(vignette_position_models_pooled) <- outcome_vars

# extract model AMCE estimates

tidy_df <- models_tidy_fun(vignette_position_models_pooled, vig_covars_df_vig_pos, "vig_attribute_varnames", drop_vig_pos = FALSE)
models_df <- gt_prep_fun(tidy_df, by = "response", varlabels_ref = "vig_attribute_varlabels_ref", arrange_by = NULL)
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
glance_df <- glance_parse_fun(vignette_position_models_pooled, by = outcome_vars, varlabels_ref = "vig_attribute_varlabels_ref") 

# generate the gt table

gt_table_fun(models_df, 
             glance_df, 
             stats_df, 
             outcome_vars = vig_outcomes_df$outcome_vars,
             outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
             sample = "Pooled",
             varlabels_ref = "vig_attribute_varlabels_ref") %>% 
  gtsave_auto("results/table-acme-vigpositions-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)


# Vignette position, by country ---------------

# run mixed-effects models, by country (fmlas from pooled section)

vignette_position_models_country <- list()
for (i in country_codes_chr) {
  vignette_position_models_country[[i]] <- map(fmlas, lmer, data = filter(data_survey_combined, resp_country2 == i))
  names(vignette_position_models_country[[i]]) <- outcome_vars
}

# extract model AMCE estimates

tidy_df <- models_tidy_fun(vignette_position_models_country, vig_covars_df_vig_pos, "vig_attribute_varnames", by_vars = country_codes_chr, outcome_vars = outcome_vars, drop_vig_pos = FALSE)

# country tables, by outcome

for(i in country_codes_chr) {
  models_df <- tidy_df %>% filter(data == i) %>% gt_prep_fun(by = "response", arrange_by = NULL, varlabels_ref = "vig_attribute_varlabels_ref")
  glance_df <- glance_parse_fun(vignette_position_models_country[[i]], country_re = FALSE, by = outcome_vars, varlabels_ref = "vig_attribute_varlabels_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df,
               glance_df,
               stats_df,
               outcome_vars = vig_outcomes_df$outcome_vars,
               outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
               sample = country_codes_df$country[country_codes_df$code == i],
               varlabels_ref = "vig_attribute_varlabels_ref") %>% 
    gtsave_auto(paste0("results/table-acme-vigpositions-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}

# outcome tables, by country

for(i in outcome_vars) {
  models_df <- tidy_df %>% filter(response == i) %>% gt_prep_fun(by = "data", arrange_by = NULL, varlabels_ref = "vig_attribute_varlabels_ref")
  glance_df <- glance_parse_fun(transpose(vignette_position_models_country)[[which(outcome_vars == i)]], country_re = FALSE, by = country_codes_chr, varlabels_ref = "vig_attribute_varlabels_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df, 
               glance_df, 
               stats_df, 
               outcome_vars = country_codes_df$code[country_codes_df$sepcountry == TRUE],
               outcome_vars_labels = country_codes_df$country[country_codes_df$sepcountry == TRUE],
               sample = vig_outcomes_df$outcome_vars_labels_regtable[vig_outcomes_df$outcome_vars == i],
               spanners = FALSE,
               varlabels_ref = "vig_attribute_varlabels_ref") %>% 
    gtsave_auto(paste0("results/table-acme-vigpositions-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}






# Target avatars, pooled ------

# generate model formulas

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ", 
                paste0(c("target_avatar", str_subset(vig_covars_df_main$vig_attribute_varnames, "target_ethnicity|target_gender", negate = TRUE)), collapse = " + "), "+ vig_pos_cat + (1|resp_id) + (1|deck_id) + (1|resp_country2)")

# run mixed-effects models, pooled

target_avatar_models_pooled <- map(fmlas, lmer, data = data_survey_combined)
names(target_avatar_models_pooled) <- outcome_vars

# extract model AMCE estimates

tidy_df <- models_tidy_fun(target_avatar_models_pooled, vig_covars_df_targetavatar, "vig_attribute_varnames")
models_df <- gt_prep_fun(tidy_df, arrange_by = "estimate_vig_perc_offensive2", by = "response", varlabels_ref = "vig_attribute_varlabels_ref")
models_df <- filter(models_df, vig_attribute_varlabels_ref == "Target avatar") # NEW
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
glance_df <- glance_parse_fun(target_avatar_models_pooled, by = outcome_vars, varlabels_ref = "vig_attribute_varlabels_ref") 

# generate the gt table

gt_table_fun(models_df, 
             glance_df, 
             stats_df, 
             outcome_vars = vig_outcomes_df$outcome_vars,
             outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
             sample = "Pooled",
             varlabels_ref = "vig_attribute_varlabels_ref",
             avatars = TRUE) %>%
  gtsave_auto("results/table-acme-targetavatar-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)


# Target avatars, by country ------

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ", 
                paste0(c("target_avatar", str_subset(vig_covars_df_main$vig_attribute_varnames, "target_ethnicity|target_gender", negate = TRUE)), collapse = " + "), "+ vig_pos_cat + (1|resp_id) + (1|deck_id)")


target_avatar_models_country <- list()
for (i in country_codes_chr) {
  target_avatar_models_country[[i]] <- map(fmlas, lmer, data = filter(data_survey_combined, resp_country2 == i))
  names(target_avatar_models_country[[i]]) <- outcome_vars
}

# extract model AMCE estimates

tidy_df <- models_tidy_fun(target_avatar_models_country, vig_covars_df_targetavatar, "vig_attribute_varnames", by_vars = country_codes_chr, outcome_vars = outcome_vars)

# country tables, by outcome

for(i in country_codes_chr) {
  models_df <- tidy_df %>% filter(data == i) %>% gt_prep_fun(by = "response", arrange_by = "estimate_vig_perc_offensive2", varlabels_ref = "vig_attribute_varlabels_ref")
  models_df <- filter(models_df, vig_attribute_varlabels_ref == "Target avatar") # NEW
  glance_df <- glance_parse_fun(target_avatar_models_country[[i]], country_re = FALSE, by = outcome_vars, varlabels_ref = "vig_attribute_varlabels_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df,
               glance_df,
               stats_df,
               outcome_vars = vig_outcomes_df$outcome_vars,
               outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
               sample = country_codes_df$country[country_codes_df$code == i],
               varlabels_ref = "vig_attribute_varlabels_ref",
               avatars = TRUE) %>% 
    gtsave_auto(paste0("results/table-acme-targetavatar-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}


# outcome tables, by country

for(i in outcome_vars) {
  models_df <- tidy_df %>% filter(response == i) %>% gt_prep_fun(by = "data", arrange_by = "estimate_bra", varlabels_ref = "vig_attribute_varlabels_ref")
  models_df <- filter(models_df, vig_attribute_varlabels_ref == "Target avatar") # NEW
  glance_df <- glance_parse_fun(transpose(target_avatar_models_country)[[which(outcome_vars == i)]], country_re = FALSE, by = country_codes_chr, varlabels_ref = "vig_attribute_varlabels_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df, 
               glance_df, 
               stats_df, 
               outcome_vars = country_codes_df$code[country_codes_df$sepcountry == TRUE],
               outcome_vars_labels = country_codes_df$country[country_codes_df$sepcountry == TRUE],
               sample = vig_outcomes_df$outcome_vars_labels_regtable[vig_outcomes_df$outcome_vars == i],
               spanners = FALSE,
               varlabels_ref = "vig_attribute_varlabels_ref",
               avatars = TRUE) %>% 
    gtsave_auto(paste0("results/table-acme-targetavatar-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}



# Sender avatars, pooled ------

# generate model formulas

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ", 
                paste0(c("sender_avatar", str_subset(vig_covars_df_main$vig_attribute_varnames, "sender_ethnicity|sender_gender", negate = TRUE)), collapse = " + "), "+ vig_pos_cat + (1|resp_id) + (1|deck_id) + (1|resp_country2)")

# run mixed-effects models, pooled

sender_avatar_models_pooled <- map(fmlas, lmer, data = data_survey_combined)
names(sender_avatar_models_pooled) <- outcome_vars

# extract model AMCE estimates

tidy_df <- models_tidy_fun(sender_avatar_models_pooled, vig_covars_df_senderavatar, "vig_attribute_varnames")
models_df <- gt_prep_fun(tidy_df, arrange_by = "estimate_vig_perc_offensive2", by = "response", varlabels_ref = "vig_attribute_varlabels_ref")
models_df <- filter(models_df, vig_attribute_varlabels_ref == "Sender avatar") # NEW
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
glance_df <- glance_parse_fun(sender_avatar_models_pooled, by = outcome_vars, varlabels_ref = "vig_attribute_varlabels_ref") 

# generate the gt table

gt_table_fun(models_df, 
                     glance_df, 
                     stats_df, 
                     outcome_vars = vig_outcomes_df$outcome_vars,
                     outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
                     sample = "Pooled",
                     varlabels_ref = "vig_attribute_varlabels_ref",
                     avatars = TRUE) %>%
  gtsave_auto("results/table-acme-senderavatar-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)


# Sender avatars, by country ------

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ", 
                paste0(c("sender_avatar", str_subset(vig_covars_df_main$vig_attribute_varnames, "sender_ethnicity|sender_gender", negate = TRUE)), collapse = " + "), "+ vig_pos_cat + (1|resp_id) + (1|deck_id)")


sender_avatar_models_country <- list()
for (i in country_codes_chr) {
  sender_avatar_models_country[[i]] <- map(fmlas, lmer, data = filter(data_survey_combined, resp_country2 == i))
  names(sender_avatar_models_country[[i]]) <- outcome_vars
}

# extract model AMCE estimates

tidy_df <- models_tidy_fun(sender_avatar_models_country, vig_covars_df_senderavatar, "vig_attribute_varnames", by_vars = country_codes_chr, outcome_vars = outcome_vars)

# country tables, by outcome

for(i in country_codes_chr) {
  models_df <- tidy_df %>% filter(data == i) %>% gt_prep_fun(by = "response", arrange_by = "estimate_vig_perc_offensive2", varlabels_ref = "vig_attribute_varlabels_ref")
  models_df <- filter(models_df, vig_attribute_varlabels_ref == "Sender avatar") # NEW
  glance_df <- glance_parse_fun(sender_avatar_models_country[[i]], country_re = FALSE, by = outcome_vars, varlabels_ref = "vig_attribute_varlabels_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df,
                       glance_df,
                       stats_df,
                       outcome_vars = vig_outcomes_df$outcome_vars,
                       outcome_vars_labels = vig_outcomes_df$outcome_vars_labels_regtable,
                       sample = country_codes_df$country[country_codes_df$code == i],
                       varlabels_ref = "vig_attribute_varlabels_ref",
               avatars = TRUE) %>% 
    gtsave_auto(paste0("results/table-acme-senderavatar-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}


# outcome tables, by country

for(i in outcome_vars) {
  models_df <- tidy_df %>% filter(response == i) %>% gt_prep_fun(by = "data", arrange_by = "estimate_bra", varlabels_ref = "vig_attribute_varlabels_ref")
  models_df <- filter(models_df, vig_attribute_varlabels_ref == "Sender avatar") # NEW
  glance_df <- glance_parse_fun(transpose(sender_avatar_models_country)[[which(outcome_vars == i)]], country_re = FALSE, by = country_codes_chr, varlabels_ref = "vig_attribute_varlabels_ref")
  stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
  gt_table_fun(models_df, 
                       glance_df, 
                       stats_df, 
                       outcome_vars = country_codes_df$code[country_codes_df$sepcountry == TRUE],
                       outcome_vars_labels = country_codes_df$country[country_codes_df$sepcountry == TRUE],
                       sample = vig_outcomes_df$outcome_vars_labels_regtable[vig_outcomes_df$outcome_vars == i],
                       spanners = FALSE,
                       varlabels_ref = "vig_attribute_varlabels_ref",
               avatars = TRUE) %>% 
    gtsave_auto(paste0("results/table-acme-senderavatar-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}




# Age X gender, pooled ---------------

# generate model formulas

resp_covars <- filter(resp_covars_df, age_gender == TRUE) %>% pull(resp_variable)

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ", 
                paste0(resp_covars, collapse = " + "), " + ", 
                paste0(vig_covars_df_main$vig_attribute_varnames, collapse = " + "), " + ",
                "(1|resp_id) + (1|deck_id)")

# run mixed-effects models, pooled

ageXgender_models_pooled <- map(paste(fmlas, "+ (1|resp_country2)"), lmer, data = data_survey_combined)
names(ageXgender_models_pooled) <- outcome_vars

# extract model AMCE estimates

tidy_df <- models_tidy_fun(ageXgender_models_pooled, resp_covars_df, "resp_variable", drop_vig_vars = TRUE)
models_df <- gt_prep_fun(tidy_df, by = "response", arrange_by = "estimate_vig_perc_offensive2", varlabels_ref = "resp_varlabel_ref")
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
glance_df <- glance_parse_fun(ageXgender_models_pooled, by = outcome_vars, varlabels_ref = "resp_varlabel_ref") 

# extract MMs

tidy_mm_df <- models_tidy_mms_fun(
  models     = ageXgender_models_pooled,                       
  predictors = resp_covars,
  data       = data_survey_combined,      
  labels_df = resp_covars_df,
  label_var = "resp_variable"
)
models_mm_df <- gt_prep_fun(tidy_mm_df, by = "response", arrange_by = NULL, varlabels_ref = "resp_varlabel")

# marginal means coefficients plot

outcome_vars_labels_regtable_vec <- outcome_vars_labels_regtable
tidy_mm_df_plot <- tidy_mm_df %>%
  separate(label, into = c("gender", "age"), sep = "\\.", extra = "merge") %>%
  left_join(vig_outcomes_df,
            by = c("response" = "outcome_vars")) %>%
  mutate(outcome_vars_labels_regtable = factor(outcome_vars_labels_regtable, levels = outcome_vars_labels_regtable_vec)) %>%
  add_confints(levels = c(.80, .90, .95, .99)) # add confidence intervals
  

tidy_mm_df_plot %>%
  ggplot(aes(x = age, y = estimate)) + 
  geom_hline(yintercept = seq(0, .6, .1), linewidth = 0.3, color = "#bfbfbf") +
  geom_point(aes(color = gender), size = 1.5, alpha = 1) +
  geom_errorbar(aes(ymin = ci90_lo, ymax = ci90_hi, color = gender), alpha = 0.8, width = 0, size = .5) +
  geom_errorbar(aes(ymin = ci80_lo, ymax = ci80_hi, color = gender), alpha = 0.8, width = 0, size = .75) +
  geom_text(
    data = tidy_mm_df_plot %>% 
      dplyr::filter(outcome_vars_labels_regtable == unique(outcome_vars_labels_regtable)[1]),
    aes(x = 8, y = 0.57),
    label = "Women",
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold",
    color = "#d7191c"
  ) + 
  geom_text(
    data = tidy_mm_df_plot %>% 
      dplyr::filter(outcome_vars_labels_regtable == unique(outcome_vars_labels_regtable)[1]),
    aes(x = 2, y = 0.4),
    label = "Men",
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold",
    color = "#56B4E9"
  ) + 
  facet_grid(~ outcome_vars_labels_regtable) +   
  scale_color_manual(values = c("#d7191c", "#56B4E9")) +
  scale_y_continuous(
    breaks = seq(0, .6, .1),
    labels = label_percent(accuracy = 1)
  ) + 
  labs(x = NULL, 
       y = "Conditional marginal means",
       title = NULL,
       subtitle = NULL) +
  guides(color = "none") + 
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_markdown(),
        axis.text.x = element_markdown(angle = 90, vjust = 0.5),
        panel.ontop = TRUE,
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        panel.spacing.x = unit(5, "pt"),        # remove facet gaps horizontally
        panel.spacing.y = unit(0, "pt"),        # (in case of multiple rows)
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.text = element_markdown(face = "bold"))
ggsave("results/effects-ageXgender-coefplot.png", width = 10, height = 3,   bg = "white", dpi = 300)



# Age X content type, pooled ---------------

# generate model formulas

resp_covars <- filter(resp_covars_df, age_category == TRUE) %>% pull(resp_variable)

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ", 
                paste0(resp_covars, collapse = " + "), " + ", 
                paste0(vig_covars_df_main$vig_attribute_varnames, collapse = " + "), " + ",
                "(1|resp_id) + (1|deck_id)")

# run mixed-effects models, pooled

ageXcategory_models_pooled <- map(paste(fmlas, "+ (1|resp_country2)"), lmer, data = data_survey_combined)
names(ageXcategory_models_pooled) <- outcome_vars

# extract model AMCE estimates

tidy_df <- models_tidy_fun(ageXcategory_models_pooled, resp_covars_df, "resp_variable", drop_vig_vars = TRUE)
models_df <- gt_prep_fun(tidy_df, by = "response", arrange_by = "estimate_vig_perc_offensive2", varlabels_ref = "resp_varlabel_ref")
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
glance_df <- glance_parse_fun(ageXcategory_models_pooled, by = outcome_vars, varlabels_ref = "resp_varlabel_ref") 

# extract MMs

tidy_mm_df <- models_tidy_mms_fun(
  models     = ageXcategory_models_pooled,                       
  predictors = resp_covars,
  data       = data_survey_combined,      
  labels_df = resp_covars_df,
  label_var = "resp_variable"
)
models_mm_df <- gt_prep_fun(tidy_mm_df, by = "response", arrange_by = NULL, varlabels_ref = "resp_varlabel")

# marginal means coefficients plot

outcome_vars_labels_regtable_vec <- outcome_vars_labels_regtable
tidy_mm_df_plot <- tidy_mm_df %>%
  separate(label, into = c("age", "content"), sep = "\\.", extra = "merge") %>%
  left_join(vig_outcomes_df,
            by = c("response" = "outcome_vars")) %>%
  mutate(outcome_vars_labels_regtable = factor(outcome_vars_labels_regtable, levels = outcome_vars_labels_regtable_vec)) %>%
  add_confints(levels = c(.80, .90, .95, .99)) # add confidence intervals
tidy_mm_df_plot$content <- factor(tidy_mm_df_plot$content, levels = c("Opinion", "Meme", "Mocking", "Insult", "Threat"))



tidy_mm_df_plot %>%
  ggplot(aes(x = age, y = estimate)) + 
  geom_hline(yintercept = seq(0, .8, .1), linewidth = 0.3, color = "#bfbfbf") +
  geom_point(aes(color = content), size = 1.5, alpha = 1) +
  geom_errorbar(aes(ymin = ci90_lo, ymax = ci90_hi, color = content), alpha = 0.8, width = 0, size = .5) +
  geom_errorbar(aes(ymin = ci80_lo, ymax = ci80_hi, color = content), alpha = 0.8, width = 0, size = .75) +
  # geom_text(
  #   data = tidy_mm_df_plot %>% 
  #     dplyr::filter(outcome_vars_labels_regtable == unique(outcome_vars_labels_regtable)[1]),
  #   aes(x = 8, y = 0.57),
  #   label = "Women",
  #   inherit.aes = FALSE,
  #   size = 3,
  #   fontface = "bold",
  #   color = "#d7191c"
  # ) + 
  # geom_text(
  #   data = tidy_mm_df_plot %>% 
  #     dplyr::filter(outcome_vars_labels_regtable == unique(outcome_vars_labels_regtable)[1]),
  #   aes(x = 2, y = 0.4),
  #   label = "Men",
  #   inherit.aes = FALSE,
  #   size = 3,
  #   fontface = "bold",
  #   color = "#56B4E9"
  # ) + 
  facet_grid(~ outcome_vars_labels_regtable) +   
  # scale_color_manual(values = c("#d7191c", "#56B4E9")) +
  scale_y_continuous(
    breaks = seq(0, .8, .1),
    labels = label_percent(accuracy = 1)
  ) + 
  labs(x = NULL, 
       y = "Conditional marginal means",
       title = NULL,
       subtitle = NULL) +
  guides(color = guide_legend(title = "Speech type")) + 
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        legend.position = "bottom",
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_markdown(),
        axis.text.x = element_markdown(angle = 90, vjust = 0.5),
        panel.ontop = TRUE,
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        panel.spacing.x = unit(5, "pt"),        # remove facet gaps horizontally
        panel.spacing.y = unit(0, "pt"),        # (in case of multiple rows)
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.text = element_markdown(face = "bold"))
ggsave("results/effects-ageXcategory-coefplot.png", width = 10, height = 3,   bg = "white", dpi = 300)




# Gender X content type, pooled ---------------

# generate model formulas

resp_covars <- filter(resp_covars_df, gender_category == TRUE) %>% pull(resp_variable)

fmlas <- paste0(outcome_vars, 
                " ~ 1 + ", 
                paste0(resp_covars, collapse = " + "), " + ", 
                paste0(vig_covars_df_main$vig_attribute_varnames, collapse = " + "), " + ",
                "(1|resp_id) + (1|deck_id)")

# run mixed-effects models, pooled

genderXcategory_models_pooled <- map(paste(fmlas, "+ (1|resp_country2)"), lmer, data = data_survey_combined)
names(genderXcategory_models_pooled) <- outcome_vars

# extract model AMCE estimates

tidy_df <- models_tidy_fun(genderXcategory_models_pooled, resp_covars_df, "resp_variable", drop_vig_vars = TRUE)
models_df <- gt_prep_fun(tidy_df, by = "response", arrange_by = "estimate_vig_perc_offensive2", varlabels_ref = "resp_varlabel_ref")
stats_df <- dplyr::select(ungroup(models_df), starts_with("statistic"))
glance_df <- glance_parse_fun(genderXcategory_models_pooled, by = outcome_vars, varlabels_ref = "resp_varlabel_ref") 

# extract MMs

tidy_mm_df <- models_tidy_mms_fun(
  models     = genderXcategory_models_pooled,                       
  predictors = resp_covars,
  data       = data_survey_combined,      
  labels_df = resp_covars_df,
  label_var = "resp_variable"
)
models_mm_df <- gt_prep_fun(tidy_mm_df, by = "response", arrange_by = NULL, varlabels_ref = "resp_varlabel")

# marginal means coefficients plot

outcome_vars_labels_regtable_vec <- outcome_vars_labels_regtable
tidy_mm_df_plot <- tidy_mm_df %>%
  separate(label, into = c("gender", "content"), sep = "\\.", extra = "merge") %>%
  left_join(vig_outcomes_df,
            by = c("response" = "outcome_vars")) %>%
  mutate(outcome_vars_labels_regtable = factor(outcome_vars_labels_regtable, levels = outcome_vars_labels_regtable_vec)) %>%
  add_confints(levels = c(.80, .90, .95, .99)) # add confidence intervals
tidy_mm_df_plot$content <- factor(tidy_mm_df_plot$content, levels = c("Opinion", "Meme", "Mocking", "Insult", "Threat"))


tidy_mm_df_plot %>%
  ggplot(aes(x = content, y = estimate)) + 
  geom_hline(yintercept = seq(0, .8, .1), linewidth = 0.3, color = "#bfbfbf") +
  geom_point(aes(color = gender), size = 1.5, alpha = 1) +
  geom_errorbar(aes(ymin = ci90_lo, ymax = ci90_hi, color = gender), alpha = 0.8, width = 0, size = .5) +
  geom_errorbar(aes(ymin = ci80_lo, ymax = ci80_hi, color = gender), alpha = 0.8, width = 0, size = .75) +
  geom_text(
    data = tidy_mm_df_plot %>%
      dplyr::filter(outcome_vars_labels_regtable == unique(outcome_vars_labels_regtable)[1]),
    aes(x = 3, y = 0.75),
    label = "Women",
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold",
    color = "#d7191c"
  ) +
  geom_text(
    data = tidy_mm_df_plot %>%
      dplyr::filter(outcome_vars_labels_regtable == unique(outcome_vars_labels_regtable)[1]),
    aes(x = 3, y = 0.677),
    label = "Men",
    inherit.aes = FALSE,
    size = 3,
    fontface = "bold",
    color = "#56B4E9"
  ) +
  facet_grid(~ outcome_vars_labels_regtable) +   
 scale_color_manual(values = c("#d7191c", "#56B4E9")) +
  scale_y_continuous(
    breaks = seq(0, .8, .1),
    labels = label_percent(accuracy = 1)
  ) + 
  labs(x = NULL, 
       y = "Conditional marginal means",
       title = NULL,
       subtitle = NULL) +
  guides(color = "none") + 
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        legend.position = "bottom",
        axis.title = element_text(size = 10, face = "bold"),
        axis.text.y = element_markdown(),
        axis.text.x = element_markdown(angle = 90, vjust = .5),
        panel.ontop = TRUE,
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        panel.spacing.x = unit(5, "pt"),        # remove facet gaps horizontally
        panel.spacing.y = unit(0, "pt"),        # (in case of multiple rows)
        plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
        strip.text = element_markdown(face = "bold"))
ggsave("results/effects-genderXcategory-coefplot.png", width = 10, height = 3,   bg = "white", dpi = 300)














