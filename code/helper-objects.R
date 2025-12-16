
# generate variable containers for analyses ---------------------------

country_codes_df <- read_xlsx("data/cooked/country-codes.xlsx")
resp_covars_df <- read_xlsx("data/cooked/resp-covars.xlsx")
exp_covars_df <- read_xlsx("data/cooked/exp-covars.xlsx")
vig_covars_df <- read_xlsx("data/cooked/vig-covars.xlsx")
tp_vars_labels <- read_csv("data/cooked/tp-vars.csv")


# prepare country_codes_df ------

country_codes_chr <- country_codes_df$code[country_codes_df$sepcountry == TRUE] %>% na.omit() %>% as.character()
country_codes_vec <- seq_along(country_codes_chr)
country_codes_num <- seq_along(country_codes_chr)


# prepare vig_covars_df ---------

vig_covars_df_main <- filter(vig_covars_df, main_model == TRUE)
vig_covars_df_main$vig_attribute_varlabels_ref <- 
  factor(vig_covars_df_main$vig_attribute_varlabels_ref, 
         levels = 
           c("Message category (vs. Opinion)",
             "Message topic (vs. SUV drivers)", 
             "Message severity (vs. Moderate)",
             "Message scope (vs. Group)",
             "Target topic stance (vs. Left)",
             "Target ethnicity (vs White)",
             "Target gender (vs. Male)",
             "Sender ethnicity (vs. White)",
             "Sender gender (vs. Male)"
           )
  )

vig_covars_df_vig_pos <- filter(vig_covars_df, vig_pos_model == TRUE)
vig_covars_df_vig_pos$vig_attribute_varlabels_ref <- 
  factor(vig_covars_df_vig_pos$vig_attribute_varlabels_ref, 
         levels = 
           c("Vignette position (vs. Position 1)",
            "Message category (vs. Opinion)"
           )
  )

vig_covars_df_targetavatar <- filter(vig_covars_df, target_avatar_model == TRUE)
vig_covars_df_targetavatar$vig_attribute_varlabels_ref <- 
  factor(vig_covars_df_targetavatar$vig_attribute_varlabels_ref, 
         levels = 
           vig_covars_df_targetavatar$vig_attribute_varlabels_ref
  )

vig_covars_df_senderavatar <- filter(vig_covars_df, sender_avatar_model == TRUE)
vig_covars_df_senderavatar$vig_attribute_varlabels_ref <- 
  factor(vig_covars_df_senderavatar$vig_attribute_varlabels_ref, 
         levels = 
           vig_covars_df_senderavatar$vig_attribute_varlabels_ref
  )


# prepare resp_covars_df ---------

resp_covars_df$resp_varlabel_ref <- factor(resp_covars_df$resp_varlabel_ref, levels = na.omit(unique(resp_covars_df$resp_varlabel_ref)))




# outcome vars for regression models
outcome_vars <- 
  c("vig_perc_offensive2",
    "vig_perc_hate2",
    "vig_remove",
    "vig_ban",
    "vig_legal",
    "vig_job"
  )

outcome_vars_labels = c(
  "Percentage of offensive content",
  "Percentage of hate speech",
  "Remove post",
  "Ban sender",
  "Legal action",
  "Job loss"
)

outcome_vars_labels2 = c(
  "Offensive",
  "Hate speech",
  "Remove post",
  "Ban sender",
  "Legal action",
  "Job loss"
)

outcome_vars_labels_regtable = c(
  "Offensive<br>speech",
  "Hate<br>speech",
  "Remove<br>post",
  "Ban<br>sender",
  "Legal<br>sanction",
  "Job<br>loss"
)

outcome_vars_cat <- c(rep("Perceptions", 2), rep("Actions", 4))

# prepare variables and labels
vig_perceptions_variables <- c("vig_perc_none", "vig_perc_offensive", "vig_perc_hate")
vig_perceptions_labels <- c("Neither offensive nor hate speech", "Offensive but not hate speech", "Hate speech")
vig_actions_variables <- c("vig_remove", "vig_ban", "vig_legal", "vig_job")
vig_actions_labels <- c("Remove post", "Ban sender", "Legal action", "Job loss")
vig_outcomes_variables <- c(vig_perceptions_variables, vig_actions_variables)
vig_outcomes_labels <- c(vig_perceptions_labels, vig_actions_labels)
vig_outcomes_labels_set <- setNames(str_replace_all(str_wrap(vig_outcomes_labels, 9), "\n", "<br>"), vig_outcomes_variables)

vig_outcomes_rename_map <- c(
  "vig_perc_offensive" = "Offensive",
  "vig_perc_hate" = "Hate",
  "vig_remove" = "Remove\npost",
  "vig_ban" = "Ban\nsender",
  "vig_legal" = "Legal\nconsequences",
  "vig_job" = "Job\nloss"
)

vig_outcomes_df <- data.frame(
  outcome_vars,
  outcome_vars_labels,
  outcome_vars_labels2,
  outcome_vars_labels_regtable,
  outcome_vars_cat
)




# respondent covariates df

resp_demographics_covars <- 
  c("gender",  "age_cat", "educ_cat", "white_cat"
  )

resp_attitudes_covars <- 
  c("polinterest", "leftright", "empathy_person", "empathy_predicting", 
    "empathy_perspective", "exp_offended", "exp_threatened",                
    "exp_witnessed", "exp_disagree", "exp_angry",                      
    "exp_postregret", "exp_postoffensive", "exp_postopinion", 
    "speak_freely", "speak_freely_pers", "silencing_unacceptable", "silencing_harmful"
  )

"silencing_score_cat"
"empathy_score_cat"
"exp_score_cat"
"hostile_score_cat"

resp_outcomes <- 
  c("tradeoffs_speech", "tradeoffs_platforms",  "tradeoffs_govments",      
    "respsblty_victims", "respsblty_witnesses", "respsblty_platforms",  
    "respsblty_lawmakers", "respsblty_justice", "respsblty_employers",
    "content_regulation"
  )




