# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")


# import survey data ---------------------------------

data_survey_complete <- readRDS("data/cooked/data_survey_complete.rds")

# how many non-zero-length definitions?
tabyl(data_survey_complete$open_hatedefinition != "")
tabyl(data_survey_complete$open_allow != "")
tabyl(str_length(data_survey_complete$open_hatedefinition) > 30)
# filter(data_survey_complete, str_length(data_survey_complete$open_hatedefinition) < 30, str_length(data_survey_complete$open_hatedefinition) > 0) %>% dplyr::select(open_hatedefinition) %>% View()


# encode experimental indicator variables --------------

# frame treatment status

data_survey_complete %<>%
  mutate(
    frame_vig = if_else(seed_frame == 1, "Neutral", 
                             if_else(seed_frame == 2, 
                                     "Free speech", 
                                     "Protect users")),
    frame_vig_na = if_else(is.na(vig_intro_free) &
                                  is.na(vig_intro_neutral) &
                                  is.na(vig_intro_protect), 1, 0) # indicates if respondent did not select "I have read and understood those instructions."
    )
data_survey_complete$frame_vig <- factor(data_survey_complete$frame_vig, levels = c("Neutral", "Free speech", "Protect users"))
attr(data_survey_complete$frame_vig, "label") <- "Vignette frame"

# frame manipulation check
  # pre-reg: "The manipulation check question will be used to report, in addition to a model using the full sample, a model that is run only using treated respondents who answered the manipulation check question in accordance with their treatment condition, and all respondents in the neutral condition irrespective of how they answered the manipulation check question."

data_survey_complete %<>% 
  mutate(
    manipcheck_frame_passed = 
      (seed_frame == 2 & vig_manipcheck == 1) |
      (seed_frame == 3 & vig_manipcheck == 2) |
      seed_frame == 1
  )
data_survey_complete$manipcheck_frame_passed[is.na(data_survey_complete$manipcheck_frame_passed)] <- FALSE
attr(data_survey_complete$manipcheck_frame_passed, "label") <- "Framing manipulation check"



# frame attention check
  # pre-reg: The response time measure on the vignette task frame will be used to report, in addition to a model using the full sample, a model that is run only using treated respondents who spent at least 10 seconds on the survey page providing the frame."

data_survey_complete %<>% 
  mutate(attcheck_passed = (t_vig_intro_Page_Submit >= 10))

# exposure treatment status

data_survey_complete %<>% 
  mutate(prefs_after_vignette = case_when(
    seed_prepost == 1 ~ "Before",
    seed_prepost == 2 ~ "After",
    TRUE ~ NA_character_
  ))
attr(data_survey_complete$prefs_after_vignette, "label") <- "Vignette task placement"

# encode covariates --------------

# gender

attribute_levels <- c("Male", "Female", "Other")
data_survey_complete$gender <- ifelse(data_survey_complete$gender == 1, attribute_levels[1], 
                                      ifelse(data_survey_complete$gender == 2, attribute_levels[2], attribute_levels[3])) %>% factor(levels = attribute_levels)
attr(data_survey_complete$gender, "label") <- "Gender"

data_survey_complete$gender2 <- data_survey_complete$gender
data_survey_complete$gender2[data_survey_complete$gender2 == "Other"] <- NA 
data_survey_complete$gender2 <- drop.levels(data_survey_complete$gender2)
  

# age

data_survey_complete$age <- remove_val_labels(data_survey_complete$birthyear) + 15
data_survey_complete$age[data_survey_complete$age < 18 | data_survey_complete$age > 90] <- NA

# age, categorical

attribute_levels <- c("18-29", "30-49", "50-69", "70+")
data_survey_complete$age10 <- data_survey_complete$age/10
data_survey_complete$age_cat <- cut(as.numeric(data_survey_complete$age10), breaks = c(0, 2.9, 4.9, 6.9, 9), include.lowest = TRUE, labels = attribute_levels)
attr(data_survey_complete$age_cat, "label") <- "Age"

# age, categorical, fine-grained

attribute_levels <- c("18-19", "20-21", "22-23", "24-25", "26-27", "28-29", "30-39", "40-49", "50-59", "60+")
data_survey_complete$age_cat2 <- cut(as.numeric(data_survey_complete$age10), breaks = c(0, 1.9, 2.1, 2.3, 2.5, 2.7, 2.9, 3.9, 4.9, 5.9, 9), include.lowest = TRUE, labels = attribute_levels)
attr(data_survey_complete$age_cat2, "label") <- "Age"


# ageXgender

data_survey_complete$ageXgender <- interaction(data_survey_complete$gender2, data_survey_complete$age_cat2)


# education

attribute_levels <- c("Low", "Intermediate", "High")

# Brazil
data_survey_complete$education_bra <- as.factor(data_survey_complete$education_bra)
data_survey_complete$educ_cat_bra <- recode_factor(data_survey_complete$education_bra, 
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[1], 
                                                   `3` = attribute_levels[2], 
                                                   `4` = attribute_levels[3], 
                                                   `5` = attribute_levels[3], 
                                                   `6` = attribute_levels[3], 
                                                   .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_bra)

# Colombia
data_survey_complete$education_col <- as.factor(data_survey_complete$education_col)
data_survey_complete$educ_cat_col <- recode_factor(data_survey_complete$education_col, 
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[1], 
                                                   `3` = attribute_levels[2], 
                                                   `4` = attribute_levels[3], 
                                                   `5` = attribute_levels[3],
                                                   `6` = attribute_levels[3],
                                                   .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_col)

# Germany
data_survey_complete$education_ger <- as.factor(data_survey_complete$education_ger)
data_survey_complete$education_ger <- recode_factor(data_survey_complete$education_ger,
                                                    '1' = '1', '7' = '2', '8' = '3', 
                                                    '9' = '4', '10' = '5', '11' = '6',
                                                    .default = NA_character_)
data_survey_complete$educ_cat_ger <- recode_factor(data_survey_complete$education_ger, 
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[1], 
                                                   `3` = attribute_levels[1], 
                                                   `4` = attribute_levels[2], 
                                                   `5` = attribute_levels[3], 
                                                   `6` = attribute_levels[3],
                                                   .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_ger)

# Indonesia
data_survey_complete$education_idn <- as.factor(data_survey_complete$education_idn)
data_survey_complete$education_idn <- recode_factor(data_survey_complete$education_idn, 
                                                    '1' = '1', '3' = '2', '4' = '3', 
                                                    '5' = '4', '6' = '5', .default = NA_character_)
data_survey_complete$educ_cat_idn <- recode_factor(data_survey_complete$education_idn,
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[1], 
                                                   `3` = attribute_levels[2], 
                                                   `4` = attribute_levels[3], 
                                                   `5` = attribute_levels[3],
                                                   .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_idn)

# India - Hindi
data_survey_complete$education_ind <- as.factor(data_survey_complete$education_ind)
data_survey_complete$educ_cat_ind <- recode_factor(data_survey_complete$education_ind, 
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[1], 
                                                   `3` = attribute_levels[1],
                                                   `4` = attribute_levels[2], 
                                                   `5` = attribute_levels[3], 
                                                   `6` = attribute_levels[3],
                                                   .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_ind)

# India - English 
data_survey_complete$education_indeng <- as.factor(data_survey_complete$education_indeng)
data_survey_complete$education_indeng <- recode_factor(data_survey_complete$education_indeng, 
                                                       '5' = '1', '14' = '2', '13' = '3', 
                                                    '12' = '4', '11' = '5', '10' = '6', .default = NA_character_)
data_survey_complete$educ_cat_indeng <- recode_factor(data_survey_complete$education_indeng, 
                                                      `1` = attribute_levels[1], 
                                                      `2` = attribute_levels[1], 
                                                      `3` = attribute_levels[1],
                                                      `4` = attribute_levels[2], 
                                                      `5` = attribute_levels[3],
                                                      `6` = attribute_levels[3],
                                                   .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_indeng)

# Philippines - Filipino
data_survey_complete$education_phl <- as.factor(data_survey_complete$education_phl)
data_survey_complete$educ_cat_phl <- recode_factor(data_survey_complete$education_phl, 
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[1], 
                                                   `3` = attribute_levels[2], 
                                                   `4` = attribute_levels[3],
                                                   `5` = attribute_levels[3],
                                                   `6` = attribute_levels[3],
                                                      .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_phl)

# Philippines - English
data_survey_complete$education_phleng <- as.factor(data_survey_complete$education_phleng)
data_survey_complete$educ_cat_phleng <- recode_factor(data_survey_complete$education_phleng,
                                                   `1` = attribute_levels[1],
                                                   `2` = attribute_levels[1], 
                                                   `3` = attribute_levels[2], 
                                                   `4` = attribute_levels[3], 
                                                   `5` = attribute_levels[3], 
                                                   `6` = attribute_levels[3],
                                                   .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_phleng)

# Nigeria
data_survey_complete$education_nig <- as.factor(data_survey_complete$education_nig)
data_survey_complete$educ_cat_nig <- recode_factor(data_survey_complete$education_nig, 
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[1], 
                                                   `3` = attribute_levels[2], 
                                                   `4` = attribute_levels[3], 
                                                   `5` = attribute_levels[3], 
                                                   `6` = attribute_levels[3],
                                                      .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_nig)

# Poland
data_survey_complete$education_pol <- as.factor(data_survey_complete$education_pol)
data_survey_complete$educ_cat_pol <- recode_factor(data_survey_complete$education_pol, 
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[1], 
                                                   `3` = attribute_levels[1], 
                                                   `4` = attribute_levels[2], 
                                                   `5` = attribute_levels[3], 
                                                   `6` = attribute_levels[3], 
                                                   `7` = attribute_levels[3], 
                                                   `8` = attribute_levels[3],
                                                   .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_pol)

# Turkey
data_survey_complete$education_tur <- as.factor(data_survey_complete$education_tur)
data_survey_complete$educ_cat_tur <- recode_factor(data_survey_complete$education_tur, 
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[1], 
                                                   `3` = attribute_levels[2], 
                                                   `4` = attribute_levels[3], 
                                                   `5` = attribute_levels[3], 
                                                   `6` = attribute_levels[3],
                                                   .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_tur)

# Great Britain
# *****Validate low/intermediate/high categories, low is particularly high (45%) *****
data_survey_complete$education_gbr <- as.factor(data_survey_complete$education_gbr)
data_survey_complete$education_gbr <- recode_factor(data_survey_complete$education_gbr, 
                                                    '1' = '1', '9' = '2', '8' = '3', '7' = '4', 
                                                    '2' = '5', '3' = '6', '4' = '7', '5' = '8', '6' = '9', 
                                                    .default = NA_character_)
data_survey_complete$educ_cat_gbr <- recode_factor(data_survey_complete$education_gbr, 
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[1],
                                                   `3` = attribute_levels[1], 
                                                   `4` = attribute_levels[1], 
                                                   `5` = attribute_levels[1], 
                                                   `6` = attribute_levels[2], 
                                                   `7` = attribute_levels[2], 
                                                   `8` = attribute_levels[3], 
                                                   `9` = attribute_levels[3], 
                                                   .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_gbr)

# USA
data_survey_complete$education_usa <- as.factor(data_survey_complete$education_usa)
data_survey_complete$educ_cat_usa <- recode_factor(data_survey_complete$education_usa, 
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[1], 
                                                   `3` = attribute_levels[2], 
                                                   `4` = attribute_levels[3], 
                                                   `5` = attribute_levels[3], 
                                                   `6` = attribute_levels[3],
                                                   .default = NA_character_) %>% as.character() %>% factor(levels = attribute_levels)
tabyl(data_survey_complete$educ_cat_usa)

# collating into a single educ_cat var

data_survey_complete <- data_survey_complete %>% 
  mutate(educ_cat = coalesce(educ_cat_bra, educ_cat_col, educ_cat_gbr, educ_cat_ger, educ_cat_idn, 
                             educ_cat_ind, educ_cat_indeng, educ_cat_nig, educ_cat_phl, educ_cat_phleng,
                             educ_cat_pol, educ_cat_tur, educ_cat_usa))
tabyl(data_survey_complete$educ_cat)

attr(data_survey_complete$educ_cat, "label") <- "Education"


# ethnicity

data_survey_complete %<>% mutate(ethnicity_num = rowSums(dplyr::select(., ethnicity_w, ethnicity_m, ethnicity_o), na.rm = TRUE))
data_survey_complete$ethnicity_num[data_survey_complete$ethnicity_num == 0] <- NA

ethnicity_labels_df <- stack(attr(data_survey_complete$ethnicity_o, 'labels')) %>% dplyr::rename(all_of(c(ethnicity_num = "values", ethnicity_code = "ind")))

data_survey_complete %<>% left_join(ethnicity_labels_df, by = "ethnicity_num")
data_survey_complete$ethnicity_cat <- str_extract(data_survey_complete$ethnicity_code, "_[:alpha:]+_") %>% str_replace_all("_", "") %>% firstup()

data_survey_complete$white_cat <- ifelse(data_survey_complete$ethnicity_cat == "White", "White", "Non-White") %>% as.factor()
attr(data_survey_complete$white_cat, "label") <- "Ethnicity"


# white / non-white distinction might make most sense; few people identify with black avatar
tabyl(data_survey_complete, ethnicity_cat, resp_country) %>% adorn_percentages(denominator = "col") %>% adorn_rounding(1)


# minority

attribute_levels <- c("Non-Minority", "Minority")
data_survey_complete$minority_cat <- ifelse(data_survey_complete$minority == 1, 
                                            attribute_levels[2], 
                                            attribute_levels[1]) %>% factor(levels = attribute_levels)
attr(data_survey_complete$minority_cat, "label") <- "Minority status"

# political interest

attribute_levels <- c("Not interested at all", "Slightly interested", "Moderately interested", "Very interested")
data_survey_complete$polinterest_cat <- dplyr::recode(data_survey_complete$polinterest, 
                                                      `1` = attribute_levels[1],
                                                      `2` = attribute_levels[2], 
                                                      `3` = attribute_levels[3],
                                                      `4` = attribute_levels[4]) %>%
                                           as.character() %>% 
                                           factor(levels = attribute_levels)
attr(data_survey_complete$polinterest_cat, "label") <- "Political interest"

attribute_levels <- c("Low", "High")
data_survey_complete$polinterest_cat2 <- dplyr::recode(data_survey_complete$polinterest, 
                                                      `1` = attribute_levels[1],
                                                      `2` = attribute_levels[1], 
                                                      `3` = attribute_levels[2],
                                                      `4` = attribute_levels[2]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$polinterest_cat2, "label") <- "Political interest"


# political ideology (leftright)

attribute_levels <- c("Left", "Center", "Right")
data_survey_complete <- data_survey_complete %>% 
  mutate(leftright_cat = case_when(
    leftright <5 ~ attribute_levels[1], 
    leftright > 5 & leftright <7 ~ attribute_levels[2],
    leftright >= 7 ~ attribute_levels[3]
  ))
data_survey_complete$leftright_cat <- factor(data_survey_complete$leftright_cat, levels = attribute_levels)
attr(data_survey_complete$leftright_cat, "label") <- "Political ideology"


# speak freely

attribute_levels <- c("More free", "Just as free", "Less free") # replace "don't know" with NA
data_survey_complete$speak_freely <- recode_factor(data_survey_complete$speak_freely , 
                                                  `1` = attribute_levels[1], 
                                                  `2` = attribute_levels[2], 
                                                  `3` = attribute_levels[3]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$speak_freely, "label") <- "Speak freely"


# speak freely personally

attribute_levels <- c("More free", "Just as free", "Less free") # replace "don't know" with NA
data_survey_complete$speak_freely_pers <- recode_factor(data_survey_complete$speak_freely_pers, 
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[2], # this is true! 3/2 flipped here
                                                   `3` = attribute_levels[3]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$speak_freely_pers, "label") <- "Speak freely personally"


# tradeoffs speech governance

# fix issue with Colombia encoding
data_survey_complete$tradeoffs_speech[data_survey_complete$tradeoffs_speech == 2 & data_survey_complete$resp_country == "col"] <- 1
data_survey_complete$tradeoffs_speech[data_survey_complete$tradeoffs_speech == 3 & data_survey_complete$resp_country == "col"] <- 2

attribute_levels <- c("Speak freely", "Welcome and safe") 
data_survey_complete$tradeoffs_speech <- as.character(data_survey_complete$tradeoffs_speech) %>% dplyr::recode("3" = "2") %>%
  as.numeric() %>%
  factor(labels = attribute_levels)
attr(data_survey_complete$tradeoffs_speech, "label") <- "Speech governance preferences"


# tradeoffs platforms

attribute_levels <- c("Not responsible", "Responsible") 
data_survey_complete$tradeoffs_platforms <- recode_factor(data_survey_complete$tradeoffs_platforms,
                                                       `1` = attribute_levels[1],  `10` = attribute_levels[2]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$tradeoffs_platforms, "label") <- "Tradeoffs platforms"


# tradeoffs governments

attribute_levels <- c("Prevent hate", "Speak freely") 
data_survey_complete$tradeoffs_govments <- recode_factor(data_survey_complete$tradeoffs_govments,
                                                          `1` = attribute_levels[1],  `2` = attribute_levels[2]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$tradeoffs_govments, "label") <- "Tradeoffs governments"


# tradeoffs content regulation

attribute_levels <- c("Social media companies", "National government", "Both", "No regulation") 
data_survey_complete$content_regulation <- recode_factor(data_survey_complete$content_regulation ,
                                                         `1` = attribute_levels[1], 
                                                         `2` = attribute_levels[2], 
                                                         `3` = attribute_levels[3], 
                                                         `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$content_regulation, "label") <- "Tradeoffs regulation"



# responsibility

attribute_levels <- c("No responsibility at all", "Rather no responsibility", "Some responsibility", "Full responsibility") 

# fix ind issue
data_survey_complete$respsblty_platforms <- as.numeric(data_survey_complete$resp_platforms)
data_survey_complete$responsibility_xxx_7 <- as.numeric(data_survey_complete$responsibility_xxx_7)

data_survey_complete$respsblty_platforms[data_survey_complete$resp_country == "ind"] <- data_survey_complete$responsibility_xxx_7[data_survey_complete$resp_country == "ind"]
data_survey_complete <- dplyr::select(data_survey_complete, -responsibility_xxx_7)


# victims
data_survey_complete$respsblty_victims <- recode_factor(data_survey_complete$resp_victims ,
                                                         `1` = attribute_levels[1], 
                                                         `2` = attribute_levels[2], 
                                                         `3` = attribute_levels[3], 
                                                        `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$respsblty_victims, "label") <- "Responsibility of victims"

# witnesses
data_survey_complete$respsblty_witnesses <- recode_factor(data_survey_complete$resp_witnesses ,
                                                     `1` = attribute_levels[1], 
                                                     `2` = attribute_levels[2], 
                                                     `3` = attribute_levels[3], 
                                                     `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$respsblty_witnesses, "label") <- "Responsibility of witnesses"

# platforms
data_survey_complete$respsblty_platforms <- recode_factor(data_survey_complete$resp_platforms ,
                                                     `1` = attribute_levels[1], 
                                                     `2` = attribute_levels[2], 
                                                     `3` = attribute_levels[3], 
                                                     `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$respsblty_platforms, "label") <- "Responsibility of platforms"

# lawmakers
data_survey_complete$respsblty_lawmakers <- recode_factor(data_survey_complete$resp_lawmakers ,
                                                     `1` = attribute_levels[1], 
                                                     `2` = attribute_levels[2], 
                                                     `3` = attribute_levels[3], 
                                                     `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$respsblty_lawmakers, "label") <- "Responsibility of lawmakers"

# justice system
data_survey_complete$respsblty_justice <- recode_factor(data_survey_complete$resp_justice ,
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[2], 
                                                   `3` = attribute_levels[3], 
                                                   `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$respsblty_justice, "label") <- "Responsibility of justice system"


# employers
data_survey_complete$respsblty_employers <- recode_factor(data_survey_complete$resp_employers ,
                                                     `1` = attribute_levels[1], 
                                                     `2` = attribute_levels[2], 
                                                     `3` = attribute_levels[3], 
                                                     `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$respsblty_employers, "label") <- "Responsibility of employers"


# censorship of opinions

attribute_levels <- c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")

# fix idn issue
data_survey_complete$silencing_unacceptable[data_survey_complete$resp_country == "idn"] <- data_survey_complete$silencing_xxx_9[data_survey_complete$resp_country == "idn"]
data_survey_complete <- dplyr::select(data_survey_complete, -silencing_xxx_9)

# fil col issue
data_survey_complete$silencing_unacceptable[data_survey_complete$resp_country == "col" & data_survey_complete$silencing_unacceptable == 5] <- 2
data_survey_complete$silencing_harmful[data_survey_complete$resp_country == "col" & data_survey_complete$silencing_harmful == 5] <- 2

# silencing scale
silencing_scale_fit <- psych::principal(data_survey_complete %>% dplyr::select(silencing_unacceptable, silencing_harmful), nfactors = 1, rotate = "varimax", missing = TRUE, impute = "mean")
data_survey_complete$silencing_score <- predict(silencing_scale_fit, dplyr::select(data_survey_complete, silencing_unacceptable, silencing_harmful))[,1]
silencing_score_quantiles <- quantile(data_survey_complete$silencing_score, probs = c(0, .33, .66, 1), na.rm = TRUE)
data_survey_complete$silencing_score_cat <- cut(data_survey_complete$silencing_score, breaks = c(-Inf, silencing_score_quantiles[2], silencing_score_quantiles[3], Inf), labels = c("Low", "Medium", "High"), right = TRUE)

# act against saying unacceptable things
data_survey_complete$silencing_unacceptable <- recode_factor(data_survey_complete$silencing_unacceptable ,
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[2], 
                                                   `3` = attribute_levels[3], 
                                                   `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$silencing_unacceptable, "label") <- "Act against saying unacceptable things"

attribute_levels <- c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree")

# sometimes harmful views need to be silenced
data_survey_complete$silencing_harmful <- recode_factor(data_survey_complete$silencing_harmful,
                                                             `1` = attribute_levels[1], 
                                                             `2` = attribute_levels[2], 
                                                             `3` = attribute_levels[3], 
                                                             `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$silencing_harmful, "label") <- "Silence harmful views"


# hate experiences online

attribute_levels <- c("Never", "Rarely", "Sometimes", "Often") 

# hate experiences score
data_survey_complete <- data_survey_complete %>% mutate(exp_score = rowMeans(across(c(exp_offended, exp_threatened, exp_witnessed, exp_disagree, exp_angry)), na.rm = TRUE))
exp_score_quantiles <- quantile(data_survey_complete$exp_score, probs = c(0, .33, .66, 1), na.rm = TRUE)
data_survey_complete$exp_score_cat <- cut(data_survey_complete$exp_score, breaks = c(-Inf, exp_score_quantiles[2], exp_score_quantiles[3], Inf), labels = c("Low", "Medium", "High"), right = TRUE)


# offended or insulted
data_survey_complete$exp_offended <- recode_factor(data_survey_complete$exp_offended,
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[2], 
                                                   `3` = attribute_levels[3], 
                                                   `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$exp_offended, "label") <- "Been offended or insulted online"

# threatened
data_survey_complete$exp_threatened <- recode_factor(data_survey_complete$exp_threatened,
                                                     `1` = attribute_levels[1], 
                                                     `2` = attribute_levels[2], 
                                                     `3` = attribute_levels[3], 
                                                     `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$exp_threatened, "label") <- "Been threatened personally online"
str(data_survey_complete$exp_threatened)

# witnessed
data_survey_complete$exp_witnessed <- recode_factor(data_survey_complete$exp_witnessed,
                                                    `1` = attribute_levels[1], 
                                                    `2` = attribute_levels[2], 
                                                    `3` = attribute_levels[3], 
                                                    `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$exp_witnessed, "label") <- "Witnessed someone else offended, insulted, threatened"

# disagree
data_survey_complete$exp_disagree <- recode_factor(data_survey_complete$exp_disagree,
                                                   `1` = attribute_levels[1], 
                                                   `2` = attribute_levels[2], 
                                                   `3` = attribute_levels[3], 
                                                   `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$exp_disagree, "label") <- "Seen views I disagree with"

# angry
data_survey_complete$exp_angry <- recode_factor(data_survey_complete$exp_angry,
                                                `1` = attribute_levels[1], 
                                                `2` = attribute_levels[2], 
                                                `3` = attribute_levels[3], 
                                                `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$exp_angry, "label") <- "Seen views that made me angry"


# hostile engagement online

# hostile engagement score
data_survey_complete <- data_survey_complete %>% mutate(hostile_score = rowMeans(across(c(exp_postregret, exp_postoffensive, exp_postopinion)), na.rm = TRUE))
hostile_score_quantiles <- quantile(data_survey_complete$hostile_score, probs = c(0, .33, .66, 1), na.rm = TRUE)
data_survey_complete$hostile_score_cat <- cut(data_survey_complete$hostile_score, breaks = c(-Inf, hostile_score_quantiles[2], hostile_score_quantiles[3], Inf), labels = c("Low", "Medium", "High"), right = TRUE)

# post regret
data_survey_complete$exp_postregret <- recode_factor(data_survey_complete$exp_postregret,
                                                     `1` = attribute_levels[1], 
                                                     `2` = attribute_levels[2], 
                                                     `3` = attribute_levels[3], 
                                                     `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$exp_postregret, "label") <- "Posted something I regretted"

# post offensive
data_survey_complete$exp_postoffensive <- recode_factor(data_survey_complete$exp_postoffensive,
                                                        `1` = attribute_levels[1], 
                                                        `2` = attribute_levels[2], 
                                                        `3` = attribute_levels[3], 
                                                        `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$exp_postoffensive, "label") <- "Posted something offensive"

# post political opinion
data_survey_complete$exp_postopinion <- recode_factor(data_survey_complete$exp_postopinion,
                                                      `1` = attribute_levels[1], 
                                                      `2` = attribute_levels[2], 
                                                      `3` = attribute_levels[3], 
                                                      `4` = attribute_levels[4]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$exp_postopinion, "label") <- "Posted a political opinion"


# topic agreement

data_survey_complete <- dplyr::select(data_survey_complete, -A)

# Coding:
  # feminism, mensrights, immigration, racism, veganism, climatechange, freespeech: 1-2 (unsure = NA)
  # partisanship, homosexuality, religious, inequality, nationalism, police, cardriver: 1-3

attribute_levels <- c("Tend to agree", "Tend to disagree")
topic_vars <- str_subset(names(data_survey_complete), "tp_")
topic_vars_l <- str_subset(names(data_survey_complete), "_l$")
topic_vars_r <- setdiff(topic_vars, topic_vars_l)

agree_fun <- function(x) {
  x <- recode_factor(x, `1` = "Tend to agree", `2` = "Tend to disagree") %>%
    as.character() %>%
  factor(levels = attribute_levels)
}

data_survey_complete <- data_survey_complete %>%
  mutate_at(all_of(topic_vars), agree_fun)

# Relabeling all tp_vars
attr(data_survey_complete$tp_feminism_l, "label") <- "Topic agreement: Feminism (L)"
attr(data_survey_complete$tp_feminism_r, "label") <- "Topic agreement: Feminism (R)"
attr(data_survey_complete$tp_mensrights_l, "label") <- "Topic agreement: Men's rights (L)"
attr(data_survey_complete$tp_mensrights_r, "label") <- "Topic agreement: Men's rights (R)"
attr(data_survey_complete$tp_immigration_l, "label") <- "Topic agreement: Immigration (L)"
attr(data_survey_complete$tp_immigration_r, "label") <- "Topic agreement: Immigration (R)"
attr(data_survey_complete$tp_racism_l, "label") <- "Topic agreement: Racism (L)"
attr(data_survey_complete$tp_racism_r, "label") <- "Topic agreement: Racism (R)"
attr(data_survey_complete$tp_veganism_l, "label") <- "Topic agreement: Veganism (L)"
attr(data_survey_complete$tp_veganism_r, "label") <- "Topic agreement: Veganism (R)"
attr(data_survey_complete$tp_climatechange_l, "label") <- "Topic agreement: Climate change (L)"
attr(data_survey_complete$tp_climatechange_r, "label") <- "Topic agreement: Climate change (R)"
attr(data_survey_complete$tp_freespeech_l, "label") <- "Topic agreement: Free speech (L)"
attr(data_survey_complete$tp_freespeech_r, "label") <- "Topic agreement: Free speech (R)"

attr(data_survey_complete$tp_partisanship_l, "label") <- "Topic agreement: Partisanship (L)"
attr(data_survey_complete$tp_partisanship_r, "label") <- "Topic agreement: Partisanship (R)"
attr(data_survey_complete$tp_homosexuality_l, "label") <- "Topic agreement: Homosexuality (L)"
attr(data_survey_complete$tp_homosexuality_r, "label") <- "Topic agreement: Homosexuality (R)"
attr(data_survey_complete$tp_religious_l, "label") <- "Topic agreement: Religious (L)"
attr(data_survey_complete$tp_religious_r, "label") <- "Topic agreement: Religious (R)"
attr(data_survey_complete$tp_inequality_l, "label") <- "Topic agreement: Inequality (L)"
attr(data_survey_complete$tp_inequality_r, "label") <- "Topic agreement: Inequality (R)"
attr(data_survey_complete$tp_nationalism_l, "label") <- "Topic agreement: Nationalism (L)"
attr(data_survey_complete$tp_nationalism_r, "label") <- "Topic agreement: Nationalism (R)"
attr(data_survey_complete$tp_police_l, "label") <- "Topic agreement: Police (L)"
attr(data_survey_complete$tp_police_r, "label") <- "Topic agreement: Police (R)"
attr(data_survey_complete$tp_cardriver_l, "label") <- "Topic agreement: Car driver (L)"
attr(data_survey_complete$tp_cardriver_r, "label") <- "Topic agreement: Car driver (R)"



# empathy


# empathy scale
empathy_scale_fit <- psych::principal(data_survey_complete %>% dplyr::select(empathy_person, empathy_predicting, empathy_perspective), nfactors = 1, rotate = "varimax", missing = TRUE, impute = "mean")
data_survey_complete$empathy_score <- predict(empathy_scale_fit, dplyr::select(data_survey_complete, empathy_person, empathy_predicting, empathy_perspective))[,1]
empathy_score_quantiles <- quantile(data_survey_complete$empathy_score, probs = c(0, .33, .66, 1), na.rm = TRUE)
data_survey_complete$empathy_score_cat <- cut(data_survey_complete$empathy_score, breaks = c(-Inf, empathy_score_quantiles[2], empathy_score_quantiles[3], Inf), labels = c("Low", "Medium", "High"), right = TRUE)

# empathy person
attribute_levels <- c("Strongly disagree", "Somewhat disagree", "Neither/nor", "Somewhat agree", "Strongly agree")
data_survey_complete$empathy_person <- recode_factor(data_survey_complete$empathy_person, 
                                                        `1` = attribute_levels[1], 
                                                        `2` = attribute_levels[2],
                                                        `3` = attribute_levels[3],
                                                        `4` = attribute_levels[4],
                                                        `5` = attribute_levels[5]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$empathy_person, "label") <- "Empathetic person"

# empathy predicting
data_survey_complete$empathy_predicting <- recode_factor(data_survey_complete$empathy_predicting, 
                                                     `1` = attribute_levels[1], 
                                                     `2` = attribute_levels[2],
                                                     `3` = attribute_levels[3],
                                                     `4` = attribute_levels[4],
                                                     `5` = attribute_levels[5]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$empathy_predicting, "label") <- "Empathy prediction"

# empathy perspective
data_survey_complete$empathy_perspective <- recode_factor(data_survey_complete$empathy_perspective, 
                                                         `1` = attribute_levels[1], 
                                                         `2` = attribute_levels[2],
                                                         `3` = attribute_levels[3],
                                                         `4` = attribute_levels[4],
                                                         `5` = attribute_levels[5]) %>%
  as.character() %>% 
  factor(levels = attribute_levels)
attr(data_survey_complete$empathy_perspective, "label") <- "Empathy perspective-taking"



# US-specific items ---------------------------

# feeling thermometers and Musk items

data_survey_complete %<>% dplyr::rename(
  all_of(c(usa_thermometer_desantis = "thermometer_usa_1",
           usa_thermometer_trump = "thermometer_usa_2",
           usa_thermometer_biden = "thermometer_usa_3",
           usa_thermometer_pelosi = "thermometer_usa_4",
           usa_thermometer_musk = "thermometer_usa_5",
           usa_thermometer_zuck = "thermometer_usa_6",
           usa_thermometer_cook = "thermometer_usa_7",
           usa_musk_twitter = "musk_twitter_usa",
           usa_musk_trump = "musk_trump_usa"
  )))


# turning all model variables into factors 

# data_survey_complete <- data_survey_complete %>%
#   mutate_at(vars(target_topic, target_position, target_message, target_ethnicity, target_avatar, target_gender, sender_scope, 
#                  sender_category, sender_scope, sender_hatescore, sender_gender, sender_ethnicity, sender_avatar), 
#             as.factor) %>%
#   ungroup()



# drop useless variables -------------------

data_survey_complete <- dplyr::select(data_survey_complete, -welcome, -vig_intro_free, -vig_intro_neutral, -vig_intro_protect, -vig_manipcheck, -ethnicity_w, -ethnicity_m, -ethnicity_o, -resp_victims, -resp_witnesses, -resp_platforms, -resp_lawmakers, -resp_justice, -resp_employers
)




# re-arrange variables -------------------

data_survey_complete <- data_survey_complete %>% relocate(
  starts_with("resp_"),
  consent, gender, birthyear, 
  starts_with("age"),
  starts_with("ethnicity"), minority, white_cat, minority_cat,
  starts_with("education"), starts_with("educ_"),
  starts_with("tp_"),
  polinterest, polinterest_cat, leftright, leftright_cat,
  starts_with("partyid_"),
  starts_with("tradeoffs_"),
  content_regulation
) %>% relocate(
  starts_with("t_"), .after = last_col()
)

# export data ---------------------------

data_survey_resp <- data_survey_complete 

save(data_survey_resp, file = "data/cooked/data_survey_resp.RData")

