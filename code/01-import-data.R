# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")

# important thoughts ---


# import raw survey data -----------------------

dat_bra_raw <- read_sav(list.files("data/raw", pattern = "BRA_", full.names = TRUE))
dat_col_raw <- read_sav(list.files("data/raw", pattern = "COL_", full.names = TRUE))
dat_gbr_raw <- read_sav(list.files("data/raw", pattern = "GBR_", full.names = TRUE))
dat_ger_raw <- read_sav(list.files("data/raw", pattern = "GER_", full.names = TRUE))
dat_idn_raw <- read_sav(list.files("data/raw", pattern = "IDN_", full.names = TRUE))
dat_ind_raw <- read_sav(list.files("data/raw", pattern = "IND_", full.names = TRUE))
dat_indeng_raw <- read_sav(list.files("data/raw", pattern = "INDENG_", full.names = TRUE))
dat_nig_raw <- read_sav(list.files("data/raw", pattern = "NIG_", full.names = TRUE))
dat_phl_raw <- read_sav(list.files("data/raw", pattern = "PHL_", full.names = TRUE))
dat_phleng_raw <- read_sav(list.files("data/raw", pattern = "PHLENG_", full.names = TRUE))
dat_pol_raw <- read_sav(list.files("data/raw", pattern = "POL_", full.names = TRUE))
dat_tur_raw <- read_sav(list.files("data/raw", pattern = "TUR_", full.names = TRUE))
dat_usa_raw <- read_sav(list.files("data/raw", pattern = "USA_", full.names = TRUE))

data_list <- list(dat_usa_raw,
                  dat_bra_raw,
                  dat_col_raw,
                  dat_gbr_raw,
                  dat_ger_raw,
                  dat_idn_raw,
                  dat_ind_raw,
                  dat_indeng_raw,
                  dat_nig_raw,
                  dat_phl_raw,
                  dat_phleng_raw,
                  dat_pol_raw,
                  dat_tur_raw
                  )

# functions to pre-process data ----------------

# function to extract deck IDs

extract_deck_id <- function(x) { 
  deck_identify <- dplyr::select(x, contains("t_vig_1_Page_Submit")) 
  which_decks_missing <- deck_identify %>% rowwise %>% is.na()
  extract_vars <- vector(mode = "numeric", length = nrow(which_decks_missing))
  for (i in 1:nrow(which_decks_missing)){ # some people drop out earlier; here we have to assign NA to deck_id
    out <- which_decks_missing[i,][which_decks_missing[i,] == FALSE]
    if(length(out) == 1){
     extract_vars[i] <- out %>% names() %>% str_extract("[:digit:]+")  %>% as.numeric()
    }else{
      extract_vars[i] <- NA
    }
  }
  x$deck_id <- extract_vars
  x
}

# function to extract first vignette image ID (as validity check)

extract_image_id <- function(x){
  # vig 1
  dat_img_id <- x %>%
    dplyr::select(matches("A\\d+_t_vig_1_Page_Submit")) %>%
    map_chr(get_label) %>% str_extract("/[[:alpha:]-]+[:digit:]+.jpg") %>% str_replace("/", "")
  x$vig_1_id <- dat_img_id[x$deck_id]
  x
}

# function to collapse vignette response variables

extract_vig_vars <- function(x){
  vig_vars <- c(
    paste0("vig_", 1:8, "_hate"),
    paste0("vig_", 1:8, "_remove"),
    paste0("vig_", 1:8, "_ban"),
    paste0("vig_", 1:8, "_legal"),
    paste0("vig_", 1:8, "_job"),
    paste0("vig_", 1:8, "_First"),
    paste0("vig_", 1:8, "_Last"),
    paste0("vig_", 1:8, "_Page"),
    paste0("vig_", 1:8, "_Click")
  )
  vig_vars_list <- vector(mode = "list", length = length(vig_vars))
  names(vig_vars_list) <- vig_vars
  for (var in vig_vars){
    vig_vars_list[[var]] <- x %>% dplyr::select(contains(var)) %>% unite(!!var, na.rm = TRUE, remove = TRUE)
  }
  vig_vars_df <- bind_cols(vig_vars_list)
  
  x_sub <- x %>% dplyr::select(-matches(paste(vig_vars, collapse = "|")))
  x_bind <- cbind(x_sub, vig_vars_df)
}



# apply functions to list of data frames ----------

condensed_dfs <- lapply(data_list, function(x){
  x <- x %>% 
    filter(Status == 0) %>% # only keep normal responses, no previews
    extract_deck_id() %>% 
    extract_image_id() %>% 
    extract_vig_vars() %>% 
    relocate(starts_with("t_"), .after = last_col())
})

# combine
data_survey_all <- bind_rows(condensed_dfs)

# export raw imported and combined data
saveRDS(data_survey_all, file = "data/cooked/data_survey_all.rds")



# modify meta variables ---------------------------------

data_survey_all <- readRDS("data/cooked/data_survey_all.rds")

# ad type indicator

data_survey_all$adtype <- str_extract(data_survey_all$Q_Adtype, "(old|young)-(neutral|topic)")
data_survey_all$adtype_neutral <- str_detect(data_survey_all$adtype, "neutral")
data_survey_all$adtype_young <- str_detect(data_survey_all$adtype, "young")

# rename meta variables
data_survey_all <- data_survey_all %>% 
  dplyr::rename(
    resp_id = ResponseId,
    resp_date_start = StartDate,
    resp_date_end = EndDate,
    resp_ip = IPAddress,
    resp_progress = Progress,
    resp_finished = Finished,
    resp_duration = Duration__in_seconds_,
    resp_lat = LocationLatitude,
    resp_lon = LocationLongitude,
    resp_language = UserLanguage,
    resp_browser = meta_information_Browser,
    resp_browser_version = meta_information_Version,
    resp_opsys = meta_information_Operating_System,
    resp_resolution = meta_information_Resolution,
    resp_adtype = adtype,
    resp_adtype_neutral = adtype_neutral,
    resp_adtype_young = adtype_young,
    resp_country = country)

# country - without language diff
data_survey_all$resp_country2 <- data_survey_all$resp_country
data_survey_all$resp_country2[data_survey_all$resp_country2 == "indeng"] <- "ind"
data_survey_all$resp_country2[data_survey_all$resp_country2 == "phleng"] <- "phl"


data_survey_all <- data_survey_all %>% 
  mutate(resp_country_lab =  
           case_match(resp_country,
                      "bra" ~ "Brazil",
                      "col" ~ "Colombia",
                      "gbr" ~ "United Kingdom",
                      "ger" ~ "Germany",
                      "idn" ~ "Indonesia",
                      "ind" ~ "India",
                      "indeng" ~ "India (English)",
                      "nig" ~ "Nigeria",
                      "phl" ~ "Philippines",
                      "phleng" ~ "Philippines (English)",
                      "pol" ~ "Poland",
                      "tur" ~ "Turkey",
                      "usa" ~ "United States")
         ) %>%
  mutate(resp_country2_lab =  
           case_match(resp_country2,
                      "bra" ~ "Brazil",
                      "col" ~ "Colombia",
                      "gbr" ~ "United Kingdom",
                      "ger" ~ "Germany",
                      "idn" ~ "Indonesia",
                      "ind" ~ "India",
                      "nig" ~ "Nigeria",
                      "phl" ~ "Philippines",
                      "pol" ~ "Poland",
                      "tur" ~ "Turkey",
                      "usa" ~ "United States")
  )
  
                                                  


# relocate variables
data_survey_all <- data_survey_all %>% relocate(starts_with("resp_"), .before = consent)

# remove unnecessary variables
data_survey_all <- data_survey_all %>%
  dplyr::select(-RecordedDate, -RecipientLastName, -RecipientFirstName, -RecipientEmail, -Q_Adtype, -Q_terminateflag, -Q_URL, -Q_Language, -Q_Sample, -Status, -ExternalReference, -DistributionChannel)


# survey progress indicator 

# vignettes completed, full survey completed, vignettes not completed
# --> exclude "vignettes not completed" in models 

data_survey_all <- data_survey_all %>%
  mutate(progress_cat = case_when(vig_8_job != "" ~ "vignettes complete", 
                                  resp_progress >= 93 ~ "survey complete",
                                  TRUE ~ "vignettes incomplete"))

# survey completed in less than 5 minutes
# Pre-reg: "We will exclude speeders from the analysis, whom we define as respondents who spent less than 5 minutes on the entire survey."

data_survey_all <- data_survey_all %>%
  mutate(speed_cat = case_when(resp_duration >= 480 ~ "8+ minutes", 
                               resp_duration >= 300 ~ "5-8 minutes",
                               TRUE ~ "less than 5 minutes"))

# keep those that have Progress >= 93%, have spent at least 5 minutes, and have provided consent
data_survey_complete <- filter(data_survey_all, 
                               progress_cat %in% c("vignettes complete", "survey complete"), 
                               speed_cat != "less than 5 minutes", 
                               consent == 1)

# fix deck_id variable
data_survey_complete$deck_id <- paste0(data_survey_complete$resp_country, "-deck-", data_survey_complete$deck_id)


# how many consented?

tabyl(data_survey_all$consent) # 36320

# how many speeders among vignettes/survey completes?

data_survey_all %>% 
  filter(progress_cat %in% c("vignettes complete", "survey complete")) %>%
  filter(consent == 1) %>% 
  tabyl(speed_cat)

# median survey time
summary(data_survey_complete$resp_duration/60)



# store vignette response variables in separate data frame -------------------

data_survey_vignettes <- data_survey_complete %>% dplyr::select(resp_id, deck_id, matches("vig_[[:digit:]].+"))

saveRDS(data_survey_vignettes, file = "data/cooked/data_survey_vignettes.rds")


# save pre-processed data -----------------------

data_survey_complete <- data_survey_complete %>% dplyr::select(-matches("vig_[[:digit:]].+"))

saveRDS(data_survey_complete, file = "data/cooked/data_survey_complete.rds")
