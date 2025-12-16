
## load packages and functions ------------------

library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
library(janitor)
library(stringi)


## functions to process vignettes data ---------

## vignettes: content

str_wrap_string <- function(string, width = 65) {
  out <- stringi::stri_wrap(string, width = width, whitespace_only = TRUE) 
  out <- paste(out, collapse = "\n")
  return(out)
}

import_vignettes_content <- function(country = "usa") {
  vignettes_content_df <- read_xlsx("messages/vignettes-content.xlsx", sheet = paste0("messages_", country))
  vignettes_content_df %<>% fill(target_topic, target_position, target_group, target_message, .direction = "down")
  vignettes_content_df$sender_category[str_detect(vignettes_content_df$sender_message, "jpeg$")] <- "meme"
  vignettes_content_df$country <- country
  vignettes_content_df$target_message_split <- map_chr(vignettes_content_df$target_message, str_wrap_string)
  vignettes_content_df$target_message_split2 <- map_chr(vignettes_content_df$target_message, str_wrap_string, width = 55)
  vignettes_content_df$sender_message_split <- map_chr(vignettes_content_df$sender_message, str_wrap_string)
  vignettes_content_df$sender_message_split2 <- map_chr(vignettes_content_df$sender_message, str_wrap_string, width = 55)
  
  vignettes_content_df$target_lines <- str_count(vignettes_content_df$target_message_split, fixed("\n")) + 1
  vignettes_content_df$target_lines2 <- str_count(vignettes_content_df$target_message_split2, fixed("\n")) + 1
  
  vignettes_content_df$sender_lines <- str_count(vignettes_content_df$sender_message_split, fixed("\n")) + 1
  vignettes_content_df$sender_lines2 <- str_count(vignettes_content_df$sender_message_split2, fixed("\n")) + 1
  
  vignettes_content_df$message_combination <- str_c(vignettes_content_df$target_topic,
                                             vignettes_content_df$target_position,
                                             vignettes_content_df$sender_category,
                                             vignettes_content_df$sender_scope,
                                             vignettes_content_df$sender_hatescore,
                                                 sep = "-")
  vignettes_content_df$message_id <- paste(country, "mssge", seq_len(nrow(vignettes_content_df)), sep = "-")
  vignettes_content_df %<>% relocate(country, message_id, message_combination)
  return(vignettes_content_df)
}

## vignettes: sender/target names

import_vignettes_names <- function(country = "usa") {
  vignettes_names_df <- read_xlsx("messages/vignettes-names.xlsx", sheet = paste0("names_", country))
  vignettes_names_df_sender <- vignettes_names_df
  vignettes_names_df_target <- vignettes_names_df
  names(vignettes_names_df_sender) <- paste0("sender_", names(vignettes_names_df))
  names(vignettes_names_df_target) <- paste0("target_", names(vignettes_names_df))
  
  ids_expand_df <- expand_grid(sender_id = vignettes_names_df_sender$sender_id,
                               target_id = vignettes_names_df_target$target_id) %>%
    filter(sender_id != target_id) # target and sender ID should not be identical -> removed
  vignettes_names_df <- left_join(ids_expand_df, vignettes_names_df_target, by = "target_id") %>%
    left_join(vignettes_names_df_sender, by = "sender_id")
  vignettes_names_df$country <- country
  vignettes_names_df$avatars_combination <- str_c("target", 
                                         str_replace_all(vignettes_names_df$target_avatar, "_", "-"),
                                         "sender",
                                         str_replace_all(vignettes_names_df$sender_avatar, "_", "-"),
                                         sep = "-")
  vignettes_names_df$names_id <- paste(country, "names", seq_len(nrow(vignettes_names_df)), sep = "-")
  vignettes_names_df %<>% relocate(country, names_id, avatars_combination)
  return(vignettes_names_df)
}

## expand full vignettes data frame

generate_vignettes_df <- function(country = "usa") {
  vignettes_content_df <- get(paste0("vignettes_content_", country, "_df"))
  vignettes_names_df <- get(paste0("vignettes_names_", country, "_df"))
  ids_expand_df <- expand_grid(message_id = vignettes_content_df$message_id,
                               names_id = vignettes_names_df$names_id) 
  vignettes_df <- left_join(ids_expand_df, vignettes_content_df, by = "message_id") %>%
    left_join(select(vignettes_names_df, -country), by = "names_id")
  vignettes_df <- vignettes_df %>% filter(target_constraint_gender == target_gender | is.na(target_constraint_gender)) # make sure target gender constraint is met
  # get rid of artificial variation across identical gender/ethnicity combinations caused by different names
 # vignettes_df$attr_combination <- paste(vignettes_df$message_combination, str_replace_all(vignettes_df$avatars_combination, "-[:digit:]", ""), sep = "-")
  vignettes_df$attr_combination <- paste(vignettes_df$message_combination, vignettes_df$avatars_combination, sep = "-")
  set.seed(123)
  #vignettes_df <- vignettes_df[sample(nrow(vignettes_df)),]
  #vignettes_df <- vignettes_df[!duplicated(vignettes_df$attr_combination),]
  # finalize, return
  vignettes_df$vig_id <- paste(country, "vig", seq_len(nrow(vignettes_df)), sep = "-")
  vignettes_df %<>% relocate(country, vig_id, message_id, names_id, message_combination, avatars_combination, attr_combination)
  return(vignettes_df)
}



## generate full vignette data frames ---------------------------------

vignettes_content_usa_df <- import_vignettes_content("usa")
vignettes_names_usa_df <- import_vignettes_names("usa")
vignettes_universe_usa_df <- generate_vignettes_df("usa")

vignettes_content_ger_df <- import_vignettes_content("ger")
vignettes_names_ger_df <- import_vignettes_names("ger")
vignettes_universe_ger_df <- generate_vignettes_df("ger")

vignettes_content_bra_df <- import_vignettes_content("bra")
vignettes_names_bra_df <- import_vignettes_names("bra")
vignettes_universe_bra_df <- generate_vignettes_df("bra")

vignettes_content_ind_df <- import_vignettes_content("ind")
vignettes_names_ind_df <- import_vignettes_names("ind")
vignettes_universe_ind_df <- generate_vignettes_df("ind")

vignettes_content_indeng_df <- import_vignettes_content("indeng")
vignettes_names_indeng_df <- import_vignettes_names("indeng")
vignettes_universe_indeng_df <- generate_vignettes_df("indeng")

vignettes_content_idn_df <- import_vignettes_content("idn")
vignettes_names_idn_df <- import_vignettes_names("idn")
vignettes_universe_idn_df <- generate_vignettes_df("idn")

vignettes_content_nig_df <- import_vignettes_content("nig")
vignettes_names_nig_df <- import_vignettes_names("nig")
vignettes_universe_nig_df <- generate_vignettes_df("nig")

vignettes_content_phl_df <- import_vignettes_content("phl")
vignettes_names_phl_df <- import_vignettes_names("phl")
vignettes_universe_phl_df <- generate_vignettes_df("phl")

vignettes_content_phleng_df <- import_vignettes_content("phleng")
vignettes_names_phleng_df <- import_vignettes_names("phleng")
vignettes_universe_phleng_df <- generate_vignettes_df("phleng")

vignettes_content_col_df <- import_vignettes_content("col")
vignettes_names_col_df <- import_vignettes_names("col")
vignettes_universe_col_df <- generate_vignettes_df("col")

vignettes_content_tur_df <- import_vignettes_content("tur")
vignettes_names_tur_df <- import_vignettes_names("tur")
vignettes_universe_tur_df <- generate_vignettes_df("tur")

vignettes_content_gbr_df <- import_vignettes_content("gbr")
vignettes_names_gbr_df <- import_vignettes_names("gbr")
vignettes_universe_gbr_df <- generate_vignettes_df("gbr")

vignettes_content_pol_df <- import_vignettes_content("pol")
vignettes_names_pol_df <- import_vignettes_names("pol")
vignettes_universe_pol_df <- generate_vignettes_df("pol")



# check that the same attr_combination values with same vig_id are generated across countries
vignettes_universe_usa_df$attr_combination[1:10]
vignettes_universe_ger_df$attr_combination[1:10]
vignettes_universe_pol_df$attr_combination[1:10]
vignettes_universe_nig_df$attr_combination[1:10]


save(vignettes_content_usa_df, vignettes_names_usa_df, vignettes_universe_usa_df,  
     vignettes_content_ger_df, vignettes_names_ger_df, vignettes_universe_ger_df,  
     vignettes_content_bra_df, vignettes_names_bra_df, vignettes_universe_bra_df,  
     vignettes_content_ind_df, vignettes_names_ind_df, vignettes_universe_ind_df,  
     vignettes_content_indeng_df, vignettes_names_indeng_df, vignettes_universe_indeng_df,  
     vignettes_content_idn_df, vignettes_names_idn_df, vignettes_universe_idn_df,  
     vignettes_content_nig_df, vignettes_names_nig_df, vignettes_universe_nig_df,  
     vignettes_content_phl_df, vignettes_names_phl_df, vignettes_universe_phl_df,  
     vignettes_content_phleng_df, vignettes_names_phleng_df, vignettes_universe_phleng_df,  
     vignettes_content_col_df, vignettes_names_col_df, vignettes_universe_col_df, 
     vignettes_content_tur_df, vignettes_names_tur_df, vignettes_universe_tur_df,  
     vignettes_content_gbr_df, vignettes_names_gbr_df, vignettes_universe_gbr_df,  
     vignettes_content_pol_df, vignettes_names_pol_df, vignettes_universe_pol_df,  
     file = "vignettes/rdata/vignettes_df.RData")


