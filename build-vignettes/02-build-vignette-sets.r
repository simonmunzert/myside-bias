
## load packages and functions ------------------

library(tidyverse)
library(magrittr)
library(readxl)
library(writexl)
library(janitor)
library(magick)
library(summarytools)
library(ragg)


### load image data ---------------------

# import interface pictures
target_message_0lines_img <- image_read("images/interface/fb-target-message-0-lines-empty.png")
target_message_1lines_img <- image_read("images/interface/fb-target-message-1-lines-empty.png")
target_message_2lines_img <- image_read("images/interface/fb-target-message-2-lines-empty.png")
target_message_3lines_img <- image_read("images/interface/fb-target-message-3-lines-empty.png")
target_message_4lines_img <- image_read("images/interface/fb-target-message-4-lines-empty.png")
sender_message_1lines_img <- image_read("images/interface/fb-sender-message-1-lines-empty.png")
sender_message_2lines_img <- image_read("images/interface/fb-sender-message-2-lines-empty.png")
sender_message_3lines_img <- image_read("images/interface/fb-sender-message-3-lines-empty.png")
sender_message_4lines_img <- image_read("images/interface/fb-sender-message-4-lines-empty.png")
sender_message_memelines_img <- image_read("images/interface/fb-sender-message-meme-lines-empty.png")
sender_message_1lines_flag_img <- image_read("images/interface/fb-sender-message-1-lines-empty-flag.png")
sender_message_2lines_flag_img <- image_read("images/interface/fb-sender-message-2-lines-empty-flag.png")
sender_message_3lines_flag_img <- image_read("images/interface/fb-sender-message-3-lines-empty-flag.png")
sender_message_4lines_flag_img <- image_read("images/interface/fb-sender-message-4-lines-empty-flag.png")
sender_message_memelines_flag_img <- image_read("images/interface/fb-sender-message-meme-lines-empty-flag.png")

message_end_img <- image_read("images/interface/fb-message-end-empty.png")
deck_id_img <- image_read("images/interface/fb-deck-id-empty.png")

# import profile pictures
imgs_list <- list.files("images/avatars", pattern = "png$", full.names = TRUE)
imgs_list %<>% lapply(image_read) %>% lapply(image_scale, "68")
names(imgs_list) <- list.files("images/avatars", pattern = "png$") %>% str_replace(".png$", "")



## load prepared vignette data -----------------------

load("vignettes/rdata/vignettes_df.RData")
country_codes_df <- read_xlsx("messages/country-codes.xlsx")
country_codes_vec <- seq_along(country_codes_df$code)


### define vignette generation functions -------------------

# function to construct balanced vignette decks
construct_deck <- function(output = "complete", country.code = "usa", oversample.group = "white") {
  strata_messages_df <- data.frame(
    # sample 8 out of 12 topics
    target_topic = sample(c("nationalism", "police", "homosexuality", "inequality", "freespeech", "immigration", "veganism", "racism", "religious", "feminism", "climatechange", "mensrights", "partisanship", "cardriver"), 8),
    # balance left/right positions
    target_position = sample(c(rep("left", 4), rep("right", 4))),
    # 2x opinion, 2x meme, 1x mocking, 1x insult, 1x threat, 1x(threat|insult)
    sender_category = c(rep("opinion", 2), rep("meme", 2), c("mocking", "insult", "threat"), sample(c("insult", "threat", "insult", "threat"), 1)),
    # opinion has to be group, the rest is randomly assigned (3x personal, 3x group)
    sender_scope = c("group", "group", "group", "personal", sample(c(rep("personal", 2), rep("group", 2)))),
    # meme and mocking has to be severity 1, the rest gets a bit more severity
    sender_hatescore = c(1, 2, 1, 1, 1, sample(c(1, 1, 2, 2, 2, 2), 3)), # ADAPTED AFTER PREREG! BEFORE: sample(c(1, 1, 2, 2), 3))
    stringsAsFactors = FALSE)
  strata_messages_df$message_combination <- str_c(strata_messages_df$target_topic,
                                                  strata_messages_df$target_position,
                                                  strata_messages_df$sender_category,
                                                  strata_messages_df$sender_scope,
                                                  strata_messages_df$sender_hatescore,
                                                  sep = "-")
  strata_avatars_df <- data.frame(
    # country code
    country = country.code,
    # balanced sender gender
    sender_gender = sample(c(rep("male", 4), rep("female", 4))),
    # balanced target gender
    target_gender = sample(c(rep("male", 4), rep("female", 4))),
    # balanced sender ethnicity + oversampled majority group
    sender_ethnicity = sample(c(c("black", "white", "latino", "southasian", "eastasian", "arabic"), rep(oversample.group, 2))),
    # balanced target ethnicity + oversampled majority group
    target_ethnicity = sample(c(c("black", "white", "latino", "southasian", "eastasian", "arabic"), rep(oversample.group, 2))),
    sender_avatar_num = sample(c(rep(1, 4), rep(2, 4))),
    target_avatar_num = sample(c(rep(1, 4), rep(2, 4))),
    stringsAsFactors = FALSE)
  strata_avatars_df$sender_avatar <- str_c(strata_avatars_df$sender_gender, strata_avatars_df$sender_ethnicity, strata_avatars_df$sender_avatar_num, sep = "_")
  strata_avatars_df$target_avatar <- str_c(strata_avatars_df$target_gender, strata_avatars_df$target_ethnicity, strata_avatars_df$target_avatar_num, sep = "_")
  strata_avatars_df$avatars_combination <- str_c("target", 
                                                  str_replace_all(strata_avatars_df$target_avatar, "_", "-"),
                                                  "sender",
                                                  str_replace_all(strata_avatars_df$sender_avatar, "_", "-"),
                                                  sep = "-")
  strata_avatars_df$sender_avatar_num <- NULL
  strata_avatars_df$target_avatar_num <- NULL
  strata_df <- cbind(strata_messages_df, strata_avatars_df)
  if(output == "messages"){
    return(strata_messages_df)
  }
  if(output == "avatars"){
    return(strata_avatars_df)
  }
  if(output == "complete"){
  return(strata_df)
  }
}


# function to build all vignettes
print_vignettes <- function(dat, file, country = "usa", font = "Helvetica", quality = 75) {
  
  # define color
  facebookblue <- rgb(61,88,148, maxColorValue = 255)
  
  # build target message
  target_message_img <- eval(parse(text = paste0("target_message_", dat$target_lines[1], "lines_img"))) 
  target_message_img_build <-
    target_message_img %>% 
    image_scale("800") %>%
    image_annotate(dat$target_message_split[1], size = 20, location = "+25+100", font = font) %>%
    image_composite(eval(parse(text = paste0("imgs_list$", dat$target_avatar[1]))), offset = "+22+20") %>%
    image_annotate(dat$target_name[1], size = 25, location = "+100+27", font = "Helvetica", color = facebookblue)
  
  # build sender message - no meme
  if(dat$sender_category != "meme"){
    sender_message_img <- eval(parse(text = paste0("sender_message_", dat$sender_lines[1], "lines_img"))) 
    sender_message_img_build <-
      sender_message_img %>% 
      image_scale("800") %>%
      image_annotate(dat$sender_name[1], size = 20, location = "+100+25", font = "Helvetica", color = facebookblue) %>%
      image_annotate(dat$sender_message_split[1], size = 20, location = "+100+50", font = font) %>%
      image_composite(image_scale(eval(parse(text = paste0("imgs_list$", dat$sender_avatar[1]))), "50") , offset = "+25+15") 
  }
  
  # build sender message - meme
  if(dat$sender_category == "meme"){
    sender_message_img <- eval(parse(text = "sender_message_memelines_img"))
    meme_img <- image_read(paste0("memes/", country, "/", paste0(str_replace(dat$sender_message[1], ".jpeg", paste0("_", country, ".jpeg")))))
    sender_message_img_build <-
      sender_message_img %>% 
      image_scale("800") %>%
      image_annotate(dat$sender_name[1], size = 20, location = "+100+25", font = "Helvetica", color = facebookblue) %>%
      image_composite(image_scale(meme_img, "x280"), offset = "+100+60") %>% 
      image_composite(image_scale(eval(parse(text = paste0("imgs_list$", dat$sender_avatar[1]))), "50") , offset = "+25+15") 
  }
  
  # combine images
  img_all <- c(target_message_img_build, sender_message_img_build, message_end_img)
  
  # build vignette
  img_out <- img_all %>% image_scale("800") %>% image_append(stack = TRUE)
  
  # export vignette
  image_write(img_out, path = file, format = "jpg", quality = quality)
}# function end



# function to build all vignettes (for INDIA; shorter width)
print_vignettes_ind <- function(dat, file, country = "ind", font = "Helvetica", quality = 75) {
  
  # define color
  facebookblue <- rgb(61,88,148, maxColorValue = 255)
  
  # build target message
  target_message_img <- eval(parse(text = paste0("target_message_", dat$target_lines2[1], "lines_img"))) 
  target_message_img_build <-
    target_message_img %>% 
    image_scale("800") %>%
   # image_annotate(dat$target_message_split2[1], size = 20, location = "+25+100", font = font, color = "white") %>%
    image_composite(eval(parse(text = paste0("imgs_list$", dat$target_avatar[1]))), offset = "+22+20") %>%
    image_annotate(dat$target_name[1], size = 25, location = "+100+27", font = "Helvetica", color = facebookblue)
  
  # build sender message - no meme
  if(dat$sender_category != "meme"){
    sender_message_img <- eval(parse(text = paste0("sender_message_", dat$sender_lines2[1]+1, "lines_img"))) 
    sender_message_img_build <-
      sender_message_img %>% 
      image_scale("800") %>%
      image_annotate(dat$sender_name[1], size = 20, location = "+100+25", font = "Helvetica", color = facebookblue) %>%
      #image_annotate(dat$sender_message_split2[1], size = 20, location = "+100+50", font = font, color = "white") %>%
      image_composite(image_scale(eval(parse(text = paste0("imgs_list$", dat$sender_avatar[1]))), "50") , offset = "+25+15") 
  }
  
  # build sender message - meme
  if(dat$sender_category == "meme"){
    sender_message_img <- eval(parse(text = "sender_message_memelines_img"))
    meme_img <- image_read(paste0("memes/", country, "/", paste0(str_replace(dat$sender_message[1], ".jpeg", paste0("_", country, ".jpeg")))))
    sender_message_img_build <-
      sender_message_img %>% 
      image_scale("800") %>%
      image_annotate(dat$sender_name[1], size = 20, location = "+100+25", font = "Helvetica", color = facebookblue) %>%
      image_composite(image_scale(meme_img, "x280"), offset = "+100+60") %>% 
      image_composite(image_scale(eval(parse(text = paste0("imgs_list$", dat$sender_avatar[1]))), "50") , offset = "+25+15") 
  }
  
  # combine images
  img_all <- c(target_message_img_build, sender_message_img_build, message_end_img)
  
  # build vignette
  img_out <- img_all %>% image_scale("800") %>% image_append(stack = TRUE)

  
  # add Devanagari message; export
  img_gg <- image_ggplot(img_out, interpolate = TRUE)
  sender_lines <- dat$sender_lines2[1]
  
  if(dat$sender_category != "meme" & sender_lines == 1) {
  img_print <- img_gg + 
    annotate("text", 25, 165, size = 3.5, label = dat$target_message_split2[1], family = "Mangal", color = "black", hjust = 0) + 
    annotate("text", 100, 60, size = 3.5, label = dat$sender_message_split2[1], family = "Mangal", color = "black", hjust = 0)
  # export vignette
  agg_jpeg(file, width = 800, height = 258, res = 142, quality = 75)
    print(img_print)
  invisible(dev.off())
  }
  
  if(dat$sender_category != "meme" & sender_lines == 2) {
    img_print <- img_gg + 
      annotate("text", 25, 185, size = 3.5, label = dat$target_message_split2[1], family = "Mangal", color = "black", hjust = 0) + 
      annotate("text", 100, 80, size = 3.5, label = dat$sender_message_split2[1], family = "Mangal", color = "black", hjust = 0)
    # export vignette
    agg_jpeg(file, width = 800, height = 276, res = 142, quality = 75)
    print(img_print)
    invisible(dev.off())
  }
  
  if(dat$sender_category == "meme") {
    img_print <- img_gg + 
      annotate("text", 25, 410, size = 3.5, label = dat$target_message_split2[1], family = "Mangal", color = "black", hjust = 0)
    # export vignette
    agg_jpeg(file, width = 800, height = 524, res = 142, quality = 75)
    print(img_print)
    invisible(dev.off())
  }
}# function end



### construct vignette decks ---------------------------------------

# draw decks
set.seed(123)
n_decks <- 400
factor_avatars <- 10

# message attributes part: re-use this list for all countries
decks_messages_list <- map(1:n_decks, ~ construct_deck(output = "messages"))

# avatar attributes part: adapt this list by country
decks_avatars_list <- vector(mode = "list", length = length(country_codes_df$code))
decks_avatars_list_red <- vector(mode = "list", length = length(country_codes_df$code))
for(i in country_codes_vec){
  decks_avatars_list[[i]] <- map(
    1:(n_decks*factor_avatars), 
    ~ construct_deck(output = "avatars", 
                   oversample.group = country_codes_df$oversample[i],
                   country.code = country_codes_df$code[i]))
  # check for uniqueness of sender/target characters within decks
  unique_avatars <- rep(NA, n_decks*factor_avatars) # as many unique avatars as possible to avoid reappearance of same avatar in deck
  duplicates_count <- rep(NA, n_decks*factor_avatars) # no identical avatars in conversation
  for (j in 1:(n_decks*factor_avatars)){
    unique_avatars[j] <- length(unique(c(decks_avatars_list[[i]][[j]]$sender_avatar, decks_avatars_list[[i]][[j]]$target_avatar)))
    duplicates_count[j] <- sum(decks_avatars_list[[i]][[j]]$sender_avatar == decks_avatars_list[[i]][[j]]$target_avatar)
  }
  decks_avatars_list_red[[i]] <- decks_avatars_list[[i]][unique_avatars >= 13 & duplicates_count == 0]
  decks_avatars_list_red[[i]] <- decks_avatars_list_red[[i]][1:n_decks]
}

# merge message and avatar strata parts by country
decks_full_list <- vector(mode = "list", length = length(country_codes_df$code))
for(i in country_codes_vec){
  decks_full_list[[i]] <- Map(cbind, decks_messages_list, decks_avatars_list_red[[i]])
}
names(decks_full_list) <- country_codes_df$code


# merge country-specific messages, avatar names
decks_1_list <- vector(mode = "list", length = length(country_codes_df$code))
names(decks_1_list) <- country_codes_df$code
decks_2_list <- vector(mode = "list", length = length(country_codes_df$code))
names(decks_2_list) <- country_codes_df$code
decks_complete_list <- vector(mode = "list", length = length(country_codes_df$code))
names(decks_complete_list) <- country_codes_df$code
for(i in country_codes_vec){
  # merge with message texts
  vignettes_content_df <- get(paste0("vignettes_content_", country_codes_df$code[i], "_df"))
  vignettes_content_df_sub <- dplyr::select(vignettes_content_df, message_combination, target_message, target_message_split, target_message_split2, sender_message, sender_message_split, sender_message_split2, target_lines, target_lines2, sender_lines, sender_lines2, target_constraint_gender)
  decks_1_list[[i]] <- Map(left_join, decks_full_list[[i]], list(vignettes_content_df_sub), by = "message_combination") %>% map(filter, target_constraint_gender == target_gender | is.na(target_constraint_gender))
  # merge with avatar names
  vignettes_avatars_df <- get(paste0("vignettes_names_", country_codes_df$code[i], "_df"))
  vignettes_avatars_df_sub <- dplyr::select(vignettes_avatars_df, avatars_combination, target_name, sender_name)
  decks_2_list[[i]] <- Map(left_join, decks_1_list[[i]], list(vignettes_avatars_df_sub), by = "avatars_combination")
  # add deck id, clean up decks
  for(j in seq_len(length(decks_2_list[[i]]))){
    decks_2_list[[i]][[j]] <- decks_2_list[[i]][[j]] %>% 
      mutate(deck_id = paste0(country_codes_df$code[i], "-deck-", j)) %>% 
      slice_sample(prop = 1) %>%
      mutate(vig_pos = row_number()) %>% 
      mutate(attr_combination = paste(message_combination, avatars_combination, sep = "-")
      ) %>% 
      relocate(country, deck_id, message_combination, avatars_combination)
  }
  # merge with vignettes universe to get vignettes id
  vignettes_universe_df <- get(paste0("vignettes_universe_", country_codes_df$code[i], "_df"))
  vignettes_universe_df_sub <- dplyr::select(vignettes_universe_df, attr_combination, vig_id)
  decks_complete_list[[i]] <- Map(left_join, decks_2_list[[i]], list(vignettes_universe_df_sub), by = c("attr_combination"))
  for(j in seq_len(length(decks_complete_list[[i]]))){
    decks_complete_list[[i]][[j]] <- decks_complete_list[[i]][[j]] %>%
      relocate(country, vig_id, deck_id, message_combination, avatars_combination, attr_combination, vig_pos)
  }
}


# fix order of vignettes in decks_complete_list (noticed sorting of vignettes within decks bug, 26.3.2024) --------


# import decks with correct vignettes order
vignette_img_ids_fixed_list <- list()
for (i in seq_along(country_codes_df$code)) {
  vignette_img_ids_fixed_list[[i]] <- read_xlsx("vignettes/rdata/vignette-img-ids-qualtrics-loop-fixed.xlsx", sheet = i)
  
  vignette_img_ids_fixed_list[[i]] <- mutate_all(vignette_img_ids_fixed_list[[i]], function(x) {
                                                 str_extract(x, "[:alpha:]+-vig-[:digit:]+")})
}



  
# re-arrange decks_complete_list according to new order

decks_complete_list_fixed <- decks_complete_list
for (i in seq_along(country_codes_df$code)) {
  for(j in 1:400) {
    df_fixed <- data.frame(vig_id = as.character(vignette_img_ids_fixed_list[[i]][j,]),
                           vig_pos = 1:8)
    decks_complete_list_fixed[[i]][[j]] <- left_join(df_fixed, dplyr::select(decks_complete_list[[i]][[j]], -vig_pos))
  }
}
decks_complete_list <- decks_complete_list_fixed


# create country-specific data frame of generated vignettes

vignettes_complete_list <- vector(mode = "list", length = length(country_codes_df$code))
names(vignettes_complete_list) <- country_codes_df$code
for(i in country_codes_vec){
  vignettes_complete_list[[i]] <- bind_rows(decks_complete_list[[i]])
}


# generate data frame with correct deck_id and vignette_id values

for (i in seq_along(country_codes_df$code)) {
  vignette_img_ids_fixed_list[[i]]$deck_id <- paste0(country_codes_df$code[i], "-deck-", 1:400)
  vignette_img_ids_fixed_list[[i]] <- pivot_longer(vignette_img_ids_fixed_list[[i]], cols = starts_with("vig"), names_to = "vig_pos", values_to = "vig_id")
}
vignette_deck_vignette_ids_df <- bind_rows(vignette_img_ids_fixed_list)
vignette_deck_vignette_ids_df$vig_pos <- str_replace(vignette_deck_vignette_ids_df$vig_pos, "vig_", "") %>% as.numeric()
save(vignette_deck_vignette_ids_df, file = "vignettes/rdata/vignette_deck_vignette_ids_df.RData")


### balance checks on vignette characteristics (dimensions, attributes) ---------------------------------------

for(i in country_codes_vec){
  vignettes_df_sumstats <- filter(vignettes_complete_list[[i]]) %>% 
    select(vig_id, target_topic, target_position,
           target_gender, target_ethnicity, target_name,
           sender_gender, sender_ethnicity, sender_name,
           sender_category, sender_scope, sender_hatescore)
  dfSummary(vignettes_df_sumstats, max.distinct.values = 20, max.string.width = 30, split.cells = 100) %>% view(file = paste0("summarystats2/vignettes_summarystats_", country_codes_df$code[i], ".html"))
}


### prepare decks / vignette ids data frame for qualtrics ---------------------------------------

# prepare deck vig_id dfs

deck_vignette_ids_list <- vector(mode = "list", length = length(country_codes_df$code))
names(deck_vignette_ids_list) <- country_codes_df$code
deck_vignette_ids_img_list <- vector(mode = "list", length = length(country_codes_df$code))
deck_vignette_ids_img_qualtrics_list <- vector(mode = "list", length = length(country_codes_df$code))
names(deck_vignette_ids_img_list) <- country_codes_df$code
for(i in country_codes_vec){
  vignette_ids_list <- map(decks_complete_list[[i]], extract, "vig_id")
  deck_vignette_ids_list[[i]] <- do.call("cbind", vignette_ids_list) %>% t %>% as.data.frame %>% set_names(paste0("vig_", 1:8)) %>% set_rownames(paste0("deck_", seq_len(length(decks_complete_list[[i]]))))
  deck_vignette_ids_img_list[[i]] <- lapply(deck_vignette_ids_list[[i]], function(x) paste0(x, ".jpg")) %>% as.data.frame()
  deck_vignette_ids_img_qualtrics_list[[i]] <- lapply(deck_vignette_ids_list[[i]], function(x) paste0("https://digitalcitizenlab.github.io/vignettes/", country_codes_df$code[i], "/", x, ".jpg")) %>% as.data.frame()
  deck_vignette_ids_img_list[[i]]$deck_id <- rownames(deck_vignette_ids_list[[i]])
  deck_vignette_ids_img_list[[i]] <- deck_vignette_ids_img_list[[i]] %>% relocate(deck_id)
}



# export all data frames with vignettes, decks + vignette/image IDs

save(decks_complete_list,
     vignettes_complete_list,
     deck_vignette_ids_list,
     deck_vignette_ids_img_list, 
     deck_vignette_ids_img_qualtrics_list,
     file = "vignettes/rdata/deck_vignette_ids_list.RDa")

for(i in country_codes_vec){
  write_xlsx(deck_vignette_ids_img_qualtrics_list[[i]], path = paste0("vignettes/rdata/deck_vignette_ids_img_qualtrics_", country_codes_df$code[i],".xlsx"), col_names = TRUE)
}





### generate vignette jpgs  -----------------------------------------

load("vignettes/rdata/deck_vignette_ids_list.RDa")

# print vignettes
for(i in country_codes_vec){
  dat <- vignettes_complete_list[[i]]
  dir.create(paste0("vignettes/img/", country_codes_df$code[i]), showWarnings = FALSE)
  pb = txtProgressBar(min = 0, max = nrow(dat), initial = 0, title = paste0("Country:", i))
  for(j in 1:nrow(dat)){
    if(!file.exists(paste0("vignettes/img/", country_codes_df$code[i], "/", dat[j,]$vig_id, ".jpg"))){
      if(i == 4) { # INDIA: DIFFERENT FONT!
      print_vignettes_ind(dat[j,], file = paste0("vignettes/img/", country_codes_df$code[i], "/",  dat[j,]$vig_id, ".jpg"), country = country_codes_df$code[i], font = "Mangal", quality = 75)
      }else{
        print_vignettes(dat[j,], file = paste0("vignettes/img/", country_codes_df$code[i], "/",  dat[j,]$vig_id, ".jpg"), country = country_codes_df$code[i], font = "Helvetica", quality = 75)
      }
    }
    setTxtProgressBar(pb, j)
  }
}



### set up Qualtrics image links deck spreadsheet  -----------------------------------------

load("vignettes/rdata/deck_vignette_ids_img_list.RDa")

qualtrics_links_export_list <- vector(mode = "list", length = length(country_codes_df$code))
qualtrics_links_import_list <- vector(mode = "list", length = length(country_codes_df$code))
for(i in country_codes_vec){
  qualtrics_links_export_list[[i]] <- read_xlsx(paste0("vignettes/rdata/", country_codes_df$code[i],"_image_name_qualtrics_link_hertie.xlsx"))
  # vig_1
  qualtrics_links_import_list[[i]] <- left_join(deck_vignette_ids_img_list[[i]], dplyr::select(qualtrics_links_export_list[[i]], name, link), by = c("vig_1" = "name"))
  qualtrics_links_import_list[[i]]["vig_1"] <- qualtrics_links_import_list[[i]]$link
  qualtrics_links_import_list[[i]]$link <- NULL
  qualtrics_links_import_list[[i]]$name <- NULL
  # vig_2
  qualtrics_links_import_list[[i]] <- left_join(qualtrics_links_import_list[[i]], dplyr::select(qualtrics_links_export_list[[i]], name, link), by = c("vig_2" = "name"))
  qualtrics_links_import_list[[i]]["vig_2"] <- qualtrics_links_import_list[[i]]$link
  qualtrics_links_import_list[[i]]$link <- NULL
  qualtrics_links_import_list[[i]]$name <- NULL
  # vig_3
  qualtrics_links_import_list[[i]] <- left_join(qualtrics_links_import_list[[i]], dplyr::select(qualtrics_links_export_list[[i]], name, link), by = c("vig_3" = "name"))
  qualtrics_links_import_list[[i]]["vig_3"] <- qualtrics_links_import_list[[i]]$link
  qualtrics_links_import_list[[i]]$link <- NULL
  qualtrics_links_import_list[[i]]$name <- NULL
  # vig_4
  qualtrics_links_import_list[[i]] <- left_join(qualtrics_links_import_list[[i]], dplyr::select(qualtrics_links_export_list[[i]], name, link), by = c("vig_4" = "name"))
  qualtrics_links_import_list[[i]]["vig_4"] <- qualtrics_links_import_list[[i]]$link
  qualtrics_links_import_list[[i]]$link <- NULL
  qualtrics_links_import_list[[i]]$name <- NULL
  # vig_5
  qualtrics_links_import_list[[i]] <- left_join(qualtrics_links_import_list[[i]], dplyr::select(qualtrics_links_export_list[[i]], name, link), by = c("vig_5" = "name"))
  qualtrics_links_import_list[[i]]["vig_5"] <- qualtrics_links_import_list[[i]]$link
  qualtrics_links_import_list[[i]]$link <- NULL
  qualtrics_links_import_list[[i]]$name <- NULL
  # vig_6
  qualtrics_links_import_list[[i]] <- left_join(qualtrics_links_import_list[[i]], dplyr::select(qualtrics_links_export_list[[i]], name, link), by = c("vig_6" = "name"))
  qualtrics_links_import_list[[i]]["vig_6"] <- qualtrics_links_import_list[[i]]$link
  qualtrics_links_import_list[[i]]$link <- NULL
  qualtrics_links_import_list[[i]]$name <- NULL
  # vig_7
  qualtrics_links_import_list[[i]] <- left_join(qualtrics_links_import_list[[i]], dplyr::select(qualtrics_links_export_list[[i]], name, link), by = c("vig_7" = "name"))
  qualtrics_links_import_list[[i]]["vig_7"] <- qualtrics_links_import_list[[i]]$link
  qualtrics_links_import_list[[i]]$link <- NULL
  qualtrics_links_import_list[[i]]$name <- NULL
  # vig_8
  qualtrics_links_import_list[[i]] <- left_join(qualtrics_links_import_list[[i]], dplyr::select(qualtrics_links_export_list[[i]], name, link), by = c("vig_8" = "name"))
  qualtrics_links_import_list[[i]]["vig_8"] <- qualtrics_links_import_list[[i]]$link
  qualtrics_links_import_list[[i]]$link <- NULL
  qualtrics_links_import_list[[i]]$name <- NULL
qualtrics_links_import_list[[i]]$deck_id <- NULL
write_xlsx(qualtrics_links_import_list[[i]], path = paste0("vignettes/rdata/", country_codes_df$code[i],"_image_name_qualtrics_crosswalk.xlsx"))
}



# check for missing image in upload

i = 1 # insert country indicator
missing_matches_mat <- is.na(qualtrics_links_import_list[[i]])
missing_images_list <- list()
for (j in 1:nrow(deck_vignette_ids_img_list[[i]])) {
  missing_images_list[[j]] <- select(deck_vignette_ids_img_list[[i]], -deck_id)[j,][as.logical(missing_matches_mat[j,])]
}
unlist(missing_images_list)


# CHECK MEMES, remove later
num_files <- numeric()
diff_list <- list()
usa_names <- list.files(paste0("memes/", country_codes_df$code[1]))
for(i in country_codes_vec){
  num_files[i] <- length(list.files(paste0("memes/", country_codes_df$code[i])))
  diff_list[[i]] <- setdiff(str_replace(usa_names, "_usa", ""), str_replace(list.files(paste0("memes/", country_codes_df$code[i])), paste0("_", country_codes_df$code[i]), ""))
}
diff_list





## check average hate score with oversampling of severity for residual speech types (insult, threat)
draws_list <- list()
for (i in 1:1000){
  draws_list[[i]] <- c(1, 2, 1, 1, 1, sample(c(1, 1, 2, 2, 2, 2), 3))
}
sapply(draws_list, mean) %>% summary()
