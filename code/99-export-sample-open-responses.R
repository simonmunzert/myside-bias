source("packages.R")
source("functions.R")

# export data of 200 sample open responses ------------

data_combined <- readRDS("../data/data_preprocessed_02jan.rds")

set.seed(123)
library(writexl)
open_answers <- dat %>% filter(country == "ger", open_hatedefinition != "", open_allow != "") %>% dplyr::select(starts_with("open")) %>% slice_sample(n = 200) %>% write_xlsx(path = "../data/open-responses-sample-ger.xlsx")

open_answers <- data_combined %>% filter(country == "usa", open_hatedefinition != "", open_allow != "") %>% dplyr::select(starts_with("open")) %>% slice_sample(n = 200) %>% write_xlsx(path = "../data/open-responses-sample-usa.xlsx")

open_answers <- data_combined %>% filter(country == "nig", open_hatedefinition != "", open_allow != "") %>% dplyr::select(starts_with("open")) %>% slice_sample(n = 200) %>% write_xlsx(path = "../data/open-responses-sample-nig.xlsx")

