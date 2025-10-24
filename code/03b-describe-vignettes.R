# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")


# load data ---------------------------

load("data/cooked/data_survey_resp.RData")
load("data/cooked/data_survey_combined.RData")


# vignette-level descriptive stats --------------

# total number of vignettes shown

nrow(data_survey_combined)

# number of unique vignettes

length(unique(data_survey_combined$vig_id))


# number of unique vignettes by country

tab <- 
data_survey_combined %>% 
  mutate(vig_id2 = str_replace(vig_id, "eng", "")) %>% 
  group_by(resp_country2_lab) %>%
  summarize(unique_vignettes = length(unique(vig_id2)))

colnames(tab) <- c("Country", "N unique vignettes")

print(xtable(tab, align = rep("r", ncol(tab) + 1), digits = 0, caption = "Number of unique vignettes shown, by country\\label{tab:unique-vignettes-by-country}"), booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!h", label = "tab:vignettes-n", include.rownames = FALSE, format.args=list(big.mark = ","), sanitize.text.function = function(x) {x}, file = "figures/vignettes-unique-by-country.tex")



# distribution of number of times a unique vignette (identical attribute level combination) was shown

dat_all <- data_survey_combined %>% mutate(resp_country2_lab = " Pooled")
dat_tab <- bind_rows(data_survey_combined, dat_all)

tab <- 
  dat_tab %>% 
  mutate(vig_id2 = str_replace(vig_id, "eng", "")) %>% 
  group_by(resp_country2_lab, vig_id) %>%
  summarize(n_vig_shown = n()) %>%
  mutate(n_vig_shown_cat = case_when(n_vig_shown <= 9 ~ as.character(n_vig_shown), TRUE ~ "10+")) %>%
  mutate(n_vig_shown_cat = factor(n_vig_shown_cat, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10+"))) %>%
  group_by(resp_country2_lab, n_vig_shown_cat) %>%
  mutate(count = n()) %>% # Count the occurrences of each category
  group_by(resp_country2_lab) %>%
  mutate(prop = count / sum(count)) %>% # Calculate proportion within each facet
  ungroup()

# compute means and medians
tab_means <- 
  tab %>%
  group_by(resp_country2_lab) %>%
  summarize(mean_vig_shown = round(mean(n_vig_shown, na.rm = TRUE), 0),
            median_vig_shown = median(n_vig_shown, na.rm = TRUE)
  ) %>%
  mutate(facet_titles = paste0("<b>", resp_country2_lab, "</b><br>(mean: ", mean_vig_shown, "; median: ", median_vig_shown, ")"))

tab <- transform(tab, 
                 resp_country2_lab = factor(resp_country2_lab, 
                                            levels = tab_means$resp_country2_lab, 
                                            labels = tab_means$facet_titles)
)


# Plot the data using the calculated proportions
ggplot(tab, aes(x = n_vig_shown_cat, y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ resp_country2_lab, ncol = 3) +
  labs(title = "Times unique vignette configuration was shown", subtitle = '"Unique" defined as non-identical attribute level combination') +
  ylab("") +
  xlab("Number of times") +
  theme_minimal() +
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position = "none",  # Hide the legend if not needed
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_markdown(size = 10),
    strip.background = element_blank(),
    strip.placement = "outside",
    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.margin = margin(5, 5, 5, 5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey90")
  )
ggsave("figures/barplot-vignettes-shown-by-country.png", width = 7, height = 5, dpi = 300)



# distribution of number of times a unique message combination was shown

dat_all <- data_survey_combined %>% mutate(resp_country2_lab = " Pooled")
dat_tab <- bind_rows(data_survey_combined, dat_all)

tab <- 
  dat_tab %>% 
  group_by(resp_country2_lab, message_combination) %>%
  summarize(n_vig_shown = n())

# compute means and medians
tab_means <- 
tab %>%
  group_by(resp_country2_lab) %>%
  summarize(mean_vig_shown = round(mean(n_vig_shown, na.rm = TRUE), 0),
            median_vig_shown = median(n_vig_shown, na.rm = TRUE)
  ) %>%
  mutate(facet_titles = paste0("<b>", resp_country2_lab, "</b><br>(mean: ", mean_vig_shown, "; median: ", median_vig_shown, ")"))

tab <- transform(tab, 
                 resp_country2_lab = factor(resp_country2_lab, 
                                            levels = tab_means$resp_country2_lab, 
                                            labels = tab_means$facet_titles)
)

# Plot the data using the calculated proportions
ggplot(tab, aes(x = n_vig_shown)) +
  geom_histogram(aes(y = ..count../sum(..count..)),  # Calculate shares (proportions)
                 fill = "grey", color = "black", alpha = 0.7) + 
  scale_x_continuous(
    trans = scales::log10_trans(),  # Logarithmic scale
    breaks = scales::log_breaks(base = 10, n = 8),  # Define more breaks
    labels = scales::label_comma(scale = 1)  # Format labels with commas
  ) +
  facet_wrap(~ resp_country2_lab, ncol = 3, scales = "fixed") +
  labs(title = "Times unique vignette messages configuration was shown", subtitle = '"Unique" defined as non-identical messages attribute level combination') +
  ylab("") +
  xlab("Number of times") +
  theme_minimal() +
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position = "none",  # Hide the legend if not needed
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_markdown(size = 10),
    strip.background = element_blank(),
    strip.placement = "outside",
    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.margin = margin(5, 5, 5, 5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey90")
  )
ggsave("figures/barplot-vignettes-messages-shown-by-country.png", width = 7, height = 5, dpi = 300)




# summary of attributes shown --------------

vig_attribute_varnames <- vig_covars_df$vig_attribute_varnames[vig_covars_df$vig_summary == TRUE]
dat_attributes <- data_survey_combined %>% dplyr::select(all_of(c("resp_country2", vig_attribute_varnames)))

# pooled
dat_attributes %>%
  select(-resp_country2) %>%
  select(where(~ is.factor(.) || is.character(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  count(variable, value, name = "n") %>%
  left_join(dplyr::select(vig_covars_df, vig_attribute_varnames, vig_attribute_varlabels), by = c("variable" = "vig_attribute_varnames")) %>%
  mutate(vig_attribute_varlabels = factor(vig_attribute_varlabels, levels = filter(vig_covars_df, vig_summary == TRUE) %>% pull(vig_attribute_varlabels))) %>%
  group_by(vig_attribute_varlabels) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  mutate(percent_lab = sprintf("%.1f%%", percent)) %>%
  mutate(count = paste0(formatC(n, format = "d", big.mark = ","), " (", percent_lab, ")")) %>%
  dplyr::select(vig_attribute_varlabels, value, count, percent) %>%
  arrange(vig_attribute_varlabels) %>%
  gt() %>%
  gt_plt_bar_pct(column = percent, scaled = TRUE,
                 fill = "blue", background = "lightblue") %>%
  cols_label(
    value = "Attribute",
    count = "Count (%)",
    percent = ""
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(everything())
  ) %>%
  opt_table_font(
    font = list(gt::google_font("Fira Sans"))
  ) %>%
  tab_options(
    table.font.size = "9px",
    row_group.padding = px(2),
    data_row.padding = px(1)
  ) %>%
  tab_caption("Pooled sample") %>%
  gtsave("figures/table-attributes-summary-pooled.png", zoom = 6, vwidth = 1000, vheight = 6000)

# as_raw_html(gt_attributes_summary) %>%  writeLines("figures/table-attributes-summary-pooled.html")


# pooled, split in half

dat_attributes %>%
  select(-resp_country2) %>%
  select(where(~ is.factor(.) || is.character(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  count(variable, value, name = "n") %>%
  left_join(dplyr::select(vig_covars_df, vig_attribute_varnames, vig_attribute_varlabels), by = c("variable" = "vig_attribute_varnames")) %>%
  mutate(vig_attribute_varlabels = factor(vig_attribute_varlabels, levels = filter(vig_covars_df, vig_summary == TRUE) %>% pull(vig_attribute_varlabels))) %>%
  group_by(vig_attribute_varlabels) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  mutate(percent_lab = sprintf("%.1f%%", percent)) %>%
  mutate(count = paste0(formatC(n, format = "d", big.mark = ","), " (", percent_lab, ")")) %>%
  dplyr::select(vig_attribute_varlabels, value, count, percent) %>%
  arrange(vig_attribute_varlabels) %>%
  filter(str_detect(vig_attribute_varlabels, "^Message")) %>% # DEFINE ATTRIBUTES TO PRINT
  gt() %>%
  gt_plt_bar_pct(column = percent, scaled = TRUE,
                 fill = "blue", background = "lightblue") %>%
  cols_label(
    value = "Attribute",
    count = "Count (%)",
    percent = ""
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(everything())
  ) %>%
  opt_table_font(
    font = list(gt::google_font("Fira Sans"))
  ) %>%
  tab_options(
    table.font.size = "9px",
    row_group.padding = px(2),
    data_row.padding = px(1)
  ) %>%
  tab_caption("Pooled sample") %>%
  gtsave("figures/table-attributes-summary-pooled-a.png", zoom = 6, vwidth = 1000, vheight = 6000)


dat_attributes %>%
  select(-resp_country2) %>%
  select(where(~ is.factor(.) || is.character(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  count(variable, value, name = "n") %>%
  left_join(dplyr::select(vig_covars_df, vig_attribute_varnames, vig_attribute_varlabels), by = c("variable" = "vig_attribute_varnames")) %>%
  mutate(vig_attribute_varlabels = factor(vig_attribute_varlabels, levels = filter(vig_covars_df, vig_summary == TRUE) %>% pull(vig_attribute_varlabels))) %>%
  group_by(vig_attribute_varlabels) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  mutate(percent_lab = sprintf("%.1f%%", percent)) %>%
  mutate(count = paste0(formatC(n, format = "d", big.mark = ","), " (", percent_lab, ")")) %>%
  dplyr::select(vig_attribute_varlabels, value, count, percent) %>%
  arrange(vig_attribute_varlabels) %>%
  filter(!str_detect(vig_attribute_varlabels, "^Message")) %>% # DEFINE ATTRIBUTES TO PRINT
  gt() %>%
  gt_plt_bar_pct(column = percent, scaled = TRUE,
                 fill = "blue", background = "lightblue") %>%
  cols_label(
    value = "Attribute",
    count = "Count (%)",
    percent = ""
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(everything())
  ) %>%
  opt_table_font(
    font = list(gt::google_font("Fira Sans"))
  ) %>%
  tab_options(
    table.font.size = "9px",
    row_group.padding = px(2),
    data_row.padding = px(1)
  ) %>%
  tab_caption("Pooled sample") %>%
  gtsave("figures/table-attributes-summary-pooled-b.png", zoom = 6, vwidth = 1000, vheight = 6000)



# by country

for (i in country_codes_chr){
dat_attributes %>%
  filter(resp_country2 == i) %>%
  select(-resp_country2) %>%
  select(where(~ is.factor(.) || is.character(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  count(variable, value, name = "n") %>%
  left_join(dplyr::select(vig_covars_df, vig_attribute_varnames, vig_attribute_varlabels), by = c("variable" = "vig_attribute_varnames")) %>%
  mutate(vig_attribute_varlabels = factor(vig_attribute_varlabels, levels = filter(vig_covars_df, vig_summary == TRUE) %>% pull(vig_attribute_varlabels))) %>%
  group_by(vig_attribute_varlabels) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  mutate(percent_lab = sprintf("%.1f%%", percent)) %>%
  mutate(count = paste0(formatC(n, format = "d", big.mark = ","), " (", percent_lab, ")")) %>%
  dplyr::select(vig_attribute_varlabels, value, count, percent) %>%
  arrange(vig_attribute_varlabels) %>%
  gt() %>%
  gt_plt_bar_pct(column = percent, scaled = TRUE,
                 fill = "blue", background = "lightblue") %>%
  cols_label(
    value = "Attribute",
    count = "Count (%)",
    percent = ""
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(everything())
  ) %>%
  opt_table_font(
    font = list(gt::google_font("Fira Sans"))
  ) %>%
  tab_options(
    table.font.size = "9px",
    row_group.padding = px(2),
    data_row.padding = px(1)
  ) %>%
  tab_caption(paste0(country_codes_df$country[country_codes_df$code == i], " sample")) %>%
  gtsave(paste0("figures/table-attributes-summary-", i, ".png"), zoom = 6, vwidth = 1000, vheight = 6000)
}

# by country, split in half


for (i in country_codes_chr){
  dat_attributes %>%
    filter(resp_country2 == i) %>%
    select(-resp_country2) %>%
    select(where(~ is.factor(.) || is.character(.))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    count(variable, value, name = "n") %>%
    left_join(dplyr::select(vig_covars_df, vig_attribute_varnames, vig_attribute_varlabels), by = c("variable" = "vig_attribute_varnames")) %>%
    mutate(vig_attribute_varlabels = factor(vig_attribute_varlabels, levels = filter(vig_covars_df, vig_summary == TRUE) %>% pull(vig_attribute_varlabels))) %>%
    group_by(vig_attribute_varlabels) %>%
    mutate(percent = round(n / sum(n) * 100, 1)) %>%
    mutate(percent_lab = sprintf("%.1f%%", percent)) %>%
    mutate(count = paste0(formatC(n, format = "d", big.mark = ","), " (", percent_lab, ")")) %>%
    dplyr::select(vig_attribute_varlabels, value, count, percent) %>%
    arrange(vig_attribute_varlabels) %>%
    filter(str_detect(vig_attribute_varlabels, "^Message")) %>% # DEFINE ATTRIBUTES TO PRINT
    gt() %>%
    gt_plt_bar_pct(column = percent, scaled = TRUE,
                   fill = "blue", background = "lightblue") %>%
    cols_label(
      value = "Attribute",
      count = "Count (%)",
      percent = ""
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups(everything())
    ) %>%
    opt_table_font(
      font = list(gt::google_font("Fira Sans"))
    ) %>%
    tab_options(
      table.font.size = "9px",
      row_group.padding = px(2),
      data_row.padding = px(1)
    ) %>%
    tab_caption(paste0(country_codes_df$country[country_codes_df$code == i], " sample")) %>%
    gtsave(paste0("figures/table-attributes-summary-", i, "-a.png"), zoom = 6, vwidth = 1000, vheight = 6000)
  
  dat_attributes %>%
    filter(resp_country2 == i) %>%
    select(-resp_country2) %>%
    select(where(~ is.factor(.) || is.character(.))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    count(variable, value, name = "n") %>%
    left_join(dplyr::select(vig_covars_df, vig_attribute_varnames, vig_attribute_varlabels), by = c("variable" = "vig_attribute_varnames")) %>%
    mutate(vig_attribute_varlabels = factor(vig_attribute_varlabels, levels = filter(vig_covars_df, vig_summary == TRUE) %>% pull(vig_attribute_varlabels))) %>%
    group_by(vig_attribute_varlabels) %>%
    mutate(percent = round(n / sum(n) * 100, 1)) %>%
    mutate(percent_lab = sprintf("%.1f%%", percent)) %>%
    mutate(count = paste0(formatC(n, format = "d", big.mark = ","), " (", percent_lab, ")")) %>%
    dplyr::select(vig_attribute_varlabels, value, count, percent) %>%
    arrange(vig_attribute_varlabels) %>%
    filter(!str_detect(vig_attribute_varlabels, "^Message")) %>% # DEFINE ATTRIBUTES TO PRINT
    gt() %>%
    gt_plt_bar_pct(column = percent, scaled = TRUE,
                   fill = "blue", background = "lightblue") %>%
    cols_label(
      value = "Attribute",
      count = "Count (%)",
      percent = ""
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups(everything())
    ) %>%
    opt_table_font(
      font = list(gt::google_font("Fira Sans"))
    ) %>%
    tab_options(
      table.font.size = "9px",
      row_group.padding = px(2),
      data_row.padding = px(1)
    ) %>%
    tab_caption(paste0(country_codes_df$country[country_codes_df$code == i], " sample")) %>%
    gtsave(paste0("figures/table-attributes-summary-", i, "-b.png"), zoom = 6, vwidth = 1000, vheight = 6000)
}




# to what extent did respondents skip vignettes? ------------

# at vignette level
count_na <- function(x) sum(is.na(x))    
count_nas_vig <- data_survey_combined %>% 
  dplyr::select(resp_id, all_of(outcome_vars)) %>%
  mutate(count_na = apply(., 1, count_na))
tabyl(count_nas_vig$count_na) # 1.2% of vignettes were not rated at all

# at respondent level
count_nas_resp <- count_nas_vig %>% group_by(resp_id) %>% summarize(mean_skip = mean(count_na == 4, na.rm = TRUE))
tabyl(count_nas_resp$mean_skip) # 96.8% of respondents did not skip a single vignette
tabyl(count_nas_resp$mean_skip > 0.5) # 0.8% of respondents skipped more than half




# response time by vignette features (position, type) ------------

# vignette position
vig_times_df <- 
data_survey_combined %>%
  group_by(vig_pos) %>%
  summarize(vig_time_median = median(as.numeric(t_vig_page), na.rm = TRUE))

# sender category
vig_times_df <- 
  data_survey_combined %>%
  group_by(sender_category) %>%
  summarize(vig_time_median = median(as.numeric(t_vig_page), na.rm = TRUE))

# sender/target message length
data_survey_combined$target_message_split2
data_survey_combined$sender_message_split2

# issue agreement
vig_times_df <- 
  data_survey_combined %>%
  group_by(target_resp_issue_agreement) %>%
  summarize(vig_time_median = median(as.numeric(t_vig_page), na.rm = TRUE))

# model

data_survey_combined$t_vig_num <- as.numeric(data_survey_combined$t_vig_page)
data_survey_combined$sender_message_length <- nchar(data_survey_combined$sender_message)
data_survey_combined$target_message_length <- nchar(data_survey_combined$target_message)

data_survey_combined$Respondents <- data_survey_combined$resp_id
data_survey_combined$Decks <- data_survey_combined$deck_id
data_survey_combined$Countries <- data_survey_combined$resp_country2

t_model_lmer <- lmer(t_vig_num ~ as.factor(vig_pos) + sender_category + sender_message_length + target_message_length + (1|Respondents) + (1|Decks) + (1|Countries), data = data_survey_combined)
summary(t_model_lmer)

# export to latex

t_model_lmer <- list(t_model_lmer)

coef_names_replace <- c("Intercept", paste0("Vignette position: ", 2:8), paste0("Message category: ", c("Meme", "Mocking", "Insult", "Threat")), "Sender message length (chars)", "Target message length (chars)")
names(t_model_lmer) <- "Response time (s)"
texreg(t_model_lmer,
       single.row = TRUE,
       ci.force = TRUE,
       ci.force.level = 0.95,
       ci.test = 0,
       custom.coef.names = coef_names_replace, 
       caption = "\\textbf{Model of vignette response times (in seconds).} Linear mixed-effects model with person, vignette deck, and country random effects. 95\\% confidence intervals in parentheses.",
       label = "tab:effects-vignette-response-times-pooled",
       dcolumn = TRUE,
       no.margin = TRUE,
       include.aic = FALSE,
       include.bic = FALSE,
       include.loglik = FALSE,
       fontsize = "tiny",
       use.packages = FALSE,
       sanitize.colnames.function = identity, sanitize.rownames.function = identity,
       file = "figures/lmer-vignette-response-times-pooled.tex")


