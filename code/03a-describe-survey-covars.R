# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")


# load data ---------------------------

load("data/cooked/data_survey_resp.RData")
load("data/cooked/data_survey_combined.RData")


# timing of survey --------------

range(data_survey_resp$resp_date_start)


# number of respondents --------------

# number of respondents

length(unique(data_survey_resp$resp_id))

# table: number of respondents by country

options(modelsummary_format_numeric_latex = "mathmode")
sum_row <- data.frame("Total", formatC(nrow(data_survey_resp), format = "d", big.mark = ","), 100)
sum_row <- data.frame(lapply(sum_row, function(x) sprintf("\\textbf{%s}", x)))
sum_row[,1] <- paste0("\\hline \n", sum_row[,1])
dat <- dplyr::select(data_survey_resp, Country = resp_country2_lab)
datasummary(as.formula("Country ~ 1 * (N + Percent('col'))"), 
            data = dat, 
            fmt = function(x) format(round(x, 0), big.mark = ","), 
            title = "Number of respondents, by country\\label{tab:respondents-n}",
            escape = FALSE,
            add_rows = sum_row, 
            align = "lrr",
            output = "figures/respondents-n-by-country.tex")



# respondent demographic covariates --------------

# table: pooled across countries

dat_summary <- data_survey_resp %>% dplyr::select(all_of(resp_demographics_covars))
datasummary(as.formula(paste0(paste0(resp_demographics_covars, collapse = "+"), " ~ N + Percent('col')")), 
            data = dat_summary, 
            fmt = function(x) format(round(x, 0), big.mark = ","), 
            title = "Respondent demographics (percentages)\\label{tab:respondents-demcovars}",
            escape = FALSE,
            output = "figures/respondents-demcovars.tex")

# table: by country

dat_summary <- data_survey_resp %>% dplyr::select(all_of(resp_demographics_covars), Country = resp_country2_lab)

datasummary(as.formula(paste0(paste0(resp_demographics_covars, collapse = "+"), " ~ Country * (Percent('col'))")), 
            data = dat_summary, 
            fmt = function(x) format(round(x, 0), big.mark = ","), 
            title = "Respondent demographics (percentages), by country\\label{tab:respondents-demcovars-country}",
            escape = FALSE,
            output = "figures/respondents-demcovars-by-country.tex")



# respondent avatars --------------


data_survey_resp$avatar_gender <- ifelse(str_detect(data_survey_resp$ethnicity_code, "female"), "Female", "Male")
avatar_sum <- data_survey_resp %>%
  group_by(avatar_gender, resp_country2_lab, ethnicity_code) %>%
  summarise(count = n()) %>%
  group_by(avatar_gender, resp_country2_lab) %>%
  mutate(percentage = round(count / sum(count) * 100, 0)) %>%
  ungroup() %>%
  select(avatar_gender, resp_country2_lab, ethnicity_code, percentage) %>%
  pivot_wider(names_from = resp_country2_lab, values_from = percentage) %>% 
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>%
  filter(!is.na(avatar_gender))

country_order <- dplyr::filter(avatar_sum, ethnicity_code == "female_white_2") %>%
  select(all_of(unique(data_survey_resp$resp_country2_lab))) %>%
  pivot_longer(everything(), names_to = "resp_country2_lab", values_to = "percentage") %>% arrange(desc(percentage)) %>% pull(resp_country2_lab)

avatar_sum <- avatar_sum %>% dplyr::relocate(all_of(country_order), .after = ethnicity_code)

  

colors <- c('#ffffff','#e0f3f8','#abd9e9','#74add1','#4575b4')
color_palette <- colorRampPalette(colors)(7)

avatar_sum %>%
  arrange(desc(Brazil)) %>%
  group_by(avatar_gender) %>%
  gt() %>%
  fmt_image(
    columns = ethnicity_code,
    height = px(35),
    path = "build-vignettes/images/avatars/",
    file_pattern = "{x}.png"
  ) %>%
  data_color(
    columns = where(is.numeric),
    palette = color_palette,
    reverse = FALSE,
    domain = range(dplyr::select(ungroup(avatar_sum), where(is.numeric)), na.rm = TRUE)
    ) %>%
  cols_label(
    ethnicity_code = ""
  ) %>%
  cols_label(
    .fn = str_wrap(width = 15)
  ) %>%
  cols_hide("avatar_gender") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  ) %>%
  cols_width(
    everything() ~ px(50)  
  ) %>%
  opt_table_font(
    font = list(gt::google_font("Fira Sans"))
  ) %>%
  tab_options(
    table.font.size = "9px",
    row_group.padding = px(5),
    data_row.padding = px(2),
  ) %>% 
  gtsave("figures/respondents-avatars-by-country.png", zoom = 6, vwidth = 4000, vheight = 3000)









# respondent attitudes and behaviors --------------

# political interest

tabyl(data_survey_resp$polinterest_cat)
dat <- catvar_summarize("polinterest_cat", by = "resp_country2_lab", data = data_survey_resp)
colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))

# order resp_country2_lab by freq
dat$resp_country2_lab <- factor(dat$resp_country2_lab, levels = dat %>% filter(vallabels == "Not interested at all") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

ggplot(filter(dat, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Political interest", subtitle = '"How interested in politics are you?"') + 
  scale_x_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0.02, 0))) + 
  scale_fill_manual(labels = label_wrap_gen(width = 30), guide = guide_legend(reverse = TRUE), values = colors) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "bottom",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))
ggsave("figures/barplot-polinterest.png", width = 10, height = 5, dpi = 300)


# left-right (11-point scale)

tabyl(data_survey_resp$leftright)
dat <- catvar_summarize("leftright", by = "resp_country2_lab", data = data_survey_resp)
colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))
color_palette <- colorRampPalette(colors)(11)
dat$vallabels <- as.factor(dat$vallabels)

leftright_avg <- data_survey_resp %>% group_by(resp_country2_lab) %>% summarize(mean = mean(leftright, na.rm = TRUE))
leftright_avg$rescaled_mean <- leftright_avg$mean / max(data_survey_resp$leftright, na.rm = TRUE)
dat <- left_join(dat, leftright_avg, by = "resp_country2_lab")

# order resp_country2_lab by freq of "upper" responses
dat$resp_country2_lab <- factor(dat$resp_country2_lab, levels = dat %>% filter(vallabels == "1") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

ggplot(filter(dat, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  xlab("") + ylab("") + 
  labs(title = "Political ideology", subtitle = '"In politics people often talk about the "left" and the "right".\nOn a scale between 1 (furthest left) and 11 (furthest right), where would you place yourself?"') + 
  scale_x_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0.02, 0))) + 
  scale_fill_manual(labels = label_wrap_gen(width = 30), guide = guide_legend(reverse = TRUE), values = color_palette) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "none",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 10),
    #axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))
ggsave("figures/barplot-leftright11.png", width = 10, height = 5, dpi = 300)


# left-right (3-point scale)

tabyl(data_survey_resp$leftright_cat)
dat <- catvar_summarize("leftright_cat", by = "resp_country2_lab", data = data_survey_resp)
colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))
color_palette <- colorRampPalette(colors)(3)
dat$vallabels <- as.factor(dat$vallabels)


# order resp_country2_lab by freq of "upper" responses
dat$resp_country2_lab <- factor(dat$resp_country2_lab, levels = dat %>% filter(vallabels == "Left") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

# recode factor variable: "Left" -> "Left (1-5)", "Center" -> "Center (6)", "Right" -> "Right (7-11)"
dat$vallabels <- recode_factor(dat$vallabels, "Left" = "Left (1-5)", "Center" = "Center (6)", "Right" = "Right (7-11)")

ggplot(filter(dat, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Political ideology", subtitle = '"In politics people often talk about the "left" and the "right".\nOn a scale between 1 (furthest left) and 11 (furthest right), where would you place yourself?"') + 
  scale_fill_manual(labels = label_wrap_gen(width = 30), guide = guide_legend(reverse = TRUE), values = color_palette) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "bottom",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 10),
    #axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))
ggsave("figures/barplot-leftright3.png", width = 10, height = 5, dpi = 300)



# party ID

partyid_vars <- data_survey_resp %>% dplyr::select(starts_with("partyid")) %>% names()

dat <- data_survey_resp %>% 
  dplyr::select("resp_country2_lab", starts_with("partyid")) %>%
  map_dfc(haven::as_factor)

dat_sum <- map_dfr(partyid_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% filter(freq >= 0.03)

dat_sum$vallabels_country <- str_c(dat_sum$vallabels, "-", dat_sum$resp_country2_lab)
dat_sum <- dat_sum %>%
  arrange(desc(freq)) %>%  # Sort by the size of the "freq" variable
  mutate(vallabels_fct = factor(vallabels_country, levels = unique(vallabels_country))) %>%
  ungroup() %>%
  filter(!is.na(vallabels_fct))


ggplot(filter(dat_sum, !is.na(vallabels_fct)), aes(x = vallabels_fct, y = freq)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ resp_country2_lab, scales = "free", ncol = 1) +
  scale_x_discrete(labels = function(x) str_wrap(dat_sum$vallabels[match(x, dat_sum$vallabels_fct)], width = 10)) +
  xlab("") + ylab("") + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "bottom",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))
ggsave("figures/barplot-partyid.png", width = 5, height = 13, dpi = 300)



# empathy

empathy_vars <- paste0("empathy_", c("person", "predicting", "perspective"))
empathy_labels <- c("I am an empathetic person.", "I am good at predicting how others feel.",
                    "I find it easy to take the perspective of others."
                    )
empathy_vars_labels <- data.frame(variable = empathy_vars, varlabel = empathy_labels, stringsAsFactors = FALSE)
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(empathy_vars))

colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))
color_palette <- colorRampPalette(colors)(5)

dat_sum <- map_dfr(empathy_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
  left_join(empathy_vars_labels, by = "variable") %>%
  mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 30)))

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "empathy_person", vallabels == "Strongly agree") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(~ varlabel_wrapped, space = "free", scales = "free_y", switch = "y") +
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Empathy", subtitle = '"To what extent do you agree with the following statements?"') + 
  scale_x_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0.02, 0))) + 
  scale_fill_manual(labels = label_wrap_gen(width = 30), guide = guide_legend(reverse = TRUE), values = color_palette) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "bottom",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))
ggsave("figures/barplot-empathy.png", width = 10, height = 5, dpi = 300)



# experience with offensive content

experience_vars <- paste0("exp_", c("disagree", "angry", "offended", "threatened", "witnessed"))
experience_labels <- c("I have seen views I disagree with.", "I have seen views that make me angry.", "I have been personally been offended or insulted.", "I have personally been threatened.", "I have witnessed how someone else has been offended, insulted, or threatened.")
experience_vars_labels <- data.frame(variable = experience_vars, varlabel = experience_labels, stringsAsFactors = FALSE)
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(experience_vars))

colors <- c('#d7191c','#fdae61', '#a6d96a','#1a9641')
color_palette <- colorRampPalette(colors)(4)

dat_sum <- map_dfr(experience_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
  left_join(experience_vars_labels, by = "variable") %>%
  mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 20), levels = str_wrap(experience_labels, width = 20)))

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "exp_disagree", vallabels == "Often") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(~ varlabel_wrapped, space = "free", scales = "free_y", switch = "y") +
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Online experiences", subtitle = '"How often has the following happened to you on the internet, including social media?"') + 
  scale_x_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0.00, 0))) + 
  scale_fill_manual(labels = label_wrap_gen(width = 30), guide = guide_legend(reverse = TRUE), values = color_palette) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "bottom",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))
ggsave("figures/barplot-experiences.png", width = 10, height = 5, dpi = 300)



# online posting behavior

posting_vars <- paste0("exp_post", c("opinion", "regret", "offensive"))
posting_labels <- c("I posted my opinion on a political issue.", "I posted or shared something that I later regretted or felt ashamed of.", "I posted or shared something that could be seen as offensive.")
posting_vars_labels <- data.frame(variable = posting_vars, varlabel = posting_labels, stringsAsFactors = FALSE)
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(posting_vars))

colors <- c('#d7191c','#fdae61', '#a6d96a','#1a9641')
color_palette <- colorRampPalette(colors)(4)

dat_sum <- map_dfr(posting_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
  left_join(posting_vars_labels, by = "variable") %>%
  mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 40), levels = str_wrap(posting_labels, width = 40)))

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "exp_postopinion", vallabels == "Often") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(~ varlabel_wrapped, space = "free", scales = "free_y", switch = "y") +
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Online posting behavior", subtitle = '"How often has the following happened to you on the internet, including social media?"') + 
  scale_x_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0.00, 0))) + 
  scale_fill_manual(labels = label_wrap_gen(width = 30), guide = guide_legend(reverse = TRUE), values = color_palette) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "bottom",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))
ggsave("figures/barplot-postings.png", width = 10, height = 5, dpi = 300)


# silencing views

silencing_vars <- paste0("silencing_", c("unacceptable", "harmful"))
silencing_labels <- c("We must not let people saying unacceptable things be heard. We must act against them.", "Sometimes even in a democracy harmful views need to be silenced.")
silencing_vars_labels <- data.frame(variable = silencing_vars, varlabel = silencing_labels, stringsAsFactors = FALSE)
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(silencing_vars))

colors <- c('#d7191c','#fdae61', '#a6d96a','#1a9641')
color_palette <- colorRampPalette(colors)(4)

dat_sum <- map_dfr(silencing_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
  left_join(silencing_vars_labels, by = "variable") %>%
  mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 40), levels = str_wrap(silencing_labels, width = 40)))

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "silencing_unacceptable", vallabels == "Strongly agree") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))


ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(~ varlabel_wrapped, space = "free", scales = "free_y", switch = "y") +
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Silencing views", subtitle = '"To what extent do you agree with the following statements?"') + 
  scale_x_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0.00, 0))) + 
  scale_fill_manual(labels = label_wrap_gen(width = 30), guide = guide_legend(reverse = TRUE), values = color_palette) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "bottom",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))
ggsave("figures/barplot-silencing.png", width = 10, height = 5, dpi = 300)



# responsibility to act against online hate speech

responsibility_vars <- paste0("respsblty_", c("victims", "witnesses", "platforms", "lawmakers", "justice", "employers"))
responsibility_labels <- c("People who are victims of online hate speech", "Other users who witness the behavior", "Online services such as social media platforms", "Lawmakers", "The justice system", "Employers of writers of hate speech")
responsibility_vars_labels <- data.frame(variable = responsibility_vars, varlabel = responsibility_labels, stringsAsFactors = FALSE)
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(responsibility_vars))

colors <- c('#d7191c','#fdae61', '#a6d96a','#1a9641')
color_palette <- colorRampPalette(colors)(4)

dat_sum <- map_dfr(responsibility_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
  left_join(responsibility_vars_labels, by = "variable") %>%
  mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 20), levels = str_wrap(responsibility_labels, width = 20)))

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "respsblty_victims", vallabels == "No responsibility at all") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(~ varlabel_wrapped, space = "free", scales = "free_y", switch = "y") +
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Responsibility to act against online hate speech", subtitle = '"To what extent, if at all, do you think each of the following groups have responsibility to act against online hate speech?"') + 
  scale_x_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0.00, 0))) + 
  scale_fill_manual(labels = label_wrap_gen(width = 30), guide = guide_legend(reverse = TRUE), values = color_palette) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "bottom",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))
ggsave("figures/barplot-responsibility.png", width = 10, height = 5, dpi = 300)



# trade-offs in regulating online speech

tradeoffs_vars <- paste0("tradeoffs_", c("speech", "platforms", "govments"))
tradeoffs_labels_lower <- c("People should be able to speak<br>their minds freely online.", "Online services should not be responsible for content<br>users post on their site, even when it’s harassing.", "People should be allowed to express unpopular<br>opinions in public, even those that are deeply<br>offensive to other people.")
tradeoffs_labels_upper <- c("People should be able to feel<br>welcome and safe online.", "Online services have a responsibility to step in when<br>harassing behavior occurs on their site.", "Government should prevent people from engaging<br>in hate speech against certain groups in public.")

# adapt facet labels
label_colors <- c(
  "1" = paste0("<span style='color: #1a9641; font-weight: bold;'>", tradeoffs_labels_upper[1], "</span> ",
               "<br>vs.<br><span style='color: #d7191c; font-weight: bold;'>", tradeoffs_labels_lower[1], "</span>"),
  "2" = paste0("<span style='color: #1a9641; font-weight: bold;'>", tradeoffs_labels_upper[2], "</span> ",
               "<br>vs.<br><span style='color: #d7191c; font-weight: bold;'>", tradeoffs_labels_lower[2], "</span>"),
  "3" = paste0("<span style='color: #1a9641; font-weight: bold;'>", tradeoffs_labels_upper[3], "</span> ",
               "<br>vs.<br><span style='color: #d7191c; font-weight: bold;'>", tradeoffs_labels_lower[3], "</span>")
)

tradeoffs_vars_labels <- data.frame(variable = tradeoffs_vars, varlabel_upper = label_colors, stringsAsFactors = FALSE)
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(tradeoffs_vars))

# recode to "upper" and "lower" labels
dat <- dat %>% 
  mutate(tradeoffs_speech = ifelse(tradeoffs_speech == "Speak freely", "upper", "lower"),
         tradeoffs_platforms = ifelse(tradeoffs_platforms == "Not responsible", "upper", "lower"),
         tradeoffs_govments = ifelse(tradeoffs_govments == "Speak freely", "upper", "lower"))

colors <- c('#d7191c','#fdae61', '#a6d96a','#1a9641')
color_palette <- colorRampPalette(colors)(2)

dat_sum <- map_dfr(tradeoffs_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
  left_join(tradeoffs_vars_labels, by = "variable") %>%
  mutate(varlabel_wrapped_upper = factor(str_wrap(varlabel_upper, width = 40), levels = str_wrap(label_colors, width = 40)))

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "tradeoffs_speech" & vallabels == "lower") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(~ varlabel_wrapped_upper, space = "free", scales = "free_y") +
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Trade-offs in tackling hate speech", subtitle = '"Which side do you agree with more?"') + 
  scale_x_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0.00, 0))) + 
  scale_fill_manual(labels = label_wrap_gen(width = 30), guide = guide_legend(reverse = TRUE), values = color_palette) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "none",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # top strip
    strip.text.x = element_markdown(size = 8, face = "bold"),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))
ggsave("figures/barplot-tradeoffs.png", width = 10, height = 5, dpi = 300)


# content_regulation

tabyl(data_survey_resp$content_regulation)
dat <- catvar_summarize("content_regulation", by = "resp_country2_lab", data = data_survey_resp)
colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))

# order resp_country2_lab by freq of "upper" responses
dat$resp_country2_lab <- factor(dat$resp_country2_lab, levels = dat %>% filter(vallabels == "No regulation") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

ggplot(filter(dat, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Responsibility for social media content regulation", subtitle = '"Who - if any - should mainly be responsible for regulating content on social media platforms?"') + 
  scale_x_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0.02, 0))) + 
  scale_fill_manual(labels = label_wrap_gen(width = 30), guide = guide_legend(reverse = TRUE), values = colors) + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "bottom",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))
ggsave("figures/barplot-contentregulation.png", width = 10, height = 5, dpi = 300)



# issue attitudes left and right

# by statement slant (left vs. right) 
# by country
# by ideology

tp_vars <- tp_vars_labels$variable
colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))
color_palette <- colorRampPalette(colors)(3)

dat <- data_survey_resp %>% dplyr::select(starts_with("tp_"))

dat <- dat %>%
  mutate(across(where(is.factor), ~ fct_explicit_na(., "Unsure"))) %>%
  mutate(across(where(is.factor), ~ factor(., levels = c("Tend to agree", "Unsure", "Tend to disagree"))))


dat_sum_pooled <- map_dfr(tp_vars, catvar_summarize, data = dat) %>% 
  left_join(tp_vars_labels, by = "variable") %>%
  mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 65))) %>%
  mutate(sample = "Pooled", .before = 1) %>% 
  mutate(stance = ifelse(str_detect(variable, "_l"), "Left positions", "Right positions"))

dat_sum_leftright <- map_dfr(tp_vars, catvar_summarize, data = bind_cols(dat, select(data_survey_resp, leftright_cat)), by = "leftright_cat") %>% 
  left_join(tp_vars_labels, by = "variable") %>%
  mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 65))) %>%
  rename(sample = leftright_cat) %>% 
  mutate(stance = ifelse(str_detect(variable, "_l"), "Left positions", "Right positions"))

dat_sum_country <- map_dfr(tp_vars, catvar_summarize, data = bind_cols(dat, select(data_survey_resp, resp_country2_lab)), by = "resp_country2_lab") %>% 
  left_join(tp_vars_labels, by = "variable") %>%
  mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 65))) %>%
  rename(sample = resp_country2_lab) %>% 
  mutate(stance = ifelse(str_detect(variable, "_l"), "Left positions", "Right positions"))

dat_comb <- bind_rows(dat_sum_pooled, dat_sum_leftright)
dat_comb$sample <- factor(dat_comb$sample, levels = c("Pooled", "Left", "Center", "Right"))

# order varlabel_wrapped
dat_comb$varlabel_wrapped <- factor(dat_comb$varlabel_wrapped, levels = dat_comb %>% filter(vallabels == "Tend to disagree" & sample == "Pooled") %>% arrange(desc(freq)) %>% pull(varlabel_wrapped))

main_plot <- 
ggplot(filter(dat_comb, !is.na(sample)), aes(fill = vallabels, y = varlabel_wrapped, x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(stance ~ sample, space = "free", scales = "free_y", switch = "y") +
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Issue positions", subtitle = '"Do you tend to agree or disagree with the following statements?"') + 
  scale_x_continuous(labels = NULL, breaks = NULL, expand = expansion(mult = c(0.02, 0))) + 
  scale_fill_manual(labels = label_wrap_gen(width = 30), guide = guide_legend(reverse = TRUE), values = color_palette) + 
  theme_bw() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "bottom",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 5, 5, 5),
    plot.caption = element_markdown(
      size = 8,             # Font size
      margin = margin(t = 5) # Add some space above the caption
    ))


# test for difference in issue attitude agreement rates between left and right

tp_vars_test <- list()
for(var in tp_vars) {
  dat <- data_survey_resp %>% 
    dplyr::select(leftright_cat, !!sym(var)) %>%
    mutate(across(var, ~ fct_explicit_na(., "Unsure"))) %>%
    filter(!is.na(!!sym(var)) & leftright_cat %in% c("Left", "Right")) %>%
    mutate(response_bin = ifelse(!!sym(var) == "Tend to agree", 1, 0))
  formula <- as.formula("response_bin ~ leftright_cat") 
  tp_vars_test[[var]] <- 
    t_test(x = dat,
           formula = formula, 
           order = c("Left", "Right"), 
           alternative = "two-sided")
  tp_vars_test[[var]]$variable <- var
}
tp_vars_test_df <- bind_rows(tp_vars_test) %>% mutate(stance = ifelse(str_detect(variable, "_l"), "Left", "Right"))
tp_vars_test_df$sample <- "Diff(L-R)"

var_sub <- dplyr::select(dat_comb, variable, varlabel_wrapped) %>% distinct(variable, .keep_all = TRUE)
tp_vars_test_df <- tp_vars_test_df %>% left_join(var_sub, by = "variable")

t_test_plot <- 
  ggplot(tp_vars_test_df, aes(y = varlabel_wrapped, x = .5, label = paste0(
    round(estimate * 100, 0), 
    " [", round(lower_ci * 100, 0), ", ", round(upper_ci * 100, 0), "]"
  ))) +
  geom_text(size = 3, family = "Fira Sans", hjust = .5) +
  facet_grid(stance ~ sample, space = "free", scales = "free_y", switch = "y") +
  xlab("") + 
  ylab("") +
  scale_x_continuous(limits = c(.3,.7)) + 
  theme_bw() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.margin = margin(0, 0, 0, 0),
    strip.text.y = element_blank()
  )

combined_plot <- main_plot + t_test_plot + plot_layout(widths = c(6, 1))
combined_plot

ggsave("figures/barplot-tp-leftright.png", width = 13, height = 9, dpi = 300)


