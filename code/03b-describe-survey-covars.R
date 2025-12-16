# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")


# load data ---------------------------

load("data/cooked/data_survey_resp.RData")
load("data/cooked/data_survey_combined.RData")

use_weights <- TRUE

# respondent avatars --------------

data_survey_resp$avatar_gender <- ifelse(str_detect(data_survey_resp$ethnicity_code, "female"), "Female", "Male")

if(use_weights) {
  avatar_sum <- data_survey_resp %>%
    # drop cases without weights, if any
    filter(!is.na(weights_country)) %>%
    group_by(avatar_gender, resp_country2_lab, ethnicity_code) %>%
    # use weighted counts instead of simple n()
    summarise(weighted_n = sum(weights_country), .groups = "drop_last") %>%
    group_by(avatar_gender, resp_country2_lab) %>%
    mutate(
      percentage = round(weighted_n / sum(weighted_n) * 100, 0)
    ) %>%
    ungroup() %>%
    select(avatar_gender, resp_country2_lab, ethnicity_code, percentage) %>%
    pivot_wider(names_from = resp_country2_lab, values_from = percentage) %>%
    mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>%
    filter(!is.na(avatar_gender))
}else{
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
}

colors <- c('#ffffff','#e0f3f8','#abd9e9','#74add1','#4575b4')
color_palette <- colorRampPalette(colors)(7)

avatar_sum_table <- 
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
  )

if(use_weights){
  avatar_sum_table %>%   gtsave("figures/respondents-avatars-by-country-weights.png", zoom = 6, vwidth = 4000, vheight = 3000)
} else{
  avatar_sum_table %>%   gtsave("figures/respondents-avatars-by-country.png", zoom = 6, vwidth = 4000, vheight = 3000)
}


# respondent ethnicity based on avatar selection --------------

if(use_weights){
  tab_ethnicity <- data_survey_resp %>%
    filter(!is.na(weights_country)) %>%
    # Make NA an explicit category before tabulation
    mutate(ethnicity_cat = ifelse(is.na(ethnicity_cat), "None", ethnicity_cat)) %>%
    group_by(resp_country2_lab, ethnicity_cat) %>%
    summarise(w = sum(weights_country), .groups = "drop_last") %>%
    mutate(prop = w / sum(w)) %>%
    ungroup() %>%
    select(resp_country2_lab, ethnicity_cat, prop) %>%
    pivot_wider(
      names_from = ethnicity_cat,
      values_from = prop
    ) %>%
    # Format as percentages
    mutate(across(where(is.numeric), ~ sprintf("%.1f", .x * 100))) %>%
    rename(Country = resp_country2_lab)
}else{
tab_ethnicity <- 
tabyl(data_survey_resp, resp_country2_lab, ethnicity_cat) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1, affix_sign = FALSE) %>%
  rename(Country = resp_country2_lab, `None` = NA_)
}

# export to latex

if(use_weights){
  latex_table <- kable(
    tab_ethnicity,
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = '\\textbf{Avatar ethnicity by country.} Distribution of selected avatar ethnicities in response to the question, "Which character do you think most closely resembles you?". Estimates apply demographic population weights (gender, age, education).',
    label = "avatar-ethnicity-by-country",
    align = c("l", "r", "r", "r", "r", "r", "r", "r")
  ) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::kable_styling(latex_options = "hold_position")
  writeLines(latex_table, "figures/avatar-ethnicity-by-country-weights.tex")
}else{
  latex_table <- kable(
    tab_ethnicity,
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = '\\textbf{Avatar ethnicity chosen, by country.} Selected avatar in response to the question "Which character do you think most closely resembles you?"',
    label = "avatar-ethnicity-by-country",
    align = c("l", "r", "r", "r", "r", "r", "r", "r")
  ) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::kable_styling(latex_options = "hold_position")
  writeLines(latex_table, "figures/avatar-ethnicity-by-country.tex")
}




# political interest  --------------

if(use_weights){
  dat <- catvar_summarize("polinterest_cat", by = "resp_country2_lab", data = data_survey_resp, weight = "weights_country")
} else {
dat <- catvar_summarize("polinterest_cat", by = "resp_country2_lab", data = data_survey_resp)
}
colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))

# order resp_country2_lab by freq
dat$resp_country2_lab <- factor(dat$resp_country2_lab, levels = dat %>% filter(vallabels == "Not interested at all") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

plot_out <- ggplot(filter(dat, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-polinterest-weights.png", width = 10, height = 3, dpi = 300)
} else{
ggsave("figures/barplot-polinterest.png", width = 10, height = 3, dpi = 300)
}

# left-right (11-point scale) --------------

tabyl(data_survey_resp$leftright)
if(use_weights){
  dat <- catvar_summarize("leftright", by = "resp_country2_lab", data = data_survey_resp, weight = "weights_country")
} else {
  dat <- catvar_summarize("leftright", by = "resp_country2_lab", data = data_survey_resp)
}
colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))
color_palette <- colorRampPalette(colors)(11)
dat$vallabels <- as.factor(dat$vallabels)

leftright_avg <- data_survey_resp %>% group_by(resp_country2_lab) %>% summarize(mean = mean(leftright, na.rm = TRUE))
leftright_avg$rescaled_mean <- leftright_avg$mean / max(data_survey_resp$leftright, na.rm = TRUE)
dat <- left_join(dat, leftright_avg, by = "resp_country2_lab")

# order resp_country2_lab by freq of "upper" responses
dat$resp_country2_lab <- factor(dat$resp_country2_lab, levels = dat %>% filter(vallabels == "1") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

plot_out <- ggplot(filter(dat, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-leftright11-weights.png", width = 10, height = 3, dpi = 300)
} else{
  ggsave("figures/barplot-leftright11.png", width = 10, height = 3, dpi = 300)
}


# left-right (3-point scale) --------------

tabyl(data_survey_resp$leftright_cat)
if(use_weights){
  dat <- catvar_summarize("leftright_cat", by = "resp_country2_lab", data = data_survey_resp, weight = "weights_country")
} else {
  dat <- catvar_summarize("leftright_cat", by = "resp_country2_lab", data = data_survey_resp)
}
colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))
color_palette <- colorRampPalette(colors)(3)
dat$vallabels <- as.factor(dat$vallabels)


# order resp_country2_lab by freq of "upper" responses
dat$resp_country2_lab <- factor(dat$resp_country2_lab, levels = dat %>% filter(vallabels == "Left") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

# recode factor variable: "Left" -> "Left (1-5)", "Center" -> "Center (6)", "Right" -> "Right (7-11)"
dat$vallabels <- recode_factor(dat$vallabels, "Left" = "Left (1-5)", "Center" = "Center (6)", "Right" = "Right (7-11)")

plot_out <- ggplot(filter(dat, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-leftright3-weights.png", width = 10, height = 3, dpi = 300, bg = "white")
} else{
  ggsave("figures/barplot-leftright3.png", width = 10, height = 3, dpi = 300, bg = "white")
}


# empathy --------------

empathy_vars <- paste0("empathy_", c("person", "predicting", "perspective"))
empathy_labels <- c("I am an empathetic person.", "I am good at predicting how others feel.",
                    "I find it easy to take the perspective of others."
                    )
empathy_vars_labels <- data.frame(variable = empathy_vars, varlabel = empathy_labels, stringsAsFactors = FALSE)
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(empathy_vars), "weights_country")

colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))
color_palette <- colorRampPalette(colors)(5)

if(use_weights){
  dat_sum <- map_dfr(empathy_vars, catvar_summarize, data = dat, by = "resp_country2_lab", weight = "weights_country") %>%
    left_join(empathy_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 30)))
} else {
  dat_sum <- map_dfr(empathy_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
    left_join(empathy_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 30)))
  }

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "empathy_person", vallabels == "Strongly agree") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

plot_out <- ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-empathy-weights.png", width = 10, height = 3, dpi = 300, bg = "white")
} else{
  ggsave("figures/barplot-empathy.png", width = 10, height = 3, dpi = 300, bg = "white")
}



# experience with offensive content --------------

experience_vars <- paste0("exp_", c("disagree", "angry", "offended", "threatened", "witnessed"))
experience_labels <- c("I have seen views I disagree with.", "I have seen views that make me angry.", "I have been personally been offended or insulted.", "I have personally been threatened.", "I have witnessed how someone else has been offended, insulted, or threatened.")
experience_vars_labels <- data.frame(variable = experience_vars, varlabel = experience_labels, stringsAsFactors = FALSE)
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(experience_vars), "weights_country")

colors <- c('#d7191c','#fdae61', '#a6d96a','#1a9641')
color_palette <- colorRampPalette(colors)(4)

if(use_weights){
  dat_sum <- map_dfr(experience_vars, catvar_summarize, data = dat, by = "resp_country2_lab", weight = "weights_country") %>% 
    left_join(experience_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 20), levels = str_wrap(experience_labels, width = 20)))
} else {
  dat_sum <- map_dfr(experience_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
    left_join(experience_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 20), levels = str_wrap(experience_labels, width = 20)))
}

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "exp_disagree", vallabels == "Often") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

plot_out <- ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-experiences-weights.png", width = 10, height = 3, dpi = 300, bg = "white")
} else{
  ggsave("figures/barplot-experiences.png", width = 10, height = 3, dpi = 300, bg = "white")
}



# online posting behavior --------------

posting_vars <- paste0("exp_post", c("opinion", "regret", "offensive"))
posting_labels <- c("I posted my opinion on a political issue.", "I posted or shared something that I later regretted or felt ashamed of.", "I posted or shared something that could be seen as offensive.")
posting_vars_labels <- data.frame(variable = posting_vars, varlabel = posting_labels, stringsAsFactors = FALSE)
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(posting_vars), "weights_country")

colors <- c('#d7191c','#fdae61', '#a6d96a','#1a9641')
color_palette <- colorRampPalette(colors)(4)

if(use_weights){
  dat_sum <- map_dfr(posting_vars, catvar_summarize, data = dat, by = "resp_country2_lab", weight = "weights_country") %>% 
    left_join(posting_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 40), levels = str_wrap(posting_labels, width = 40)))
} else {
  dat_sum <- map_dfr(posting_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
    left_join(posting_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 40), levels = str_wrap(posting_labels, width = 40)))
}

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "exp_postopinion", vallabels == "Often") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

plot_out <- ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-hostile-engagement-weights.png", width = 10, height = 3, dpi = 300, bg = "white")
} else{
  ggsave("figures/barplot-hostile-engagement.png", width = 10, height = 3, dpi = 300, bg = "white")
}


# silencing views --------------

silencing_vars <- paste0("silencing_", c("unacceptable", "harmful"))
silencing_labels <- c("We must not let people saying unacceptable things be heard. We must act against them.", "Sometimes even in a democracy harmful views need to be silenced.")
silencing_vars_labels <- data.frame(variable = silencing_vars, varlabel = silencing_labels, stringsAsFactors = FALSE)
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(silencing_vars), "weights_country")

colors <- c('#d7191c','#fdae61', '#a6d96a','#1a9641')
color_palette <- colorRampPalette(colors)(4)

if(use_weights){
  dat_sum <- map_dfr(silencing_vars, catvar_summarize, data = dat, by = "resp_country2_lab", weight = "weights_country") %>% 
    left_join(silencing_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 40), levels = str_wrap(silencing_labels, width = 40)))
} else {
  dat_sum <- map_dfr(silencing_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
    left_join(silencing_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 40), levels = str_wrap(silencing_labels, width = 40)))
}

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "silencing_unacceptable", vallabels == "Strongly agree") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

plot_out <- ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-silencing-weights.png", width = 10, height = 3, dpi = 300, bg = "white")
} else{
  ggsave("figures/barplot-silencing.png", width = 10, height = 3, dpi = 300, bg = "white")
}



# responsibility to act against online hate speech --------------

responsibility_vars <- paste0("respsblty_", c("victims", "witnesses", "platforms", "lawmakers", "justice", "employers"))
responsibility_labels <- c("People who are victims of online hate speech", "Other users who witness the behavior", "Online services such as social media platforms", "Lawmakers", "The justice system", "Employers of writers of hate speech")
responsibility_vars_labels <- data.frame(variable = responsibility_vars, varlabel = responsibility_labels, stringsAsFactors = FALSE)
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(responsibility_vars), "weights_country")

colors <- c('#d7191c','#fdae61', '#a6d96a','#1a9641')
color_palette <- colorRampPalette(colors)(4)

if(use_weights){
  dat_sum <- map_dfr(responsibility_vars, catvar_summarize, data = dat, by = "resp_country2_lab", weight = "weights_country") %>% 
    left_join(responsibility_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 20), levels = str_wrap(responsibility_labels, width = 20)))
} else {
  dat_sum <- map_dfr(responsibility_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
    left_join(responsibility_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 20), levels = str_wrap(responsibility_labels, width = 20)))
}

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "respsblty_victims", vallabels == "No responsibility at all") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

plot_out <- ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-responsibility-weights.png", width = 10, height = 3, dpi = 300, bg = "white")
} else{
  ggsave("figures/barplot-responsibility.png", width = 10, height = 3, dpi = 300, bg = "white")
}


# trade-offs in regulating online speech --------------

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
dat <- data_survey_resp %>% dplyr::select("resp_country2_lab", all_of(tradeoffs_vars), "weights_country")

# recode to "upper" and "lower" labels
dat <- dat %>% 
  mutate(tradeoffs_speech = ifelse(tradeoffs_speech == "Speak freely", "upper", "lower"),
         tradeoffs_platforms = ifelse(tradeoffs_platforms == "Not responsible", "upper", "lower"),
         tradeoffs_govments = ifelse(tradeoffs_govments == "Speak freely", "upper", "lower"))

colors <- c('#d7191c','#fdae61', '#a6d96a','#1a9641')
color_palette <- colorRampPalette(colors)(2)


if(use_weights){
  dat_sum <- map_dfr(tradeoffs_vars, catvar_summarize, data = dat, by = "resp_country2_lab", weight = "weights_country") %>% 
    left_join(tradeoffs_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped_upper = factor(str_wrap(varlabel_upper, width = 40), levels = str_wrap(label_colors, width = 40)))
} else {
  dat_sum <- map_dfr(tradeoffs_vars, catvar_summarize, data = dat, by = "resp_country2_lab") %>% 
    left_join(tradeoffs_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped_upper = factor(str_wrap(varlabel_upper, width = 40), levels = str_wrap(label_colors, width = 40)))
}

# order resp_country2_lab by freq of "upper" responses
dat_sum$resp_country2_lab <- factor(dat_sum$resp_country2_lab, levels = dat_sum %>% filter(variable == "tradeoffs_speech" & vallabels == "lower") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

plot_out <- ggplot(filter(dat_sum, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-tradeoffs-weights.png", width = 10, height = 3, dpi = 300, bg = "white")
} else{
  ggsave("figures/barplot-tradeoffs.png", width = 10, height = 3, dpi = 300, bg = "white")
}


# content regulation --------------

if(use_weights){
  dat <- catvar_summarize("content_regulation", by = "resp_country2_lab", data = data_survey_resp, weight =  "weights_country")
} else {
  dat <- catvar_summarize("content_regulation", by = "resp_country2_lab", data = data_survey_resp)
}

colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))

# order resp_country2_lab by freq of "upper" responses
dat$resp_country2_lab <- factor(dat$resp_country2_lab, levels = dat %>% filter(vallabels == "No regulation") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

plot_out <- ggplot(filter(dat, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-contentregulation-weights.png", width = 10, height = 3, dpi = 300, bg = "white")
} else{
  ggsave("figures/barplot-contentregulation.png", width = 10, height = 3, dpi = 300, bg = "white")
}


# free to speak (personally), now and then --------------

if(use_weights){
  dat <- catvar_summarize("speak_freely_pers", by = "resp_country2_lab", data = data_survey_resp, weight = "weights_country")
} else {
  dat <- catvar_summarize("speak_freely_pers", by = "resp_country2_lab", data = data_survey_resp)
}

colors <- c('#d7191c','#fdae61', '#1a9641')
color_palette <- colorRampPalette(colors)(3)

# order resp_country2_lab by freq of "upper" responses
dat$resp_country2_lab <- factor(dat$resp_country2_lab, levels = dat %>% filter(vallabels == "More free") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

plot_out <- ggplot(filter(dat, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Freedom to speak sb.'s mind, personal", subtitle = '"Do you feel more or less free to speak your mind than you used to, or about the same?"') + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-freetospeak-personal-weights.png", width = 10, height = 3, dpi = 300, bg = "white")
} else{
  ggsave("figures/barplot-freetospeak-personal.png", width = 10, height = 3, dpi = 300, bg = "white")
}



# free to speak (generally), now and then --------------

if(use_weights){
  dat <- catvar_summarize("speak_freely", by = "resp_country2_lab", data = data_survey_resp, weight = "weights_country")
} else {
  dat <- catvar_summarize("speak_freely", by = "resp_country2_lab", data = data_survey_resp)
}

colors <- c('#d7191c','#fdae61', '#1a9641')
color_palette <- colorRampPalette(colors)(3)

# order resp_country2_lab by freq of "upper" responses
dat$resp_country2_lab <- factor(dat$resp_country2_lab, levels = dat %>% filter(vallabels == "More free") %>% arrange(desc(freq)) %>% pull(resp_country2_lab))

plot_out <- ggplot(filter(dat, !is.na(vallabels)), aes(fill = fct_rev(vallabels), y = fct_rev(resp_country2_lab), x = freq)) + 
  geom_bar(stat = "identity", position = "fill") + 
  geom_text(aes(
    label = ifelse(freq >= 0.05, scales::percent(freq, accuracy = 1, suffix = ""), ""),  # Only show label if percentage >= 10%
  ), position = position_fill(vjust = 0.5), size = 3.5, fontface = "bold", family = "Fira Sans", color = "white") + 
  xlab("") + ylab("") + 
  labs(title = "Freedom to speak sb.'s mind, general", subtitle = '"Which of these statements comes closest to your opinion? - People in this country are [more free/just as free/less free] to say what they think than/as they used to."') + 
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
plot_out + 
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )
if(use_weights){
  ggsave("figures/barplot-freetospeak-general-weights.png", width = 10, height = 3, dpi = 300, bg = "white")
} else{
  ggsave("figures/barplot-freetospeak-general.png", width = 10, height = 3, dpi = 300, bg = "white")
}



# issue attitudes left and right --------------

# by statement slant (left vs. right) 
# by country
# by ideology

tp_vars <- tp_vars_labels$variable
colors <- rev(c('#d7191c','#fdae61', '#a6d96a','#1a9641'))
color_palette <- colorRampPalette(colors)(3)

dat <- data_survey_resp %>% dplyr::select(starts_with("tp_"), "weights_country")

dat <- dat %>%
  mutate(across(where(is.factor), ~ fct_explicit_na(., "Unsure"))) %>%
  mutate(across(where(is.factor), ~ factor(., levels = c("Tend to agree", "Unsure", "Tend to disagree"))))


if(use_weights){
  dat_sum_pooled <- map_dfr(tp_vars, catvar_summarize, data = dat, weight = "weights_country") %>% 
    left_join(tp_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 65))) %>%
    mutate(sample = "Pooled", .before = 1) %>% 
    mutate(stance = ifelse(str_detect(variable, "_l"), "Left positions", "Right positions"))
  
  dat_sum_leftright <- map_dfr(tp_vars, catvar_summarize, data = bind_cols(dat, select(data_survey_resp, leftright_cat)), by = "leftright_cat", weight = "weights_country") %>% 
    left_join(tp_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 65))) %>%
    rename(sample = leftright_cat) %>% 
    mutate(stance = ifelse(str_detect(variable, "_l"), "Left positions", "Right positions"))
  
  dat_sum_country <- map_dfr(tp_vars, catvar_summarize, data = bind_cols(dat, select(data_survey_resp, resp_country2_lab)), by = "resp_country2_lab", weight = "weights_country") %>% 
    left_join(tp_vars_labels, by = "variable") %>%
    mutate(varlabel_wrapped = factor(str_wrap(varlabel, width = 65))) %>%
    rename(sample = resp_country2_lab) %>% 
    mutate(stance = ifelse(str_detect(variable, "_l"), "Left positions", "Right positions"))
} else {
  dat_sum_pooled <- map_dfr(tp_vars, catvar_summarize, data = dat, weight = "weights_country") %>% 
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
}


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
    )) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank()
  )


# test for difference in issue attitude agreement rates between left and right

tp_vars_test <- list()
for (var in tp_vars) {
  if (use_weights) {
    ## ----------------- WEIGHTED VERSION (survey::svyttest) ----------------- ##
    dat <- data_survey_resp %>% 
      dplyr::select(leftright_cat, !!sym(var), weights_country) %>%
      mutate(
        !!sym(var) := fct_explicit_na(!!sym(var), "Unsure")
      ) %>%
      filter(
        !is.na(!!sym(var)),
        leftright_cat %in% c("Left", "Right"),
        !is.na(weights_country)
      ) %>%
      mutate(
        response_bin  = ifelse(!!sym(var) == "Tend to agree", 1, 0),
        leftright_cat = factor(leftright_cat, levels = c("Left", "Right"))
      )
    
    des <- svydesign(
      ids     = ~1,
      weights = ~weights_country,
      data    = dat
    )
    
    svy_res <- svyttest(response_bin ~ leftright_cat, design = des)
    
    tp_vars_test[[var]] <- broom::tidy(svy_res) %>%
      transmute(
        estimate,
        lower_ci = conf.low,
        upper_ci = conf.high,
        p        = p.value,
        variable = var,
        method   = "weighted"
      )
    
  } else {
    ## ----------------- UNWEIGHTED VERSION (infer::t_test) ------------------ ##
    dat <- data_survey_resp %>% 
      dplyr::select(leftright_cat, !!sym(var)) %>%
      mutate(
        !!sym(var) := fct_explicit_na(!!sym(var), "Unsure")
      ) %>%
      filter(
        !is.na(!!sym(var)),
        leftright_cat %in% c("Left", "Right")
      ) %>%
      mutate(
        response_bin = ifelse(!!sym(var) == "Tend to agree", 1, 0)
      )
    
    formula <- response_bin ~ leftright_cat
    
    res <- t_test(
      x          = dat,
      formula    = formula,
      order      = c("Left", "Right"),
      alternative = "two-sided"
    )
    
    tp_vars_test[[var]] <- res %>%
      mutate(
        variable = var,
        method   = "unweighted"
      )
  }
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

if(use_weights){
  ggsave("figures/barplot-tp-leftright-weights.png", width = 13, height = 9, dpi = 300, bg = "white")
} else{
  ggsave("figures/barplot-tp-leftright.png", width = 13, height = 9, dpi = 300, bg = "white")
}


