# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")


# load data ---------------------------

load("data/cooked/data_survey_resp.RData")
load("data/cooked/data_survey_combined.RData")


# heat map of perceived hatefulness vs. preferred actions -------

# pooled

vig_scores_actions <- 
  data_survey_combined %>% 
  filter(!is.na(vig_hate_fct)) %>%
  group_by(vig_hate_fct) %>%
  summarize(n_scores = n(),
            mean_remove = mean(vig_remove, na.rm = TRUE),
            mean_ban = mean(vig_ban, na.rm = TRUE),
            mean_legal = mean(vig_legal, na.rm = TRUE),
            mean_job = mean(vig_job, na.rm = TRUE)
  )
vig_scores_actions_long <- vig_scores_actions %>% pivot_longer(starts_with("mean"), names_to = "action_var", values_to = "value")
vig_scores_actions_long$action_var_fct <- rep(vig_actions_labels, length(unique(vig_scores_actions$vig_hate_fct))) %>% factor(levels = vig_actions_labels)

# generate perception totals
vig_scores_perceptions_total <- 
  data_survey_combined %>% 
  filter(!is.na(vig_hate_fct)) %>%
  group_by(vig_hate_fct) %>%
  summarize(n_scores = n())
vig_scores_perceptions_total$action_var <- "Total"
vig_scores_perceptions_total$value <- vig_scores_perceptions_total$n_scores / sum(vig_scores_actions$n_scores)
vig_scores_perceptions_total$action_var_fct <- "Total"

# generate action totals
vig_scores_actions_total <- 
  vig_scores_actions_long %>%
  mutate(n_actions = n_scores*value) %>% 
  group_by(action_var) %>%
  summarize(n_scores = sum(n_actions))
vig_scores_actions_total$vig_hate_fct <- "Total"
vig_scores_actions_total$value <- vig_scores_actions_total$n_scores / sum(vig_scores_actions$n_scores)

sub <- filter(vig_scores_actions_long, vig_hate_fct == "Hate speech") %>% dplyr::select(action_var, action_var_fct)
vig_scores_actions_total <- left_join(vig_scores_actions_total, sub, by = "action_var")

vig_scores_long <- bind_rows(vig_scores_actions_long, vig_scores_perceptions_total, vig_scores_actions_total)
vig_scores_long$vig_hate_fct <- factor(vig_scores_long$vig_hate_fct, levels = unique(vig_scores_long$vig_hate_fct)[c(3, 2, 1, 4)])
  
ggplot(mapping =  aes(x = vig_hate_fct, y = action_var_fct, fill = value*100)) +
  geom_tile(data = filter(vig_scores_long, vig_hate_fct != "Total", action_var_fct != "Total"), aes(color = "black")) +
  geom_point(data = filter(vig_scores_long, vig_hate_fct == "Total" | action_var_fct == "Total"), aes(color = "black"), size = 12, shape = 21) + 
  geom_text(data = vig_scores_long, aes(label = round(value*100, 0), color = ifelse(value < .6, "white", "black"))) +
  scale_color_manual(values = c("white", "black")) + 
  scale_fill_gradientn(colors = hcl.colors(20, "Reds3", rev = TRUE)) +
  scale_x_discrete(labels = str_wrap(as.character(unique(vig_scores_long$vig_hate_fct))[c(3, 2, 1, 4)], 10), position = "top") +
  scale_y_discrete(limits = c("Total", as.character(unique(vig_scores_actions_long$action_var_fct)))) +
  theme_minimal() + 
  coord_fixed(clip = "off") +
  ylab("") + 
  xlab("") + 
  theme(text = element_text(family = "Fira Sans"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "none",
        legend.title = element_blank(),
        plot.margin = unit(c(0.5,0.1,0.5,0.1),"cm")) +
  annotation_custom(grob = linesGrob(), xmin = -1.2, xmax = -1.2, ymin = 1.8, ymax = 5.2) +
  annotation_custom(grob = linesGrob(), xmin = .5, xmax = 3.5, ymin = 6.75, ymax = 6.75) +
  annotation_custom(grob = grid::textGrob(label = "Perception", gp = gpar(fontface = "bold", family = "Fira Sans")), xmin = -.7, ymin = 7, ymax = 7)  + 
  annotation_custom(grob = grid::textGrob(label = "Preferred actions", gp = gpar(fontface = "bold", family = "Fira Sans"), hjust = 0, rot = 90), xmin = -1.45, xmax = -1.45, ymin = -1.2) 
ggsave("figures/heatplot-actions-by-perceptions-pooled.png", width = 4, height = 4, dpi = 300)


# by country

for (i in country_codes_chr) {
vig_scores_actions <- 
  data_survey_combined %>% 
  filter(resp_country2 == i) %>%
  filter(!is.na(vig_hate_fct)) %>%
  group_by(vig_hate_fct) %>%
  summarize(n_scores = n(),
            mean_remove = mean(vig_remove, na.rm = TRUE),
            mean_ban = mean(vig_ban, na.rm = TRUE),
            mean_legal = mean(vig_legal, na.rm = TRUE),
            mean_job = mean(vig_job, na.rm = TRUE)
  )
vig_scores_actions_long <- vig_scores_actions %>% pivot_longer(starts_with("mean"), names_to = "action_var", values_to = "value")
vig_scores_actions_long$action_var_fct <- rep(vig_actions_labels, length(unique(vig_scores_actions$vig_hate_fct))) %>% factor(levels = vig_actions_labels)

# generate perception totals
vig_scores_perceptions_total <- 
  data_survey_combined %>% 
  filter(resp_country2 == i) %>%
  filter(!is.na(vig_hate_fct)) %>%
  group_by(vig_hate_fct) %>%
  summarize(n_scores = n())
vig_scores_perceptions_total$action_var <- "Total"
vig_scores_perceptions_total$value <- vig_scores_perceptions_total$n_scores / sum(vig_scores_actions$n_scores)
vig_scores_perceptions_total$action_var_fct <- "Total"

# generate action totals
vig_scores_actions_total <- 
  vig_scores_actions_long %>%
  mutate(n_actions = n_scores*value) %>% 
  group_by(action_var) %>%
  summarize(n_scores = sum(n_actions))
vig_scores_actions_total$vig_hate_fct <- "Total"
vig_scores_actions_total$value <- vig_scores_actions_total$n_scores / sum(vig_scores_actions$n_scores)

sub <- filter(vig_scores_actions_long, vig_hate_fct == "Hate speech") %>% dplyr::select(action_var, action_var_fct)
vig_scores_actions_total <- left_join(vig_scores_actions_total, sub, by = "action_var")

vig_scores_long <- bind_rows(vig_scores_actions_long, vig_scores_perceptions_total, vig_scores_actions_total)
vig_scores_long$vig_hate_fct <- factor(vig_scores_long$vig_hate_fct, levels = unique(vig_scores_long$vig_hate_fct)[c(3, 2, 1, 4)])

ggplot(mapping =  aes(x = vig_hate_fct, y = action_var_fct, fill = value*100)) +
  geom_tile(data = filter(vig_scores_long, vig_hate_fct != "Total", action_var_fct != "Total"), aes(color = "black")) +
  geom_point(data = filter(vig_scores_long, vig_hate_fct == "Total" | action_var_fct == "Total"), aes(color = "black"), size = 12, shape = 21) + 
  geom_text(data = vig_scores_long, aes(label = round(value*100, 0), color = ifelse(value < .6, "white", "black"))) +
  scale_color_manual(values = c("white", "black")) + 
  scale_fill_gradientn(colors = hcl.colors(20, "Reds3", rev = TRUE)) +
  scale_x_discrete(labels = str_wrap(as.character(unique(vig_scores_long$vig_hate_fct))[c(3, 2, 1, 4)], 10), position = "top") +
  scale_y_discrete(limits = c("Total", as.character(unique(vig_scores_actions_long$action_var_fct)))) +
  theme_minimal() + 
  coord_fixed(clip = "off") +
  ylab("") + 
  xlab("") + 
  theme(text = element_text(family = "Fira Sans"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "none",
        legend.title = element_blank(),
        plot.margin = unit(c(0.5,0.1,0.5,0.1),"cm")) +
  annotation_custom(grob = linesGrob(), xmin = -1.2, xmax = -1.2, ymin = 1.8, ymax = 5.2) +
  annotation_custom(grob = linesGrob(), xmin = .5, xmax = 3.5, ymin = 6.75, ymax = 6.75) +
  annotation_custom(grob = grid::textGrob(label = "Perception", gp = gpar(fontface = "bold", family = "Fira Sans")), xmin = -.7, ymin = 7, ymax = 7)  + 
  annotation_custom(grob = grid::textGrob(label = "Preferred actions", gp = gpar(fontface = "bold", family = "Fira Sans"), hjust = 0, rot = 90), xmin = -1.45, xmax = -1.45, ymin = -1.2) 
ggsave(paste0("figures/heatplot-actions-by-perceptions-", i, ".png"), width = 4, height = 4, dpi = 300)

}



# profile plot of perceived hatefulness vs. preferred actions -------

# compute by country
vig_scores_all_countries <- list()
for (i in country_codes_chr) {
  vig_scores_actions <- 
    data_survey_combined %>% 
    filter(resp_country2 == i) %>%
    filter(!is.na(vig_hate_fct)) %>%
    group_by(vig_hate_fct) %>%
    summarize(n_scores = n(),
              mean_remove = mean(vig_remove, na.rm = TRUE),
              mean_ban = mean(vig_ban, na.rm = TRUE),
              mean_legal = mean(vig_legal, na.rm = TRUE),
              mean_job = mean(vig_job, na.rm = TRUE)
    )
  vig_scores_actions_long <- vig_scores_actions %>% pivot_longer(starts_with("mean"), names_to = "action_var", values_to = "value")
  vig_scores_actions_long$action_var_fct <- rep(vig_actions_labels, length(unique(vig_scores_actions$vig_hate_fct))) %>% factor(levels = vig_actions_labels)
  
  vig_scores_actions_long$country_code <- i
  vig_scores_all_countries[[i]] <- vig_scores_actions_long
}
vig_scores_all <- bind_rows(vig_scores_all_countries)
vig_scores_all$vig_hate_fct <- factor(vig_scores_all$vig_hate_fct, levels = vig_perceptions_labels)

# compute pooled across countries
vig_scores_pooled <- vig_scores_all %>%
  group_by(vig_hate_fct, action_var, action_var_fct) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(country_code = "Pooled")

# combine with pooled
vig_scores_all <- bind_rows(vig_scores_all, vig_scores_pooled)


vig_scores_all <- vig_scores_all %>%
  mutate(
    action_var_fct = fct_relabel(action_var_fct, ~ str_wrap(.x, width = 7)),
    vig_hate_fct = factor(str_wrap(as.character(vig_hate_fct), width = 20),
                          levels = str_wrap(levels(factor(vig_hate_fct)), width = 20))
  )
pooled_only <- vig_scores_all %>% filter(country_code == "Pooled")
others_only <- vig_scores_all %>% filter(country_code != "Pooled")


ggplot() +
  # Plot other countries (grey, semi-transparent)
  geom_line(data = others_only, aes(x = action_var_fct, y = value, group = country_code),
            color = "#2c7bb6", alpha = 0.4, linewidth = 0.8) +
  geom_point(data = others_only, aes(x = action_var_fct, y = value, group = country_code),
             color = "#2c7bb6", alpha = 0.4, size = 2) +
  
  # Plot pooled profile (black, bold)
  geom_line(data = pooled_only, aes(x = action_var_fct, y = value, group = country_code),
            color = "black", linewidth = 1.2) +
  geom_point(data = pooled_only, aes(x = action_var_fct, y = value),
             color = "black", size = 5) +
  
  # Add white text labels inside black pooled points
  geom_text(data = pooled_only,
            aes(x = action_var_fct, y = value, label = round(value * 100)),
            color = "white", fontface = "bold", size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  facet_wrap(~ vig_hate_fct) +
  labs(x = "", y = "Support for action", color = "Country", alpha = "Country") +
  theme_minimal() + 
  theme(text = element_text(family = "Fira Sans"),
        strip.text = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_text(face = "bold"),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, face = "bold"),
        legend.position = "none",
        legend.title = element_blank(),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"))
ggsave("figures/profileplot-actions-by-perceptions.png", width = 8, height = 2.5, dpi = 300, bg = "white")



# distribution of preferred sanctions, by country -------

colors <- c('#d7191c','#fdae61', '#a6d96a','#1a9641')
color_palette <- colorRampPalette(colors)(5)

vig_outcomes_by_country <- 
  data_survey_combined %>%
  group_by(resp_country2_lab) %>%  
  summarize(across(all_of(vig_outcomes_variables), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(desc(vig_perc_none)) 

# gt table
vig_outcomes_by_country %>%
  gt() %>%
  fmt_percent(columns = starts_with("vig_"), decimals = 0) %>%
  data_color(
    columns = vig_outcomes_variables,
    domain = c(min(dplyr::select(vig_outcomes_by_country, where(is.numeric))), max(dplyr::select(vig_outcomes_by_country, where(is.numeric)))),
    palette = color_palette,
    reverse = TRUE
  ) %>%
  cols_label(
    !!!vig_outcomes_labels_set,
    resp_country2_lab = "Country",
    .fn = md
  ) %>%
  fmt_percent(columns = starts_with("vig_"), decimals = 0) %>%
  tab_spanner(label = "Preferred actions", columns = vig_actions_variables) |>
  tab_spanner(label = "Perceptions", columns = vig_perceptions_variables) |>
  tab_options(
    table.font.names = "Fira Sans",
    table.font.size = "9px",
    data_row.padding = px(1)
  ) %>%
  # make spanner labels bold
  tab_style(
    style = list(
      cell_text(weight = "bold") 
    ),
    locations = cells_column_spanners()
  ) %>%
  # set equal column width
  cols_width(
    starts_with("vig_") ~ px(50)  # Apply equal width to all columns except the first
  ) %>%
  # center-align cell content
  tab_style(
    style = list(
      cell_text(align = "center") 
    ),
    locations = cells_column_labels(-1)
  ) %>%
  # make column headers bold
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(columns = 1)
  ) %>%
  # add whitespace
  tab_style(
    style = cell_borders(sides = "left", color = "white", weight = px(5), style = "solid"),
    locations = cells_body(
      columns = vig_actions_variables[1])
  ) %>% 
  gtsave("figures/table-perceptions-actions-by-country.png", zoom = 20)



# diverging bar plot: distribution of perceptions and preferred sanctions, by country -------
# code from https://albert-rapp.de/posts/ggplot2-tips/22_diverging_bar_plot/22_diverging_bar_plot

# prepare data

colors <- c('#636363', '#084594', '#d7191c')

n_by_country <- 
  data_survey_resp %>%
  group_by(resp_country2_lab) %>%  
  summarize(
    n_obs = n()) %>%
  mutate(label2 = paste0(resp_country2_lab, " (", format(n_obs, big.mark = ",", trim = TRUE), ")", sep = "")) %>%
  rename(label = resp_country2_lab)
n_by_country$label2[n_by_country$label == "Colombia"] <- str_replace(n_by_country$label2[n_by_country$label == "Colombia"], fixed("("), "(n=")
n_by_country <- bind_rows(n_by_country, 
                          data.frame(label = "Pooled", n_obs = sum(n_by_country$n_obs), label2 = paste0("Pooled (", format(sum(n_by_country$n_obs), big.mark = ",", trim = TRUE), ")", sep = "")))


vig_outcomes_pooled <- 
  data_survey_combined %>%
  summarize(
    across(all_of(vig_outcomes_variables), \(x) mean(x, na.rm = TRUE))) %>%
  mutate(label = "Pooled")

vig_outcomes_by_country <- 
  data_survey_combined %>%
  group_by(resp_country2_lab) %>%  
  summarize(
    across(all_of(vig_outcomes_variables), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>% 
  arrange(desc(vig_perc_none)) %>% 
  rename(label = resp_country2_lab) %>%
  bind_rows(vig_outcomes_pooled) %>%
  left_join(n_by_country, by = "label") %>%
mutate(label = fct_reorder(label, vig_perc_none, .desc = TRUE)) %>%
  mutate(label2 = fct_reorder(label2, vig_perc_none, .desc = TRUE))

vig_outcomes_by_country <- vig_outcomes_by_country %>%
  mutate(label2 = fct_relevel(label2, as.character(filter(vig_outcomes_by_country, label == "Pooled") %>% pull(label2)), after = 0))
  

dat_longer <- vig_outcomes_by_country |> 
  pivot_longer(
    cols = vig_perc_none:vig_job,
    values_to = 'percentage',
    names_to = 'preference'
  )
  
dat_perceptions <- dat_longer |> 
  filter(preference %in% c('vig_perc_none', 'vig_perc_offensive', 'vig_perc_hate'))

dat_actions <- dat_longer |> 
  filter(!(preference %in% c('vig_perc_none', 'vig_perc_offensive', 'vig_perc_hate'))) %>%
  mutate(preference = factor(preference, levels = vig_actions_variables))

dat_perceptions_comp <- dat_perceptions |> 
  mutate(
    middle_shift = sum(percentage[1]),
    lagged_percentage = lag(percentage, default = 0),
    left = cumsum(lagged_percentage) - middle_shift,
    right = cumsum(percentage) - middle_shift,
    middle_point = (left + right) / 2,
    width = right - left,
    .by = label2
  )

# diverging bar plot: perceptions
bar_width <- 0.75
perceptions_plot <- 
  dat_perceptions_comp |> 
  ggplot() +
  geom_tile(
    aes(
      x = middle_point, 
      y = label2,
      width = width,
      fill = ifelse(label == "Pooled", paste0(preference, "_pooled"), preference)),
    height = bar_width
  ) + 
  geom_vline(
    xintercept = 0,
    color = 'black',
    linewidth = 0.25
  ) + 
  geom_text(
    aes(
      x = middle_point,
      y = label2,
      label = round(percentage*100, 0)
    ),
    color = "white",
    family = 'Fira Sans',
    fontface = 'bold',
    size = 2.5   # adjust size as needed
  ) + 
  geom_richtext(
    data = filter(dat_perceptions_comp, label == "Colombia"),
    aes(
      x = middle_point,
      y = label2,
      label = c("Neither offensive<br>nor hate speech", "Offensive but<br>not hate speech", "Hate speech"),
      color = preference
    ),
    nudge_y = 1.5,
    size = 2.5,
    fill = NA,
    label.color = NA,
    family = 'Fira Sans',
    fontface = 'bold',
    vjust = 1,
    lineheight = 0.8
  ) +
  scale_color_manual(
    values = colors[c(3, 1, 2)]
  ) + 
  scale_fill_manual(
    values = c(
      "vig_perc_none" = colors[1],
      "vig_perc_offensive" = colors[2],
      "vig_perc_hate" = colors[3],
      "vig_perc_none_pooled" = alpha(colors[1], .7),
      "vig_perc_offensive_pooled"    = alpha(colors[2], .7),
      "vig_perc_hate_pooled"                     = alpha(colors[3], .7)
    )
  ) + 
  theme_minimal(
    base_size = 8,
    base_family = 'Fira Sans'
  ) +
  theme(
    legend.position = 'none',
    strip.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold")
  )

# other bar plots: actions
actions_plot <- 
dat_actions %>%
  ggplot() +
  geom_col(
    aes(
      x = percentage, 
      y = label2,
      fill = ifelse(label == "Pooled", "pooled", "other")
    ),
    width = bar_width
    ) + 
  facet_grid(
    cols = vars(preference)
  ) + 
  geom_text(
    aes(
      x = percentage,
      y = label2,
      label = round(percentage*100, 0)
    ),
    color = "black",
    family = 'Fira Sans',
    fontface = 'bold',
    size = 2.5,  
    hjust = -0.25
  ) + 
  geom_richtext(
    data = filter(dat_actions, label == "Colombia"),
    aes(
      x = 0,
      y = label2,
      label = vig_actions_labels
    ),
    nudge_y = 1.5,
    size = 2.5,
    fill = NA,
    label.color = NA,
    family = 'Fira Sans',
    fontface = 'bold',
    vjust = 1,
    hjust = .05,
    lineheight = 0.8
  ) +
  scale_fill_manual(
    values = c("pooled" = alpha("darkgrey", 0.6),
               "other"  = "darkgrey")
  ) + 
  scale_x_continuous(
    limits = c(0, max(dat_actions$percentage, na.rm = TRUE) + 0.25),
    expand = expansion(mult = c(0, 0))
  ) + 
  theme_minimal(
    base_size = 8,
    base_family = 'Fira Sans'
  ) +
  theme(
    legend.position = 'none',
    strip.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# combine
perceptions_plot +
  actions_plot +
  plot_layout(
    ncol = 2,
    widths = c(1, 1)
  )

ggsave(paste0("figures/barplot-perceptions-actions-by-country.png"), width = 10, height = 3.5, dpi = 600)








# table of most hateful messages ------------------------

data_survey_combined$vig_num_selected <- rowSums(dplyr::select(data_survey_combined, all_of(vig_actions_variables)), na.rm = TRUE)

# pooled 

vig_messages_sum <- data_survey_combined %>% 
  group_by(message_combination) %>% 
  summarize(num_ratings = n(),
            across(all_of(vig_outcomes_variables), \(x) mean(x, na.rm = TRUE)), 
            data_actions = list(vig_num_selected),
            .groups = "drop") %>% 
  arrange(desc(vig_perc_hate)) 

vig_meta_df <- dplyr::select(data_survey_combined, resp_country, message_combination, sender_message, sender_category, target_position) %>% filter(resp_country == "usa") %>% distinct(message_combination, .keep_all = TRUE)
vig_messages_df <- left_join(vig_messages_sum, vig_meta_df, by = join_by(message_combination))

# gt
vig_messages_df %>%
  dplyr::select(sender_message, sender_category, target_position, vig_outcomes_variables, data_actions) %>%
  arrange(desc(vig_perc_hate)) %>%
  slice_head(n = 15) %>%
  gt() %>%
  fmt_percent(columns = starts_with("vig_"), decimals = 0) %>%
  gt_plt_dist(data_actions, type = "histogram", line_color = "white",  fill_color = "black", bw = 1) %>%
  data_color(
    columns = vig_perceptions_variables,
    palette = "inferno",
    reverse = TRUE,
    domain = c(min(dplyr::select(vig_messages_df, starts_with("vig_perc"))), max(dplyr::select(vig_messages_df, starts_with("vig_perc"))))
  ) %>%
  tab_options(
    table.font.names = "Fira Sans",
    table.font.size = "9px",
    data_row.padding = px(1)
  ) %>%
  cols_label(
    sender_message = "Message (US version)",
    sender_category = "Sender",
    target_position = "Position",
    !!!vig_outcomes_labels_set,
    data_actions = "Number of<br>selected actions",
    .fn = md
  ) %>%
  tab_spanner(label = "Message features", columns = c(starts_with("sender"), starts_with("target"))) %>%
  tab_spanner(label = "Perceptions", columns = vig_perceptions_variables) %>%
  tab_spanner(label = "Preferred actions", columns = c(vig_actions_variables, data_actions)) %>%
  # make spanner labels bold
  tab_style(
    style = list(
      cell_text(weight = "bold") 
    ),
    locations = cells_column_spanners()
  ) %>%
  # set equal column width
  cols_width(vig_outcomes_variables ~ px(50)  # Apply equal width to all columns except the first
  ) %>%
  tab_options(
    table.font.size = "9px",
    data_row.padding = px(1)
  ) %>%
  gtsave("figures/table-messages-ranked-pooled.png", zoom = 3)


# by country

for(i in country_codes_chr){
  
  vig_messages_sum <- data_survey_combined %>% 
    filter(resp_country == i) %>%
    group_by(message_combination) %>% 
    summarize(num_ratings = n(),
              across(all_of(vig_outcomes_variables), \(x) mean(x, na.rm = TRUE)), 
              data_actions = list(vig_num_selected),
              .groups = "drop") %>% 
    arrange(desc(vig_perc_hate)) 
  vig_meta_df <- dplyr::select(data_survey_combined, resp_country, message_combination, sender_message, sender_category, target_position) %>% filter(resp_country == i) %>% distinct(message_combination, .keep_all = TRUE)
  vig_messages_df <- left_join(vig_messages_sum, vig_meta_df, by = join_by(message_combination))
  
  # gt
  vig_messages_df %>%
    dplyr::select(sender_message, sender_category, target_position, vig_outcomes_variables, data_actions) %>%
    arrange(desc(vig_perc_hate)) %>%
    { bind_rows(head(., n = 5), tail(., n = 5)) } %>%
    distinct() %>%
    gt() %>%
    fmt_percent(columns = starts_with("vig_"), decimals = 0) %>%
    gt_plt_dist(data_actions, type = "histogram", line_color = "white",  fill_color = "black", bw = 1) %>%
    data_color(
      columns = vig_perceptions_variables,
      palette = "inferno",
      reverse = TRUE,
      domain = c(min(dplyr::select(vig_messages_df, starts_with("vig_perc"))), max(dplyr::select(vig_messages_df, starts_with("vig_perc"))))
    ) %>%
    tab_options(
      table.font.names = "Fira Sans",
      table.font.size = "9px",
      data_row.padding = px(1)
    ) %>%
    cols_label(
      sender_message = "Message",
      sender_category = "Sender",
      target_position = "Position",
      !!!vig_outcomes_labels_set,
      data_actions = "Number of<br>selected actions",
      .fn = md
    ) %>%
    tab_spanner(label = "Message features", columns = c(starts_with("sender"), starts_with("target"))) %>%
    tab_spanner(label = "Perceptions", columns = vig_perceptions_variables) %>%
    tab_spanner(label = "Preferred actions", columns = c(vig_actions_variables, data_actions)) %>%
    # make spanner labels bold
    tab_style(
      style = list(
        cell_text(weight = "bold") 
      ),
      locations = cells_column_spanners()
    ) %>%
    # set equal column width
    cols_width(vig_outcomes_variables ~ px(50)  # Apply equal width to all columns except the first
    ) %>%
    tab_options(
      table.font.size = "9px",
      data_row.padding = px(1)
    ) %>%
    gtsave(paste0("figures/table-messages-ranked-", i, ".png"), zoom = 3)
}




# table of most hateful memes ------------------------


# pooled

vig_messages_sum <- data_survey_combined %>% 
  group_by(message_combination) %>% 
  summarize(num_ratings = n(),
            across(all_of(vig_outcomes_variables), \(x) mean(x, na.rm = TRUE)), 
            data_actions = list(vig_num_selected),
            .groups = "drop") %>% 
  arrange(desc(vig_perc_hate)) 

vig_meta_df <- dplyr::select(data_survey_combined, resp_country, message_combination, sender_message, sender_category, target_position) %>% filter(resp_country == "usa") %>% distinct(message_combination, .keep_all = TRUE)
vig_messages_df <- left_join(vig_messages_sum, vig_meta_df, by = join_by(message_combination))

vig_messages_df_memes <- filter(vig_messages_df, sender_category == "Meme")
vig_messages_df_memes$sender_message <- str_replace(vig_messages_df_memes$sender_message, ".jpeg", "_usa.jpeg")

# gt
vig_messages_df_memes %>%
  dplyr::select(sender_message, target_position, vig_outcomes_variables, data_actions) %>%
  arrange(desc(vig_perc_hate)) %>%
  { bind_rows(head(., n = 5), tail(., n = 5)) } %>%
  distinct() %>%
  gt() %>%
  fmt_image(
    columns = sender_message,
    height = px(35),
    path = "build-vignettes/memes/usa",
    file_pattern = "{x}"
  ) %>%
  fmt_percent(columns = starts_with("vig_"), decimals = 0) %>%
  gt_plt_dist(data_actions, type = "histogram", line_color = "white",  fill_color = "black", bw = 1) %>%
  data_color(
    columns = vig_perceptions_variables,
    palette = "inferno",
    reverse = TRUE,
    domain = c(min(dplyr::select(vig_messages_df, starts_with("vig_perc"))), max(dplyr::select(vig_messages_df, starts_with("vig_perc"))))
  ) %>%
  tab_options(
    table.font.names = "Fira Sans",
    table.font.size = "9px",
    data_row.padding = px(1)
  ) %>%
  cols_label(
    sender_message = "Meme (US version)",
    target_position = "Position",
    !!!vig_outcomes_labels_set,
    data_actions = "Number of<br>selected actions",
    .fn = md
  ) %>%
  tab_spanner(label = "Message features", columns = c(starts_with("sender"), starts_with("target"))) %>%
  tab_spanner(label = "Perceptions", columns = vig_perceptions_variables) %>%
  tab_spanner(label = "Preferred actions", columns = c(vig_actions_variables, data_actions)) %>%
  # make spanner labels bold
  tab_style(
    style = list(
      cell_text(weight = "bold") 
    ),
    locations = cells_column_spanners()
  ) %>%
  # set equal column width
  cols_width(vig_outcomes_variables ~ px(50)  # Apply equal width to all columns except the first
  ) %>%
  tab_options(
    table.font.size = "9px",
    data_row.padding = px(1)
  ) %>%
  gtsave("figures/table-memes-ranked-pooled.png", zoom = 3)


# by country

for(i in country_codes_chr){
  vig_messages_sum <- data_survey_combined %>% 
    filter(resp_country == i) %>%
    group_by(message_combination) %>% 
    summarize(num_ratings = n(),
              across(all_of(vig_outcomes_variables), \(x) mean(x, na.rm = TRUE)), 
              data_actions = list(vig_num_selected),
              .groups = "drop") %>% 
    arrange(desc(vig_perc_hate)) 
  
  vig_meta_df <- dplyr::select(data_survey_combined, resp_country, message_combination, sender_message, sender_category, target_position) %>% filter(resp_country == i) %>% distinct(message_combination, .keep_all = TRUE)
  vig_messages_df <- left_join(vig_messages_sum, vig_meta_df, by = join_by(message_combination))
  
  vig_messages_df_memes <- filter(vig_messages_df, sender_category == "Meme")
  vig_messages_df_memes$sender_message <- str_replace(vig_messages_df_memes$sender_message, ".jpeg", paste0("_", i, ".jpeg"))
  
  # gt
  vig_messages_df_memes %>%
    dplyr::select(sender_message, target_position, vig_outcomes_variables, data_actions) %>%
    arrange(desc(vig_perc_hate)) %>%
    { bind_rows(head(., n = 5), tail(., n = 5)) } %>%
    distinct() %>%
    gt() %>%
    fmt_image(
      columns = sender_message,
      height = px(35),
      path = paste0("build-vignettes/memes/", i),
      file_pattern = "{x}"
    ) %>%
    fmt_percent(columns = starts_with("vig_"), decimals = 0) %>%
    gt_plt_dist(data_actions, type = "histogram", line_color = "white",  fill_color = "black", bw = 1) %>%
    data_color(
      columns = vig_perceptions_variables,
      palette = "inferno",
      reverse = TRUE,
      domain = c(min(dplyr::select(vig_messages_df, starts_with("vig_perc"))), max(dplyr::select(vig_messages_df, starts_with("vig_perc"))))
    ) %>%
    tab_options(
      table.font.names = "Fira Sans",
      table.font.size = "9px",
      data_row.padding = px(1)
    ) %>%
    cols_label(
      sender_message = "Meme",
      target_position = "Position",
      !!!vig_outcomes_labels_set,
      data_actions = "Number of<br>selected actions",
      .fn = md
    ) %>%
    tab_spanner(label = "Message features", columns = c(starts_with("sender"), starts_with("target"))) %>%
    tab_spanner(label = "Perceptions", columns = vig_perceptions_variables) %>%
    tab_spanner(label = "Preferred actions", columns = c(vig_actions_variables, data_actions)) %>%
    # make spanner labels bold
    tab_style(
      style = list(
        cell_text(weight = "bold") 
      ),
      locations = cells_column_spanners()
    ) %>%
    # set equal column width
    cols_width(vig_outcomes_variables ~ px(50)  # Apply equal width to all columns except the first
    ) %>%
    tab_options(
      table.font.size = "9px",
      data_row.padding = px(1)
    ) %>%
    gtsave(paste0("figures/table-memes-ranked-", i, ".png"), zoom = 3)
}



# table of messages with biggest heterogeneity in perceptions across countries ----------------

colors <- c('#d7191c','#ffefd6', '#dcf0d1','#1a9641')
color_palette <- colorRampPalette(colors)(10)

data_messages_select <- 
data_survey_combined %>%
  group_by(resp_country2_lab, message_combination) %>%
  summarize(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = resp_country2_lab, values_from = n) %>%
  # keep observations if all values in row are >= 5
  filter(rowSums(select(., -message_combination) >= 10) == ncol(.) - 1)

data_messages_sub <- filter(data_survey_combined, message_combination %in% data_messages_select$message_combination)


# summarize hatefulness by vignette messages
vig_messages_sum <- data_messages_sub %>% 
  group_by(resp_country2_lab, message_combination) %>% 
  summarize(mean_hate_perc = mean(vig_perc_hate, na.rm = TRUE),
            .groups = "drop") 

vig_messages_sum_country <- vig_messages_sum %>%
  group_by(message_combination) %>%
  summarize(mean_hate_perc_country = mean(mean_hate_perc, na.rm = TRUE),
            sd_hate_perc_country = sd(mean_hate_perc, na.rm = TRUE),
            .groups = "drop")

vig_messages_heterogeneity <- 
  vig_messages_sum %>%
  pivot_wider(names_from = resp_country2_lab, values_from = mean_hate_perc) %>%
  left_join(vig_messages_sum_country, by = join_by(message_combination)) %>%
  left_join(vig_meta_df, by = join_by(message_combination))

# gt
vig_messages_heterogeneity %>%
  dplyr::select(-message_combination, -resp_country, -sender_category, -target_position) %>%
  relocate(sender_message, .before = Brazil) %>%
  arrange(desc(sd_hate_perc_country)) %>%
  #(function(x){bind_rows(slice_head(x, n = 10), slice_tail(x, n = 10))}) %>%
  slice_head(n = 25) %>%
  gt() %>%
  fmt_percent(
    columns = where(is.numeric),
    decimals = 0              
  ) %>%
  data_color(
    columns = sd_hate_perc_country,
    palette = "inferno",
    reverse = TRUE
  ) %>%
  data_color(
    columns = Brazil:`United States`,
    palette = color_palette,
    reverse = TRUE,
    domain = c(0, 1)
  ) %>%
  tab_options(
    table.font.names = "Fira Sans",
    table.font.size = "9px",
    data_row.padding = px(1)
  ) %>%
  cols_label(
    sender_message = "Message (US version)",
    mean_hate_perc_country = "Mean<br>across<br>countries",
    sd_hate_perc_country = "Sd<br>across<br>countries",
    .fn = md
  ) %>%
  tab_spanner(label = 'Country shares, perception as "hate"', columns = Brazil:`United States`) %>%
  tab_spanner(label = "Summary statistics", columns = c(mean_hate_perc_country, sd_hate_perc_country)) %>%
  # make spanner labels bold
  tab_style(
    style = list(
      cell_text(weight = "bold") 
    ),
    locations = cells_column_spanners()
  ) %>%
  # set equal column width
  cols_width(sender_message ~ px(200),
             everything() ~ px(50)
             ) %>%
  tab_options(
    table.font.size = "9px",
    data_row.padding = px(1)
  ) %>%
  gtsave("figures/table-messages-heterogeneity-ranked.png", zoom = 3)






# table of opinions with biggest share of censorship across countries ----------------

colors <- c('#d7191c','#ffefd6', '#dcf0d1','#1a9641')
color_palette <- colorRampPalette(colors)(10)


# summarize hatefulness by opinion message
vig_opinions_by_pooled <- data_survey_combined %>%
  filter(sender_category == "Opinion") %>%
  group_by(message_combination, target_topic, sender_hatescore) %>% 
  summarize(mean_remove_perc = mean(vig_remove, na.rm = TRUE),
            .groups = "drop")  %>%
  rename(Pooled = mean_remove_perc) %>% 
  select(message_combination, Pooled)

# summarize hatefulness by opinion message and country
vig_opinions_by_country <- data_survey_combined %>%
  filter(sender_category == "Opinion") %>%
  group_by(resp_country2_lab, message_combination, target_topic, sender_hatescore) %>% 
  summarize(mean_remove_perc = mean(vig_remove, na.rm = TRUE),
            .groups = "drop")  %>%
  pivot_wider(names_from = resp_country2_lab, values_from = mean_remove_perc) 
  
# combine
vig_opinions_combined <- 
  vig_opinions_by_pooled %>% 
  left_join(vig_opinions_by_country, by = join_by(message_combination)) %>%
  left_join(vig_meta_df, by = join_by(message_combination))

# gt
vig_opinions_combined %>%
  dplyr::select(-message_combination, -resp_country, -sender_category) %>%
  relocate(sender_message, .before = 1) %>%
  relocate(target_topic, target_position, sender_hatescore, Pooled, .before = Brazil) %>%
  arrange(desc(Pooled)) %>%
  #(function(x){bind_rows(slice_head(x, n = 10), slice_tail(x, n = 10))}) %>%
  gt() %>%
  fmt_percent(
    columns = where(is.numeric),
    decimals = 0              
  ) %>%
  data_color(
    columns = Pooled:`United States`,
    palette = color_palette,
    reverse = TRUE,
    domain = c(0, .8)
  ) %>%
  tab_options(
    table.font.names = "Fira Sans",
    table.font.size = "9px",
    data_row.padding = px(1)
  ) %>%
  cols_label(
    sender_message = "Message (US version)",
    target_topic = "Message topic",
    target_position = "Target stance",
    sender_hatescore = "Opinion type",
    .fn = md
  ) %>%
  tab_spanner(label = 'Country shares, opt for "removal"', columns = Brazil:`United States`) %>%
  tab_spanner(label = "Message type", columns = c(sender_message, target_topic, target_position, sender_hatescore)) %>%
  # make spanner labels bold
  tab_style(
    style = list(
      cell_text(weight = "bold") 
    ),
    locations = cells_column_spanners()
  ) %>%
  # set equal column width
  cols_width(sender_message ~ px(200),
             target_topic ~ px(70),
             everything() ~ px(50)
  ) %>%
  tab_options(
    table.font.size = "9px",
    data_row.padding = px(1)
  ) %>%
  gtsave("figures/table-opinions-removal-ranked.png", zoom = 3)




# scatterplot of hateful vs. offensive perceptions of messages ----------------

# usa 

country <- "usa"

data_survey_combined$vig_num_selected <- rowSums(dplyr::select(data_survey_combined, all_of(vig_actions_variables)), na.rm = TRUE)

vig_messages_sum <- data_survey_combined %>% 
  filter(resp_country == country) %>%
  group_by(message_combination) %>% 
  summarize(num_ratings = n(),
            across(all_of(vig_outcomes_variables), \(x) mean(x, na.rm = TRUE)),             .groups = "drop") %>% 
  filter(num_ratings > 20) %>%
  arrange(desc(vig_perc_hate)) 


vig_meta_df <- dplyr::select(data_survey_combined, resp_country, message_combination, sender_message, sender_category, target_position) %>% filter(resp_country == country) %>% distinct(message_combination, .keep_all = TRUE)

vig_messages_df <- left_join(vig_messages_sum, vig_meta_df, by = join_by(message_combination))

vig_messages_df <- vig_messages_df %>%
  mutate(tooltip_text = ifelse(
    sender_category == "Meme",
    paste0("<img src='build-vignettes/memes/", country, "/", sender_message, "' width='150'>"),
    sender_message
  ))

message_type_cols <- c(
  "#3C5488",  # Strong Indigo
  "#4DBBD5",  # Strong Cyan/Blue
  "#00A087",  # Strong Green-Teal
  "#F39B7F",  # Strong Coral
  "#E64B35"   # Strong Red
) %>% alpha(0.8)


# interactive scatterplot
p <- ggplot(vig_messages_df, aes(x = vig_perc_offensive, y = vig_perc_hate)) +
  geom_segment(aes(x = 0, y = 0, xend = max(vig_perc_offensive, na.rm = TRUE), yend = max(vig_perc_offensive, na.rm = TRUE)),
               linetype = "solid", color = "grey") + 
  geom_point(aes(color = sender_category, text = tooltip_text), size = 2) +
  labs(title = "Perceived offensiveness vs. perceived hatefulness",
       x = "Perceived offensiveness",
       y = "Perceived hatefulness") +
  scale_color_manual(values = message_type_cols) +
  xlim(0, .8) + ylim(0, .8) + 
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(0.5,0.1,0.5,0.1),"cm"))

# Turn into interactive plot
ggplotly(p, tooltip = "text") %>%
  layout(
    legend = list(
      orientation = "h",
      x = 0.5,
      y = -0.2,
      xanchor = "center",
      title = list(text = "")
    )
  ) %>%
  config(displayModeBar = FALSE) %>%
  style(hoverlabel = list(align = "left"))





# scatterplot of hateful vs. offensive perceptions of opinions ----------------

data_survey_combined$vig_num_selected <- rowSums(dplyr::select(data_survey_combined, all_of(vig_actions_variables)), na.rm = TRUE)

# pooled 

vig_messages_sum <- data_survey_combined %>% 
  filter(sender_category == "Opinion") %>%
  group_by(message_combination) %>% 
  summarize(num_ratings = n(),
            across(all_of(vig_outcomes_variables), \(x) mean(x, na.rm = TRUE)), 
            data_actions = list(vig_num_selected),
            .groups = "drop") %>% 
  arrange(desc(vig_perc_hate)) 

vig_meta_df <- dplyr::select(data_survey_combined, resp_country, message_combination, sender_message, sender_category, target_position) %>% filter(resp_country == "usa") %>% distinct(message_combination, .keep_all = TRUE)
vig_messages_df <- left_join(vig_messages_sum, vig_meta_df, by = join_by(message_combination))

vig_messages_df$perc_hate_offensive_diff <- vig_messages_df$vig_perc_hate - vig_messages_df$vig_perc_offensive

# scatterplot vig_perc_offensive vs. vig_perc_hate, with sender_message as label

ggplot(vig_messages_df, aes(x = vig_perc_offensive, y = vig_perc_hate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  # lm abline
  geom_smooth(method = "lm", se = FALSE, color = "red", size = .5) + 
  geom_point(aes(color = sender_category), size = 2) +
  geom_text_repel(
    data = subset(vig_messages_df, vig_perc_hate >  1.5 * vig_perc_offensive | vig_perc_offensive > 6 * vig_perc_hate),
    aes(label = str_wrap(sender_message, width = 25)),
    size = 3,
    max.overlaps = 20
  ) +
  scale_color_manual(values = c("black", "red")) +
  labs(title = "Perceived offensiveness vs. perceived hatefulness",
       x = "Perceived offensiveness",
       y = "Perceived hatefulness") +
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "none",
        legend.title = element_blank(),
        plot.margin = unit(c(0.5,0.1,0.5,0.1),"cm"))
ggsave(paste0("figures/scatterplot-opinions-hateful-vs-offensive-pooled.png"), width = 8, height = 5, dpi = 300)



# scatterplot of hateful vs. removal of opinions ----------------

data_survey_combined$vig_num_selected <- rowSums(dplyr::select(data_survey_combined, all_of(vig_actions_variables)), na.rm = TRUE)

# pooled 

vig_messages_sum <- data_survey_combined %>% 
  filter(sender_category == "Opinion") %>%
  group_by(message_combination) %>% 
  summarize(num_ratings = n(),
            across(all_of(vig_outcomes_variables), \(x) mean(x, na.rm = TRUE)), 
            data_actions = list(vig_num_selected),
            .groups = "drop") %>% 
  arrange(desc(vig_perc_hate)) 

vig_meta_df <- dplyr::select(data_survey_combined, resp_country, message_combination, sender_message, sender_category, target_position) %>% filter(resp_country == "usa") %>% distinct(message_combination, .keep_all = TRUE)
vig_messages_df <- left_join(vig_messages_sum, vig_meta_df, by = join_by(message_combination))

vig_messages_df$perc_hate_removal_diff <- vig_messages_df$vig_perc_hate - vig_messages_df$vig_remove

# scatterplot vig_perc_offensive vs. vig_perc_hate, with sender_message as label

ggplot(vig_messages_df, aes(x = vig_perc_hate, y = vig_remove)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  # lm abline
  geom_smooth(method = "lm", se = FALSE, color = "red", size = .5) + 
  geom_point(aes(color = sender_category), size = 2) +
  geom_text_repel(
    data = subset(vig_messages_df, vig_perc_hate >  1.5 * vig_perc_offensive | vig_perc_offensive > 6 * vig_perc_hate),
    aes(label = str_wrap(sender_message, width = 25)),
    size = 3,
    max.overlaps = 20
  ) +
  scale_color_manual(values = c("black", "red")) +
  labs(title = "Perceived hatefulness vs. opt for removal",
       x = "Perceived hatefulness",
       y = "Removal") +
  theme_minimal() +
  theme(text = element_text(family = "Fira Sans"),
        plot.title.position = "plot",
        plot.title = element_markdown(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.position = "none",
        legend.title = element_blank(),
        plot.margin = unit(c(0.5,0.1,0.5,0.1),"cm"))
ggsave(paste0("figures/scatterplot-opinions-hateful-vs-offensive-pooled.png"), width = 8, height = 5, dpi = 300)



# agreement in hate speech classifications ----------------

# vignette metadata
vig_meta_df <- dplyr::select(data_survey_combined, resp_country, message_combination, sender_message, sender_category, target_position) %>% filter(resp_country == "usa") %>% distinct(message_combination, .keep_all = TRUE)

# binary classifiers: hate vs. no hate, any action vs. no action
data_survey_combined$vig_num_selected <- rowSums(dplyr::select(data_survey_combined, all_of(vig_actions_variables)), na.rm = TRUE)
data_survey_combined$vig_action_yes <- ifelse(data_survey_combined$vig_num_selected == 0, 0, 1)

classification_vars <- c("vig_perc_hate", "vig_action_yes")

# Function to compute classification agreement and message-level agreement rates
compute_agreement_cdf <- function(data, group_var1 = NULL, group_var2 = NULL, class_var = "vig_perc_hate") {
  # Define grouping variables
  group_vars <- c("message_combination", group_var1, group_var2) %>% discard(is.null)
  overall_group <- c(group_var1, group_var2) %>% discard(is.null)
  
  # Aggregate agreement metrics
  result <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_raters = n(),
      p = mean(.data[[class_var]] == 1, na.rm = TRUE),
      p_agree = pmax(p, 1 - p),
      rev_entropy = reverse_entropy(p),
      .groups = "drop"
    )
  
  # CDF calculations
  result <- result %>%
    group_by(across(all_of(overall_group))) %>%
    arrange(p_agree) %>%
    mutate(
      rank_agree = row_number(),
      pct_messages_agree = 1 - (rank_agree - 1) / n()
    ) %>%
    arrange(rev_entropy) %>%
    mutate(
      rank_entropy = row_number(),
      pct_messages_entropy = 1 - (rank_entropy - 1) / n()
    ) %>%
    ungroup()
  
  # Add metadata columns
  result <- result %>%
    mutate(
      class_var = class_var,
      subgroup = case_when(
        length(overall_group) == 0 ~ "All",
        length(overall_group) == 1 ~ overall_group[1],
        length(overall_group) == 2 ~ paste(overall_group, collapse = " & ")
      )
    )
  
  # Return result with all group_vars retained as separate columns
  result %>%
    select(
      message_combination,
      all_of(overall_group),
      class_var,
      n_raters,
      p_agree,
      rev_entropy,
      pct_messages_agree,
      pct_messages_entropy,
      subgroup
    )
}





### Hate classification agreement

# perception agreement

cdf_hate_all <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", class_var = "vig_perc_hate")
cdf_hate_gender <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", group_var2 = "gender", class_var = "vig_perc_hate")
cdf_hate_lr <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", group_var2 = "leftright_cat", class_var = "vig_perc_hate")
cdf_hate_combined <- bind_rows(cdf_hate_all, cdf_hate_gender, cdf_hate_lr)

# action preference agreement

cdf_action_all <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", class_var = "vig_action_yes")
cdf_action_gender <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", group_var2 = "gender", class_var = "vig_action_yes")
cdf_action_lr <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", group_var2 = "leftright_cat", class_var = "vig_action_yes")
cdf_action_combined <- bind_rows(cdf_action_all, cdf_action_gender, cdf_action_lr)

# combine all
cdf_combined_all <- bind_rows(
  cdf_hate_combined %>% mutate(class_var_label = 'Perception as\n"hate speech"'),
  cdf_action_combined %>% mutate(class_var_label = 'At least one\naction selected')
) %>% 
  # filter(gender != "Other") %>% 
  left_join(dplyr::select(vig_meta_df, -sender_category), by = "message_combination")
cdf_combined_all$subgroup_value <- paste0(cdf_combined_all$gender, cdf_combined_all$leftright_cat) %>% str_replace_all("NA", "")
cdf_combined_all$subgroup_value[cdf_combined_all$subgroup_value == "" & cdf_combined_all$subgroup == "sender_category"] <- "Pooled"
cdf_combined_all <- filter(cdf_combined_all, !(subgroup_value %in% c("Other", "")))
cdf_combined_all$subgroup_value <- factor(cdf_combined_all$subgroup_value, levels = c("Pooled", "Female", "Male", "Left", "Center", "Right"))
cdf_combined_all$class_var_label <- factor(cdf_combined_all$class_var_label, levels = c('Perception as\n"hate speech"', 'At least one\naction selected'))

# Plot
custom_colors_named <- c(
  "Pooled" = "black",
  "Male" = "darkgreen",
  "Female" = "goldenrod",
  "Left" = "darkblue",
  "Right" = "darkred",
  "Center" = "grey"
) %>% alpha(0.5)

ggplot(cdf_combined_all, 
  aes(x = p_agree, y = pct_messages_agree, color = subgroup_value)
) +
  geom_line(size = .75) +
  facet_grid(class_var_label ~ sender_category) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0.5, 1), breaks = seq(0.5, 1, 0.1)) +
  scale_color_manual(values = custom_colors_named) + 
  labs(
    title = "Agreement distributions by outcome and subgroup",
    x = "Rater agreement",
    y = "% of messages with rater agreement ≥ x",
  ) +
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.placement = "outside",
    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.margin = margin(5, 5, 5, 5),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey90")
  )
ggsave(paste0("figures/agreement-cumulative-distributions.png"), width = 12, height = 6, dpi = 300)




### Hate classification agreement

# perception agreement

cdf_hate_all <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", class_var = "vig_perc_hate")
cdf_hate_gender <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", group_var2 = "gender", class_var = "vig_perc_hate")
cdf_hate_lr <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", group_var2 = "resp_country2", class_var = "vig_perc_hate")
cdf_hate_combined <- bind_rows(cdf_hate_all, cdf_hate_gender, cdf_hate_lr)

# action preference agreement

cdf_action_all <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", class_var = "vig_action_yes")
cdf_action_gender <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", group_var2 = "gender", class_var = "vig_action_yes")
cdf_action_lr <- compute_agreement_cdf(data_survey_combined, group_var1 = "sender_category", group_var2 = "resp_country2", class_var = "vig_action_yes")
cdf_action_combined <- bind_rows(cdf_action_all, cdf_action_gender, cdf_action_lr)

# combine all
cdf_combined_all <- bind_rows(
  cdf_hate_combined %>% mutate(class_var_label = 'Perception as\n"hate speech"'),
  cdf_action_combined %>% mutate(class_var_label = 'At least one\naction selected')
) %>% 
  # filter(gender != "Other") %>% 
  left_join(dplyr::select(vig_meta_df, -sender_category), by = "message_combination")
cdf_combined_all$subgroup_value <- paste0(cdf_combined_all$gender, cdf_combined_all$resp_country2) %>% str_replace_all("NA", "")
cdf_combined_all$subgroup_value[cdf_combined_all$subgroup_value == "" & cdf_combined_all$subgroup == "sender_category"] <- "Pooled"
cdf_combined_all <- filter(cdf_combined_all, !(subgroup_value %in% c("Other", "")))
cdf_combined_all$subgroup_value <- factor(cdf_combined_all$subgroup_value, levels = c("Pooled", "Female", "Male", "Left", "Center", "Right"))
cdf_combined_all$class_var_label <- factor(cdf_combined_all$class_var_label, levels = c('Perception as\n"hate speech"', 'At least one\naction selected'))

# Plot
custom_colors_named <- c(
  "Pooled" = "black",
  "Male" = "darkgreen",
  "Female" = "goldenrod",
  "Left" = "darkblue",
  "Right" = "darkred",
  "Center" = "grey"
) %>% alpha(0.5)

ggplot(cdf_combined_all, 
       aes(x = p_agree, y = pct_messages_agree, color = subgroup_value)
) +
  geom_line(size = .75) +
  facet_grid(class_var_label ~ sender_category) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0.5, 1), breaks = seq(0.5, 1, 0.1)) +
  #scale_color_manual(values = custom_colors_named) + 
  labs(
    title = "Agreement distributions by outcome and subgroup",
    x = "Rater agreement",
    y = "% of messages with rater agreement ≥ x",
  ) +
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.placement = "outside",
    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.margin = margin(5, 5, 5, 5),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey90")
  )
ggsave(paste0("figures/agreement-cumulative-distributions-2.png"), width = 12, height = 6, dpi = 300)


# Also: by topic, by severity, by combinations of all/some (14*2*5 = 140 combinations! -> very sparse for the message-level stats)
# groups: countries, silencing speech score (low-medium-high), target message alignment


