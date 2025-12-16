# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")


# load data ---------------------------

load("data/cooked/data_survey_resp.RData")
load("data/cooked/data_survey_combined.RData")
load("data/cooked/vignettes_content_df.RData")


# table of vignette target messages --------------



target_message_df <- 
tp_vars_labels %>% 
  select(vartopic, position = variable, varlabel) %>%
  mutate(position = ifelse(str_extract(position, "[lr]$") == "l", "Left", "Right")) %>%
  mutate(varlabel = paste0("\\emph{", varlabel, "}")) %>%
  arrange(vartopic)

  
latex_table <- kable(
  target_message_df,
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  caption = "Target statements, by topic and stance (U.S. version)",
  label = "target-messages",
  col.names = c("Topic", "Stance", "Statement"),
  align = c("l", "l", "l"),
  escape = FALSE
) %>%
  kableExtra::row_spec(0, bold = TRUE) %>%
  kableExtra::kable_styling(latex_options = "H")
writeLines(latex_table, "figures/target-messages.tex")


# tables of vignette sender messages --------------

tp_vars_labels$target_topic <- 
  tp_vars_labels$variable %>% str_replace("tp_", "") %>% str_replace("_[lr]", "")

sender_messages_df <- 
  vignettes_content_df %>% 
  filter(country == "usa", target_constraint_gender == "female" | is.na(target_constraint_gender), sender_category != "meme") %>%
  select(target_topic, target_position, sender_scope, sender_category, sender_hatescore, sender_message) %>%
  left_join(distinct(select(tp_vars_labels, target_topic, vartopic)), by = "target_topic")
sender_messages_df$target_position <- firstup(sender_messages_df$target_position)
sender_messages_df$sender_scope <- firstup(sender_messages_df$sender_scope)
sender_messages_df$sender_category <- firstup(sender_messages_df$sender_category)
sender_messages_df$hatescore <- ifelse(sender_messages_df$sender_hatescore == 1, "Moderate", "Extreme")
sender_messages_df <- sender_messages_df %>% 
  relocate(sender_message, .after = last_col())  


target_topics <- unique(sender_messages_df$vartopic)
for(i in target_topics){
  i_name <- str_replace_all(i, "[ '-]", "") %>% tolower()
  tab <- 
    sender_messages_df %>%
    filter(vartopic == i) %>%
    select(-target_topic, -vartopic, -sender_hatescore) %>%
    mutate(sender_message = paste0("\\emph{", sender_message, "}"))  # <- italicize text
  latex_table <- kable(
    tab,
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = paste0("Sender statements for target topic: ", i, " (U.S. version)"),
    label = paste0("sender-messages-", i_name),
    col.names = c("Target stance", "Scope", "Category", "Severity", "Sender message"),
    align = c("l", "l", "l", "l", "l"),
    escape = FALSE
  ) %>%
  kableExtra::row_spec(0, bold = TRUE)
  writeLines(latex_table, paste0("figures/sender-messages-", i_name, ".tex"))
}




# tables of vignette avatars --------------

# usa 

vignettes_names_df <- read_xlsx("data/cooked/vignettes-names.xlsx", sheet = "names_usa") %>%
  select(gender, ethnicity, name, avatar)
vignettes_names_df$gender <- firstup(vignettes_names_df$gender)
vignettes_names_df$ethnicity <- firstup(vignettes_names_df$ethnicity)
vignettes_names_df$avatar <- paste0("\\raisebox{-.25\\height}{\\includegraphics[height=.75cm]{figures/avatars/", vignettes_names_df$avatar, ".png}}")
write_xlsx(vignettes_names_df, "figures/vignette-avatars-usa.xlsx", col_names = TRUE)


# libs
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(readr)

# If `firstup()` isn't in your session, uncomment:
# firstup <- function(x) sub("^(.)(.*)$", "\\U\\1\\L\\2", x, perl = TRUE)

countries <- c("bra","col","ger","ind","idn","nig","phl","pol","tur","gbr","usa")

country_labels <- c(
  bra = "Brazil",
  col = "Colombia",
  ger = "Germany",
  ind = "India",
  idn = "Indonesia",
  nig = "Nigeria",
  phl = "Philippines",
  pol = "Poland",
  tur = "Turkey",
  gbr = "U.K.",
  usa = "U.S."
)

# Minimal LaTeX escaping for names/fields (tune if needed)
latex_escape <- function(x) {
  x |>
    str_replace_all("\\\\", "\\\\textbackslash{}") |>
    str_replace_all("([&_#%$])", "\\\\$1") |>
    str_replace_all("\\{", "\\\\{") |>
    str_replace_all("\\}", "\\\\}") |>
    str_replace_all("~", "\\\\textasciitilde{}") |>
    str_replace_all("\\^", "\\\\textasciicircum{}")
}

make_avatar_cell <- function(file_stem) {
  paste0("\\raisebox{-.25\\height}{\\includegraphics[height=.75cm]{figures/avatars/",
         file_stem, ".png}}")
}

make_tex_for_country <- function(ctry) {
  sheet_name <- paste0("names_", ctry)
  caption_country <- country_labels[[ctry]] %||% toupper(ctry)
  label_id <- paste0("tab:avatars-", ctry)
  
  df <- read_xlsx("data/cooked/vignettes-names.xlsx", sheet = sheet_name) |>
    select(gender, ethnicity, name, avatar) |>
    mutate(
      gender    = firstup(gender),
      ethnicity = firstup(ethnicity),
      name      = latex_escape(name),
      avatar    = make_avatar_cell(avatar)
    )
  
  # Order: Female then Male (if both present), then by Ethnicity, then Name
  gender_order <- c("Female", "Male")
  df <- df |>
    mutate(gender = factor(gender, levels = gender_order)) |>
    arrange(gender, ethnicity, name)
  
  # Build table header
  lines <- c(
    "\\begin{table}[htbp]",
    paste0("\t\\caption{Sender and target avatars (", caption_country, " sample)}"),
    "\t\\centering",
    paste0("\t\\label{", label_id, "}"),
    "\\begin{tabular}{@{}lccc@{}}",
    "\\toprule",
    "\\multicolumn{1}{c}{\\textbf{Gender}} & \\multicolumn{1}{c}{\\textbf{Ethnicity}} & \\multicolumn{1}{c}{\\textbf{Name}} & \\multicolumn{1}{c}{\\textbf{Avatar}} \\\\ \\midrule"
  )
  
  # Build body with multirow for Gender and Ethnicity blocks
  genders <- split(df, df$gender)
  first_gender_written <- FALSE
  
  for (g in names(genders)) {
    gdf <- genders[[g]]
    n_g <- nrow(gdf)
    
    # Split by ethnicity within gender
    eth_groups <- split(gdf, gdf$ethnicity)
    
    # Count total rows for the gender multirow
    n_g_total <- sum(vapply(eth_groups, nrow, integer(1)))
    
    gender_inserted <- FALSE
    
    # For consistent output order by ethnicity name
    for (eth in sort(names(eth_groups))) {
      edf <- eth_groups[[eth]]
      n_e <- nrow(edf)
      
      # For the first row of this ethnicity block:
      # - insert Gender cell if not inserted yet (multirow over all rows in this gender)
      # - insert Ethnicity cell as multirow over this ethnicity's rows
      # Then subsequent rows get empty cells for those columns.
      for (i in seq_len(n_e)) {
        row <- edf[i, ]
        if (!gender_inserted) {
          gender_cell <- paste0("\\multirow{", n_g_total, "}{*}{", row$gender, "}")
          gender_inserted <- TRUE
        } else {
          gender_cell <- " "
        }
        
        if (i == 1) {
          eth_cell <- paste0("\\multirow{", n_e, "}{*}{", row$ethnicity, "}")
        } else {
          eth_cell <- " "
        }
        
        line <- paste0(
          gender_cell, " & ",
          eth_cell, " & ",
          row$name, " & ",
          row$avatar, " \\\\"
        )
        lines <- c(lines, line)
      }
    }
    
    # After finishing one gender block, add a cmidrule like in your example
    lines <- c(lines, "\\cmidrule(l){1-4}")
  }
  
  # Footer
  lines <- c(
    lines,
    "\\end{tabular}",
    "\\end{table}"
  )
  
  out_path <- file.path("figures", paste0("vignette-avatars-", ctry, ".tex"))
  write_lines(lines, out_path)
  message("Wrote: ", out_path)
  invisible(out_path)
}

# Run for all countries
walk(countries, make_tex_for_country)





# vignette-level descriptive stats --------------

# total number of vignettes shown

nrow(data_survey_combined)

# number of unique vignettes

length(unique(data_survey_combined$vig_id))

# unique message combinations

length(unique(data_survey_combined$message_combination))



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
ggsave("figures/barplot-vignettes-shown-by-country-title.png", width = 7, height = 5, dpi = 300)


# Plot the data using the calculated proportions
ggplot(tab, aes(x = n_vig_shown_cat, y = prop)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ resp_country2_lab, ncol = 3) +
  ylab("") +
  xlab("Number of times unique vignette was shown") +
  theme_minimal() +
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position = "none",  # Hide the legend if not needed
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    strip.text = element_markdown(size = 6),
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
ggsave("figures/barplot-vignettes-shown-by-country.png", width = 7, height = 5, dpi = 300, bg = "white")



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
ggsave("figures/barplot-vignettes-messages-shown-by-country-title.png", width = 7, height = 5, dpi = 300)

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
  ylab("") +
  xlab("Number of times unique message configuration was shown") +
  theme_minimal() +
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position = "none",  # Hide the legend if not needed
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    strip.text = element_markdown(size = 6),
    strip.background = element_blank(),
    strip.placement = "outside",
    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 16),
    plot.subtitle = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_line(color = "grey90")
  )
ggsave("figures/barplot-vignettes-messages-shown-by-country.png", width = 7, height = 5, dpi = 300, bg = "white")




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
       fontsize = "normalsize",
       use.packages = FALSE,
       sanitize.colnames.function = identity, sanitize.rownames.function = identity,
       file = "figures/lmer-vignette-response-times-pooled.tex")


