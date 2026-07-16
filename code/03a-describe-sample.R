# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")


# load data ---------------------------

load("data/cooked/data_survey_resp.RData")
load("data/cooked/data_survey_combined.RData")

geo_variables_available <- FALSE # geo variables not available in published dataset

# number of respondents --------------

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
            output = "results/respondents-n-by-country.tex")


# attrition and costs --------------

data_survey_all <- readRDS("data/cooked/data_survey_all_comp.rds")
meta_campaign <- read_xlsx(path = "data/cooked/meta-campaign-costs.xlsx") %>% 
  clean_names() %>%
  group_by(country) %>%
  summarize(costs = sum(ausgegebener_betrag_eur))

data_survey_all <- data_survey_all %>% 
  mutate(complete = case_when(
    progress_cat %in% c("vignettes complete", "survey complete") & 
      speed_cat != "less than 5 minutes" & 
      consent == 1 ~ 1,
    TRUE ~ 0
  ))

survey_attrition <- data_survey_all %>%
  group_by(resp_country2) %>%
  summarize(
    country_lab = first(resp_country2_lab),
    n_started = n(),
    n_completed = sum(complete),
    pct_completed = round((n_completed / n_started) * 100, 1),
    pct_sample = round((n_completed / 19172) *100, 1)
  ) %>%
  ungroup() %>%  
  left_join(meta_campaign, by = c("resp_country2" = "country")) %>%
  mutate(costs = round(costs, 0),
        cost_per_completed = round(costs / n_completed, 2),
         n_started = formatC(n_started, format = "d", big.mark = ","),
         n_completed = formatC(n_completed, format = "d", big.mark = ","),
         pct_completed = formatC(pct_completed, format = "f", digits = 1),
         pct_sample = formatC(pct_sample, format = "f", digits = 1)
         
  ) %>%
  select(-resp_country2) %>%
  arrange(country_lab)

# export to latex
latex_table <- kable(
  dplyr::select(survey_attrition, country_lab, n_started, n_completed, pct_completed, pct_sample),
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  caption = "Recruitment statistics, by country",
  label = "survey-attrition",
  col.names = c("Country", "Started (n)", "Completed (n)", "Completion (%)", "Sample (%)"),
  align = c("l", "r", "r", "r", "r")
) %>%
  kableExtra::row_spec(0, bold = TRUE)
writeLines(latex_table, "results/survey-attrition.tex")

# export to latex
latex_table <- kable(
  dplyr::select(survey_attrition, country_lab, costs, cost_per_completed),
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  caption = "Survey costs, by country",
  label = "survey-costs",
  col.names = c("Country", "Costs (EUR)", "CPI (EUR)"),
  align = c("l", "r", "r")
) %>%
  kableExtra::row_spec(0, bold = TRUE)
writeLines(latex_table, "results/survey-costs.tex")


# recruitment over time --------------

# data prep

data_survey_resp <- data_survey_resp %>%
  dplyr::arrange(resp_country2_lab, resp_date_start) %>%
  dplyr::group_by(resp_country2_lab) %>%
  dplyr::mutate(resp_cum = dplyr::row_number()) %>%
  dplyr::ungroup() %>% 
  mutate(resp_date = as.Date(resp_date_start))

date_min <- min(data_survey_resp$resp_date, na.rm = TRUE)
date_max <- max(data_survey_resp$resp_date, na.rm = TRUE)


last_points <- data_survey_resp %>%
  group_by(resp_country2_lab) %>%
  summarize(resp_cum = sum(n()),
            resp_date = max(resp_date)) %>%
  ungroup()

# plot

plot_out <- 
ggplot(data_survey_resp, aes(x = resp_date, y = resp_cum, color = resp_country2_lab)) +
  geom_line(size = 1) +
  geom_point(size = 0) +
  geom_label_repel(
    data = last_points,
    aes(label = resp_country2_lab),
    nudge_x = 0.5,         # Push labels slightly to the right
    force = 0.3,             # ↓ reduces the repelling strength (default = 1)
    max.overlaps = Inf,      # allow all labels to appear
    min.segment.length = 0,  # keep connecting segments short
    box.padding = 0.1,       # ↓ reduce box-to-box margin (default = 0.25)
    point.padding = 0.05,     # ↓ reduce distance from point to label (default = 1e-6–0.25)    direction = "y",     # Spread them vertically
    hjust = 0,           # Left-align labels
    size = 3.5,
    show.legend = FALSE,
    segment.color = "gray60",
    family = "Fira Sans"
  ) +
  scale_x_date(
    limits = c(date_min, date_max + 2),
    breaks = seq(date_min, date_max, by = "4 days"),  # exact start
    date_labels = "%b %d",
    expand = expansion(mult = c(0, 0.1))
  ) + 
labs(
    title = "", #Cumulative number of survey respondents over time",
    subtitle = "", #Finalized interviews, by country",
    x = "",
    y = "Completed interviews",
    color = "Country"
  ) +
theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "none",
    legend.title = element_blank(),
    legend.margin=margin(-15,0,0,0),
    legend.spacing.x = unit(.25, 'cm'),
    legend.key.size = unit(.75,"line"),    
    plot.title.position = "plot",
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
plot_out
ggsave("results/lineplot-recruitment-respondents.png", width = 10, height = 5, dpi = 300, bg = "white")



# ad delivery by target groups ----------------------

data_survey_resp$adtype <- str_extract(data_survey_resp$resp_adtype, "topic|neutral") %>% firstup()
data_survey_resp$resp_adtype <- str_replace(data_survey_resp$resp_adtype, "-", " + ") %>% str_to_title()

# table: ad type, pooled

survey_ads_pooled <- tabyl(data_survey_resp$resp_adtype) %>%
  clean_names() %>%
  mutate(
    percent = round(percent * 100, 0)
  ) %>%
    select(-valid_percent) %>%
  mutate(n = formatC(n, format = "d", big.mark = ",")) %>%
  filter(!is.na(data_survey_resp_resp_adtype))
  

# export to latex
latex_table <- kable(
  survey_ads_pooled,
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  caption = "Recruitment by ad type and target group",
  label = "survey-adtype-pooled",
  col.names = c("Target group + Ad type", "Completed interviews (n)", "Percent"),
  align = c("l", "r", "r")
) %>%
  kableExtra::row_spec(0, bold = TRUE)
writeLines(latex_table, "results/survey-adtype-pooled.tex")


# table: ad type by country
survey_ads <- data_survey_resp %>%
  group_by(resp_country2_lab) %>%
  summarize(
    pct_neutral = round(sum(adtype == "Neutral", na.rm = TRUE)/n() * 100, 0),
    pct_topic = round(sum(adtype == "Topic", na.rm = TRUE)/n() * 100, 0)
)

# export to latex
latex_table <- kable(
  survey_ads,
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  caption = "Recruitment by ad type and country",
  label = "survey-adtype",
  col.names = c("Country", "Neutral ad (%)", "Topic ad (%)"),
  align = c("l", "r", "r")
) %>%
  kableExtra::row_spec(0, bold = TRUE)
writeLines(latex_table, "results/survey-adtype.tex")



# location --------------

# THIS CODE ONLY RUNS IF GEO VARIABLES ARE AVAILABLE IN THE DATASET

if(geo_variables_available) {
  
# Load world map as an sf object (no Antarctica)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(name != "Antarctica")

# Projection
robin_crs <- "+proj=robin"

data_survey_resp$resp_lon <- as.numeric(data_survey_resp$resp_lon)
data_survey_resp$resp_lat <- as.numeric(data_survey_resp$resp_lat)

# Create sf points from your survey data
points_sf <- data_survey_resp %>%
  dplyr::select(resp_lon, resp_lat, resp_country2_lab) %>%
  dplyr::filter(!is.na(resp_lon), !is.na(resp_lat)) %>%
  st_as_sf(coords = c("resp_lon", "resp_lat"), crs = 4326) %>%
  st_transform(crs = robin_crs)

# Optional: set a stable legend order (alphabetical by country label)
points_sf$resp_country2_lab <- factor(
  points_sf$resp_country2_lab,
  levels = sort(unique(points_sf$resp_country2_lab))
)

# Transform world to match projection
world_robin <- st_transform(world, crs = robin_crs)

color_codes <- c(
  '#002776',  # Brazil 
  '#e6b800',  # Colombia 
  '#b2df8a',  # Germany
  '#FF9933',  # India 
  '#007FFF',  # Indonesia
  '#008751',  # Nigeria 
  '#8B4513',  # Philippines
  '#FF69B4',  # Poland
  '#E30A17',  # Turkey
  '#6a3d9a',  # UK
  '#555555'   # USA
)

# Plot: gray world background + respondent points colored by country label
plot_world <- ggplot(world_robin) +
  geom_sf(fill = "white", color = "gray50", size = 0.1) +
  geom_sf(
    data = points_sf,
    aes(color = resp_country2_lab),
    size = 0.25,
    alpha = 0.5    # <-- alpha blending for points
  ) +
  labs(color = "Country") +
  scale_color_manual(
    values = color_codes,
    name = "Country"
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 3, alpha = 1)  # solid symbols in legend
  )
  ) + 
  theme_map() + 
  theme_minimal() + 
  theme(
    text = element_text(family = "Fira Sans"),
    legend.position= "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10, face = "bold"),
    legend.margin = margin(-5,0,0,0),
    legend.spacing.x = unit(.15, 'cm'),
    legend.key.size = unit(.75,"line"),   
    legend.text = element_markdown(size = 10, family = "Fira Sans"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA))

plot_world
plot <- plot_world + coord_sf(crs = robin_crs)
ggsave("results/map-respondent-locations.png", plot = plot, width = 10, height = 6, dpi = 300, bg = "white")

}

# survey times --------------

# mean and median survey completion times, by country and pooled

data_survey_resp$resp_duration_mins <- data_survey_resp$resp_duration/60

survey_times_country <- data_survey_resp %>%
  group_by(resp_country2_lab) %>%
  summarize(
    mean_time = round(mean(resp_duration_mins, na.rm = TRUE), 1),
    median_time = round(median(resp_duration_mins, na.rm = TRUE), 1)
  ) %>%
  ungroup()

survey_times_pooled <- data_survey_resp %>%
  summarize(
    mean_time = round(mean(resp_duration_mins, na.rm = TRUE), 1),
    median_time = round(median(resp_duration_mins, na.rm = TRUE), 1)
  ) %>%
  mutate(resp_country2_lab = "Pooled") %>%
  select(resp_country2_lab, mean_time, median_time)

survey_times <- 
  bind_rows(survey_times_country, survey_times_pooled) %>%
  mutate(
    mean_time = formatC(mean_time, format = "f", digits = 1),
    median_time = formatC(median_time, format = "f", digits = 1)
  )

# export to latex, make Pooled bold

survey_times <- survey_times %>%
  mutate(
    resp_country2_lab = ifelse(resp_country2_lab == "Pooled",
                              paste0("\\textbf{", resp_country2_lab, "}"),
                              resp_country2_lab),
    mean_time = ifelse(resp_country2_lab == "\\textbf{Pooled}",
                       paste0("\\textbf{", mean_time, "}"),
                       mean_time),
    median_time = ifelse(resp_country2_lab == "\\textbf{Pooled}",
                         paste0("\\textbf{", median_time, "}"),
                         median_time)
  )

latex_table <- kable(
  survey_times,
  format = "latex",
  booktabs = TRUE,
  linesep = "",
  caption = "Survey completion times, by country and pooled",
  label = "survey-completion-times",
  col.names = c("Country", "Mean time (minutes)", "Median time (minutes)"),
  align = c("l", "r", "r"),
  escape = FALSE
)  %>%
  kableExtra::row_spec(0, bold = TRUE)
writeLines(latex_table, "results/survey-completion-times.tex")




# demographics --------------

# table: pooled across countries

dat_summary <- data_survey_resp %>% dplyr::select(all_of(resp_demographics_covars))
datasummary(as.formula(paste0(paste0(resp_demographics_covars, collapse = "+"), " ~ N + Percent('col')")), 
            data = dat_summary, 
            fmt = function(x) format(round(x, 0), big.mark = ","), 
            title = "Respondent demographics (percentages)\\label{tab:respondents-demcovars}",
            escape = FALSE,
            output = "results/respondents-demcovars.tex")

# table: by country

dat_summary <- data_survey_resp %>% dplyr::select(all_of(resp_demographics_covars), Country = resp_country2) %>% mutate(Country = toupper(Country))

datasummary(as.formula(paste0(paste0(resp_demographics_covars, collapse = "+"), " ~ Country * (Percent('col'))")), 
            data = dat_summary, 
            fmt = function(x) format(round(x, 0), big.mark = ","), 
            title = "Respondent demographics (percentages), by country\\label{tab:respondents-demcovars-country}",
            escape = FALSE,
            output = "results/respondents-demcovars-by-country.tex")


# sample vs. population demographics -------------

pop_age <- 
  read_xlsx("data/cooked/pop-sex-age.xlsx") %>%
  group_by(resp_country2, age_cat3) %>%
  summarize(
    share = sum(share_pop)
  ) %>%
  mutate(variable = "Age") %>%
  rename(value = age_cat3)

pop_sex <- 
  read_xlsx("data/cooked/pop-sex-age.xlsx") %>%
  group_by(resp_country2, gender2) %>%
  summarize(
    share = sum(share_pop)
  ) %>%
  mutate(variable = "Sex") %>%
  rename(value = gender2)

pop_edu <- read_xlsx("data/cooked/pop-edu.xlsx") %>%
  group_by(resp_country2, educ_cat) %>%
  summarize(
    share = sum(share_pop)
  ) %>%
  mutate(variable = "Education") %>%
  rename(value = educ_cat)

pop_dem <- bind_rows(
  pop_age, pop_sex, pop_edu
) %>%
  rename(share_pop = share)

sample_dem <- data_survey_resp %>%
  dplyr::select(
    Age       = age_cat3,
    Sex       = gender2,
    Education = educ_cat,
    resp_country2
  ) %>%
  pivot_longer(
    cols      = c(Age, Sex, Education),
    names_to  = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%                        # drop missings first
  group_by(resp_country2, variable, value) %>%
  summarize(n = n(), .groups = "drop_last") %>%    # counts per category
  mutate(
    share_sample = 100 * n / sum(n)                # correct baseline per country × variable
  ) %>%
  ungroup() %>%
  select(-n)


demographics_df <- left_join(sample_dem, pop_dem, by = c("resp_country2", "variable", "value")) %>%
  mutate(
    share_sample = round(share_sample, 1),
    share_pop = round(share_pop, 1),
    diff = share_sample - share_pop
  ) %>%
  left_join(country_codes_df %>% select(code, country), by = c("resp_country2" = "code"))


# export one gt table per country
walk(unique(demographics_df$resp_country2), function(ctry) {
  demographics_df %>%
    filter(resp_country2 == ctry) %>%
    select(variable, value, share_sample, share_pop, diff) %>%
    gt(
      rowname_col = "variable",
      auto_align  = TRUE
    ) %>%
    fmt_number(
      columns  = c(share_sample, share_pop, diff),
      decimals = 1
    ) %>%
    cols_label(
      value        = "Value",
      share_sample = "Sample (%)",
      share_pop    = "Population (%)",
      diff         = "Difference (%)"
    ) %>%
    data_color(
      columns = c(diff),
      colors  = scales::col_numeric(
        palette = c("red", "white", "blue"),
        domain  = c(-100, 100)
      )
    ) %>%
    tab_options(
      table.font.names = "Fira Sans",
      table.font.size  = px(10),
      row_group.font.weight = "bold"
    ) %>%
    gtsave_auto(
      filename = glue("results/demographics-{ctry}.png")
    )
})



  




