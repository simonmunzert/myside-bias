# function to extract results from cregg:cj

extract_cj <- function(dataFrame) {
  dataFrame <- filter(dataFrame, !is.na(p))
  tr <- createTexreg(
    coef = dataFrame$estimate,
    coef.names = as.character(dataFrame$level),
    pvalues = dataFrame$p,
    se = dataFrame$std.error
    # ci.low = dataFrame$lower,
    # ci.up = dataFrame$upper
  )
  texregObjects <- list(tr)
  return(texregObjects)
}

# colors

cols2 <- viridis_pal()(10)[c(2, 8)]
cols5 <- rev(c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"))

colors <- c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffff", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4")
colors2 <- rev(c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffff"))
color_palette <- colorRampPalette(colors)(7)
color_palette2 <- colorRampPalette(colors2)(7)



# code from colorspace
colorspace_mod <- function(n, h = c(300, 75), c. = c(35, 95), l = c(15, 90), power = c(
                             0.8,
                             1.2
                           ), fixup = TRUE, gamma = NULL, alpha = 1, ...) {
  if (!is.null(gamma)) {
    warning("'gamma' is deprecated and has no effect")
  }
  if (n < 1L) {
    return(character(0L))
  }
  h <- rep(h, length.out = 2L)
  c <- rep(c., length.out = 2L)
  l <- rep(l, length.out = 2L)
  power <- rep(power, length.out = 2L)
  rval <- seq(1, 0, length = n)
  rval <- hex(polarLUV(
    L = l[2L] - diff(l) * rval^power[2L],
    C = c[2L] - diff(c) * rval^power[1L], H = h[2L] - diff(h) *
      rval
  ), fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)),
      width = 2L, upper.case = TRUE
    )
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}


# find closest matches to identify label positions
find_matches <- function(x, y) {
  if (length(x) > length(y)) stop("x must not be longer than y.")
  dist_mat <- sapply(x, function(xx) abs(y - xx))
  rank_mat <- sapply(x, function(xx) rank(abs(y - xx)))
  closest_matches <- numeric()
  for (i in 1:ncol(dist_mat)) {
    min_rank <- order(rank_mat[, i])[1]
    closest_matches[i] <- y[min_rank]
    rank_mat <- rank_mat[-min_rank, ]
    dist_mat <- dist_mat[-min_rank, ]
  }
  return(closest_matches)
}

range01 <- function(x, ...) {
  (x - min(x, ...)) / (max(x, ...) - min(x, ...))
}


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Make zeros print as "0" always
prettyZero <- function(l) {
  max.decimals <- max(nchar(str_extract(l, "\\.[0-9]+")), na.rm = T) - 1
  lnew <- formatC(l,
    replace.zero = T, # zero.print = "0",
    digits = max.decimals, format = "f", preserve.width = T
  )
  return(lnew)
}


# overwrite N() function to prettify printing of large numbers in datasummary tables
N <- \(x) format(length(x), big.mark = ",")


# prepare categorical variables for barplot summarizes
catvar_summarize <- function(var, data, by = NULL, weight = NULL) {
  dat <- data
  
  if (is.null(weight)) {
    # Unweighted version
    dat %<>%
      group_by(across(all_of(c(by, var)))) %>%
      summarize(n = n(), .groups = "drop_last") %>%
      mutate(freq = n / sum(n)) %>%
      ungroup()
  } else {
    # Weighted version
    dat %<>%
      # optionally drop missing weights; remove this line if you want to keep them
      filter(!is.na(.data[[weight]])) %>%
      group_by(across(all_of(c(by, var)))) %>%
      summarize(n = sum(.data[[weight]], na.rm = TRUE), .groups = "drop_last") %>%
      mutate(freq = n / sum(n)) %>%
      ungroup()
  }
  
  dat$variable <- var
  dat <- rename(dat, vallabels = all_of(var))
  dat
}



# function to extract estimates from AMCE models
tidy_extend2 <- function(x, data = "", covars = "", covars_filter = "", model = "me") {
  out <- x
  if (model == "me") {
    out <- tidy_plus_plus(x, add_header_rows = TRUE) %>%
      filter(var_type != "ran_pars") %>%
      group_by(var_label) %>%
      mutate(n_levels = cumsum(n()))
    if (covars_filter != "") {
      out <- filter(out, str_detect(variable, regex(covars_filter, ignore_case = TRUE)))
    }
    out$response <- names(x@frame)[1]
  }
  out$data <- data
  out$covars <- covars
  out$xpos <- rev(seq_len(nrow(out)))
  out
}


# function to extract estimates from exposure models
tidy_extend3 <- function(x, data = "", covars = "", covars_filter = "") {
  out <- tidy(x)
  if (covars_filter != "") {
    out <- filter(out, str_detect(term, regex(paste0("^(?!.*:).*", covars_filter, ".*$"), ignore_case = TRUE)))
  }
  out$data <- data
  out$covars <- covars
  out
}



# function to extract marginal means + differences in marginal means

tidy_interact_mms <- function(model, var_vignette, var_resp, data, sample, diff = FALSE, flip = FALSE) {
  if (diff == TRUE) {
    # define vectors of linear combinations to compute differences in marginal means
    # see https://www.andrewheiss.com/blog/2023/07/25/conjoint-bayesian-frequentist-guide/#conditional-marginal-means
    if (length(unique(pull(data, var_vignette))) == 2) {
      mat <- c(
        -1, 1, 0, 0,
        0, 0, -1, 1
      )
    }
    if (length(unique(pull(data, var_vignette))) == 3) {
      mat <- c(
        -1, 1, 0, 0, 0, 0,
        0, 0, -1, 1, 0, 0,
        0, 0, 0, 0, -1, 1
      )
    }
    if (length(unique(pull(data, var_vignette))) == 4) {
      mat <- c(
        -1, 1, 0, 0, 0, 0, 0, 0,
        0, 0, -1, 1, 0, 0, 0, 0,
        0, 0, 0, 0, -1, 1, 0, 0,
        0, 0, 0, 0, 0, 0, -1, 1
      )
    }
    if (length(unique(pull(data, var_vignette))) > 4) {
      stop("Number of categories of var_vignette > 4: Fix tidy_interact_mms() function.")
    }
    group_diffs_terms <- matrix(
      mat * if_else(flip == FALSE, 1, -1),
      ncol = length(levels(pull(data, var_vignette)))
    ) %>%
      magrittr::set_colnames(levels(pull(data, var_vignette)))

    # compute marginal means
    interact_mms_diff <- marginal_means(
      model = model,
      variables = c(var_vignette, var_resp),
      cross = TRUE,
      wts = "cells",
      hypothesis = group_diffs_terms
    ) %>% as_tibble()
    out <- interact_mms_diff
  } else {
    interact_mms <- marginal_means(
      model = model,
      variables = c(var_vignette, var_resp),
      cross = TRUE,
      wts = "cells"
    ) %>% as_tibble()
    out <- interact_mms
    out <- dplyr::rename(out, all_of(c(var_vignette_label = var_vignette, var_resp_label = var_resp)))
  }
  model_out <- model
  out$response <- names(model_out@frame)[1]
  out$data <- sample
  out$var_resp <- var_resp
  out$var_vignette <- var_vignette
  out$est_type <- ifelse(diff == TRUE, "mm_diff", "mm")
  return(out)
}

# function to add confidence intervals to effects df

# Add confidence intervals to a data frame with columns `estimate` and `std.error`
add_confints <- function(
    df,
    levels = c(.80, .95, .99),
    est_col = "estimate",
    se_col = "std.error",
    method = c("z", "t"),
    df_resid = NULL, # only needed for method = "t"
    lower_suffix = "_lo",
    upper_suffix = "_hi") {
  method <- match.arg(method)

  if (!all(c(est_col, se_col) %in% names(df))) {
    stop("Data must contain columns '", est_col, "' and '", se_col, "'.")
  }

  for (lvl in levels) {
    alpha <- 1 - lvl
    crit <- if (method == "z") {
      qnorm(1 - alpha / 2)
    } else {
      if (is.null(df_resid)) stop("For method = 't', please provide df_resid.")
      qt(1 - alpha / 2, df = df_resid)
    }

    lo <- df[[est_col]] - crit * df[[se_col]]
    hi <- df[[est_col]] + crit * df[[se_col]]

    tag <- gsub("\\.", "", sprintf("%.0f", lvl * 100)) # e.g., 0.95 -> "95"

    df[[paste0("ci", tag, lower_suffix)]] <- lo
    df[[paste0("ci", tag, upper_suffix)]] <- hi
  }

  df
}




# functions to process vignette models for AMCE reporting ----------

# extract model estimates
models_tidy_fun <- function(models_list, labels_df, label_var, by_vars = NULL, outcome_vars, drop_vig_pos = TRUE, drop_vig_vars = FALSE) {
  if (is.null(by_vars)) {
    models_tidy <-
      map_df(models_list, tidy_extend2, data = "pooled") %>%
      left_join(labels_df, by = c("variable" = label_var)) %>%
      ungroup()
  } else {
    models_tidy_list <- list()
    for (i in seq_along(outcome_vars)) {
      models_tidy_list[[i]] <- map2(models_list, by_vars, ~ tidy_extend2(.x[[i]], data = .y))
    }
    models_tidy <-
      map(models_tidy_list, ~ bind_rows(.x)) %>%
      bind_rows() %>%
      left_join(labels_df, by = c("variable" = label_var)) %>%
      ungroup()
  }
  if (drop_vig_pos == TRUE) {
    models_tidy <- filter(models_tidy, variable != "vig_pos_cat")
  }
  if (drop_vig_vars == TRUE) {
    models_tidy <- filter(models_tidy, !(variable %in% vig_covars_df_main$vig_attribute_varnames))
  }
  models_tidy
}


# extract contrasts from emmeans models (for hypothesis tests)
models_tidy_contrast_fun <- function(models_list, var_x, var_z = NULL, by_vars = NULL, outcome_vars, labels_df, label_var) {
  # helper function to extract contrasts from a model
  extract_emm <- function(model, data = NULL) {
    if (is.null(var_z)) {
      formula_str <- paste(var_x)
    } else {
      formula_str <- paste(var_x, "|", var_z)
    }
    emm_results <- emmeans(model, specs = as.formula(paste("~", formula_str))) %>%
      contrast(method = "revpairwise") %>%
      tidy()
    if (is.null(var_z)) {
      emm_results$variable <- var_x
      emm_results$label <- str_extract(emm_results$contrast, "^[^-]+")
    } else {
      emm_results$variable <- paste0(var_x, "*", var_z)
      emm_results$label <- paste0(str_extract(emm_results$contrast, "^[^-]+"), "| ", emm_results[[var_z]])
      emm_results[, var_z] <- NULL
    }
    emm_results$response <- names(model@frame)[1]
    if (!is.null(data)) {
      emm_results$data <- data
    } else {
      emm_results$data <- "pooled"
    }
    emm_results
  }
  # apply the function to each model
  if (is.null(by_vars)) {
    models_tidy <- map(models_list, extract_emm) %>% bind_rows()
  } else {
    models_tidy_list <- list()
    for (i in seq_along(outcome_vars)) {
      models_tidy_list[[i]] <- map2(models_list, by_vars, ~ extract_emm(model = .x[[i]], data = .y))
    }
    models_tidy <-
      map(models_tidy_list, ~ bind_rows(.x)) %>%
      bind_rows()
  }
  # Join with labels
  models_tidy_out <- left_join(models_tidy, labels_df, by = c("variable" = label_var)) %>% ungroup()
}

# prepare model estimates for gt table
gt_prep_fun <- function(x, by = NULL, arrange_by = NULL, varlabels_ref = NULL) {
  x_out <- x %>%
    dplyr::select(all_of(c("data", varlabels_ref, "label", "estimate", "statistic", "response"))) %>%
    pivot_wider(names_from = all_of(by), values_from = c(estimate, statistic)) %>%
    # dplyr::select(-c(header_row, reference_row)) %>%
    filter(!is.na(.[[names(select(., starts_with("estimate_")))[1]]]) &
      .[[names(select(., starts_with("estimate_")))[1]]] != 0) %>%
    group_by(!!sym(varlabels_ref))
  if (is.null(arrange_by)) {
    x_out <- x_out %>%
      arrange(!!sym(varlabels_ref))
  } else {
    x_out <- x_out %>%
      arrange(!!sym(varlabels_ref), desc(!!sym(arrange_by)))
  }
  x_out
}

# extract number of observations from lme4
extract_nobs_fun <- function(x) {
  nobs_vec <- sapply(grp <- lme4::getME(x, "flist"), function(i) length(unique(i)))
  nobs_vec
}

# extract model summary
glance_parse_fun <- function(x, country_re = TRUE, by = NULL, varlabels_ref = NULL) {
  out <- map(x, glance) %>%
    bind_rows() %>%
    dplyr::select(-logLik, -REMLcrit, -df.residual, -AIC, -BIC)
  out_n <- map_dfr(x, extract_nobs_fun)
  out <- cbind(out, out_n)
  out$by <- by
  if (country_re == TRUE) {
    out_wide <- out %>%
      pivot_longer(
        cols = c(nobs, resp_id, deck_id, resp_country2, sigma),
        names_to = "metric",
        values_to = "value"
      ) %>%
      pivot_wider(names_from = by, values_from = value)
  } else {
    out_wide <- out %>%
      pivot_longer(
        cols = c(nobs, resp_id, deck_id, sigma),
        names_to = "metric",
        values_to = "value"
      ) %>%
      pivot_wider(names_from = by, values_from = value)
  }
  names(out_wide) <- paste0("estimate_", names(out_wide))
  out_wide[, paste0("statistic_", by)] <- NA
  out_wide <- rename(out_wide, label = estimate_metric)
  out_wide[, varlabels_ref] <- "Model summary"
  out_wide <- out_wide %>% relocate(!!sym(varlabels_ref), 1)
  out_wide <- out_wide %>% relocate(label, .after = !!sym(varlabels_ref))
  out_wide <- out_wide %>% group_by(!!sym(varlabels_ref))
  out_wide$label <- out_wide$label %>%
    str_replace("nobs", "N<sub>Observations</sub>") %>%
    str_replace("resp_id", "N<sub>Respondents</sub>") %>%
    str_replace("deck_id", "N<sub>Vignette decks</sub>") %>%
    str_replace("resp_country2", "N<sub>Countries</sub>") %>%
    str_replace("sigma", "σ")
  out_wide
}

# build gt table
gt_table_fun <- function(gt_df, glance_df = NULL, stats_df, outcome_vars, outcome_vars_labels, sample = "Pooled", spanners = TRUE, varlabels_ref = NULL, avatars = FALSE, estimates = "amce") {
  list_cols_labels <- set_names(c(sample, outcome_vars_labels), c("label", paste0("estimate_", outcome_vars)))
  colors <- c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffff", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4")
  colors2 <- rev(c("#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffff"))
  color_palette <- colorRampPalette(colors)(7)
  color_palette2 <- colorRampPalette(colors2)(7)
  gt_table_out <- gt_df %>%
    gt() %>%
    {
      if (avatars == TRUE) {
        fmt_image(.,
          columns = label,
          path = "build-vignettes/images/avatars/",
          file_pattern = "{x}.png"
        ) %>%
          cols_align(
            align = "center",
            columns = label
          )
      } else {
        cols_align(.,
          align = "left",
          columns = label
        )
      }
    } %>%
    {
      if (estimates == "amce") {
        fmt_number(.,
          columns = starts_with("estimate_"),
          decimals = 1,
          force_sign = TRUE,
          scale_by = 100,
          pattern = "{x}pp"
        ) %>%
          reduce(outcome_vars, ~ underline_style(.x, source_dat = stats_df, var = .y), .init = .) %>%
          data_color(
            columns = starts_with("estimate"),
            palette = color_palette,
            reverse = FALSE,
            domain = c(-1, 1) * max(abs(range(dplyr::select(ungroup(gt_df), starts_with("estimate")), na.rm = TRUE)))
          )
      } else {
        fmt_number(.,
          columns = starts_with("estimate_"),
          decimals = 1,
          force_sign = FALSE,
          scale_by = 100,
          pattern = "{x}%"
        ) %>%
          data_color(
            columns = starts_with("estimate"),
            palette = color_palette2,
            reverse = FALSE,
            domain = range(dplyr::select(ungroup(gt_df), starts_with("estimate")), na.rm = TRUE)
          )
      }
    } %>%
    {
      if (!is.null(glance_df)) {
        rows_add(., .list = glance_df)
      } else {
        .
      }
    } %>%
    cols_label(
      .list = list_cols_labels,
      .fn = md
    ) %>%
    cols_hide(columns = c(starts_with("statistic"), any_of(c("data", "response")))) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels()
    ) %>%
    # set equal column width
    cols_width(
      starts_with("estimate_") ~ px(55) # Apply equal width to all columns except the first
    ) %>%
    opt_table_font(
      font = list(gt::google_font("Fira Sans"))
    ) %>%
    tab_options(
      table.font.size = "9px",
      row_group.padding = px(2),
      data_row.padding = px(1)
    )
  if (spanners == TRUE) {
    gt_table_out <- gt_table_out %>%
      tab_spanner(label = md("<b>Perceptions</b>"), columns = contains("perc")) %>%
      tab_spanner(label = md("<b>Actions</b>"), columns = estimate_vig_remove:estimate_vig_job)
  }
  if (is.null(glance_df)) {
    gt_table_out <- gt_table_out
  } else {
    gt_table_out <- gt_table_out %>%
      tab_style(
        style = list(
          cell_fill(color = "#efefef"),
          cell_text(color = "black")
        ),
        locations = cells_body(
          rows = !!sym(varlabels_ref) == "Model summary"
        )
      ) %>%
      tab_style(
        style = list(
          cell_fill(color = "#efefef"),
          cell_text(color = "black")
        ),
        locations = cells_row_groups(
          groups = "Model summary"
        )
      ) %>%
      fmt_markdown(rows = !!sym(varlabels_ref) == "Model summary") %>%
      fmt_number(
        rows = !!sym(varlabels_ref) == "Model summary",
        sep_mark = ",",
        decimals = 0
      ) %>%
      fmt_number(
        rows = label == "σ",
        sep_mark = ",",
        decimals = 2
      )
  }
  gt_table_out
}


# helper function to underline significant estimates in gt() for model
underline_style <- function(gt_dat, source_dat, var, stat_threshold = 2) {
  col_name <- paste0("estimate_", var)
  stat_name <- paste0("statistic_", var)
  tab_style(gt_dat,
    style = cell_text(decorate = "underline"),
    locations = cells_body(
      columns = all_of(col_name),
      rows = which(abs(source_dat[[stat_name]]) > stat_threshold, arr.ind = TRUE)
    )
  )
}


## functions to compute marginal means for vignette models -----

# collapse arbitrary columns into one, dropping the originals
collapse_columns <- function(.data, cols, name = "collapsed", sep = "_", na.rm = TRUE) {
  # .data: data.frame / tibble
  # cols:  character vector of column names to combine
  # name:  name of the new column (string)
  # sep:   separator between pasted pieces
  # na.rm: if TRUE, skip NAs when pasting
  tidyr::unite(.data, !!sym(name), tidyselect::all_of(cols),
    sep = sep, remove = TRUE, na.rm = na.rm
  )
}

# Helper: get outcome name from model
.outcome_name <- function(mod) {
  f <- stats::formula(mod)
  as.character(f[[2]])
}

# Helper: categorical predictor -> population-level marginal means
mm_categorical <- function(mod, var, re.form = NA) {
  avg_predictions(
    mod,
    by = var,
    re.form = re.form
  ) |>
    dplyr::mutate(variable = var, .after = 1)
}

# Helper: numeric predictor -> grid over observed range in `data`
mm_numeric <- function(mod, var, data, n = 5, grid_type = "mean_or_mode", re.form = NA) {
  rng <- range(data[[var]], na.rm = TRUE)
  newdata <- datagrid(
    model = mod,
    grid_type = grid_type
  )
  # override `var` with an evenly spaced grid over observed data range
  newdata <- tidyr::expand_grid(
    newdata,
    !!!set_names(list(seq(rng[1], rng[2], length.out = n)), var)
  )
  avg_predictions(mod, newdata = newdata, by = var, re.form = re.form) |>
    dplyr::mutate(variable = var, .after = 1)
}


# compute marginal means for a set of models & predictors
models_tidy_mms_fun <- function(models_list,
                                predictors,
                                data,
                                labels_df,
                                label_var,
                                drop_vig_pos = TRUE,
                                drop_vig_vars = FALSE,
                                sample = "pooled") {
  # ensure models are named
  if (is.null(names(models_list)) || any(names(models_list) == "")) {
    names(models_list) <- vapply(models_list, .outcome_name, character(1))
  }
  # factor vs numeric check based on the provided `data`
  is_factorish <- function(v) is.factor(data[[v]]) || is.character(data[[v]])
  is_numericish <- function(v) is.numeric(data[[v]]) || is.integer(data[[v]])
  # extract marginal means
  mms_df <-
    purrr::imap(models_list, function(mod, mname) {
      purrr::map_dfr(predictors, function(v) {
        if (is_factorish(v)) {
          mm_categorical(mod, v, re.form = NA)
        } else if (is_numericish(v)) {
          mm_numeric(mod, v, data = data, n = 5, grid_type = "mean_or_mode", re.form = NA)
        } else {
          stop(sprintf("Predictor '%s' is neither numeric nor factor/character in `data`.", v))
        }
      }) |>
        dplyr::mutate(response = mname, .before = 1)
    }) |>
    dplyr::bind_rows() |>
    unite("label", all_of(predictors), sep = "_", remove = TRUE, na.rm = TRUE)
  if (drop_vig_pos == TRUE) {
    mms_df <- filter(mms_df, variable != "vig_pos_cat")
  }
  if (drop_vig_vars == TRUE) {
    mms_df <- filter(mms_df, !(variable %in% vig_covars_df_main$vig_attribute_varnames))
  }
  mms_df <- mms_df %>% left_join(labels_df, by = c("variable" = label_var))
  mms_df <- mms_df %>% mutate(data = sample)
}







## functions to compute classification agreement stats ---------

# compute binary entropy
binary_entropy <- function(p) {
  ifelse(
    p %in% c(0, 1),
    0,
    -p * log2(p) - (1 - p) * log2(1 - p)
  )
}

reverse_entropy <- function(p) {
  1 - ifelse(p %in% c(0, 1), 0, -p * log2(p) - (1 - p) * log2(1 - p))
}
