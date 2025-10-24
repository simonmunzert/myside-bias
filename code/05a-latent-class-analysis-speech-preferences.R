# load packages, functions, and helper objects --------

source("code/packages.R")
source("code/functions.R")
source("code/helper-objects.R")

library(poLCA)
library(ggalluvial)

# load data ---------------------------

load("data/cooked/data_survey_combined.RData")
load("data/cooked/data_survey_resp.RData")


# select speech items ----------------

speech_items <- resp_covars_df$resp_variable[resp_covars_df$speech_outcomes == TRUE]
speech_df <- dplyr::select(data_survey_resp, all_of(speech_items))


# estimate latent class model ----------------

# model formula
lca_formula <- as.formula(
    paste("cbind(", paste(names(speech_df), collapse = ", "), ") ~ 1")
  )
  
# custom entropy calculator
calc_entropy <- function(post) {
  mean(apply(post, 1, function(p) {
    -sum(p * log(p + 1e-10)) / log(length(p))
  }))
}

# fit models with 1 to 5 classes

set.seed(123)
model_results <- map_df(1:5, function(k) {
  model <- poLCA(f, data = speech_df, nclass = k, 
                 na.rm = FALSE, verbose = FALSE, maxiter = 1000)
  tibble(
    n_classes = k,
    log_likelihood = model$llik,
    df = model$resid.df,
    AIC = model$aic,
    BIC = model$bic,
    Gsq = model$Gsq,
    chi_sq = model$Chisq,
    entropy = calc_entropy(model$posterior),
    min_class_prob = min(model$P),
    max_class_prob = max(model$P)
  )
})

# explore fit indices

ggplot(model_results, aes(n_classes)) +
  geom_line(aes(y = BIC), color = "blue") +
  geom_line(aes(y = AIC), color = "red") +
  labs(title = "Model Fit by Number of Classes",
       y = "Information Criterion", x = "Number of Classes")

# Store all models and class assignments
set.seed(123)
model_assignments <- map_df(1:5, function(k) {
  model <- poLCA(f, data = speech_df, nclass = k, 
                 na.rm = FALSE, verbose = FALSE, maxiter = 1000)
  
  tibble(
    id = 1:nrow(speech_df),  # or use another unique ID column
    n_classes = k,
    class = factor(model$predclass)
  )
})

assignment_wide <- model_assignments %>%
  pivot_wider(names_from = n_classes, values_from = class, names_prefix = "class_")

# alluvial plot

alluvial_df <- model_assignments %>%
  mutate(n_classes = paste0("K", n_classes))  # for cleaner x-axis labels
ggplot(alluvial_df,
       aes(x = n_classes, stratum = class, alluvium = id, fill = class, label = class)) +
  geom_flow(stat = "alluvium", lode.guidance = "forward", alpha = 0.5) +
  geom_stratum() +
  theme_minimal() +
  labs(title = "Class Membership Across Latent Class Models",
       x = "Number of Classes", y = "Count") +
  theme(legend.position = "none")

# tile heatmap

assignment_wide %>%
  count(class_2, class_3) %>%
  ggplot(aes(x = class_2, y = class_3, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  labs(title = "Class Assignment Transition: 2-Class vs 3-Class Model",
       x = "2-Class Membership", y = "3-Class Membership") +
  theme_minimal()


