library(tidyverse)
library(tidymodels)
library(furrr)

set.seed(123)
memory.limit(1e9)
# future::plan(multiprocess)

# Synthetic data ----------------------------------------------------------

#' tibble with random data
#' @param rows number of rows
#' @param cols number of columns
#' @param related proportion of variables related to outcome
#' @return data frame with specified number of rows and columns
rdata <- function(cols = 1e3, related = 1, rows = 1e3) {

  # Random functions chosen randomly to generate random data
  # First name of functions (for presentation), then functions themselves
  funs_s  <- sample(c("rnorm", "rexp", "runif"), cols, replace = TRUE)
  funs    <- lapply(funs_s, get)

  rel <- seq_len(cols) <= max(cols * related, 1)

  X  <- sapply(funs, exec, rows)
  Xy <- X[,  rel, drop = FALSE]
  Xe <- X[, !rel, drop = FALSE]
  b  <- rnorm(ncol(Xy))
  Y  <- Xy %*% b + rnorm(rows) * 10 # linear comb with random coefs

  out <- cbind(Y, Xy, Xe)
  colnames(out) <- c(
    "Y",
    paste0("Xy", seq_len(ncol(Xy))),
    if (ncol(Xe) > 0) paste0("Xe", seq_len(ncol(Xe)))
  )
  out
}

#' Remove some data points at random.
#' @param x matrix
#' @param prop proportion of values set to NA
miss <- function(x, prop = 0) {
  X <- x[, -1]
  Y <- x[, 1]
  lx <- length(X)
  X[sample.int(lx, floor(lx * prop))] <- NA
  cbind(Y, X)
}

# Data object -------------------------------------------------------------
df_all <-
  crossing(
    p = 50, # seq(10, 50, 1e1),
    related = seq(0, 1, .1),
  ) %>%
  mutate(data = future_map2(p, related, rdata)) %>%
  crossing(missing = seq(0, 1, .1)) %>%
  mutate(
    data = future_map2(data, missing, miss),
    data = map(data, as_tibble)
  )


# Impute ------------------------------------------------------------------

# KOlla vilka imputeringsmetoder som finns implementerade
ls("package:recipes", pattern = "impute$")

reci <- function(x) {
  recipe(Y ~ ., x) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())
}

df_imp <-
  df_all %>%
  # slice(1:3) %>%
  crossing(
    impute_nm = c(
      # "step_bagimpute",    # Funkar men långsamt!
      # "step_knnimpute",    # leder till minnesproblem/krasch
      # "step_lowerimpute",  # för censurerad data
      "step_meanimpute",
      "step_medianimpute",
      # "step_modeimpute",   # Ej för numeriska data
      # "step_rollimpute"    # för tidsbunden data!
      "step_naomit"
    )
  ) %>%
  mutate(
    imp_fun  = map(impute_nm, get),
    rec      = future_map(data, reci, .progress = TRUE),
    imp_rec  = future_map2(rec, imp_fun, ~ .x %>% .y(all_predictors()),
                           .progress = TRUE),
    imp_prep = future_map2(imp_rec, data, prep, .progress = TRUE),
    imp_data = future_map(imp_prep, juice, .progress = TRUE)
  ) %>%
  select(p, related, missing, impute_nm, imp_data)

# plan(sequential) # Close workers to free memory


# Lasso -------------------------------------------------------------------

# Fit Lasso regression to data
lasso <- function(dat, penalty) {
  linear_reg(mixture = 1, penalty = penalty) %>%
  set_engine("glmnet") %>%
  fit(Y ~ ., data = dat)
}

# Fins best penalty for lasso using CV
cv_glmnet <- function(data) {
  glmnet::cv.glmnet(as.matrix(select(data, -Y)), data$Y)
}

n_zero <- function(cvobj) {
  if (!is.null(cvobj)) with(cvobj, nzero[lambda == lambda.min]) else NA
}

# Observed and predicted values to compare
obspred <- function(fit, new_data) {
  bind_cols(truth = new_data$Y, predict(fit, new_data))
}

# Assessment metrics to evaluate prediction power
metr <- function(data) {
  yardstick::metrics(data, truth, .pred)
}

# Summarise metrics from CV
metr_sum <- function(x) {
  group_by(x, .metric) %>%
  summarise(
    median = median(  .estimate,       na.rm = TRUE),
    ci_low = quantile(.estimate, .025, na.rm = TRUE),
    ci_hi  = quantile(.estimate, .925, na.rm = TRUE)
  ) %>%
  ungroup()
}

# plan(multiprocess) # Get it back!

df_lasso <-
  df_imp %>%
  mutate(
    cv.glmnet = future_map(imp_data, possibly(cv_glmnet, NULL),
                           .progress = TRUE),
    lambda    = map_dbl(cv.glmnet,
                        ~ if (!is.null(.)) {.$lambda.min} else {NA}),
    df        = map_dbl(cv.glmnet, n_zero),
    boot      = map(imp_data, possibly(bootstraps, NULL)),
    df_train  = map(boot, ~ map(.$splits, analysis)),
    df_test   = map(boot, ~ map(.$splits, assessment)),
    fit       = future_map2(
                  df_train, lambda, ~ map(.x, possibly(lasso, NULL), .y),
                  .progress = TRUE),
    obspred   = future_map2(fit, df_test, map2, possibly(obspred, NULL),
                            .progress = TRUE),
    metr      = future_map(obspred, map, possibly(metr, NULL), .progress = TRUE),
    metr      = map(metr, possibly(bind_rows, NULL)),
    metr_sum  = map(metr, possibly(metr_sum,  tibble()))
  )


# Figures -----------------------------------------------------------------

# Figure data
df_fig <-
  df_lasso %>%
  select(p, related, missing, impute_nm, lambda, df, metr_sum) %>%
  filter(!is.null(metr_sum)) %>%
  unnest(metr_sum) %>%
  mutate(Imputation = gsub("(^step_)|(impute$)", "", impute_nm)) %>%
  filter(.metric == "rmse") %>%
  gather("measure", "value", df, median) %>%
  mutate_at(vars(ci_low, ci_hi), ~ replace(., measure == "df", NA)) %>%
  select(-.metric, -impute_nm) %>%
  mutate(measure = replace(measure, measure == "median", "RMSE"))

# Make figure
fig <- function(data) {
  data %>%
  ggplot(aes(missing, value)) +
  facet_grid(measure ~ related, scales = "free") +
  geom_line(aes(color = Imputation)) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_hi, fill = Imputation), alpha = .3) +
  theme_light() +
  theme(
    legend.position = "bottom"
  ) +
  scale_x_continuous(minor_breaks = NULL) +
  labs(
    y     = "",
    x     = "Proportion of missing data before imputation",
    title = "Missing data decrease predictive power"
  )
}

# Figure including na.omit onstead of imputation.
# Distorts the axis but might be an interesting comparison
fig(df_fig)
ggsave("naomit.png", height = 20, width = 30, units = "cm")

# Remove naomit to see results more clearly
fig(filter(df_fig, Imputation != "naomit"))
ggsave("mean_median.png", height = 20, width = 30, units = "cm")



# KNN and bagged imputation ----------------------------------------------------

# Was not able to do this simultanesly as other methods since it requires too
# much memory to allow for bootstrapped based CI etc.
# I use the same object names to overwrite earlier (no longer needed) objects
# to save memory


df_imp <-
  df_all %>%
  filter(related == .5, missing %in% c(.2, .4, .6, .8)) %>%
  crossing(
    impute_nm = c("step_bagimpute", "step_knnimpute")
  ) %>%
  mutate(
    imp_fun  = map(impute_nm, get),
    rec      = future_map(data, reci, .progress = TRUE),
    imp_rec  = future_map2(rec, imp_fun, ~ .x %>% .y(all_predictors()),
                           .progress = TRUE),
    imp_prep = future_map2(imp_rec, data, prep, .progress = TRUE),
    imp_data = future_map(imp_prep, juice, .progress = TRUE)
  ) %>%
  select(p, related, missing, impute_nm, imp_data)
  mutate(
    rec      = future_map(data, reci, .progress = TRUE),
    imp_rec  = future_map(rec, step_knnimpute, all_predictors(),
                           .progress = TRUE),
    imp_prep = future_map2(imp_rec, data, prep, .progress = TRUE),
    imp_data = future_map(imp_prep, juice, .progress = TRUE)
  ) %>%
  select(p, related, missing, imp_data)


df_lasso <-
  df_imp %>%
  mutate(
    cv.glmnet = future_map(imp_data, possibly(cv_glmnet, NULL),
                           .progress = TRUE),
    lambda    = map_dbl(cv.glmnet,
                        ~ if (!is.null(.)) {.$lambda.min} else {NA}),
    df        = map_dbl(cv.glmnet, n_zero),
    df_split  = map(imp_data, initial_split),
    df_train  = map(df_split, training),
    df_test   = map(df_split, testing),
    fit       = future_map2(
                  df_train, lambda, possibly(lasso, NULL),
                  .progress = TRUE),
    obspred   = future_map2(fit, df_test, possibly(obspred, NULL),
                            .progress = TRUE),
    metr_sum   = future_map(obspred, possibly(metr, NULL), .progress = TRUE),
  )

df_fig <-
  df_lasso %>%
  select(p, missing, lambda, df, metr_sum, impute_nm) %>%
  filter(!is.null(metr_sum)) %>%
  unnest(metr_sum) %>%
  filter(.metric == "rmse") %>%
  gather("measure", "value", df, .estimate) %>%
  select(-.metric) %>%
  mutate(measure = replace(measure, measure == ".estimate", "RMSE")) %>%
  mutate(impute_nm = gsub("(^step_)|(impute$)", "", impute_nm))


df_fig %>%
  ggplot(aes(missing, value, color = impute_nm)) +
  facet_wrap(. ~ measure, scales = "free") +
  geom_line() +
  theme_light() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  scale_x_continuous(minor_breaks = NULL) +
  labs(
    y     = "",
    x     = "Proportion of missing data before imputation",
    title = "Bagged imputation",
    subtitle = "p = 50, related = .5"
  )

ggsave("knn_bagged.png", height = 20, width = 30, units = "cm")


# Notes! ----------------------------------------------------------
# Lecture slides 11 slide 13 says that Bootstrap methods etc are
# not good for significance tests etc for lasso.
# Good to mention!
