
# Setup -------------

library(DoubleML)
library(mlr3)
library(mlr3learners)
library(fixest)
library(data.table)
library(tidyverse)

options(
  # define default options for data.table's fread()
  datatable.fread.datatable = FALSE,
  datatable.na.strings = c("", "NA"),
  # otherwise I get an error
  arrow.unsafe_metadata = TRUE
)

# define base ggplot theme
theme_set(theme_bw())
theme_update(
  # remove the lines in the background
  panel.grid = element_blank(),
  # change the facet style
  strip.background = element_rect(fill="gray30"),
  strip.text = element_text(colour = 'white')
)

# Load data --------------

data_replication <- "Brierley and Nathan replication package/data_clean.csv" |>
  # load data
  read_csv()


# Process data for models -------

data_replication_clean <- data_replication |>
  # recode education into dummies
  pivot_wider(names_from = bio_educ_three,
              values_from = bio_educ_three,
              values_fn = length,
              values_fill = 0,
              # add prefix
              names_prefix = "bio_educ_") |>
  # turn constituencies into dummies
  pivot_wider(names_from = constituency.x,
              values_from = constituency.x,
              values_fn = length,
              values_fill = 0,
              # add prefix
              names_prefix = "constituency_")


# Define treatment, outcome, and covariates ---------

outcome_t5 <- "big_pat_immed"
outcome_t6 <- "big_pat_after2"
treatment_t5 <- "npp12to16_ps_swing.NEW"
treatment_t6 <- "cxn_up_percentage_correct_full"
covariates_t5 <- c(
  "cpgn_brok_index",
  "cxn_up_percentage_correct_full",
  "cxn_down_percentage_correct_full",
  "age",
  "female",
  "chief_relative",
  "constexec_relative",
  "da_relative",
  "mpdce_relative",
  "local_eth_minority",
  "lives_outside_ps",
  "petty_trader",
  "formal_sector",
  "asset_index",
  "years_active_npp",
  "years_comm",
  "km_to_capital_wave1",
  "wealth_index_2km.x"
)
covariates_t6 <- c(
  "broker_up",
  "broker_down",
  "cpgn_brok_index", 
  "cxn_down_percentage_correct_full",
  "npp12to16_ps_swing.NEW", 
  "old_branch_position",
  "age",
  "female",
  "chief_relative",
  "constexec_relative",
  "da_relative",
  "mpdce_relative", 
  "local_eth_minority",
  "lives_outside_ps",
  "petty_trader", 
  "formal_sector",
  "asset_index", 
  "years_active_npp",
  "years_comm",
  "km_to_capital_wave1", 
  "wealth_index_2km.x"
)
dummies <- data_replication_clean |>
  # select dummies
  select(starts_with("bio_educ_"), starts_with("constituency_")) |>
  # get column names
  colnames() |>
  # remove one of the levels to avoid perfect multicollinearity
  setdiff(c("bio_educ_1", "constituency_agona_east"))
clusters <- "anon.ps.name"


# Replicate the first column of Table 5 and 6 of the original paper --------

data_replication_t5 <- data_replication_clean |>
  # ensure that respondents were a branch officer
  filter(old_branch_position == 1) |>
  # keep only outcome, treatment, covariates, dummies, and polling station identifier
  select(all_of(c(outcome_t5, treatment_t5, covariates_t5, dummies, clusters))) |>
  # remove missing values
  drop_na()

data_replication_t6 <- data_replication_clean |>
  # ensure that respondents are current executives
  filter(current_exec == 1) |>
  # keep only outcome, treatment, covariates, dummies, and polling station identifier
  select(all_of(c(outcome_t6, treatment_t6, covariates_t6, dummies, clusters))) |>
  # remove missing values
  drop_na()

model_replication_t5 <- data_replication_t5 |>
  # define predictors
  feols(reformulate(c(treatment_t5, covariates_t5, dummies),
                    # define outcome
                    response = outcome_t5),
        # polling station clustered SEs
        cluster = clusters)

model_replication_t6 <- data_replication_t6 |>
  # define predictors
  feols(reformulate(c(treatment_t6, covariates_t6, dummies),
                    # define outcome
                    response = outcome_t6),
        # polling station clustered SEs
        cluster = clusters)

etable(model_replication_t5, model_replication_t6)


# Define list of learners -----------

table_learners <- mlr_learners |>
  # extract information
  as.data.table() |>
  # turn into tibble
  tibble() |>
  # keep relevant variables
  select(label, key, task_type)

learner_net <- lrn("regr.nnet")
learner_forest <- lrn("regr.ranger")
learner_svm <- lrn("regr.svm")
learner_boost <- lrn("regr.xgboost")
# alpha = 0 is the ridge penalty
learner_ridge <- lrn("regr.glmnet", alpha = 0)
# alpha = 1 is the lasso penalty
learner_lasso <- lrn("regr.glmnet", alpha = 1)


# Double machine learning -------

data_dml_t5 <- data_replication_t5 |>
  # turn into data.table
  as.data.table() |>
  # create the DML data object
  DoubleMLClusterData$new(y_col = outcome_t5,
                          d_cols = treatment_t5,
                          x_cols = c(covariates_t5, dummies),
                          cluster_cols = clusters)

data_dml_t6 <- data_replication_t6 |>
  # turn into data.table
  as.data.table() |>
  # create the DML data object
  DoubleMLClusterData$new(y_col = outcome_t6,
                          d_cols = treatment_t6,
                          x_cols = c(covariates_t6, dummies),
                          cluster_cols = clusters)

# set the seed
set.seed(02139)

double_ml_coefficients <- function(model, method){
  
  # define data
  data <- if (model == "t5") data_dml_t5 else data_dml_t6
  
  learner <- if (method == "ridge") {
    learner_ridge
  } else if (method == "lasso") {
    learner_lasso
  } else if (method == "net") {
    learner_net
  } else if (method == "forest") {
    learner_forest
  } else if (method == "svm") {
    learner_svm
  } else if (method == "boost") {
    learner_boost
  }
  
  # clone the learner for l and m
  ml_l_sim <- learner$clone()
  ml_m_sim <- learner$clone()
  
  dml_object <- data |>
    # specify the parameters
    DoubleMLPLR$new(ml_l=ml_l_sim, ml_m=ml_m_sim,
                    # use 5-fold cross-fitting
                    n_folds=5)
  
  # fit the model
  dml_object$fit()
  
  coefficients <- tibble(estimate = dml_object$coef, se = dml_object$se) |>
    # calculate confidence intervals
    mutate(ci_low = estimate - 1.96 * se,
           ci_high = estimate + 1.96 * se,
           # include information on the model and learner
           model = model,
           method = method)
  
  return(coefficients)
  
}

# obtain all combinations
data_dml <- expand.grid(model = c("t5", "t6"), method = c("ridge", "lasso", "net", "forest", "svm", "boost")) |>
  # apply function to each row
  pmap_dfr(double_ml_coefficients) |>
  # code type
  mutate(type = "DML")

data_feols <- tibble(
  estimate = c(coef(model_replication_t5)[treatment_t5],
               coef(model_replication_t6)[treatment_t6]),
  se = c(se(model_replication_t5)[treatment_t5],
         se(model_replication_t6)[treatment_t6]),
  model = c("t5", "t6"),
  method = "OLS",
  type = "FEOLS") |>
  # calculate confidence intervals
  mutate(ci_low = estimate - 1.96 * se,
         ci_high = estimate + 1.96 * se)

data_plot <- bind_rows(data_dml, data_feols)

data_plot |>
  # improve labels for plot
  mutate(method = recode(method,
                         ridge = "Ridge",
                         lasso = "Lasso",
                         net = "Neural net",
                         forest = "Random forest",
                         svm = "SVM",
                         boost = "XGBoost"),
         model = if_else(model == "t5", "Table 5: vote swing", "Table 6: more connections"),
         # create order so that OLS is on top
         method = fct_relevel(method, "OLS", after = 6)) |>
  # create ggplot object
  ggplot(aes(x = estimate, y = method, color = type)) +
  # plot points
  geom_point(aes(shape = type), size = 2) +
  # plot confidence intervals
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0.2) +
  # facet by model
  facet_wrap(~ model, scales = "free_x") +
  # include a line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray90") +
  # define colors manually
  scale_color_manual(values = c("DML" = "gray30", "FEOLS" = "gray70")) +
  # add labels
  labs(y = "Estimator",
       x = "Effect of treatment") +
  # remove legend
  theme(legend.position = "none")

ggsave("figures/observational_coefplot_dml.png", width = 8, height = 6)


# Predicting treatment with covariates to examine whether the "groups" are balanced -----------

model_balance_t5 <- data_replication_t5 |>
  # define predictors
  feols(reformulate(c(covariates_t5, dummies),
                    # define treatment as the outcome
                    response = treatment_t5),
        # polling station clustered SEs
        cluster = clusters)

model_balance_t6 <- data_replication_t6 |>
  # define predictors
  feols(reformulate(c(covariates_t6, dummies),
                    # define treatment as the outcome
                    response = treatment_t6),
        # polling station clustered SEs
        cluster = clusters)

etable(model_balance_t5, model_balance_t6)

data_plot_balance <- lst(model_balance_t5, model_balance_t6) |>
  # extract the coefficients from each model
  map_df(~as.data.frame(pluck(.x, "coeftable")), .id = "model") |>
  # add row names as a column
  rownames_to_column("variable") |>
  # extract everything before ...
  mutate(variable = str_remove(variable, "\\.\\.\\..*")) |>
  # remove the intercept
  filter(variable != "(Intercept)") |>
  # rename variables
  rename(estimate = Estimate, se = `Std. Error`) |>
  # calculate confidence intervals
  mutate(ci_low = estimate - 1.96 * se,
         ci_high = estimate + 1.96 * se,
         model = if_else(model == "model_balance_t5", "Table 5: vote swing", "Table 6: more connections"))

data_plot_balance |>
  # remove one covariate to improve visualization
  filter(variable != "npp12to16_ps_swing.NEW") |>
  # create ggplot object
  ggplot(aes(x = estimate, y = variable)) +
  # include a line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
  # plot points
  geom_point(size = 2, color = "gray30") +
  # plot confidence intervals
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0.2, color = "gray30") +
  # facet by model
  facet_wrap(~ model) +
  # add labels
  labs(y = "Covariate",
       x = "Effect on treatment (continuous)") +
  # remove legend
  theme(legend.position = "none")

ggsave("figures/observational_coefplot_balance.png", width = 8, height = 6)


# Examine the distribution of residuals ---------

data_residuals <- lst(model_replication_t5, model_replication_t6) |>
  # extract the residuals from each model
  map_df(~tibble(residuals = residuals(.x)), .id = "model") |>
  # improve labels for plot
  mutate(model = if_else(model == "model_replication_t5", "Table 5: vote swing", "Table 6: more connections"))

data_residuals |>
  # create ggplot object
  ggplot(aes(x = residuals)) +
  # plot density
  geom_density(alpha = 0.6, fill = "gray60", color = "gray30") +
  # facet by model
  facet_wrap(~ model) +
  # add labels
  labs(x = "Residuals", y = "Density")

ggsave("figures/observational_density_residuals.png", width = 8, height = 3)


# Do DML on observations with extreme residuals -----------

data_replication_t5_extreme <- data_replication_t5 |>
  # add residuals
  mutate(residuals = residuals(model_replication_t5)) |>
  # keep only observations either in the upper 15% or lower 15% distribution of residuals
  filter(residuals <= quantile(residuals, 0.1) | residuals >= quantile(residuals, 0.9))

data_replication_t6_extreme <- data_replication_t6 |>
  # add residuals
  mutate(residuals = residuals(model_replication_t6)) |>
  # keep only observations either in the upper 15% or lower 15% distribution of residuals
  filter(residuals <= quantile(residuals, 0.1) | residuals >= quantile(residuals, 0.9))

data_dml_t5 <- data_replication_t5_extreme |>
  # turn into data.table
  as.data.table() |>
  # create the DML data object
  DoubleMLClusterData$new(y_col = outcome_t5,
                          d_cols = treatment_t5,
                          x_cols = c(covariates_t5, dummies),
                          cluster_cols = clusters)

data_dml_t6 <- data_replication_t6_extreme |>
  # turn into data.table
  as.data.table() |>
  # create the DML data object
  DoubleMLClusterData$new(y_col = outcome_t6,
                          d_cols = treatment_t6,
                          x_cols = c(covariates_t6, dummies),
                          cluster_cols = clusters)

data_dml_extreme <- expand.grid(model = c("t5", "t6"), method = c("ridge", "lasso", "net", "forest", "svm", "boost")) |>
  # apply function to each row
  pmap_dfr(double_ml_coefficients) |>
  # code type
  mutate(type = "DML")

model_replication_t5_extreme <- data_replication_t5_extreme |>
  # define predictors
  feols(reformulate(c(treatment_t5, covariates_t5, dummies),
                    # define outcome
                    response = outcome_t5),
        # polling station clustered SEs
        cluster = clusters)

model_replication_t6_extreme <- data_replication_t6_extreme |>
  # define predictors
  feols(reformulate(c(treatment_t6, covariates_t6, dummies),
                    # define outcome
                    response = outcome_t6),
        # polling station clustered SEs
        cluster = clusters)

data_feols_extreme <- tibble(
  estimate = c(coef(model_replication_t5_extreme)[treatment_t5],
               coef(model_replication_t6_extreme)[treatment_t6]),
  se = c(se(model_replication_t5_extreme)[treatment_t5],
         se(model_replication_t6_extreme)[treatment_t6]),
  model = c("t5", "t6"),
  method = "OLS",
  type = "FEOLS") |>
  # calculate confidence intervals
  mutate(ci_low = estimate - 1.96 * se,
         ci_high = estimate + 1.96 * se)

data_plot_extreme <- bind_rows(data_dml_extreme, data_feols_extreme) |>
  # code that they are using the extreme values
  mutate(type = "Extreme residuals") |>
  # add the original results
  bind_rows(data_plot |>
              # code that it is using the entire sample
              mutate(type = "All observations"))

data_plot_extreme |>
  # improve labels for plot
  mutate(method = recode(method,
                         ridge = "Ridge",
                         lasso = "Lasso",
                         net = "Neural net",
                         forest = "Random forest",
                         svm = "SVM",
                         boost = "XGBoost"),
         model = if_else(model == "t5", "Table 5: vote swing", "Table 6: more connections"),
         # create order so that OLS is on top
         method = fct_relevel(method, "OLS", after = 6)) |>
  # create ggplot object
  ggplot(aes(x = estimate, y = method, color = type)) +
  # include a line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray90") +
  # plot points
  geom_point(aes(shape = type), size = 2, position = position_dodge(0.5)) +
  # plot confidence intervals
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0.2, position = position_dodge(0.5)) +
  # facet by model
  facet_wrap(~ model, scales = "free_x") +
  # define colors manually
  scale_color_manual(values = c("Extreme residuals" = "gray30", "All observations" = "gray70")) +
  # add labels
  labs(y = "Estimator",
       x = "Effect of treatment",
       color = "Sample",
       shape = "Sample") +
  # legend at the bottom
  theme(legend.position = "bottom")

ggsave("figures/observational_coefplot_residuals.png", width = 8, height = 5)


# Replicate to examine distribution ---------

# data_distribution <- expand.grid(model = c("t5", "t6"), method = c("ridge", "lasso", "net", "forest")) |>
#   # repeat 10000 times
#   slice(rep(1:n(), each = 100)) |>
#   # apply function to each row
#   pmap_dfr(double_ml_coefficients, .progress = TRUE)
# 
# data_distribution |>
#   # improve labels for plot
#   mutate(method = recode(method,
#                          ridge = "Ridge",
#                          lasso = "Lasso",
#                          net = "Elastic net",
#                          forest = "Random forest")) |>
#   # create ggplot object
#   ggplot(aes(x = estimate)) +
#   # plot density
#   geom_density(alpha = 0.6, fill = "gray60", color = "gray30") +
#   # facet by model and method
#   facet_grid(method ~ model, scales = "free_x") +
#   # add vertical line with the OLS estimate
#   geom_vline(data = data_feols |> select(estimate, model),
#              aes(xintercept = estimate),
#              linetype = "dashed", color = "gray30") +
#   # remove legend
#   theme(legend.position = "none") +
#   # add labels
#   labs(x = "Estimated coefficient",
#        y = "Density")
