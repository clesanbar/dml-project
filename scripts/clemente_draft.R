
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


# Replicate the main findings of the paper --------

model_replication_t5 <- data_replication_clean |>
  # ensure that respondents were a branch officer
  filter(old_branch_position == 1) |>
  # keep only outcome, treatment, covariates, dummies, and polling station identifier
  select(all_of(c(outcome_t5, treatment_t5, covariates_t5, dummies, clusters))) |>
  # remove missing values
  drop_na() |>
  # define predictors
  feols(reformulate(c(treatment_t5, covariates_t5, dummies),
                    # define outcome
                    response = outcome_t5),
        # polling station clustered SEs
        cluster = clusters)

model_replication_t6 <- data_replication_clean |>
  # ensure that respondents are current executives
  filter(current_exec == 1) |>
  # keep only outcome, treatment, covariates, dummies, and polling station identifier
  select(all_of(c(outcome_t6, treatment_t6, covariates_t6, dummies, clusters))) |>
  # remove missing values
  drop_na() |>
  # define predictors
  feols(reformulate(c(treatment_t6, covariates_t6, dummies),
                    # define outcome
                    response = outcome_t6),
        # polling station clustered SEs
        cluster = clusters)

etable(model_replication_t5, model_replication_t6)


# Double machine learning -------

# set the seed
set.seed(02139)

double_ml_coefficients <- function(model, method){
  
  data <- if (model == "t5") {
    
    data_replication_clean |>
      # ensure that respondents were a branch officer
      filter(old_branch_position == 1) |>
      # keep only outcome, treatment, covariates, dummies, and polling station identifier
      select(all_of(c(outcome_t5, treatment_t5, covariates_t5, dummies, clusters))) |>
      # remove missing values
      drop_na() |>
      # turn into data.table
      as.data.table() |>
      # create the DML data object
      DoubleMLClusterData$new(y_col = outcome_t5,
                              d_cols = treatment_t5,
                              x_cols = c(covariates_t5, dummies),
                              cluster_cols = clusters)
    
  } else if (model == "t6") {
    
    data_replication_clean |>
      # ensure that respondents are current executives
      filter(current_exec == 1) |>
      # keep only outcome, treatment, covariates, dummies, and polling station identifier
      select(all_of(c(outcome_t6, treatment_t6, covariates_t6, dummies, clusters))) |>
      # remove missing values
      drop_na() |>
      # turn into data.table
      as.data.table() |>
      # create the DML data object
      DoubleMLClusterData$new(y_col = outcome_t6,
                              d_cols = treatment_t6,
                              x_cols = c(covariates_t6, dummies),
                              cluster_cols = clusters)
    
  }
  
  learner <- if (method == "ridge"){
    lrn("regr.glmnet", alpha = 0)
  } else if (method == "lasso"){
    lrn("regr.glmnet", alpha = 1)
  } else if (method == "net"){
    lrn("regr.glmnet", alpha = 0.5)
  } else if (method == "forest"){
    lrn("regr.ranger", num.trees = 500)
  }
  
  ml_l_sim <- learner$clone()
  ml_m_sim <- learner$clone()
  
  dml_object <- data |>
    # specify the parameters
    DoubleMLPLR$new(ml_l=ml_l_sim, ml_m=ml_m_sim,
                    # use 5-fold cross-fitting
                    n_folds=5)
  
  dml_object$fit() |>
    # do not print messages
    suppressWarnings()
  
  coefficients <- tibble(estimate = dml_object$coef, se = dml_object$se) |>
    # calculate confidence intervals
    mutate(ci_low = estimate - 1.96 * se,
           ci_high = estimate + 1.96 * se,
           # include information on the model and learner
           model = model,
           method = method)
  
  return(coefficients)
  
}

# obtain all 8 combinations
data_dml <- expand.grid(model = c("t5", "t6"), method = c("ridge", "lasso", "net", "forest")) |>
  # apply function to each row
  pmap_dfr(double_ml_coefficients) |>
  # code type
  mutate(type = "DML")

data_feols <- tibble(estimate = c(coef(model_replication_t5)[treatment_t5],
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
                         net = "Elastic net",
                         forest = "Random forest"),
         model = if_else(model == "t5", "Treatment 5: branch officers", "Treatment 6: current executives"),
         # create order so that OLS is on top
         method = fct_relevel(method, "OLS", after = 4)) |>
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
  scale_color_manual(values = c("DML" = "gray30", "FEOLS" = "gray60")) +
  # add labels
  labs(y = "Machine learning method",
       x = "Estimated coefficient") +
  # remove legend
  theme(legend.position = "none")


# Replicate to examine distribution ---------

data_distribution <- expand.grid(model = c("t5", "t6"), method = c("ridge", "lasso", "net", "forest")) |>
  # repeat 10000 times
  slice(rep(1:n(), each = 100)) |>
  # apply function to each row
  pmap_dfr(double_ml_coefficients, .progress = TRUE)

data_distribution |>
  # improve labels for plot
  mutate(method = recode(method,
                         ridge = "Ridge",
                         lasso = "Lasso",
                         net = "Elastic net",
                         forest = "Random forest")) |>
  # create ggplot object
  ggplot(aes(x = estimate)) +
  # plot density
  geom_density(alpha = 0.6, fill = "gray60", color = "gray30") +
  # facet by model and method
  facet_grid(method ~ model, scales = "free_x") +
  # add vertical line with the OLS estimate
  geom_vline(data = data_feols |> select(estimate, model),
             aes(xintercept = estimate),
             linetype = "dashed", color = "gray30") +
  # remove legend
  theme(legend.position = "none") +
  # add labels
  labs(x = "Estimated coefficient",
       y = "Density")
