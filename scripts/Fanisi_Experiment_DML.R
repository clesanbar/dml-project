library(fixest)
library(tidyr)
library(broom)
library(dplyr)
library(ggplot2)
library(data.table)
library(mlr3)
library(mlr3learners)
library(data.table)
library(DoubleML)
library(magrittr)
library(tibble)
library(purrr)
library(patchwork)

##### Data ########
dml_df <- readstata13::read.dta13("~/Documents/Temp GHub/mtc_analysis_closethegap.dta")

# outcome variables
outcomes <- c("votednrm", "votedopp", "votedindep")

# global covariate lists - this is the covariates standardised for all respondents in the sample
controls <- c("age_gepub_st", "female_gepub_st", "educyr_gepub_st", "wealthindex_gepub_st",
              "nrmpref_gepub_st", "lastturnout_gepub_st", "powerful_gepub_st",
              "counting_gepub_st", "salience_gepub_st", "source_gepub_st", "news_gepub_st")

# interaction term variables (already present in the data)
control_interact <- paste0("treat_", controls)
#############################################
# intend NRM controls (standardised): the authors demean/standardise within the subgroup in order to estimate tau
controls_intnrm <- c("age_ge_intnrm_st", "female_ge_intnrm_st", "educyr_ge_intnrm_st",
  "wealthindex_ge_intnrm_st", "nrmpref_ge_intnrm_st", "lastturnout_ge_intnrm_st",
  "powerful_ge_intnrm_st", "counting_ge_intnrm_st", "salience_ge_intnrm_st",
  "source_ge_intnrm_st", "news_ge_intnrm_st")

# interactions with treatment (columns like treat*age_ge_intnrm_st, etc.)
controls_intnrm_treat <- paste0("treat_", controls_intnrm)

########################## Replication ##########################
results <- list()
for (y in outcomes) {
  # full sample, with controls
   all_resp <- feols(as.formula(paste(y, "~ treat +",
                                      paste(c(controls, control_interact),
                                            collapse = " + "), "| code")), 
                     data = dml_df %>% filter(uniqueid == 1), 
                     cluster = ~ codeps)

  # people who initially intended to vote for nrm, with the subgroup specific controls
  int_nrm <- feols(as.formula(paste(y, "~ treat +",
                                    paste(c(controls_intnrm, controls_intnrm_treat),
                                          collapse = " + "), "| code")), 
                   data = dml_df %>% filter(uniqueid == 1, intendnrm == 1), 
                   cluster = ~ codeps)

  results[[y]] <- list(all_resp = all_resp, int_nrm = int_nrm)}

##################################################
# coefficients & CIs
plot_df <- imap_dfr(results, function(mods, y) {     
  imap_dfr(mods, function(m, m_name) {               
    tidy(m, conf.int = TRUE, conf.level = 0.95) %>%
      filter(term == "treat") %>%
      mutate(ci90_low = confint(m, level = 0.90)["treat", 1],
        ci90_high = confint(m, level = 0.90)["treat", 2],
        outcome = y, model = m_name)})})

#clean up
plot_df$outcome <- factor(plot_df$outcome, levels = c("votedindep", "votedopp", "votednrm" ))
plot_df$model <- ifelse(plot_df$model == "int_nrm", "Lean NRM", "All") 
plot_df$model <- factor(plot_df$model, levels = c("Lean NRM", "All"))


# replicate figure 4 in paper
# replicate figure 4 in paper
exp_rep <- ggplot(plot_df,
                  aes(x = estimate, y = outcome,
                      shape = model, colour = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = ci90_low, xmax = ci90_high),
                 position = position_dodge(width = 0.5),
                 height = 0.25, linewidth = 0.9) +
  geom_vline(xintercept = 0, colour = "grey40") + 
  geom_label(aes(label = round(estimate, 3)), position = position_dodge(width = 0.5),
             vjust = -0.6, fill = "white", size = 5) +
  labs(x = "Average Treatment Effect", y = NULL, shape = NULL, colour = NULL) +
  scale_colour_grey(start = 0.2, end = 0.6) + theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank()) 

exp_rep
ggsave("~/Documents/Temp GHub/experiment_repl.png", width = 8, height = 6)

########################## Double Machine Learning ##########################


# function wrapper (PLR)
run_dml_plr <- function(df, outcome, subgroup = c("all","nrm"), method,
                        treatment = "treat", ct = controls_intnrm, clusters = "codeps", fe = fe_var) {
  
  # subset data
  dsub <- df %>% filter(uniqueid == 1,
           if (subgroup == "nrm") intendnrm == 1 else TRUE) %>%
    select(all_of(c(outcomes, treat, ct, clusters, fe))) %>%
    drop_na(all_of(c(outcome, treat, ct, clusters, fe)))
  
  # for lasso/ridge learners, categorical fixed effect needs to one-hot encoded 
  R <- model.matrix(~ code, data = dsub)
  dfX <- cbind(dsub %>% select(- code), as.data.frame(R[, -1, drop = FALSE]))  
  x_names <- c(ct, colnames(R)[-1])
  
  # classification learners for partial linear model 
  learner <- if (method == "ridge"){
    lrn("regr.glmnet", alpha = 0)
  } else if (method == "lasso"){
    lrn("regr.glmnet", alpha = 1)
  } else if (method == "net"){
    lrn("regr.nnet")
  } else if (method == "forest"){
    lrn("regr.ranger")
  } else if (method == "svm"){
    lrn("regr.svm")
  } else if (method == "xgboost"){
    lrn("regr.xgboost")
  }
  
  ml_l_sim <- learner$clone()
  ml_m_sim <- learner$clone()
  
  # DoubleML cluster object
  dml_data <- DoubleMLClusterData$new(
    data = as.data.table(dfX),
    y_col = outcome,
    d_cols = treatment,
    x_cols = x_names,
    cluster_cols = clusters)
  
  dml_plr_ob = DoubleMLPLR$new(dml_data, ml_l= ml_l_sim, ml_m= ml_m_sim, n_folds=5)
  set.seed(17806); dml_plr_ob$fit()
  
  tibble(outcome = outcome,
    subgroup = subgroup,
    method = method,
    estimate = dml_plr_ob$coef,
    se = dml_plr_ob$se,
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se)}

# code for full grid (I do not run the code for full grid, I just filter by relevant estimates )
outcomes  <- c("votednrm","votedopp","votedindep")
subgroups <- c("all","nrm")
methods   <- c("ridge","lasso","net","forest","svm","xgboost")
param_grid <- expand.grid(outcome = outcomes,
                          subgroup = subgroups,
                          method = methods,
                          stringsAsFactors = FALSE)

# filter for the main result of figure 4
nrm_only <- param_grid %>% filter(subgroup == "nrm", outcome == "votednrm")

# only run on partial grid (the lower votednrm outcome for those who leaned nrm)
results_plr_nrm <- purrr::pmap_dfr(nrm_only, function(outcome, subgroup, method) {
  run_dml_plr(df = dml_df, outcome = outcome, subgroup = subgroup,
  method = method, clusters = "codeps")})

################################### Comparison Plot ####################################
#add OLS row
ols_row <- tibble(outcome= "votednrm",subgroup= "nrm",method="ols",
  estimate = -0.063415613, se= 0.024113730) %>%
  mutate(ci_low  = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se)

plr_plot <- results_plr_nrm %>%
  tibble::add_row(ols_row) %>%
  mutate(method = factor(method, 
      levels = c("lasso", "net", "forest", "ridge", "svm", "xgboost", "ols"),
      labels = c("Lasso", "Net", "Forest", "Ridge", "SVM", "XGBoost", "OLS")))

p1 <- ggplot(plr_plot, aes(y = method, x = estimate,
                           colour = ifelse(method == "OLS", "OLS", "Other"))) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6, colour = "grey65") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, linewidth = 0.9) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("OLS" = "grey60", "Other" = "black"), guide = "none") +
  labs(x = "ATE (estimate ± 95% CI)", y = "Estimators", title = "ATE Coefficient Estimate (PLR)") +
  geom_label(aes(label = round(estimate, 3)), 
             vjust = -0.6, fill = "white", size = 5) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank()) +
  theme(plot.title = ggtext::element_textbox_simple(size = 12, color = "white",
                                                    fill = "grey35",
                                                    padding = margin(6, 6, 6, 6),
                                                    margin = margin(0, 0, 8, 0),
                                                    halign = 0.5))


################### Version 2: IRM ##########################

run_dml_irm <- function(df, outcome, subgroup = c("all","nrm"), method,
                        treatment = "treat", ct = controls_intnrm, clusters = "codeps",
                        fe = fe_var) {
  
  # subset data for each group
  dsub <- df %>% filter(uniqueid == 1,
                        if (subgroup == "nrm") intendnrm == 1 else TRUE) %>%
    select(all_of(c(outcomes, treat, ct, clusters, fe))) %>%
    drop_na(all_of(c(outcome, treat, ct, clusters, fe)))
  
  # for lasso/ridge learners need to one-hot encoded fixed effect variable
  R <- model.matrix(~ code, data = dsub)
  dfX <- cbind(dsub %>% select(- code), as.data.frame(R[, -1, drop = FALSE]))  
  x_names <- c(ct, colnames(R)[-1])
  
  
  # classification learners for binary outcome and control (IRM)
  learner <- if (method == "ridge"){
    lrn("classif.glmnet", alpha = 0) 
  } else if (method == "lasso"){
    lrn("classif.glmnet", alpha = 1) 
  } else if (method == "net"){
    lrn("classif.nnet", maxit = 100)
  } else if (method == "forest"){
    lrn("classif.ranger", num.trees = 500)
  } else if (method == "xgboost"){
    lrn("classif.xgboost", predict_type = "prob")
  } else if (method == "svm"){
    lrn("classif.svm")
  }
  
  ml_g <- learner$clone()
  ml_m <- learner$clone()
  
  # DoubleML cluster object
  dml_data <- DoubleMLClusterData$new(
    data = as.data.table(dfX),
    y_col = outcome,
    d_cols = treatment,
    x_cols = x_names,
    cluster_cols = clusters)
  
  dml_irm_ob = DoubleMLIRM$new(dml_data, ml_g, ml_m, n_folds=5)
  set.seed(17806); dml_irm_ob$fit()
  
  tibble(
    outcome = outcome,
    subgroup = subgroup,
    method = method,
    estimate = dml_irm_ob$coef,
    se = dml_irm_ob$se,
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se)}


# iterate through all learners, only of lean-nrm, vote nrm estimate
results_tbl_irm <- purrr::pmap_dfr(nrm_only, function(outcome, subgroup, method) {
  run_dml_irm(df = dml_df, outcome = outcome, subgroup = subgroup,
              method = method, clusters = "codeps")})

################ Plotting #############
irm_plot <- results_tbl_irm %>%
  tibble::add_row(ols_row) %>%
  mutate(method = factor(method, 
                         levels = c("lasso", "net", "forest", "ridge", "svm", "xgboost", "ols"),
                         labels = c("Lasso", "Net", "Forest", "Ridge", "SVM", "XGBoost", "OLS")))

# plotting but excluding xgboost until I find out what the hell happened
p2 <- ggplot(irm_plot %>% filter(method != "XGBoost"),
             aes(y = method, x = estimate,
                 colour = ifelse(method == "OLS", "OLS", "Other"))) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6, colour = "grey65") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.2, linewidth = 0.9) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("OLS" = "grey60", "Other" = "black"), guide = "none") +
  labs(y = " ",
       title = "ATE Coefficient Estimate (IRM)") +
  geom_label(aes(label = round(estimate, 3)), 
             vjust = -0.6, fill = "white", size = 5) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank()) +
  theme(plot.title = ggtext::element_textbox_simple(size = 12, color = "white",
                                                    fill = "grey35",
                                                    padding = margin(6, 6, 6, 6),
                                                    margin = margin(0, 0, 8, 0),
                                                    halign = 0.5))

#combine and export plots for now
dml_plot = p1 + p2


dml_plot
ggsave("~/Documents/Temp GHub/experiment_coefplot_dml.png", width = 8, height = 6)

