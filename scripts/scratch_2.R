
##############################################
# Presentation 1                             #                      
# Task: DoubleML Brierley and Nathan         #                                                                             
# Date: 09/18/2025                           #                                                                         
##############################################



# Set Up ------------------------------------------------------------------

# Clear
rm(list = ls())

# Packages 
library(DoubleML)        
library(mlr3)            
library(mlr3learners)    
library(mlr3pipelines)   
library(fixest)          
library(data.table)      
library(tidyverse) 
library(stringr)


# Set seed 
set.seed(12345)          

# Load data 
data_replication <- read_csv("Brierley and Nathan replication package/data_clean.csv") |>
  mutate(
    bio_educ_three = as.factor(bio_educ_three),   
    constituency.x = as.factor(constituency.x)    
  )


# Specify Variables --------------------------------------------------------

# Table 5 
dv_var_t5 <- "big_pat_immed"                      
t_var_t5 <- "npp12to16_ps_swing.NEW"              
control_vars_t5 <- c("cpgn_brok_index", "cxn_up_percentage_correct_full",
                       "cxn_down_percentage_correct_full", "age", "female",
                       "chief_relative", "constexec_relative", "da_relative",
                       "mpdce_relative", "local_eth_minority", "lives_outside_ps",
                       "petty_trader", "formal_sector", "bio_educ_three",
                       "asset_index", "years_active_npp", "years_comm",
                       "km_to_capital_wave1", "wealth_index_2km.x",
                       "constituency.x"                            
)

# Table 6 
dv_var_t6 <- "big_pat_after2"                      
t_var_t6 <- "cxn_up_percentage_correct_full"              
control_vars_t6 <- c("broker_up", "broker_down", "cpgn_brok_index", 
                       "cxn_down_percentage_correct_full", "npp12to16_ps_swing.NEW", 
                       "old_branch_position", "age", "female", "chief_relative",
                       "constexec_relative", "da_relative", "mpdce_relative", 
                       "local_eth_minority", "lives_outside_ps", "petty_trader", 
                       "formal_sector", "bio_educ_three", "asset_index", 
                       "years_active_npp", "years_comm", "km_to_capital_wave1", 
                       "wealth_index_2km.x", "constituency.x"
)

# Subsample ---------------------------------------------------------------

dat_t5 <- data_replication |>
  filter(old_branch_position == 1) |>                 
  drop_na(all_of(c(dv_var_t5, t_var_t5, control_vars_t5)))            

dat_t6 <- data_replication |>
  filter(current_exec == 1) |>
  drop_na(all_of(c(dv_var_t6, t_var_t6, control_vars_t6)))  


# FE OLS Benchmark --------------------------------------------------------

# Replicate the main findings of the paper --------

model_feols_t5 <- data_replication |>
  # ensure that respondents were a branch officer
  filter(old_branch_position == 1) |>
  # outcome: big patronage immediately after the election
  feols(big_pat_immed ~
          # main variable of interest
          npp12to16_ps_swing.NEW +
          # control variables
          cpgn_brok_index +
          cxn_up_percentage_correct_full +
          cxn_down_percentage_correct_full +
          age +
          female +
          chief_relative +
          constexec_relative +
          da_relative +
          mpdce_relative +
          local_eth_minority +
          lives_outside_ps +
          petty_trader +
          formal_sector +
          bio_educ_three +
          asset_index +
          years_active_npp +
          years_comm +
          km_to_capital_wave1 +
          wealth_index_2km.x +
          # constituency fixed effects
          constituency.x,
        # polling station clustered SEs
        cluster = ~anon.ps.name)

model_feols_t6 <- data_replication |>
  # ensure that respondents are current executives
  filter(current_exec == 1) |>
  feols(big_pat_after2 ~
          cxn_up_percentage_correct_full +
          broker_up +
          broker_down +
          cpgn_brok_index +
          cxn_down_percentage_correct_full + 
          npp12to16_ps_swing.NEW +
          old_branch_position + 
          age +
          female + 
          chief_relative +
          constexec_relative +
          da_relative +
          mpdce_relative +
          local_eth_minority +
          lives_outside_ps +
          petty_trader +
          formal_sector +
          bio_educ_three +
          asset_index + 
          years_active_npp +
          years_comm +
          km_to_capital_wave1 +
          wealth_index_2km.x +
          # constituency fixed effects
          constituency.x,
        # polling station clustered SEs
        cluster = ~anon.ps.name)

etable(model_feols_t5, model_feols_t6)


# Residualization ---------------------------------------------------------

# Create function that takes a variable, regresses it on fixed effects, and returns the residuals
residualize_var <- function(var, fe, data) {
  v <- data[[var]]
  if (is.numeric(v)) {                                # numeric variable
    return(resid(feols(as.formula(paste0(var, " ~ 1 | ", fe)), data = data)))
  } else if (is.factor(v)) {                          # factor variable to dummies
    mm <- model.matrix(~ . - 1, data = data[var])     # dummy-encode
    resids <- apply(mm, 2, function(col) {            # residualize each dummy separately
      tmp <- data.frame(col = col, fe = data[[fe]])
      resid(feols(col ~ 1 | fe, data = tmp))
    })
    resids <- as.data.frame(resids)                   # return all dummy residuals
    return(resids)
  } else {
    stop("Unsupported type: ", var)                   # throw error if unsupported
  }
}

# Create function that residualizes outcome, treatment, and all controls on fixed effects
residualize_all <- function(data, y, d, x, fe) {
  n <- nrow(data)
  dat_resid <- data.frame(row_id = seq_len(n))       
  
  # residualize Y and D
  dat_resid[[y]] <- residualize_var(y, fe, data)
  dat_resid[[d]] <- residualize_var(d, fe, data)
  
  # residualize controls
  for (v in setdiff(x, fe)) {                         # skip FE variable itself
    r <- residualize_var(v, fe, data)
    if (is.data.frame(r)) {
      dat_resid <- bind_cols(dat_resid, r)            # add multiple dummy residuals
    } else {
      dat_resid[[v]] <- r                             # add single residual column
    }
  }
  
  # clean dataset
  dat_resid <- dat_resid |>
    select(-row_id) |>
    mutate(across(where(is.character), as.factor),    # ensure no character variables
           across(where(is.logical), as.numeric)) |>  # convert logical to numeric
    select(where(~ !(all(is.na(.)) || length(unique(.)) == 1)))  # drop all-NA/constant cols
  
  # define covariates
  x_new <- setdiff(names(dat_resid), c(y, d))
  
  list(data = dat_resid[, c(y, d, x_new)], x_vars = x_new) 
}


# Apply residualization
resid_out_t5 <- residualize_all(dat_t5, dv_var_t5, t_var_t5, control_vars_t5, "constituency.x")
dat_resid_t5 <- resid_out_t5$data               
x_vars_resid_t5 <- resid_out_t5$x_vars   


resid_out_t6 <- residualize_all(dat_t6, dv_var_t6, t_var_t6, control_vars_t6, "constituency.x")
dat_resid_t6 <- resid_out_t6$data               
x_vars_resid_t6 <- resid_out_t6$x_vars            



# DoubleML ----------------------------------------------------------------

# Create function to run doubleml
run_dml <- function(data, dv, t, controls, learner) {
  dml_data <- DoubleMLData$new(                      # wrap dataset for DoubleML
    data = as.data.table(data),
    y_col = dv,
    d_cols = t,
    x_cols = controls
  )
  dml <- DoubleMLPLR$new(data = dml_data, ml_l = learner, ml_m = learner) # partial linear regression

  set.seed(123)
  dml$fit()                                          # fit model with cross-fitting
  
  summ <- dml$summary()                              # summary table
  tibble(
    Estimate = summ[ , "Estimate."],                 # treatment effect estimate
    SE       = summ[ , "Std. Error"],                # standard error
    CI_low   = summ[ , "Estimate."] - 1.96 * summ[ , "Std. Error"],  # 95% CI lower
    CI_high  = summ[ , "Estimate."] + 1.96 * summ[ , "Std. Error"]   # 95% CI upper
  )
}


# Learners with one-hot encoding
ridge_enc <- as_learner(po("encode") %>>% lrn("regr.glmnet", alpha = 0))   # Ridge
lasso_enc <- as_learner(po("encode") %>>% lrn("regr.glmnet", alpha = 1))   # Lasso
enet_enc  <- as_learner(po("encode") %>>% lrn("regr.glmnet", alpha = 0.5)) # Elastic Net
rf_enc    <- as_learner(po("encode") %>>% lrn("regr.ranger", num.trees = 500)) # Random Forest

# Run DML

# Table 5
res_ridge_t5 <- run_dml(dat_resid_t5, dv_var_t5, t_var_t5, x_vars_resid_t5, ridge_enc)
res_lasso_t5 <- run_dml(dat_resid_t5, dv_var_t5, t_var_t5, x_vars_resid_t5, lasso_enc)
res_enet_t5  <- run_dml(dat_resid_t5, dv_var_t5, t_var_t5, x_vars_resid_t5, enet_enc)
res_rf_t5    <- run_dml(dat_resid_t5, dv_var_t5, t_var_t5, x_vars_resid_t5, rf_enc)

# Table 6
res_ridge_t6 <- run_dml(dat_resid_t6, dv_var_t6, t_var_t6, x_vars_resid_t6, ridge_enc)
res_lasso_t6 <- run_dml(dat_resid_t6, dv_var_t6, t_var_t6, x_vars_resid_t6, lasso_enc)
res_enet_t6  <- run_dml(dat_resid_t6, dv_var_t6, t_var_t6, x_vars_resid_t6, enet_enc)
res_rf_t6    <- run_dml(dat_resid_t6, dv_var_t6, t_var_t6, x_vars_resid_t6, rf_enc)

# Get results

results_t5 <- tibble(
  Model = c("FE OLS", "DML Ridge", 
            "DML Lasso", "DML Elastic Net", 
            "DML Random Forest"),
  Estimate = c(coef(model_feols_t5)[t_var_t5],  
               res_ridge_t5$Estimate, res_lasso_t5$Estimate,
               res_enet_t5$Estimate, res_rf_t5$Estimate),
  CI_low = c(confint(model_feols_t5)[t_var_t5, 1], 
             res_ridge_t5$CI_low, res_lasso_t5$CI_low,
             res_enet_t5$CI_low, res_rf_t5$CI_low),
  CI_high = c(confint(model_feols_t5)[t_var_t5, 2], 
              res_ridge_t5$CI_high, res_lasso_t5$CI_high,
              res_enet_t5$CI_high, res_rf_t5$CI_high)
)

print(results_t5)


results_t6 <- tibble(
  Model = c("FE OLS", "DML Ridge", 
            "DML Lasso", "DML Elastic Net", 
            "DML Random Forest"),
  Estimate = c(coef(model_feols_t6)[t_var_t6],  
               res_ridge_t6$Estimate, res_lasso_t6$Estimate,
               res_enet_t6$Estimate, res_rf_t6$Estimate),
  CI_low = c(confint(model_feols_t6)[t_var_t6, 1], 
             res_ridge_t6$CI_low, res_lasso_t6$CI_low,
             res_enet_t6$CI_low, res_rf_t6$CI_low),
  CI_high = c(confint(model_feols_t6)[t_var_t6, 2], 
              res_ridge_t6$CI_high, res_lasso_t6$CI_high,
              res_enet_t6$CI_high, res_rf_t6$CI_high)
)

print(results_t6)



# Plot --------------------------------------------------------------------


ggplot(results_t5, aes(x = Model, y = Estimate)) +
  # Points
  geom_point(aes(color = Model, shape = Model), size = 4) +
  # Error bars with color mapped to Model too
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, color = Model), width = 0.25, linewidth = 1) +
  # Labels on points
  geom_label(aes(label = round(Estimate, 3)), 
             vjust = -0.6, 
             fill = "white", 
             size = 5) +
  # Zero line
  geom_hline(yintercept = 0, linetype = "dotted", color = "red", linewidth = 1) +
  # Manual colors/shapes
  scale_color_manual(values = c("FE OLS" = "grey40", 
                                "DML Ridge" = "black",
                                "DML Lasso" = "black",
                                "DML Elastic Net" = "black",
                                "DML Random Forest" = "black")) +
  scale_shape_manual(values = c("FE OLS" = 17,  
                                "DML Ridge" = 16, 
                                "DML Lasso" = 16, 
                                "DML Elastic Net" = 16, 
                                "DML Random Forest" = 16)) +
  coord_flip() +
  theme_bw() +
  labs(
    title = str_wrap("Effect of a branch’s relative performance compared to the rest of the constituency on the likelihood of receiving major patronage", width = 70),
    y = "Coefficient Estimate", 
    x = ""
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 14),  
    axis.text.x = element_text(size = 14),                 
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5) 
  )



ggplot(results_t6, aes(x = Model, y = Estimate)) +
  # Points
  geom_point(aes(color = Model, shape = Model), size = 4) +
  # Error bars with color mapped to Model too
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high, color = Model), width = 0.25, linewidth = 1) +
  # Labels on points
  geom_label(aes(label = round(Estimate, 3)), 
             vjust = -0.6, 
             fill = "white", 
             size = 5) +
  # Zero line
  geom_hline(yintercept = 0, linetype = "dotted", color = "red", linewidth = 1) +
  # Manual colors/shapes
  scale_color_manual(values = c("FE OLS" = "grey40", 
                                "DML Ridge" = "black",
                                "DML Lasso" = "black",
                                "DML Elastic Net" = "black",
                                "DML Random Forest" = "black")) +
  scale_shape_manual(values = c("FE OLS" = 17, 
                                "DML Ridge" = 16, 
                                "DML Lasso" = 16, 
                                "DML Elastic Net" = 16, 
                                "DML Random Forest" = 16)) +
  coord_flip() +
  theme_bw() +
  labs(
    title = str_wrap("Effect of connections to local elites on payments in the electoral off-cycle", width = 70),
    y = "Coefficient Estimate", 
    x = ""
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 14),   
    axis.text.x = element_text(size = 14),                 
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5) 
  )



