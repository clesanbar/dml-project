
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
  read_csv() |>
  # turn into factor
  mutate(bio_educ_three = as.factor(bio_educ_three))


# Replicate the main findings of the paper --------

model_replication_t5 <- data_replication |>
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

model_replication_t6 <- data_replication |>
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

etable(model_replication_t5, model_replication_t6)


# Double machine learning -------

data_double_ml <- data_replication |>
  # remove missing values on relevant variables
  drop_na(big_pat_immed,
          npp12to16_ps_swing.NEW,
          cpgn_brok_index,
          cxn_up_percentage_correct_full,
          cxn_down_percentage_correct_full,
          age,
          female,
          chief_relative,
          constexec_relative,
          da_relative,
          mpdce_relative,
          local_eth_minority,
          lives_outside_ps,
          petty_trader,
          formal_sector,
          bio_educ_three,
          asset_index,
          years_active_npp,
          years_comm,
          km_to_capital_wave1,
          wealth_index_2km.x,
          # constituency fixed effects
          constituency.x)

#### Define data for DDML
y <- data_double_ml$big_pat_immed
d <- data_double_ml$npp12to16_ps_swing.NEW
x <- data_double_ml |>
  select(cpgn_brok_index,
         cxn_up_percentage_correct_full,
         cxn_down_percentage_correct_full,
         age,
         female,
         chief_relative,
         constexec_relative,
         da_relative,
         mpdce_relative,
         local_eth_minority,
         lives_outside_ps,
         petty_trader,
         formal_sector,
         bio_educ_three,
         asset_index,
         years_active_npp,
         years_comm,
         km_to_capital_wave1,
         wealth_index_2km.x,
         # constituency fixed effects
         constituency.x)

dml_data_sim <- double_ml_data_from_matrix(X = x, y = y, d = d)

#### Select LASSO learner
learner <- lrn("regr.ranger")
ml_l_sim <- learner$clone()
ml_m_sim <- learner$clone()

#### Execute DDML
obj_dml_plr_sim <- DoubleMLPLR$new(dml_data_sim, ml_l=ml_l_sim, ml_m=ml_m_sim)
obj_dml_plr_sim$fit()
print(obj_dml_plr_sim)
