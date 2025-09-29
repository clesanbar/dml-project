#################################################
####### Replication code for "Motivating the Machine" Journal of Politics
####### This file includes replication code for all analyses in the SUPPLEMENTARY INFORMATION
###################################################

## R-Version ## ---------

## R version 4.1.0 (2021-05-18) -- "Camp Pontanezen"

##-------------------------------
## Packages used
##-------------------------------

rm(list=ls())

library(foreign)
library(tidyverse)
library(xtable)
library(ivpack)
library(AER)
library(car)
library(sandwich)
library(lmtest)
library(calibrate)
library(data.table)
library(apsrtable)
library(stargazer)
library(ggplot2)
library(reshape2)
library(MASS)
library(plm) #needed for first difference regs -- using version 2.2-3 (earlier versions take different arguments and won't replicate)
library(RCurl) # for clustered standard errors 

url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

### -----
##   LOAD IN DATA 
### -----

load("BrierleyNathan_payments_toupload.Rdata")

## Note: data file is labelled "d", with 1152 observations. 
dim(d)

######### 
### Variable creation 
######### 

d$branch_position_contest <- paste(d$anon.ps.name, d$ps2018_contest_position, sep="--")
d$branch_position_contest[is.na(d$ps2018_contest_position)==T] <- NA


d$paid_camp_big[is.na(d$paid_for_campaign)==F] <- 0
d$paid_camp_big[d$paid_for_camp_loan==1] <- 1
d$paid_camp_big[d$paid_for_camp_bicycle==1] <- 1
d$paid_camp_big[d$paid_for_camp_moto==1] <- 1

d$paid_for_camp_bike_moto[is.na(d$paid_for_campaign)==F] <- 0
d$paid_for_camp_bike_moto[d$paid_for_camp_bicycle==1] <- 1
d$paid_for_camp_bike_moto[d$paid_for_camp_moto ==1] <- 1

d$paid_camp_small[is.na(d$paid_for_campaign)==F] <- 0
d$paid_camp_small[d$paid_for_camp_food ==1] <- 1
d$paid_camp_small[d$paid_for_camp_money ==1] <- 1
d$paid_camp_small[d$paid_for_camp_cloth ==1] <- 1
d$paid_camp_small[d$paid_for_camp_credit ==1] <- 1
d$paid_camp_small[d$paid_for_camp_phone ==1] <- 1


######################################################################
### CODE FOLLOWS ORDER OF PRESENTATION IN THE SI 
######################################################################


####################
##Table OA.1: Individual-level correlates of attrition
####################

d$attrit <-ifelse(is.na(d$version.y==T), 1, 0)

table(d$attrit)
# 155 respondents attrited 

m1 <- lm(attrit ~ cpgn_brok_index +  newly_elected + age + female +  chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + big_pat_immed +  small_pat_immed + as.factor(anon.ps.name),  data=d)

m2 <- lm(attrit ~  cpgn_brok_index  +  newly_elected+ age + female +  chief_relative +  constexec_relative +  da_relative+ mpdce_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp   + years_comm   + big_pat_immed +small_pat_immed+      km_to_capital_wave1 +  wealth_index_2km.x +as.factor(constituency.x),  data=d)

stargazer(m1,m2, omit=c("anon.ps.name", "constituency.x"))




####################
##Table OA.2: Economic returns to being a branch leader: winners vs. losers 
####################


m1 <- lm(change_econ_retro ~ won_seat + big_pat_total + small_pat_total + as.factor(branch_position_contest), data=d[d$ps2018_contestant==1 & d$contestation_type %in% c("contested"),])

cluster_se1 <- as.vector(summary(m1,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m1a <- lm(change_econ_retro ~ won_seat + big_pat_total + small_pat_total + as.factor(anon.ps.name), data=d[d$ps2018_contestant==1,])

cluster_se1a <- as.vector(summary(m1a,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m2 <- lm(change_consumer_conf ~ won_seat  + big_pat_total + small_pat_total + as.factor(branch_position_contest), data=d[d$ps2018_contestant==1 & d$contestation_type %in% c("contested"),])

cluster_se2 <- as.vector(summary(m2,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m2a <- lm(change_consumer_conf ~ won_seat + big_pat_total + small_pat_total + as.factor(anon.ps.name), data=d[d$ps2018_contestant==1,])

cluster_se2a <- as.vector(summary(m2a,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m3 <- lm(change_total_assets ~ won_seat + big_pat_total + small_pat_total + as.factor(branch_position_contest), data=d[d$ps2018_contestant==1 & d$contestation_type %in% c("contested"),])

cluster_se3 <- as.vector(summary(m3,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m3a <- lm(change_total_assets ~ won_seat + big_pat_total + small_pat_total + as.factor(anon.ps.name), data=d[d$ps2018_contestant==1,])

cluster_se3a <- as.vector(summary(m3a,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

stargazer(m1, m1a, m2, m2a, m3, m3a,  
          se = list(cluster_se1, cluster_se1a, cluster_se2, cluster_se2a, cluster_se3, cluster_se3a),  omit=c("constituency.x", "anon.ps.name", "branch_position_contest", "age",  "pol_relative" , "chief_relative", 
  "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x"))



####################
##Table OA.3: Economic returns to payments and brokerage activity
####################


m1.controls <- lm(change_econ_retro ~ big_pat_total + small_pat_total + broker_up.y + broker_down.y  +  age + female + constexec_relative + 
 da_relative+mpdce_relative  + chief_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$won_seat==1 & is.na(d$won_seat)==F,])

cluster_se1 <- as.vector(summary(m1.controls,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m2.controls <- lm(change_consumer_conf ~ big_pat_total + small_pat_total + broker_up.y + broker_down.y  +  age + female + + constexec_relative +   da_relative+mpdce_relative  + chief_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$won_seat==1 & is.na(d$won_seat)==F,])

cluster_se2 <- as.vector(summary(m2.controls,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m3.controls <- lm(change_total_assets ~ big_pat_total + small_pat_total + broker_up.y + broker_down.y  +  age + female + constexec_relative +  da_relative+ mpdce_relative  + chief_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$won_seat==1 & is.na(d$won_seat)==F,])

cluster_se3 <- as.vector(summary(m3.controls, cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])


stargazer(m1.controls, m2.controls, m3.controls,    se = list(cluster_se1, cluster_se2, cluster_se3), omit=c("constituency.x", "anon.ps.name", "age",   "pol_relative" , "chief_relative", "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x", "female", "constexec_relative", "da_relative", "mpdce_relative"))



####################
##Figure OA.2: Distribution of connections up variable in each wave
####################


#mean connections in wave 1
summary(d$cxn_up_percentage_correct_full)
#mean connections in wave 1
summary(d$cxn_up_percentage_correct_full_w2)

#sd connections in wave 1
sd(d$cxn_up_percentage_correct_full)
#sd connections in wave 1
sd(d$cxn_up_percentage_correct_full_w2, na.rm=T)

# FIGURE 
x1 <- data.frame(v1=d$cxn_up_percentage_correct_full, v2=d$cxn_up_percentage_correct_full_w2)
data<- melt(x1)

ggplot(data,aes(x=value, fill=variable)) +
  geom_density(alpha=0.25) +scale_y_continuous(name = "Density") +
  scale_x_continuous(name = "Connections up") + theme_classic() + 
  scale_fill_grey(name = "", labels = c("Wave 1", "Wave 2")) 


####################
##Table OA.4: Summary of activities that branch leaders perform
####################


d_brok <- dplyr::select(d, cpgn_brok_index, #wave1
                        campaign_canvass,#wave1
                        campaign_orgrallies,#wave1
                        campaign_orgevents, #wave1
                        campaign_gifts, #wave1
                        campaign_orgtransport,#wave1
                        campaign_assistance, #wave1
                        campaign_workwchief,#wave1
                        campaign_drivevoters,#wave1
                        campaign_jobs,#wave1
                        broker_up,#wave1
                        helps_comm_w_party,#wave1
                        helps_comm_w_da,#wave1
                        broker_down,#wave1
                        helps_party_idcomm, #wave1
                        broker_up.y,#wave2
                        helps_comm_w_party.y,#wave2
                        helps_comm_w_da.y,#wave2
                        broker_down.y, #wave2
                        helps_party_idcomm.y #wave2 
)
stargazer(d_brok, iqr=F)



####################
##Table OA.5: Pairwise correlations among broker activities
####################


testd <- na.omit(d[,c("cpgn_brok_index", "broker_up", "broker_down", "broker_up.y", "broker_down.y")])
mat <- cor(testd)
xtable(mat)

####################
##Table OA.6: Campaign activity and campaign payment: 2016 election
####################

m1 <- lm(cpgn_brok_index ~ paid_for_campaign + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + pol_relative + chief_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + wealth_index_2km.x  + pres2012_ps_margin  + as.factor(constituency.x), data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])

cluster_se1 <- as.vector(summary(m1,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])


m2 <- lm(cpgn_brok_index ~ paid_camp_big + paid_camp_small + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + pol_relative + chief_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + wealth_index_2km.x  + pres2012_ps_margin  + as.factor(constituency.x), data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])

cluster_se2 <- as.vector(summary(m2,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])


m3 <- lm(cpgn_brok_index ~ paid_for_camp_money + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + pol_relative + chief_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + wealth_index_2km.x  + pres2012_ps_margin  + as.factor(constituency.x), data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])

cluster_se3 <- as.vector(summary(m3,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])


#make the table:
stargazer(m1, m2, m3,  se = list(cluster_se1, cluster_se2, cluster_se3), omit=c("constituency.x", "anon.ps.name", "age",   "pol_relative" , "chief_relative", "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x", "female", "constexec_relative", "da_relative", "mpdce_relative", " paid_camp_small"))


####################
##Table OA.7: Minor patronage payments immediately after the election
####################


m1 <- lm(small_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + constexec_relative +  da_relative+  mpdce_relative  + chief_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])

cluster_se1 <- as.vector(summary(m1,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m2 <- lm(small_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + constexec_relative +  da_relative+   mpdce_relative  + chief_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +swing_2012_2016_const, data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])

cluster_se2 <- as.vector(summary(m2,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m3 <- lm(small_pat_immed ~ npp_rawvotes + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + constexec_relative +   da_relative+ mpdce_relative  + chief_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x + as.factor(constituency.x) , data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])

cluster_se3 <- as.vector(summary(m3,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])


stargazer(m1, m2, m3,  se = list(cluster_se1, cluster_se2, cluster_se3),  dep.var.labels   = "Minor patronage (2017)", 
          omit=c("constituency.x", #"age", 
                 "pol_relative" , "chief_relative", 
                 "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x", "cxn_down_percentage_correct_full", "age", "female", "constexec_relative", "da_relative", "mpdce_relative "),
          add.lines = list(c("Constituency FEs", "Y", "N", "Y", "Y", "N", "Y"), c("Individual-level controls", "Y", "Y", "Y", "Y", "Y", "Y", "Y"), c("Polling station-level controls", "Y", "Y", "Y", "Y","Y","Y")))



####################
##Table OA.8: Predictors of minor patronage in the non-electoral period (2018-2019)
####################


s1 <- lm(small_pat_after ~  cxn_up_percentage_correct_full +  broker_up +   broker_down + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative +  constexec_relative + 
           da_relative+
           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_1 <- as.vector(summary(s1,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])


s2 <- lm(small_pat_after ~  cxn_up_politicians +  cxn_up_bureaucrats  + cxn_up_constexecs + broker_up +  broker_down + cpgn_brok_index +cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position + age + female +chief_relative + 
           constexec_relative + 
           da_relative+
           mpdce_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index +   years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_2 <- as.vector(summary(s2,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])



s3 <- lm(small_pat_after ~  cxn_up_percentage_correct_full + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative +  constexec_relative + 
                   da_relative+
                   mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_3  <- as.vector(summary(s3 ,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

s4 <- lm(small_pat_after ~  broker_up +   broker_down + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative + 
                   constexec_relative + 
                   da_relative+
                   mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_4  <- as.vector(summary(s4 ,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

s5 <- lm(small_pat_after ~ broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative + 
                           constexec_relative + 
                           da_relative+
                           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_5  <- as.vector(summary(s5 ,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

s6 <- lm(small_pat_after ~  cxn_up_percentage_correct_full + broker_up +   broker_down +  broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative + 
                      constexec_relative + 
                      da_relative+
                      mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_6 <- as.vector(summary(s6,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

stargazer(s1,s2,s3,s4,s5,s6, 
          se = list(cluster_1, cluster_2, cluster_3, cluster_4, cluster_5, cluster_6), 
          dep.var.labels   = "Minor patronage (2018-2019)", 
          omit=c("constituency.x", #"age", 
                 "pol_relative" , "chief_relative", 
                 "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x", "age", "female", "constexec_relative", "da_relative", "mpdce_relative"),
          add.lines = list(c("Constituency FEs", "Y", "Y", "Y", "Y"), c("Individual-level controls", "Y", "Y", "Y", "Y"), c("Polling station-level controls", "Y", "Y", "Y", "Y")), star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"))


####################
##Table OA.9: Logistic models (replication of Table 5: cols 1-3)
####################

#MAIN REGRESSION TABLE
m1_logit <- glm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + as.numeric(km_to_capital_wave1) + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,], family=binomial)
summary(m1_logit)
cluster_se1 <- as.vector(summary(m1_logit,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m2_logit <- glm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + as.numeric(km_to_capital_wave1) + wealth_index_2km.x +swing_2012_2016_const, data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,], family=binomial)
cluster_se2 <- as.vector(summary(m2_logit,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m3_logit<- glm(big_pat_immed ~ npp_rawvotes + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + as.numeric(km_to_capital_wave1) + wealth_index_2km.x + as.factor(constituency.x) , data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,], family=binomial)
cluster_se3 <- as.vector(summary(m3_logit,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])


stargazer(m1_logit, m2_logit, m3_logit, 
          se = list(cluster_se1, cluster_se2, cluster_se3), 
          dep.var.labels   = "Major patronage (2017)", 
          omit=c("constituency.x", #"age", 
                 "pol_relative" , "chief_relative", 
                 "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x"),
          add.lines = list(c("Constituency FEs", "Y", "N", "Y"), c("Individual-level controls", "Y", "Y", "Y"), c("Polling station-level controls", "Y", "Y", "Y")))



####################
##Table OA.10: Major patronage in Period 3 interacted with constituency competitiveness
####################


m1 <- lm(big_pat_after2 ~  cxn_up_percentage_correct_full +  broker_up +   broker_down + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +  constexec_relative + 
           da_relative+
           mpdce_relative + chief_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  urban.x + npp_const_2016.x + comp_const_2016.x, data=d[d$current_exec==1,])

cluster_se1 <- as.vector(summary(m1,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m1a <- lm(big_pat_after2 ~  (cxn_up_percentage_correct_full*npp_const_2016.x) + (cxn_up_percentage_correct_full*comp_const_2016.x) + cxn_up_percentage_correct_full +  broker_up +   broker_down + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +  constexec_relative + 
            da_relative+
            mpdce_relative + chief_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  urban.x + npp_const_2016.x + comp_const_2016.x, data=d[d$current_exec==1,])
   
cluster_se1a <- as.vector(summary(m1a,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m3 <- lm(big_pat_after2 ~  cxn_up_percentage_correct_full_w2 + broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +    old_branch_position + age + female +  constexec_relative + 
           da_relative+
           mpdce_relative + chief_relative + local_eth_minority  + lives_outside_ps + petty_trader + formal_sector +   as.factor(bio_educ_three) + asset_index +  years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  urban.x + npp_const_2016.x + comp_const_2016.x, data=d[d$current_exec==1,])

cluster_se3 <- as.vector(summary(m3,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m3a <- lm(big_pat_after2 ~  (cxn_up_percentage_correct_full_w2*npp_const_2016.x) + (cxn_up_percentage_correct_full_w2*comp_const_2016.x) + cxn_up_percentage_correct_full_w2 + broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +    old_branch_position + age + female +  constexec_relative + 
            da_relative+
            mpdce_relative + chief_relative + local_eth_minority  + lives_outside_ps + petty_trader + formal_sector +   as.factor(bio_educ_three) + asset_index +  years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  urban.x + npp_const_2016.x + comp_const_2016.x, data=d[d$current_exec==1,])
cluster_se3a <- as.vector(summary(m3a,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])


stargazer(m1, m1a,  m3, m3a, dep.var.labels   = "Major patronage (2018-2019)",
          se = list(cluster_se1, cluster_se1a, cluster_se3, cluster_se3a))




####################
##Table OA.11: Robustness check – OLS dropping outliers in vote swings
####################

quantile(d$npp12to16_ps_swing.NEW, c(.25,.5, .75), na.rm=T) 
#Interquartile range: 
#1.5 x IQR: 
# 1.5*(0.09141333 - 0.03080198) = 0.09091702

# #Median + outlier range
# 0.05536161 + 0.09091702 = 0.1462786
# 0.05536161 - 0.09091702 = -0.03555541

quantile(d$npp_rawvotes, c(.25, .5, .75), na.rm=T) 
# #Interquartile range: 
# 31--3 = 34
# #1.5 x IQR: 
# 51
# 10+51 = 61
# 10-51 = -41
d$outlier_12_16_swing <- ifelse(d$npp12to16_ps_swing.NEW>0.1462786 | d$npp12to16_ps_swing.NEW< (-0.03555541) , 1, 0)
table(d$outlier_12_16_swing)

d$outlier_12_16_raw <- ifelse(d$npp_rawvotes>61 | d$npp_rawvotes< -41, 1, 0)
table(d$outlier_12_16_raw )


#MAIN REGRESSION TABLE
m1 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female +  chief_relative +   constexec_relative +  da_relative+ mpdce_relative+ local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F & d$outlier_12_16_swing==0,])

cluster_se1 <- as.vector(summary(m1,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m2 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female +  chief_relative +  constexec_relative + da_relative+
           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +swing_2012_2016_const, data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F & d$outlier_12_16_swing==0,])
summary(m2)
cluster_se2 <- as.vector(summary(m2,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m3 <- lm(big_pat_immed ~ npp_rawvotes + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female +  chief_relative +  constexec_relative + da_relative+ mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x + as.factor(constituency.x) , data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F & d$outlier_12_16_raw==0,])
cluster_se3 <- as.vector(summary(m3,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

#COLLAPSE TO THE BRANCH LEVEL: 

cvars <- c("big_pat_immed" , "cpgn_brok_index" , "npp12to16_ps_swing.NEW" , "cxn_up_percentage_correct_full" , "cxn_down_percentage_correct_full", "age" , "female" , "chief_relative" , "constexec_relative",   "da_relative",   "mpdce_relative", "local_eth_minority" , "lives_outside_ps" , "petty_trader" , "formal_sector" , "bio_educ_three" , "asset_index" , "years_active_npp" , "years_comm" , "km_to_capital_wave1" , "wealth_index_2km.x"  , "urban.x", "npp_const_2016.x", "comp_const_2016.x", "npp_rawvotes", "swing_2012_2016_const")

bl.dat <-  as.data.frame(matrix(NA, nrow=length(unique(d$anon.ps.name)), ncol=length(cvars) + 2))
colnames(bl.dat) <- c("anon.ps.name", "constituency.x", cvars)
bl.dat$anon.ps.name <- unique(d$anon.ps.name)

for(i in 1:length(unique(d$anon.ps.name))){
  
  sub <- d[d$anon.ps.name ==unique(d$anon.ps.name)[i], ]
  bl.dat$constituency.x[i] <- sub$constituency.x[1] 
  
  for(j in 1:length(cvars)){
    bl.dat[i,j+2] <- mean(sub[sub$old_branch_position==1 & is.na(sub$old_branch_position)==F, cvars[j]], na.rm=T)
    
  }
  
}

bl.dat$outlier_12_16_swing <- ifelse(bl.dat$npp12to16_ps_swing.NEW>0.1462786 | bl.dat$npp12to16_ps_swing.NEW< (-0.03555541) , 1, 0)
table(bl.dat$outlier_12_16_swing)

bl.dat$outlier_12_16_raw <- ifelse(bl.dat$npp_rawvotes>61 |  bl.dat$npp_rawvotes< -41, 1, 0)
table(bl.dat$outlier_12_16_raw )


m4 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative + constexec_relative +  da_relative+
           mpdce_relative+  local_eth_minority + lives_outside_ps + petty_trader + formal_sector + bio_educ_three + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=bl.dat[bl.dat$outlier_12_16_swing==0,])

#NO CLUSTERED ERRORS BECAUSE THE UNIT IS THE POLLING STATION ALREADY AND WE HAVE CONST FEs

m5 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female +    chief_relative +  constexec_relative +  da_relative+
           mpdce_relative+ local_eth_minority + lives_outside_ps + petty_trader + formal_sector + bio_educ_three + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x + swing_2012_2016_const , data=bl.dat[bl.dat$outlier_12_16_swing==0,])

m6 <- lm(big_pat_immed ~ npp_rawvotes + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female +   chief_relative +  constexec_relative +  da_relative+ mpdce_relative+
           local_eth_minority + lives_outside_ps + petty_trader + formal_sector + bio_educ_three + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=bl.dat[bl.dat$outlier_12_16_raw==0,])

#### TABLE IN PAPER 

stargazer(m1, m2, m3, m4,m5,m6, 
          se = list(cluster_se1, cluster_se2, cluster_se3), 
          dep.var.labels   = "Major patronage (2017)", 
          omit=c("constituency.x", #"age", 
                 "pol_relative" , "chief_relative", 
                 "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x"),
          add.lines = list(c("Constituency FEs", "Y", "N", "Y", "Y", "N", "Y"), c("Individual-level controls", "Y", "Y", "Y", "Y", "Y", "Y", "Y"), c("Polling station-level controls", "Y", "Y", "Y", "Y","Y","Y")))


####################
##Table OA.12: Table 6: Logistic regression
####################


m1_logit <- glm(big_pat_after2 ~  
                 cxn_up_percentage_correct_full +  
                 broker_up +   
                 broker_down + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative + 
           constexec_relative + 
           da_relative+
           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,], family = "binomial")

cluster_se1_logit <- as.vector(summary(m1_logit,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])
#https://economictheoryblog.com/2016/12/13/clustered-standard-errors-in-r/


m2_logit <- glm(big_pat_after2 ~  cxn_up_politicians +  cxn_up_bureaucrats  + cxn_up_constexecs + broker_up +  broker_down + cpgn_brok_index +cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position + age + female +chief_relative + 
           constexec_relative + 
           da_relative+
           mpdce_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index +   years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,], family = "binomial")
cluster_se2_logit <- as.vector(summary(m2_logit,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m1.cxnonly_logit <- glm(big_pat_after2 ~  cxn_up_percentage_correct_full + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative + 
                   constexec_relative + 
                   da_relative+
                   mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,], family="binomial")
cluster_se1.m1.cxnonly_logit <- as.vector(summary(m1.cxnonly_logit ,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m1.brkonly_logit <- glm(big_pat_after2 ~  broker_up +   broker_down + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative + 
                   constexec_relative + 
                   da_relative+
                   mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,], family="binomial")
cluster_se1.m1.brkonly_logit  <- as.vector(summary(m1.brkonly_logit ,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m1.brkonly.current_logit <- glm(big_pat_after2 ~ broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative + 
                           constexec_relative + 
                           da_relative+
                           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,], family="binomial")
cluster_se1.m1.brkonly.current_logit  <- as.vector(summary(m1.brkonly.current_logit ,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])
 
m1.brkcurrent_logit<- glm(big_pat_after2 ~  cxn_up_percentage_correct_full + broker_up +   broker_down +  broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative + 
                      constexec_relative + 
                      da_relative+
                      mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,], family="binomial")
cluster_se1.m1.brkcurrent_logit <- as.vector(summary(m1.brkcurrent_logit,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

stargazer(m1_logit, m2_logit,  m1.cxnonly_logit, m1.brkonly_logit,  m1.brkonly.current_logit, m1.brkcurrent_logit, 
          se = list(cluster_se1_logit, cluster_se2_logit, cluster_se1.m1.cxnonly_logit, cluster_se1.m1.brkonly_logit, cluster_se1.m1.brkonly.current_logit, cluster_se1.m1.brkcurrent_logit), 
          dep.var.labels   = "Major patronage (2018-2019)", 
          omit=c("constituency.x", #"age", 
                 "pol_relative" , "chief_relative", 
                 "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x"),
          add.lines = list(c("Constituency FEs", "Y", "Y", "Y", "Y"), c("Individual-level controls", "Y", "Y", "Y", "Y"), c("Polling station-level controls", "Y", "Y", "Y", "Y")), star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"))




####################
##Table OA.13: Major patronage (2018-2019): Disaggregated by jobs, loans, training
####################


m1.job <- lm(job_after ~  cxn_up_percentage_correct_full +  broker_up +   broker_down + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +  chief_relative +  constexec_relative +  da_relative+ mpdce_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_m1.job <- as.vector(summary(m1.job,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

### SAME IS TRUE IF WE LOOK AT THEIR CURRENT CNX UP 
m2.job <- lm(job_after ~  cxn_up_percentage_correct_full_w2 + broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +    old_branch_position + age + female +  chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority  + lives_outside_ps + petty_trader + formal_sector +   as.factor(bio_educ_three) + asset_index +  years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_m2.job <- as.vector(summary(m2.job,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

#### LOANS ##### APPENDIX 

m1.loan <- lm(loan_after ~  cxn_up_percentage_correct_full +  broker_up +   broker_down + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +  chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_m1.loan <- as.vector(summary(m1.loan,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

### SAME IS TRUE IF WE LOOK AT THEIR CURRENT CNX UP 
m2.loan <- lm(loan_after ~  cxn_up_percentage_correct_full_w2 + broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +    old_branch_position + age + female +  chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority  + lives_outside_ps + petty_trader + formal_sector +   as.factor(bio_educ_three) + asset_index +  years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_m2.loan <- as.vector(summary(m2.loan,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

#### TRAINING ##### APPENDIX 

m1.train <- lm(train_after ~  cxn_up_percentage_correct_full +  broker_up +   broker_down + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +  chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_m1.train <- as.vector(summary(m1.train,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

### SAME IS TRUE IF WE LOOK AT THEIR CURRENT CNX UP 
m2.train <- lm(train_after ~  cxn_up_percentage_correct_full_w2 + broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +    old_branch_position + age + female +  chief_relative +  constexec_relative +  da_relative+ mpdce_relative++ local_eth_minority  + lives_outside_ps + petty_trader + formal_sector +   as.factor(bio_educ_three) + asset_index +  years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_m2.train <- as.vector(summary(m2.train,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

### Table

stargazer(m1.job, m2.job, m1.loan, m2.loan, m1.train, m2.train,  se = list(cluster_m1.job,cluster_m2.job,cluster_m1.loan,cluster_m2.loan,cluster_m1.train, cluster_m2.train), 
          omit=c("constituency.x", #"age", 
                 "pol_relative" , "chief_relative", 
                 "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x"),
          add.lines = list(c("Constituency FEs", "Y", "Y","Y", "Y","Y", "Y"), c("Indiv. controls", "Y", "Y", "Y", "Y","Y", "Y"), c("PS. controls", "Y", "Y","Y", "Y","Y", "Y")))


####################
##Table OA.14: Placebo test: past payments and future connections
####################

d$change_cnx_up ~ big_pat_immed

summary(d$change_cnx_up)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.3600  0.0000  0.0400  0.0605  0.1200  0.4800     155 

m1.noC <- lm(change_cnx_up ~  big_pat_immed  +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_se1.noC <- as.vector(summary(m1.noC,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])
#big_pat_immed                               0.012806   0.014194   0.902   0.3672    


m1 <- lm(change_cnx_up ~  big_pat_immed + npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative + 
           constexec_relative + 
           da_relative+
           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_se1 <- as.vector(summary(m1,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])
#big_pat_immed                               1.645e-02  1.442e-02   1.141  0.25425    

### MAKE A TABLE OF THIS ###
stargazer(m1.noC, m1, 
          se = list(cluster_se1.noC, cluster_se1), star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"))


####################
##Table OA.15: Broker activity on connections up among sample from Table 6
####################


m1 <- lm(broker_up ~  cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + npp12to16_ps_swing.NEW + old_branch_position +  age + female +   chief_relative + 
           constexec_relative + 
           da_relative+
           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1 & is.na(d$big_pat_after2)==F,])
cluster_se1 <- as.vector(summary(m1,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m2 <- lm(broker_down ~  cxn_up_percentage_correct_full + cxn_down_percentage_correct_full  +npp12to16_ps_swing.NEW + old_branch_position +  age + female +   chief_relative + 
           constexec_relative + 
           da_relative+
           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1 & is.na(d$big_pat_after2)==F,])
cluster_se2 <- as.vector(summary(m2,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m3 <- lm(broker_up.y ~  cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + npp12to16_ps_swing.NEW + old_branch_position +  age + female +   chief_relative + 
           constexec_relative + 
           da_relative+
           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1 & is.na(d$big_pat_after2)==F,])
cluster_se3 <- as.vector(summary(m3,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m4 <- lm(broker_down.y ~  cxn_up_percentage_correct_full + cxn_down_percentage_correct_full  + npp12to16_ps_swing.NEW  + old_branch_position +  age + female +   chief_relative + 
           constexec_relative + 
           da_relative+
           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1 & is.na(d$big_pat_after2)==F,])
cluster_se4 <- as.vector(summary(m4,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

stargazer(m1,  m2,m3, m4,
          se = list(cluster_se1,  cluster_se2, cluster_se3, cluster_se4), star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"))


####################
##Table OA.16: Interaction between connections up and current brokerage activity
####################


m1.brkcurrentA <- lm(big_pat_after2 ~  (cxn_up_percentage_correct_full * broker_up.y) + cxn_up_percentage_correct_full + broker_up +   broker_down +  broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative + 
           constexec_relative + 
           da_relative+
           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_se1.m1.brkcurrentA <- as.vector(summary(m1.brkcurrentA,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m1.brkcurrentB <- lm(big_pat_after2 ~  (cxn_up_percentage_correct_full * broker_down.y) + cxn_up_percentage_correct_full + broker_up +   broker_down +  broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative + 
           constexec_relative + 
           da_relative+
           mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_se1.m1.brkcurrentB <- as.vector(summary(m1.brkcurrentB,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

stargazer(m1.brkcurrentA, m1.brkcurrentB ,
          se = list(cluster_se1.m1.brkcurrentA, cluster_se1.m1.brkcurrentB), star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"))





####################
##Table OA.17: Table 5: coefficient for distance from polling station to district capital
####################


m1 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])
cluster_se1 <- as.vector(summary(m1,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m2 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +swing_2012_2016_const, data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])
summary(m2)
cluster_se2 <- as.vector(summary(m2,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m3 <- lm(big_pat_immed ~ npp_rawvotes + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x + as.factor(constituency.x) , data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])
cluster_se3 <- as.vector(summary(m3,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

#COLLAPSE TO THE BRANCH LEVEL: 

cvars <- c("big_pat_immed" , "cpgn_brok_index" , "npp12to16_ps_swing.NEW" , "cxn_up_percentage_correct_full" , "cxn_down_percentage_correct_full", "age" , "female" ,  "chief_relative" ,
           "constexec_relative" ,  "da_relative",  "mpdce_relative", "local_eth_minority" , "lives_outside_ps" , "petty_trader" , "formal_sector" , "bio_educ_three" , "asset_index" , "years_active_npp" , "years_comm" , "km_to_capital_wave1" , "wealth_index_2km.x"  , "urban.x", "npp_const_2016.x", "comp_const_2016.x", "npp_rawvotes", "swing_2012_2016_const")

bl.dat <-  as.data.frame(matrix(NA, nrow=length(unique(d$anon.ps.name)), ncol=length(cvars) + 2))
colnames(bl.dat) <- c("anon.ps.name", "constituency.x", cvars)
bl.dat$anon.ps.name <- unique(d$anon.ps.name)

for(i in 1:length(unique(d$anon.ps.name))){
  
  sub <- d[d$anon.ps.name ==unique(d$anon.ps.name)[i], ]
  bl.dat$constituency.x[i] <- sub$constituency.x[1] 
  
  for(j in 1:length(cvars)){
    bl.dat[i,j+2] <- mean(sub[sub$old_branch_position==1 & is.na(sub$old_branch_position)==F, cvars[j]], na.rm=T)
    
  }
  
}

m4 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative +  constexec_relative +  da_relative+ mpdce_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + bio_educ_three + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=bl.dat)

#NO CLUSTERED ERRORS BECAUSE THE UNIT IS THE POLLING STATION ALREADY AND WE HAVE CONST FEs

m5 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + bio_educ_three + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x + swing_2012_2016_const , data=bl.dat)

m6 <- lm(big_pat_immed ~ npp_rawvotes + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative +  constexec_relative +  da_relative+ mpdce_relative+ + local_eth_minority + lives_outside_ps + petty_trader + formal_sector + bio_educ_three + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=bl.dat)

#### TABLE IN PAPER 

stargazer(m1, m2, m3, m4,m5,m6, 
          se = list(cluster_se1, cluster_se2, cluster_se3), 
          dep.var.labels   = "Major patronage (2017)", 
          omit=c("npp_rawvotes", "cpgn_brok_index",  "cxn_up_percentage_correct_full",  "cxn_down_percentage_correct_full", "npp12to16_ps_swing.NEW", "constexec_relative", "da_relative", "mpdce_relative",
                 "constituency.x", "age", "female", "swing_2012_2016_const",  "pol_relative" , "chief_relative",  "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm",  "wealth_index_2km.x"),
          add.lines = list(c("Constituency FEs", "Y", "N", "Y", "Y", "N", "Y"), c("Individual-level controls", "Y", "Y", "Y", "Y", "Y", "Y", "Y"), c("Polling station-level controls", "Y", "Y", "Y", "Y","Y","Y")))


####################
##Table OA.18: Table 6: coefficient for distance from polling station to district capital
####################


m1 <- lm(big_pat_after2 ~  cxn_up_percentage_correct_full +  broker_up +   broker_down + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative +   constexec_relative + da_relative+   mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_se1 <- as.vector(summary(m1,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m2 <- lm(big_pat_after2 ~  cxn_up_politicians +  cxn_up_bureaucrats  + cxn_up_constexecs + broker_up +  broker_down + cpgn_brok_index +cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position + age + female +chief_relative + constexec_relative +  da_relative+ mpdce_relative + local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index +   years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_se2 <- as.vector(summary(m2,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m1.cxnonly <- lm(big_pat_after2 ~  cxn_up_percentage_correct_full + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative +   constexec_relative +  da_relative+   mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_se1.m1.cxnonly  <- as.vector(summary(m1.cxnonly ,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m1.brkonly <- lm(big_pat_after2 ~  broker_up +   broker_down + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative +   constexec_relative +  da_relative+  mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_se1.m1.brkonly  <- as.vector(summary(m1.brkonly ,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m1.brkonly.current <- lm(big_pat_after2 ~ broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative +  constexec_relative +   da_relative+  mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_se1.m1.brkonly.current  <- as.vector(summary(m1.brkonly.current ,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m1.brkcurrent <- lm(big_pat_after2 ~  cxn_up_percentage_correct_full + broker_up +   broker_down +  broker_up.y +broker_down.y + cpgn_brok_index + cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW +  old_branch_position +  age + female +   chief_relative +  constexec_relative +  da_relative+  mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_se1.m1.brkcurrent <- as.vector(summary(m1.brkcurrent,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

# Table 

stargazer(m1, m2,  m1.cxnonly, m1.brkonly,m1.brkonly.current, m1.brkcurrent,  se = list(cluster_se1, cluster_se2, cluster_se1.m1.cxnonly, cluster_se1.m1.brkonly, cluster_se1.m1.brkonly.current, cluster_se1.m1.brkcurrent), 
          dep.var.labels   = "Major patronage (2018-2019)", omit=c("constituency.x", "age",   "pol_relative" , "chief_relative", "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm",  "wealth_index_2km.x", "cxn_up_percentage_correct_full", "broker_up","broker_down","broker_up.y", "broker_down.y", "cpgn_brok_index",  "cxn_down_percentage_correct_full", "npp12to16_ps_swing.NEW", "old_branch_position" , "cxn_up_politicians",  "cxn_up_bureaucrats",   "cxn_up_constexecs",  "female",  "constexec_relative",  "da_relative",  "mpdce_relative"), add.lines = list(c("Constituency FEs", "Y", "Y", "Y", "Y"), c("Individual-level controls", "Y", "Y", "Y", "Y"), c("Polling station-level controls", "Y", "Y", "Y", "Y")), star.char = c("+", "*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"))

