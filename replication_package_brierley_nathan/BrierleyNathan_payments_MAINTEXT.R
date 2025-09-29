#################################################
####### Replication code for "Motivating the Machine" Journal of Politics
####### This file includes replication code for all analyses in the MAIN TEXT
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

load("Brierley and Nathan replication package/BrierleyNathan_payments_toupload.Rdata")

## Note: data file is labelled "d", with 1152 observations. 
dim(d)

######### 
### Variable creation 
######### 

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
### CODE FOLLOWS ORDER OF PRESENTATION IN MAIN TEXT
######################################################################
 
############################################
##Table 1: What compensation do brokers hope to receive?
################################################

#CATEGORY 1 == accomodation
d$expects.accomodation[is.na(d$expected_personal_category1.x)==F|is.na(d$expected_personal_category1.y)==F] <- 0
d$expects.accomodation[d$expected_personal_category1.x=="accomodation"|d$expected_personal_category1.y=="accomodation"| d$expected_personal_category2.x=="accomodation"|d$expected_personal_category2.y=="accomodation"| d$expected_personal_category3.x=="accomodation"|d$expected_personal_category3.y=="accomodation"| d$expected_personal_category4.x=="accomodation"|d$expected_personal_category4.y=="accomodation"] <- 1
d$expects.accomodation[d$expected_personal_category1.x=="building materials"|d$expected_personal_category1.y=="building materials"| d$expected_personal_category2.x=="building materials"|d$expected_personal_category2.y=="building materials"| d$expected_personal_category3.x=="building materials"|d$expected_personal_category3.y=="building materials"|  d$expected_personal_category4.x=="building materials"|d$expected_personal_category4.y=="building materials"] <- 1

#CATEGORY 2 == bur favors
d$expects.bur.favors[is.na(d$expected_personal_category1.x)==F|is.na(d$expected_personal_category1.y)==F] <- 0
d$expects.bur.favors[d$expected_personal_category1.x=="electricity"|d$expected_personal_category1.y=="electricity"| d$expected_personal_category2.x=="electricity"|d$expected_personal_category2.y=="electricity"|d$expected_personal_category3.x=="electricity"|d$expected_personal_category3.y=="electricity"|d$expected_personal_category4.x=="electricity"|d$expected_personal_category4.y=="electricity"] <- 1
d$expects.bur.favors[d$expected_personal_category1.x=="license"|d$expected_personal_category1.y=="license"| d$expected_personal_category2.x=="license"|d$expected_personal_category2.y=="license"| d$expected_personal_category3.x=="license"|d$expected_personal_category3.y=="license"| d$expected_personal_category4.x=="license"|d$expected_personal_category4.y=="license"] <- 1
d$expects.bur.favors[d$expected_personal_category1.x=="health insurance"|d$expected_personal_category1.y=="health insurance"| d$expected_personal_category2.x=="health insurance"|d$expected_personal_category2.y=="health insurance"|  d$expected_personal_category3.x=="health insurance"|d$expected_personal_category3.y=="health insurance"| d$expected_personal_category4.x=="health insurance"|d$expected_personal_category4.y=="health insurance"] <- 1
d$expects.bur.favors[d$expected_personal_category1.x=="school admissions"|d$expected_personal_category1.y=="school admissions"| d$expected_personal_category2.x=="school admissions"|d$expected_personal_category2.y=="school admissions"| d$expected_personal_category3.x=="school admissions"|d$expected_personal_category3.y=="school admissions"| d$expected_personal_category4.x=="school admissions"|d$expected_personal_category4.y=="school admissions"] <- 1

d$expects.fees[is.na(d$expected_personal_category1.x)==F|is.na(d$expected_personal_category1.y)==F] <- 0
d$expects.fees[d$expected_personal_category1.x=="school fees"|d$expected_personal_category1.y=="school fees"|d$expected_personal_category2.x=="school fees"|d$expected_personal_category2.y=="school fees"|d$expected_personal_category3.x=="school fees"|d$expected_personal_category3.y=="school fees"|d$expected_personal_category4.x=="school fees"|d$expected_personal_category4.y=="school fees"] <- 1
d$expects.fees[d$expected_personal_category1.x=="health bills"|d$expected_personal_category1.y=="health bills"| d$expected_personal_category2.x=="health bills"|d$expected_personal_category2.y=="health bills"| d$expected_personal_category3.x=="health bills"|d$expected_personal_category3.y=="health bills"| d$expected_personal_category4.x=="health bills"|d$expected_personal_category4.y=="health bills"] <- 1

d$expects.cash[is.na(d$expected_personal_category1.x)==F|is.na(d$expected_personal_category1.y)==F] <- 0
d$expects.cash[d$expected_personal_category1.x=="cash"|d$expected_personal_category1.y=="cash"| d$expected_personal_category2.x=="cash"|d$expected_personal_category2.y=="cash"|d$expected_personal_category3.x=="cash"|d$expected_personal_category3.y=="cash"|d$expected_personal_category4.x=="cash"|d$expected_personal_category4.y=="cash"] <- 1

d$expects.inputs[is.na(d$expected_personal_category1.x)==F|is.na(d$expected_personal_category1.y)==F] <- 0
d$expects.inputs[d$expected_personal_category1.x=="inputs"|d$expected_personal_category1.y=="inputs"|d$expected_personal_category2.x=="inputs"|d$expected_personal_category2.y=="inputs"|d$expected_personal_category3.x=="inputs"|d$expected_personal_category3.y=="inputs"|d$expected_personal_category4.x=="inputs"|d$expected_personal_category4.y=="inputs"] <- 1
d$expects.inputs[d$expected_personal_category1.x=="business help"|d$expected_personal_category1.y=="business help"|  d$expected_personal_category2.x=="business help"|d$expected_personal_category2.y=="business help"| d$expected_personal_category3.x=="business help"|d$expected_personal_category3.y=="business help"| d$expected_personal_category4.x=="business help"|d$expected_personal_category4.y=="business help"] <- 1

d$expects.job[is.na(d$expected_personal_category1.x)==F|is.na(d$expected_personal_category1.y)==F] <- 0
d$expects.job[d$expected_personal_category1.x=="job"|d$expected_personal_category1.y=="job"| d$expected_personal_category2.x=="job"|d$expected_personal_category2.y=="job"| d$expected_personal_category3.x=="job"|d$expected_personal_category3.y=="job"|  d$expected_personal_category4.x=="job"|d$expected_personal_category4.y=="job"] <- 1
d$expects.job[d$expected_personal_category1.x=="job "|d$expected_personal_category1.y=="job "| d$expected_personal_category2.x=="job "|d$expected_personal_category2.y=="job "| d$expected_personal_category3.x=="job "|d$expected_personal_category3.y=="job "| d$expected_personal_category4.x=="job "|d$expected_personal_category4.y=="job "] <- 1
d$expects.job[d$expected_personal_category1.x=="contracts"|d$expected_personal_category1.y=="contracts"| d$expected_personal_category2.x=="contracts"|d$expected_personal_category2.y=="contracts"|d$expected_personal_category3.x=="contracts"|d$expected_personal_category3.y=="contracts"| d$expected_personal_category4.x=="contracts"|d$expected_personal_category4.y=="contracts"] <- 1

d$expects.job.family[is.na(d$expected_personal_category1.x)==F|is.na(d$expected_personal_category1.y)==F] <- 0
d$expects.job.family[d$expected_personal_category1.x=="job for family"|d$expected_personal_category1.y=="job for family"| d$expected_personal_category2.x=="job for family"|d$expected_personal_category2.y=="job for family"| d$expected_personal_category3.x=="job for family"|d$expected_personal_category3.y=="job for family"| d$expected_personal_category4.x=="job for family"|d$expected_personal_category4.y=="job for family"] <- 1

d$expects.loan[is.na(d$expected_personal_category1.x)==F|is.na(d$expected_personal_category1.y)==F] <- 0
d$expects.loan[d$expected_personal_category1.x=="loan"|d$expected_personal_category1.y=="loan"|d$expected_personal_category2.x=="loan"|d$expected_personal_category2.y=="loan"|d$expected_personal_category3.x=="loan"|d$expected_personal_category3.y=="loan"|d$expected_personal_category4.x=="loan"|d$expected_personal_category4.y=="loan"] <- 1

d$expects.moto[is.na(d$expected_personal_category1.x)==F|is.na(d$expected_personal_category1.y)==F] <- 0
d$expects.moto[d$expected_personal_category1.x=="motorvehicles (car, moto)"|d$expected_personal_category1.y=="motorvehicles (car, moto)"| d$expected_personal_category2.x=="motorvehicles (car, moto)"|d$expected_personal_category2.y=="motorvehicles (car, moto)"| d$expected_personal_category3.x=="motorvehicles (car, moto)"|d$expected_personal_category3.y=="motorvehicles (car, moto)"|  d$expected_personal_category4.x=="motorvehicles (car, moto)"|d$expected_personal_category4.y=="motorvehicles (car, moto)"] <- 1
d$expects.moto[d$expected_personal_category1.x=="motorvehicle (car, moto)"|d$expected_personal_category1.y=="motorvehicle (car, moto)"|  d$expected_personal_category2.x=="motorvehicle (car, moto)"|d$expected_personal_category2.y=="motorvehicle (car, moto)"|d$expected_personal_category3.x=="motorvehicle (car, moto)"|d$expected_personal_category3.y=="motorvehicle (car, moto)"| d$expected_personal_category4.x=="motorvehicle (car, moto)"|d$expected_personal_category4.y=="motorvehicle (car, moto)"] <- 1

d$expects.other[is.na(d$expected_personal_category1.x)==F|is.na(d$expected_personal_category1.y)==F] <- 0
d$expects.other[d$expected_personal_category1.x=="electronics"|d$expected_personal_category1.y=="electronics"| d$expected_personal_category2.x=="electronics"|d$expected_personal_category2.y=="electronics"| d$expected_personal_category3.x=="electronics"|d$expected_personal_category3.y=="electronics"| d$expected_personal_category4.x=="electronics"|d$expected_personal_category4.y=="electronics"] <- 1
d$expects.other[d$expected_personal_category1.x=="higher salary"|d$expected_personal_category1.y=="higher salary"| d$expected_personal_category2.x=="higher salary"|d$expected_personal_category2.y=="higher salary"|d$expected_personal_category3.x=="higher salary"|d$expected_personal_category3.y=="higher salary"| d$expected_personal_category4.x=="higher salary"|d$expected_personal_category4.y=="higher salary"] <- 1
d$expects.other[d$expected_personal_category1.x=="transfer"|d$expected_personal_category1.y=="transfer"| d$expected_personal_category2.x=="transfer"|d$expected_personal_category2.y=="transfer"|  d$expected_personal_category3.x=="transfer"|d$expected_personal_category3.y=="transfer"| d$expected_personal_category4.x=="transfer"|d$expected_personal_category4.y=="transfer"] <- 1
d$expects.other[d$expected_personal_category1.x=="training"|d$expected_personal_category1.y=="training"| d$expected_personal_category2.x=="training"|d$expected_personal_category2.y=="training"| d$expected_personal_category3.x=="training"|d$expected_personal_category3.y=="training"| d$expected_personal_category4.x=="training"|d$expected_personal_category4.y=="training"] <- 1
d$expects.other[d$expected_personal_category1.x=="benefits"|d$expected_personal_category1.y=="benefits"| d$expected_personal_category2.x=="benefits"|d$expected_personal_category2.y=="benefits"| d$expected_personal_category3.x=="benefits"|d$expected_personal_category3.y=="benefits"| d$expected_personal_category4.x=="benefits"|d$expected_personal_category4.y=="benefits"] <- 1


tab.mat <- as.data.frame(matrix(NA, nrow=10, ncol=3))
colnames(tab.mat) <- c("benefit", "perc", "n")

tab.mat[,1] <- c("Housing", "Bureaucratic favors", "Assistance w/ fees", "Cash", "Business inputs", "A job", "A job for family member", "A loan", "Motorbike", "Other")
tab.mat[,2] <- c(mean(d$expects.accomodation, na.rm=T),  mean(d$expects.bur.favors, na.rm=T), mean(d$expects.fees, na.rm=T),mean(d$expects.cash, na.rm=T),mean(d$expects.inputs, na.rm=T),mean(d$expects.job, na.rm=T),mean(d$expects.job.family, na.rm=T),mean(d$expects.loan, na.rm=T),mean(d$expects.moto, na.rm=T),mean(d$expects.other, na.rm=T))
tab.mat[,3] <- c(table(d$expects.accomodation)[2], table(d$expects.bur.favors)[2], table(d$expects.fees)[2], table(d$expects.cash)[2], table(d$expects.inputs)[2], table(d$expects.job)[2], table(d$expects.job.family)[2], table(d$expects.loan)[2], table(d$expects.moto)[2], table(d$expects.other)[2])

tab.mat2 <- tab.mat[order(tab.mat$perc, decreasing=T), ]
tab.mat2[,2] <- round(tab.mat2[,2]*100, digits=2) 
tab.mat2[,2] <- paste(tab.mat2[,2], " (", tab.mat2[,3], ")", sep="")
tab.mat2[,1:2]

xtable(tab.mat2[,1:2])



####################
##Table 2: Broker payments across the electoral cycle
####################


tab1 <- as.data.frame(matrix(NA, nrow=13, ncol=7))
colnames(tab1) <- c("Variable", "Percent (%)", "N", "Percent (%)", "N", "Percent (%)", "N")

#FIRST WAVE: during the 2016 campaign season

tab1[1,1] <- "Paid major patronage"
tab1[1,2] <- paste(round(mean(d$paid_camp_big[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[1,3] <- length(na.omit(d$paid_camp_big[d$old_branch_position==1]))

tab1[1,4] <- paste(round(mean(d$big_pat_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[1,5] <- length(na.omit(d$big_pat_immed[d$old_branch_position==1]))

tab1[1,6] <- paste(round(mean(d$big_pat_after2[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[1,7] <- length(na.omit(d$big_pat_after2[d$old_branch_position==1]))


tab1[2,1] <- "Given a job"
tab1[2,2] <- 0
tab1[2,3] <- length(na.omit(d$paid_camp_big[d$old_branch_position==1]))

tab1[2,4] <- paste(round(mean(d$job_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[2,5] <- length(na.omit(d$job_immed[d$old_branch_position==1]))

tab1[2,6] <- paste(round(mean(d$job_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[2,7] <- length(na.omit(d$job_after[d$old_branch_position==1]))


tab1[3,1] <- "Given a micro-finance loan"
tab1[3,2] <- paste(round(mean(d$paid_for_camp_loan[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[3,3] <- length(na.omit(d$paid_for_camp_loan[d$old_branch_position==1]))

tab1[3,4] <- paste(round(mean(d$loan_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[3,5] <- length(na.omit(d$loan_immed[d$old_branch_position==1]))

tab1[3,6] <- paste(round(mean(d$loan_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[3,7] <- length(na.omit(d$loan_after[d$old_branch_position==1]))

tab1[4,1] <- "Enrolled in training program"
tab1[4,2] <- 0
tab1[4,3] <- length(na.omit(d$paid_camp_big[d$old_branch_position==1]))

tab1[4,4] <- paste(round(mean(d$train_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[4,5] <- length(na.omit(d$train_immed[d$old_branch_position==1]))

tab1[4,6] <- paste(round(mean(d$train_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[4,7] <- length(na.omit(d$train_after[d$old_branch_position==1]))

tab1[5,1] <- "Given motorbike or bicycle"
tab1[5,2] <- paste(round(mean(d$paid_for_camp_bike_moto[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[5,3] <- length(na.omit(d$paid_for_camp_bike_moto[d$old_branch_position==1]))

tab1[5,4] <- paste(round(mean(d$bike_moto_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[5,5] <- length(na.omit(d$bike_moto_immed[d$old_branch_position==1]))

tab1[5,6] <- paste(round(mean(d$bike_moto_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[5,7] <- length(na.omit(d$bike_moto_after[d$old_branch_position==1]))

tab1[6,1] <- "Paid minor benefits"
tab1[6,2] <- paste(round(mean(d$paid_camp_small[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[6,3] <- length(na.omit(d$paid_camp_small[d$old_branch_position==1]))

tab1[6,4] <- paste(round(mean(d$small_pat_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[6,5] <- length(na.omit(d$small_pat_immed[d$old_branch_position==1]))

tab1[6,6] <- paste(round(mean(d$small_pat_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[6,7] <- length(na.omit(d$small_pat_after[d$old_branch_position==1]))

tab1[7,1] <- "Paid petty cash"
tab1[7,2] <- paste(round(mean(d$paid_for_camp_money[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[7,3] <- length(na.omit(d$paid_for_camp_money[d$old_branch_position==1]))

tab1[7,4] <- paste(round(mean(d$money_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[7,5] <- length(na.omit(d$money_immed[d$old_branch_position==1]))

tab1[7,6] <- paste(round(mean(d$money_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[7,7] <- length(na.omit(d$money_after[d$old_branch_position==1]))

tab1[8,1] <- "Paid in food"
tab1[8,2] <- paste(round(mean(d$paid_for_camp_food[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[8,3] <- length(na.omit(d$paid_for_camp_food[d$old_branch_position==1]))

tab1[8,4] <- paste(round(mean(d$food_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[8,5] <- length(na.omit(d$food_immed[d$old_branch_position==1]))

tab1[8,6] <- paste(round(mean(d$food_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[8,7] <- length(na.omit(d$food_after[d$old_branch_position==1]))

tab1[9,1] <- "Given clothing or cloth"
tab1[9,2] <- paste(round(mean(d$paid_for_camp_cloth[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[9,3] <- length(na.omit(d$paid_for_camp_cloth[d$old_branch_position==1]))

tab1[9,4] <- paste(round(mean(d$cloth_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[9,5] <- length(na.omit(d$cloth_immed[d$old_branch_position==1]))

tab1[9,6] <- paste(round(mean(d$cloth_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[9,7] <- length(na.omit(d$cloth_after[d$old_branch_position==1]))

tab1[10,1] <- "Given electronics (phone, etc)"
tab1[10,2] <- paste(round(mean(d$paid_for_camp_phone[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[10,3] <- length(na.omit(d$paid_for_camp_phone[d$old_branch_position==1]))

tab1[10,4] <- paste(round(mean(d$electronics_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[10,5] <- length(na.omit(d$electronics_immed[d$old_branch_position==1]))

tab1[10,6] <- paste(round(mean(d$electronics_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[10,7] <- length(na.omit(d$electronics_after[d$old_branch_position==1]))

tab1[11,1] <- "Given minor inputs (fertilizer, etc)"
tab1[11,2] <- NA
tab1[11,3] <- NA

tab1[11,4] <- paste(round(mean(d$inputs_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[11,5] <- length(na.omit(d$inputs_immed[d$old_branch_position==1]))

tab1[11,6] <- paste(round(mean(d$inputs_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[11,7] <- length(na.omit(d$inputs_after[d$old_branch_position==1]))

d$not_paid_for_campaign <- 1 - d$paid_for_campaign

tab1[12,1] <- "Not paid in this period"
tab1[12,2] <- paste(round(mean(d$not_paid_for_campaign[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[12,3] <- length(na.omit(d$not_paid_for_campaign[d$old_branch_position==1]))

tab1[12,4] <- paste(round(mean(d$nothing_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[12,5] <- length(na.omit(d$nothing_immed[d$old_branch_position==1]))

tab1[12,6] <- paste(round(mean(d$nothing_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[12,7] <- length(na.omit(d$nothing_after[d$old_branch_position==1]))

d$nothing_start_to_immed[is.na(d$nothing_immed)==F] <- 0
d$nothing_start_to_immed[d$nothing_immed==1 & d$paid_for_campaign==0] <- 1

d$nothing_start_to_after[is.na(d$nothing_after)==F] <- 0
d$nothing_start_to_after[d$nothing_after ==1 & d$nothing_immed==1 & d$paid_for_campaign==0] <- 1

tab1[13,1] <- "Not paid cumulatively to date"
tab1[13,2] <- paste(round(mean(d$not_paid_for_campaign[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[13,3] <- length(na.omit(d$not_paid_for_campaign[d$old_branch_position==1]))

tab1[13,4] <- paste(round(mean(d$nothing_start_to_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[13,5] <- length(na.omit(d$nothing_start_to_immed[d$old_branch_position==1]))

tab1[13,6] <- paste(round(mean(d$nothing_start_to_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[13,7] <- length(na.omit(d$nothing_start_to_after[d$old_branch_position==1]))


d$major_start_to_immed[is.na(d$big_pat_immed)==F] <- 0
d$major_start_to_immed[d$big_pat_immed ==1 | d$paid_camp_big ==1] <- 1

d$major_start_to_after[is.na(d$nothing_after)==F] <- 0
d$major_start_to_after[(d$big_pat_immed ==1 | d$paid_camp_big ==1 | d$big_pat_after2 ==1) & is.na(d$nothing_after)==F ] <- 1

d$minor_start_to_immed[is.na(d$small_pat_immed)==F] <- 0
d$minor_start_to_immed[d$small_pat_immed ==1 | d$paid_camp_small ==1] <- 1

d$minor_start_to_after[is.na(d$nothing_after)==F] <- 0
d$minor_start_to_after[(d$small_pat_immed ==1 | d$paid_camp_small ==1 | d$small_pat_after ==1) & is.na(d$nothing_after)==F ] <- 1


tab1[14,1] <- "Paid major patronage cumulatively to date (0,1)"
tab1[14,2] <- paste(round(mean(d$paid_camp_big[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[14,3] <- length(na.omit(d$paid_camp_big[d$old_branch_position==1]))

tab1[14,4] <- paste(round(mean(d$major_start_to_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[14,5] <- length(na.omit(d$major_start_to_immed[d$old_branch_position==1]))

tab1[14,6] <- paste(round(mean(d$major_start_to_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[14,7] <- length(na.omit(d$major_start_to_after[d$old_branch_position==1]))


tab1[15,1] <- "Paid minor patronage cumulatively to date (0,1)"
tab1[15,2] <- paste(round(mean(d$paid_camp_small[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[15,3] <- length(na.omit(d$paid_camp_small[d$old_branch_position==1]))

tab1[15,4] <- paste(round(mean(d$minor_start_to_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[15,5] <- length(na.omit(d$minor_start_to_immed[d$old_branch_position==1]))

tab1[15,6] <- paste(round(mean(d$minor_start_to_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[15,7] <- length(na.omit(d$minor_start_to_after[d$old_branch_position==1]))

tab1[16,1] <- "A state contract"
tab1[16,2] <- 0
tab1[16,3] <- length(na.omit(d$paid_camp_big[d$old_branch_position==1]))

tab1[16,4] <- paste(round(mean(d$contracts_immed[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[16,5] <- length(na.omit(d$contracts_immed[d$old_branch_position==1]))

tab1[16,6] <- paste(round(mean(d$contracts_after[d$old_branch_position==1], na.rm=T), digits=3) * 100, "%", sep="")
tab1[16,7] <- length(na.omit(d$contracts_after[d$old_branch_position==1]))

###--------
### PRINT TABLE 
###--------

xtable(tab1)
#note that we manually changed the order of some rows in the final version for aesthetic reasons. the figures produced by this replication code match. 


####################
##Table 3: What could you do to improve your compensation?
####################

d$howto_paid_r1[d$howto_paid_r1=="refuse"] = NA
d$howto_paid_r1[d$howto_paid_r1=="off_topic_response"] = NA
d$howto_paid_r1[d$howto_paid_r1=="dont_know"] = NA

d$howto_paid_r1[d$howto_paid_r1=="ensure_peaceful_elections"] = "other"
d$howto_paid_r1[d$howto_paid_r1=="rise_higher"] = "other"




col1  <- table(c(d$howto_paid_r1, d$howto_paid_r2[d$howto_paid_r2!="NA"]))
col2 <- round(col1/954*100, 2)

col1
col2
## NOTE: Table 3 is made by hand, to include some of the open-ended verbatim responses from the survey respondents. The figures in  "col1" and "col2" correpsond to the figures reported in columns 1 and 2 of the table, respectively.  

## NOTE: the example quotes come of of this string variable
head(d$payments.increase_pay_open)

d$payments.increase_pay_open[d$respid==10502]
d$payments.increase_pay_open[d$respid==10677]
d$payments.increase_pay_open[d$respid==10290]
d$payments.increase_pay_open[d$respid==10297]
d$payments.increase_pay_open[d$respid==10524]
d$payments.increase_pay_open[d$respid==10658]
d$payments.increase_pay_open[d$respid==10229]
d$payments.increase_pay_open[d$respid==10041]
d$payments.increase_pay_open[d$respid==10352]
d$payments.increase_pay_open[d$respid==10316]
d$payments.increase_pay_open[d$respid==10521]
d$payments.increase_pay_open[d$respid==10321]
d$payments.increase_pay_open[d$respid==10184]
d$payments.increase_pay_open[d$respid==10140]
d$payments.increase_pay_open[d$respid==10130]
d$payments.increase_pay_open[d$respid==10780]
d$payments.increase_pay_open[d$respid==10955]
d$payments.increase_pay_open[d$respid==10100]
d$payments.increase_pay_open[d$respid==10564]
d$payments.increase_pay_open[d$respid==10413]
d$payments.increase_pay_open[d$respid==10822]

####################
##Table 4: Understanding of reasoning for payments on payments received
####################

how1 <- lm(howto_cxnup ~ big_pat_after + big_pat_immed + change_cnx_up + female + age+  cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW + old_branch_position +age + female +  chief_relative +  constexec_relative +  da_relative+mpdce_relative + local_eth_minority+ lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_h1<- as.vector(summary(how1, cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

how4<- lm(howto_effort_performance ~ big_pat_after +big_pat_immed + change_cnx_up + female + age+  cxn_down_percentage_correct_full+ npp12to16_ps_swing.NEW + old_branch_position +age + female +  chief_relative + constexec_relative +  da_relative+ mpdce_relative+ local_eth_minority+ lives_outside_ps + petty_trader + formal_sector +  as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$current_exec==1,])
cluster_h4 <- as.vector(summary(how4, cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

stargazer(how1, how4, se = list(cluster_h1, cluster_h4),  omit=c("constituency.x",  "pol_relative" , "chief_relative",   "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x"),single.row=T ,  add.lines = list(c("Constituency FEs", "Y", "Y"), c("Indiv. controls", "Y", "Y"), c("PS. controls", "Y", "Y")))




####################
##Table 5: Major patronage payments immediately after the election
###################


### save data (THIS WAS NOT IN THE ORIGINAL REPLICATION PACKAGE, ADDED TO MAKE THINGS SIMPLER IN THE DML SCRIPT)
d |>
  write_csv("replication_package_brierley_nathan/data_clean.csv")

m1 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female +  chief_relative +   constexec_relative +  da_relative+ mpdce_relative+ local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])

cluster_se1 <- as.vector(summary(m1,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m2 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female +chief_relative +  constexec_relative +
 da_relative+ mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +swing_2012_2016_const, data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])
summary(m2)
cluster_se2 <- as.vector(summary(m2,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

m3 <- lm(big_pat_immed ~ npp_rawvotes + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female +  chief_relative +  constexec_relative +  da_relative+ mpdce_relative+local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x + as.factor(constituency.x) , data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,])
cluster_se3 <- as.vector(summary(m3,cluster = c("anon.ps.name"))$coefficients[,"Std. Error"])

summary(d$npp12to16_ps_swing.NEW)
sd(d$npp12to16_ps_swing.NEW, na.rm=T)

d_low = d
d_low$npp12to16_ps_swing.NEW=(0.05968-0.04860537)
d_high  = d 
d_high$npp12to16_ps_swing.NEW=(0.05968+0.04860537)

mean(predict(m1, newdata = d_low), na.rm=T)
#0.05106129
mean(predict(m1, newdata = d_high), na.rm=T)
#0.1257006

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

m4 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative + constexec_relative +  da_relative+
           mpdce_relative+  local_eth_minority + lives_outside_ps + petty_trader + formal_sector + bio_educ_three + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=bl.dat)

#NO CLUSTERED ERRORS BECAUSE THE UNIT IS THE POLLING STATION ALREADY AND WE HAVE CONST FEs

m5 <- lm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female +    chief_relative +  constexec_relative +  da_relative+
           mpdce_relative+ local_eth_minority + lives_outside_ps + petty_trader + formal_sector + bio_educ_three + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x + swing_2012_2016_const , data=bl.dat)

m6 <- lm(big_pat_immed ~ npp_rawvotes + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female +   chief_relative +  constexec_relative +  da_relative+ mpdce_relative+
           local_eth_minority + lives_outside_ps + petty_trader + formal_sector + bio_educ_three + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=bl.dat)

#### PRINT TABLE

stargazer(m1, m2, m3, m4,m5,m6, 
          se = list(cluster_se1, cluster_se2, cluster_se3), 
          dep.var.labels   = "Major patronage (2017)", 
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          omit=c("constituency.x", #"age", 
                 "pol_relative" , "chief_relative",  "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x"),
          add.lines = list(c("Constituency FEs", "Y", "N", "Y", "Y", "N", "Y"), c("Individual-level controls", "Y", "Y", "Y", "Y", "Y", "Y", "Y"), c("Polling station-level controls", "Y", "Y", "Y", "Y","Y","Y")))



####################
##Figure 1: Major patronage after the election (Period 2)
####################


m1 <- glm(big_pat_immed ~ npp12to16_ps_swing.NEW + cpgn_brok_index + cxn_up_percentage_correct_full + cxn_down_percentage_correct_full + age + female + chief_relative + constexec_relative +  da_relative+ mpdce_relative+ local_eth_minority + lives_outside_ps + petty_trader + formal_sector + as.factor(bio_educ_three) + asset_index + years_active_npp + years_comm + km_to_capital_wave1 + wealth_index_2km.x +  as.factor(constituency.x), data=d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,], family="binomial")

summary(m1)

set.seed(48103)
vcovCL <- vcov(m1)
N <- 1000
betas <- mvrnorm(n = N, mu= m1$coefficients, vcovCL)

summary(d$npp12to16_ps_swing.NEW)
var.seq <- seq(-0.08, 0.21, by=0.005)

pred.all <- as.data.frame(matrix(NA, nrow=N, ncol=length(var.seq)))

d2 <- d[is.na(d$npp12to16_ps_swing.NEW)==F & d$old_branch_position==1 & is.na(d$old_branch_position)==F,]

d2$assin_south <- ifelse(d2$constituency.x=="assin_south", 1, 0)
d2$atwima_mponua <- ifelse(d2$constituency.x=="atwima_mponua", 1, 0)
d2$ayawaso_central <- ifelse(d2$constituency.x=="ayawaso_central", 1, 0)
d2$bortianor <- ifelse(d2$constituency.x=="bortianor", 1, 0)
d2$dome_kwabenya <- ifelse(d2$constituency.x=="dome_kwabenya", 1, 0)
d2$ho_central <- ifelse(d2$constituency.x=="ho_central", 1, 0)
d2$lower_manya_krobo <- ifelse(d2$constituency.x=="lower_manya_krobo", 1, 0)
d2$manhyia_south <- ifelse(d2$constituency.x=="manhyia_south", 1, 0)
d2$north_tongu <- ifelse(d2$constituency.x=="north_tongu", 1, 0)
d2$bio_educ_three.2 <-  ifelse(d2$bio_educ_three ==2, 1, 0)
d2$bio_educ_three.3 <- ifelse(d2$bio_educ_three ==3, 1, 0)

for(i in 1:length(var.seq)){
  
  xt <- cbind(1, var.seq[i], d2$cpgn_brok_index, d2$cxn_up_percentage_correct_full, d2$cxn_down_percentage_correct_full, d2$age, d2$female, d2$chief_relative,  d2$constexec_relative,d2$da_relative,d2$mpdce_relative,  d2$local_eth_minority, d2$lives_outside_ps, d2$petty_trader, d2$formal_sector, d2$bio_educ_three.2, d2$bio_educ_three.3, d2$asset_index, d2$years_active_npp, d2$years_comm, d2$km_to_capital_wave1, d2$wealth_index_2km.x,  d2$assin_south, d2$atwima_mponua, d2$ayawaso_central, d2$bortianor, d2$dome_kwabenya, d2$ho_central, d2$lower_manya_krobo, d2$manhyia_south, d2$north_tongu)
  
  xt <- na.omit(xt)
  
  tprob<-apply((1/(1+exp(-(as.matrix(xt))%*% t(betas)))),1,as.vector)
  tprob.means <- apply(tprob, 1, mean)
  
  pred.all[,i] <- tprob.means
  
}

means <- apply(pred.all, 2, mean)
lowCI <- c()
highCI <- c()
for(j in 1:ncol(pred.all)){
  lowCI[j] <- quantile(pred.all[,j], probs=c(0.025))
  highCI[j] <- quantile(pred.all[,j], probs=c(0.975))
}

plot(var.seq, means, xlab="Polling station NPP vote swing (2012-2016)", ylab="Predicted probability major patronage (Period 2)", ylim=c(0, max(highCI)+0.02), pch=16, cex=1.2, col="blue", axes=F)
axis(1)
axis(2)
#main="Major patronage in 2017\nby presidential vote swing at branch", axes=F)
segments(var.seq, lowCI, var.seq, highCI, lty="solid", lwd=0.5, col="grey33")
points(var.seq, means,  pch=16, cex=1.1, col="dodgerblue3")
#abline(h=0, col="firebrick")
rug(jitter(d2$npp12to16_ps_swing.NEW))
abline(h=mean(d[d$old_branch_position==1 & is.na(d$old_branch_position)==F,"big_pat_immed"], na.rm=T), lty="dashed", lwd=1.3, col="firebrick")
abline(v=mean(d$npp12to16_ps_swing.NEW, na.rm=T), lty="dashed", lwd=1.3, col="firebrick")




############
##Table 6: Predictors of major patronage in the non-electoral period (2018-2019)
############


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
          dep.var.labels   = "Major patronage (2018-2019)", omit=c("constituency.x", #"age", 
  "pol_relative" , "chief_relative", "local_eth_minority", "lives_outside_ps", "petty_trader", "formal_sector", "bio_educ_three", "asset_index", "years_active_npp" , "years_comm", "km_to_capital_wave1",  "wealth_index_2km.x"), add.lines = list(c("Constituency FEs", "Y", "Y", "Y", "Y"), c("Individual-level controls", "Y", "Y", "Y", "Y"), c("Polling station-level controls", "Y", "Y", "Y", "Y")), star.char = c("+", "*", "**", "***"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"))




############
##Table 7: Predictors of change in receiving major patronage in non-electoral period (2018-2019)
#############

#########
### First difference models
########

#make cumulative 
d$big_pat_wave1_sum <- d$paid_post_camp_job.x+  d$paid_post_camp_loan.x + d$paid_post_camp_training.x+ d$paid_post_camp_contracts.x + d$paid_post_camp_moto.x+ d$paid_post_camp_bicycle.x + d$training_since2016.x+ d$gov_loan_since2016.x + d$yea_since2016.x

d$big_pat_wave2_sum <- d$paid_post_camp_job.y +  d$paid_post_camp_loan.y + d$paid_post_camp_training.y + d$paid_post_camp_contracts.y + d$paid_post_camp_moto.y + d$paid_post_camp_bicycle.y + d$training_since2016.y + d$gov_loan_since2016.y + d$yea_since2016.y

#need to add one when people said it in wave 1 but not in wave 2 for the same item (if they said it in both waves it may be litearlly the same benefit)
d$big_pat_wave2_sum[d$paid_post_camp_job.x==1 & d$paid_post_camp_job.y==0 & is.na(d$paid_post_camp_job.y)==F] <- d$big_pat_wave2_sum[d$paid_post_camp_job.x==1 & d$paid_post_camp_job.y==0 & is.na(d$paid_post_camp_job.y)==F] + 1

d$big_pat_wave2_sum[d$paid_post_camp_loan.x ==1 & d$paid_post_camp_loan.y ==0 & is.na(d$paid_post_camp_loan.y)==F] <- d$big_pat_wave2_sum[d$paid_post_camp_loan.x ==1 & d$paid_post_camp_loan.y ==0 & is.na(d$paid_post_camp_loan.y)==F] + 1

d$big_pat_wave2_sum[d$paid_post_camp_training.x ==1 & d$paid_post_camp_training.y ==0 & is.na(d$paid_post_camp_training.y)==F] <- d$big_pat_wave2_sum[d$paid_post_camp_training.x ==1 & d$paid_post_camp_training.y ==0 & is.na(d$paid_post_camp_training.y)==F] + 1

d$big_pat_wave2_sum[d$paid_post_camp_contracts.x ==1 & d$paid_post_camp_contracts.y ==0& is.na(d$paid_post_camp_contracts.y)==F] <- d$big_pat_wave2_sum[d$paid_post_camp_contracts.x ==1 & d$paid_post_camp_contracts.y ==0 & is.na(d$paid_post_camp_contracts.y)==F] + 1

d$big_pat_wave2_sum[d$paid_post_camp_moto.x ==1 & d$paid_post_camp_moto.y ==0& is.na(d$paid_post_camp_moto.y)==F] <- d$big_pat_wave2_sum[d$paid_post_camp_moto.x ==1 & d$paid_post_camp_moto.y ==0& is.na(d$paid_post_camp_moto.y)==F] + 1

d$big_pat_wave2_sum[d$paid_post_camp_bicycle.x ==1 & d$paid_post_camp_bicycle.y ==0& is.na(d$paid_post_camp_bicycle.y)==F] <- d$big_pat_wave2_sum[d$paid_post_camp_bicycle.x ==1 & d$paid_post_camp_bicycle.y ==0& is.na(d$paid_post_camp_bicycle.y)==F] + 1

d$big_pat_wave2_sum[d$training_since2016.x ==1 & d$training_since2016.y ==0& is.na(d$training_since2016.y)==F] <- d$big_pat_wave2_sum[d$training_since2016.x ==1 & d$training_since2016.y ==0& is.na(d$training_since2016.y)==F] + 1

d$big_pat_wave2_sum[d$gov_loan_since2016.x ==1 & d$gov_loan_since2016.y==0& is.na(d$gov_loan_since2016.y)==F] <- d$big_pat_wave2_sum[d$gov_loan_since2016.x ==1 & d$gov_loan_since2016.y==0& is.na(d$gov_loan_since2016.y)==F] + 1

d$big_pat_wave2_sum[d$yea_since2016.x ==1 & d$yea_since2016.y==0& is.na(d$yea_since2016.y)==F] <- d$big_pat_wave2_sum[d$yea_since2016.x ==1 & d$yea_since2016.y==0& is.na(d$yea_since2016.y)==F] + 1

table(d$big_pat_wave1_sum, d$big_pat_wave2_sum)
summary(d$big_pat_wave2_sum - d$big_pat_wave1_sum)
#no 0s... 

d_w1<- dplyr::select(d, respid, current_exec,  big_pat_immed, anon.ps.name, cxn_up_percentage_correct_full, cxn_up_politicians ,   cxn_up_bureaucrats ,  cxn_up_constexecs , broker_up, broker_down ,asset_total.x, big_pat_wave1_sum) 
d_w1$wave <- 1

d_w1 <- dplyr::rename(d_w1, respid=respid,  big_pat = big_pat_immed,   c_up = cxn_up_percentage_correct_full  , c_upp = cxn_up_politicians, c_upb = cxn_up_bureaucrats  ,  c_upc  = cxn_up_constexecs     ,
                      b_up= broker_up  , b_down   =  broker_down, assets  = asset_total.x   ,  wave  =wave, big_pat_sum = big_pat_wave1_sum)

d_w1 <- filter(d_w1, current_exec==1)

d_w2 <- dplyr::select(d, respid, anon.ps.name, current_exec,  big_pat_after,  cxn_up_percentage_correct_full_w2,cxn_up_politicians_w2 ,  cxn_up_bureaucrats_w2 ,cxn_up_constexecs_w2 ,   broker_up.y ,  broker_down.y ,  asset_total.y, big_pat_wave2_sum) 

d_w2$wave <- 2

d_w2 <- dplyr::rename(d_w2, respid=respid, big_pat = big_pat_after,   c_up = cxn_up_percentage_correct_full_w2  , c_upp = cxn_up_politicians_w2,c_upb = cxn_up_bureaucrats_w2  , c_upc  = cxn_up_constexecs_w2    ,  b_up= broker_up.y      , b_down   =  broker_down.y          ,  assets   = asset_total.y   , wave  =wave, big_pat_sum = big_pat_wave2_sum)

d_w2 <- filter(d_w2, current_exec==1)

### STACK THE DATA 

d_panel <- rbind(d_w1, d_w2)

#d_panel.1 <- rbind(d_w1.1, d_w2) # EXLUDE THOSE WHO GOT BIG PAT IN PERIOD 2

### PANEL DATASET 
d_panel2<- pdata.frame(d_panel, index=c("respid", "wave"))

fd_1_sum <- plm(big_pat_sum ~ c_up + b_up + b_down +  assets,   data = d_panel2, model = "fd")
cluster1 <- coeftest(fd_1_sum, vcov=vcovHC(fd_1_sum, type="sss", cluster="group"))  
cluster_se5 <- cluster1[, 2]

fd_2_sum <- plm(big_pat_sum ~ c_upp +c_upb +c_upc + b_up + b_down +  assets ,   data = d_panel2, model = "fd")
cluster2 <- coeftest(fd_2_sum, vcov=vcovHC(fd_2_sum, type="sss", cluster="group"))  
cluster_se6 <- cluster2[,2]

fd_3_sum <- plm(big_pat_sum ~ c_up +  assets,   data = d_panel2, model = "fd")
cluster3 <- coeftest(fd_3_sum, vcov=vcovHC(fd_3_sum, type="sss", cluster="group"))  
cluster_se7 <- cluster3[, 2]

stargazer(fd_1_sum, fd_2_sum, fd_3_sum,  se = list(cluster_se5, cluster_se6, cluster_se7), star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"))

