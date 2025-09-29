


##############################################
# Paper: Methods Presentation                #                      
# Task: Chu and Recchia cleaning             #                                                                             
# Date:                                      #                                                                         
##############################################


# Set Up ------------------------------------------------------------------

rm(list = ls())

# Set working directory
if(Sys.info()["user"] == "gabriellepeloquinskulski"){
  setwd("/Users/gabriellepeloquinskulski/MIT Dropbox/Gabrielle Peloquin-Skulski/dml-project")
  plot_path <- "~/figures"
}else{
  setwd("") 
  plot_path <- ""    
}

# Packages 
library(haven)
library(dplyr)
library(tidyr)
library(mlogit)




