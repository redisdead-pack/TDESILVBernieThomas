# load useful libraries
library(tidyverse)
library(dplyr)
library(knitr)
library(skimr)
library(lubridate)
library(gridExtra)
library(scorecard)
library(ggplot2)
library(ggplotify)
library(plotly)
library(questionr)
library(caret)
library(pROC)
library(purrr)

datainput = read_delim('./data/trainRed.csv', col_names = TRUE, delim = ',')
write.csv(datainput[createDataPartition(dataWrangled$loan_default,p=0.1,list=FALSE),],'./data/trainRed1.csv')

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
datainput = read_delim('./data/trainRed1.csv', col_names = TRUE, delim = ',')
skim(datainput)

dataWrangled <- datainput  %>%
  mutate(Employment.Type = replace_na(datainput$Employment.Type, "None")) %>%
  mutate(State_ID =  as.factor(State_ID)) %>%
  mutate(branch_id =  as.factor(branch_id)) %>%
  mutate(loan_default =  as.factor(loan_default)) %>%
  mutate(VoterID_flag =  as.factor(VoterID_flag)) %>%
  mutate(supplier_id =  as.factor(supplier_id)) %>%
  mutate(manufacturer_id =  as.factor(manufacturer_id)) %>%
  mutate(Current_pincode_ID =  as.factor(Current_pincode_ID)) %>%
  mutate(Employee_code_ID =  as.factor(Employee_code_ID)) %>%
  mutate(MobileNo_Avl_Flag =  as.factor(MobileNo_Avl_Flag)) %>%
  mutate(Aadhar_flag =  as.factor(Aadhar_flag)) %>%
  mutate(PAN_flag =  as.factor(PAN_flag)) %>%
  mutate(Driving_flag =  as.factor(Driving_flag)) %>%
  mutate(Passport_flag =  as.factor(Passport_flag)) 

#Regroupement de modalités
dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION[dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION %in% 
                                             c("C-Very Low Risk", "A-Very Low Risk", "B-Very Low Risk","D-Very Low Risk","F-Low Risk","E-Low Risk","G-Low Risk")] <- "Low"
dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION[dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION %in% 
                                             c("H-Medium Risk", "I-Medium Risk")] <- "Medium"
dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION[dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION %in% 
                                             c("J-High Risk", "K-High Risk","L-Very High Risk", "M-Very High Risk")] <- "High"
dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION[dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION %in% 
                                             c("Not Scored: More than 50 active Accounts found", 
                                               "Not Scored: Only a Guarantor",
                                               "Not Scored: Not Enough Info available on the customer",
                                               "Not Scored: No Activity seen on the customer (Inactive)",
                                               "Not Scored: No Updates available in last 36 months",
                                               "Not Scored: Sufficient History Not Available",
                                               "No Bureau History Available")] <- "Not Scored"

#refresh de la variable avec les nouvelles modalités
datainput$PERFORM_CNS.SCORE.DESCRIPTION <- factor(datainput$PERFORM_CNS.SCORE.DESCRIPTION)

dataWrangled <- dataWrangled %>%
  mutate(AAA = as.integer((as.numeric(str_match_all( datainput$AVERAGE.ACCT.AGE,"\\d+(?=yrs)")) *12 
                           + as.numeric(str_match_all( datainput$AVERAGE.ACCT.AGE,"\\d+(?=mon)"))))) %>%
  mutate(CHL = (as.numeric(str_match_all( datainput$CREDIT.HISTORY.LENGTH,"\\d+(?=yrs)")) *12 
                + as.numeric(str_match_all( datainput$CREDIT.HISTORY.LENGTH,"\\d+(?=mon)"))))

thisyear = year(Sys.Date())
dataWrangled <- dataWrangled %>%
  mutate(BorrowerAge = thisyear-year(dmy(Date.of.Birth))) 
dataWrangled <- dataWrangled %>%
  mutate(NbrMonthRelation = 12*(thisyear-year(dmy(DisbursalDate)) + month(dmy(DisbursalDate))))

#liste des variables a discretiser : toutes sauf les id, la cible, et les variables non transformees
to_bin <- names(dataWrangled)
to_bin <- to_bin[!to_bin %in% c("loan_default","UniqueID","Current_pincode_ID", "Date.of.Birth", "DisbursalDate", "Employee_code_ID", "AVERAGE.ACCT.AGE", "CREDIT.HISTORY.LENGTH")]

bins = woebin(dataWrangled[, c(to_bin,'loan_default')], y = 'loan_default',check_cate_num = F, bin_num_limit = 3)

woebin_plot(bins$ltv)$ltv
woebin_plot(bins$disbursed_amount)$disbursed_amount
woebin_plot(bins$PRI.CURRENT.BALANCE)$PRI.CURRENT.BALANCE
woebin_plot(bins$PRI.DISBURSED.AMOUNT)$PRI.DISBURSED.AMOUNT
woebin_plot(bins$PRIMARY.INSTAL.AMT)$PRIMARY.INSTAL.AMT
woebin_plot(bins$BorrowerAge)$BorrowerAge
woebin_plot(bins$AAA)$AAA
woebin_plot(bins$BorrowerAge)$BorrowerAge
woebin_plot(bins$PERFORM_CNS.SCORE.DESCRIPTION)$PERFORM_CNS.SCORE.DESCRIPTION
woebin_plot(bins$PRI.OVERDUE.ACCTS)$PRI.OVERDUE.ACCTS
woebin_plot(bins$CHL)$CHL
