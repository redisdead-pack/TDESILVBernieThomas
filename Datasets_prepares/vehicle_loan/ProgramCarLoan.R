# load useful libraries
library(pacman)
library(tidyverse)
library(skimr)
library(dplyr)
library(caret)
library(pROC)
library(recipes) # could also load the tidymodels package
library(corrplot)
library(lubridate)
library(data.table) # data mgmt
library(gtools) # combination
library(ggplot2) # graphics
library(plotly) # interactive graphics
library(lsr)
library(mltools)
library(labelled)
library(questionr)
library(ggcorrplot)

#seed for replication
set.seed(7)

# set up so that all variables of tibbles are printed
options(dplyr.width = Inf)

#make the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
list.files('./data')

datainput = read_delim('./data/train.csv', col_names = TRUE, delim = ',')
colnames <- names(datainput)

#display information
names(datainput)


#https://www.kaggle.com/arpitjain007/logistic-regression

datainputreduced <- datainput  %>%
  select (
          ltv,
          disbursed_amount,
          NO.OF_INQUIRIES,
          Date.of.Birth,
          State_ID,
          branch_id,
          supplier_id,
          Employment.Type,
          VoterID_flag,
          asset_cost,
          SEC.ACTIVE.ACCTS,
          disbursed_amount,
          PERFORM_CNS.SCORE.DESCRIPTION,
          DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS,
          SEC.NO.OF.ACCTS,
          #PRI.CURRENT.BALANCE,
          PRI.ACTIVE.ACCTS,
          AVERAGE.ACCT.AGE,
          CREDIT.HISTORY.LENGTH,
          
          NEW.ACCTS.IN.LAST.SIX.MONTHS,
          SEC.INSTAL.AMT,
          PRIMARY.INSTAL.AMT,
          SEC.DISBURSED.AMOUNT,
          SEC.SANCTIONED.AMOUNT,
          SEC.CURRENT.BALANCE,
          SEC.OVERDUE.ACCTS,
          SEC.ACTIVE.ACCTS,
          loan_default,
          
          
          manufacturer_id,
          Current_pincode_ID,
          Employee_code_ID,
          MobileNo_Avl_Flag,
          Aadhar_flag,
          PAN_flag,
          Driving_flag,
          
          Passport_flag,
          PERFORM_CNS.SCORE,
          PERFORM_CNS.SCORE.DESCRIPTION,
          PRI.NO.OF.ACCTS,
          PRI.ACTIVE.ACCTS,
          PRI.OVERDUE.ACCTS,
          PRI.CURRENT.BALANCE,
          PRI.SANCTIONED.AMOUNT, 
          PRI.DISBURSED.AMOUNT) %>%
  mutate(replace_na(datainput$Employment.Type)) %>%
  mutate(State_ID =  as.factor(State_ID)) %>%
  mutate(branch_id =  as.factor(branch_id)) %>%
  mutate(loan_default =  as.factor(loan_default)) %>%
  mutate(VoterID_flag =  as.factor(VoterID_flag)) %>%
  mutate(AAA = as.integer((as.numeric(str_match_all( datainput$AVERAGE.ACCT.AGE,"\\d+(?=yrs)")) *12 
         + as.numeric(str_match_all( datainput$AVERAGE.ACCT.AGE,"\\d+(?=mon)"))))) %>%
  mutate(CHL = (as.numeric(str_match_all( datainput$CREDIT.HISTORY.LENGTH,"\\d+(?=yrs)")) *12 
         + as.numeric(str_match_all( datainput$CREDIT.HISTORY.LENGTH,"\\d+(?=mon)"))))
  


datainputreduced$PERFORM_CNS.SCORE.DESCRIPTION[datainputreduced$PERFORM_CNS.SCORE.DESCRIPTION %in% 
                                                 c("C-Very Low Risk", "A-Very Low Risk", "B-Very Low Risk","D-Very Low Risk","F-Low Risk","E-Low Risk","G-Low Risk")] <- "Low"
datainputreduced$PERFORM_CNS.SCORE.DESCRIPTION[datainputreduced$PERFORM_CNS.SCORE.DESCRIPTION %in% 
                                                 c("H-Medium Risk", "I-Medium Risk")] <- "Medium"
datainputreduced$PERFORM_CNS.SCORE.DESCRIPTION[datainputreduced$PERFORM_CNS.SCORE.DESCRIPTION %in% 
                                                 c("J-High Risk", "K-High Risk","L-Very High Risk", "M-Very High Risk")] <- "High"
datainputreduced$PERFORM_CNS.SCORE.DESCRIPTION[datainputreduced$PERFORM_CNS.SCORE.DESCRIPTION %in% 
                                                 c("Not Scored: More than 50 active Accounts found", "Not Scored: Only a Guarantor",
                                                   "Not Scored: Not Enough Info available on the customer", "Not Scored: No Activity seen on the customer (Inactive)",
                                                   "Not Scored: No Updates available in last 36 months", "Not Scored: Sufficient History Not Available",
                                                   "No Bureau History Available")] <- "Not Scored"
thisyear = year(Sys.Date())
datainputreduced <- datainputreduced %>%
  mutate(NbrYear = thisyear-year(dmy(Date.of.Birth))) 
datainputreduced <- datainputreduced %>%
  mutate(NbrYearRelation = 12*(thisyear-year(dmy(Date.of.Birth)) + month(dmy(Date.of.Birth)))) 

partition(skim(datainputreduced))

##############################################################
###--------------------------------------------------------###
###----------------- Analyse univarié ---------------###
hist(datainputreduced$NbrYear, main = "Age des clients", 
     xlab = "Age", ylab = "Effectif")
boxplot(datainputreduced$NbrYear, main = "Age des clients", 
        ylab = "Age")
#le truc en matrix de kaggle

##############################################################
###--------------------------------------------------------###
###----------------- Analyse of correlation ---------------###

#cas du score et regroupement
# https://juba.github.io/tidyverse/04-bivarie.html#croisement-de-deux-variables-qualitatives
tcd <- table(datainput$PERFORM_CNS.SCORE.DESCRIPTION, datainput$loan_default)
lprop(tcd)
tcd <- table(datainputreduced$PERFORM_CNS.SCORE.DESCRIPTION, datainputreduced$loan_default)
lprop(tcd)

chisq.test(tcd)
chisq.residuals(tcd)
cramer.v(tcd)

#faire graph distrub supperposés 0-1 sur variavle continue et significative / non signif


##############################################################
###--------------------------------------------------------###
###----------------- Analyse of correlation ---------------###
# datainputcor <- datainputreduced  %>%
#   select (
#           loan_default,
#           NO.OF_INQUIRIES,
#           State_ID,
#           branch_id,
#           Employment.Type,
#           PERFORM_CNS.SCORE.DESCRIPTION,
#           VoterID_flag,
#           SEC.NO.OF.ACCTS,
#           PRI.ACTIVE.ACCTS
#           ) %>%
#   slice(1:5000)

# datainputcor <- datainputreduced  %>%
#   select (ltv, #
#           disbursed_amount, #
#           NO.OF_INQUIRIES,
#           Date.of.Birth,
#           asset_cost, #
#           SEC.ACTIVE.ACCTS,
#           DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS,
#           loan_default,
#           AAA,
#           CHL) %>%
#   slice(1:5000)

# datainputcor <- datainputreduced  %>%
#   select (          NEW.ACCTS.IN.LAST.SIX.MONTHS,
#                     SEC.INSTAL.AMT,
#                     PRIMARY.INSTAL.AMT,#
#                     SEC.DISBURSED.AMOUNT,
#                     SEC.SANCTIONED.AMOUNT,
#                     SEC.CURRENT.BALANCE,
#                     SEC.OVERDUE.ACCTS,
#                     SEC.ACTIVE.ACCTS,
#                     loan_default,) %>%
#   slice(1:5000)



# datainputcor <- datainputreduced  %>%
#   select (                    manufacturer_id,
#                               Current_pincode_ID,#
#                               Employee_code_ID,#
#                               MobileNo_Avl_Flag,
#                               Aadhar_flag,
#                               PAN_flag,
#                               Driving_flag,
#                     loan_default,) %>%
#   slice(1:5000)

# datainputcor <- datainputreduced  %>%
#   select (                    Current_pincode_ID,
#                               Employee_code_ID,
#                               PRIMARY.INSTAL.AMT,
#                               asset_cost,
#                               disbursed_amount,
#                               ltv,
#                               loan_default,) %>%
#   slice(1:5000)

datainputcor <- datainputreduced  %>%
  select (                              Passport_flag,
                                        PERFORM_CNS.SCORE,#
                                        PERFORM_CNS.SCORE.DESCRIPTION,
                                        PRI.NO.OF.ACCTS,
                                        PRI.ACTIVE.ACCTS,
                                        PRI.OVERDUE.ACCTS,
                                        PRI.CURRENT.BALANCE,#
                                        PRI.SANCTIONED.AMOUNT,# 
                                        PRI.DISBURSED.AMOUNT,#
                              loan_default,) %>%
  slice(1:5000)


# function to get chi square p value and Cramers V
fCramerFunction = function(x,y) {
  #message(sprintf(" %s || %s", x, y))
  tbl = datainputcor %>% select(x,y) %>% table()
  chisq_pval = round(chisq.test(tbl)$p.value, 2)
  cramV = round(cramer.v(tbl), 2) 
  data.frame(x, y, chisq_pval, cramV) }

# create unique combinations of column names
# sorting will help getting a better plot (upper triangular)
df_comb = data.frame(t(combn(sort(names(datainputcor)), 2)), stringsAsFactors = F)

# apply function to each variable combination
df_res = map2_df(df_comb$X1, df_comb$X2, fCramerFunction)

# plot results
df_res %>%
  #ggplot(aes(x,y,fill=chisq_pval))+
  ggplot(aes(x,y,fill=cramV))+
  geom_tile()+
  geom_text(aes(x,y,label=cramV))+
  scale_fill_gradient(low="red", high="yellow")+
  theme_classic()

#######################################################################################

##split
# Resulting bins have an equal number of observations in each group
datainputreduced[, "wt2"] <- bin_data(datainputreduced$NbrYearRelation, bins=3, binType = "quantile")

#creating indices
trainIndex <- createDataPartition(datainputreduced$loan_default,p=0.3,list=FALSE)

#splitting data into training/testing data using the trainIndex object
training1_TRAIN <- datainputreduced[trainIndex,] #training data (75% of data)
training1_TEST <- datainputreduced[-trainIndex,] #testing data (25% of data)


var_simple_glm = reformulate(termlabels = c("ltv",
                                            "disbursed_amount",
                                            "Employment.Type",
                                            #"PRIMARY.INSTAL.AMT",
                                            "asset_cost",
                                            "Current_pincode_ID",
                                            "Employee_code_ID",
                                            "PERFORM_CNS.SCORE.DESCRIPTION",
                                            "PRI.CURRENT.BALANCE",
                                            #"PRI.SANCTIONED.AMOUNT",
                                            #"PRI.DISBURSED.AMOUNT",
                                            #"NbrYear",
                                            "wt2"
                                            ), 
                             response = "loan_default")



simple_logit_model = glm(var_simple_glm, data = training1_TRAIN , family = binomial(link = "logit"))



training1_TEST$Score = predict(simple_logit_model, newdata = training1_TEST, type = "response")
test_roc_simple = roc(training1_TEST$loan_default ~ training1_TEST$Score, plot = TRUE, print.auc = TRUE)

summary(simple_logit_model) # display results

#decouper mon score par decile et croiser avec le taux de def et faire rep graphique
#cout de bon classement et mauvais classement (risque d'être conservateur ) erreur type 1 & type2 

