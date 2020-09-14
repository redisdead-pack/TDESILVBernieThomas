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
library(logiBin)

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

#detecter outliers https://statistique-et-logiciel-r.com/comment-detecter-les-outliers-avec-r/
#https://www.r-bloggers.com/how-to-remove-outliers-in-r/

#https://www.kaggle.com/arpitjain007/logistic-regression

datainputreduced <- datainput  %>%
  select (
          ltv,
          disbursed_amount,
          NO.OF_INQUIRIES,
          DisbursalDate,
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
  mutate(Employment.Type = replace_na(datainput$Employment.Type, "None")) %>%
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
  mutate(BorrowerAge = thisyear-year(dmy(Date.of.Birth))) 
datainputreduced <- datainputreduced %>%
  mutate(NbrMonthRelation = 12*(thisyear-year(dmy(DisbursalDate)) + month(dmy(DisbursalDate)))) 

partition(skim(datainputreduced))

##############################################################
###--------------------------------------------------------###
###----------------- Analyse univarié ---------------###
hist(datainputreduced$BorrowerAge, main = "Age des clients", 
     xlab = "Age", ylab = "Effectif")
boxplot(datainputreduced$BorrowerAge, main = "Age des clients", 
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




##split
# Resulting bins have an equal number of observations in each group
datainputreduced[, "wt2"] <- bin_data(datainputreduced$NbrYearRelation, bins=3, binType = "quantile")
#https://nextjournal.com/eda/discretize-cont-var


######################################################################
## Calculate bin breaks for numeric variables with respect to their relationships with the outcome variable Churn
library(scorecard)
library(ggplot2)
library(ggplotify)
library(plotly)

bins = woebin(datainputreduced[, c('ltv', 
                                   'disbursed_amount',
                                   'asset_cost',
                                   'loan_default',
                                   'PRI.CURRENT.BALANCE',
                                   'PRI.SANCTIONED.AMOUNT',
                                   'PRI.DISBURSED.AMOUNT',
                                   'PRIMARY.INSTAL.AMT',
                                   'BorrowerAge')], y = 'loan_default',  method="tree")

## Visualize bins
woebin_plot(bins$ltv)$ltv
woebin_plot(bins$disbursed_amount)$disbursed_amount
woebin_plot(bins$PRI.CURRENT.BALANCE)$PRI.CURRENT.BALANCE
woebin_plot(bins$PRI.SANCTIONED.AMOUNT)$PRI.SANCTIONED.AMOUNT
woebin_plot(bins$PRI.DISBURSED.AMOUNT)$PRI.DISBURSED.AMOUNT
woebin_plot(bins$PRIMARY.INSTAL.AMT)$PRIMARY.INSTAL.AMT
woebin_plot(bins$BorrowerAge)$BorrowerAge

## Compare results with conditional probability density plot
ggplotly(ggplot(datainputreduced, aes_string(datainputreduced$ltv_bin, fill = datainputreduced$loan_default)) + 
           geom_density(position='fill', alpha = 0.5) + 
           xlab('ltv') + labs(fill='loan_default') +
           theme(legend.text=element_text(size=10), 
                 axis.title=element_text(size=10)))

datainputreduced = woebin_ply(datainputreduced, bins, to = 'bin')

#####################################################

##############################################################
###--------------------------------------------------------###
###----------------- Analyse of correlation ---------------###


# function to get chi square p value and Cramers V
fCramerFunction = function(x,y) {
  #message(sprintf(" %s || %s", x, y))
  tbl = datainputreduced %>% slice(1:7000) %>% select(x,y) %>% table()
  chisq_pval = round(chisq.test(tbl)$p.value, 2)
  cramV = round(cramer.v(tbl), 2) 
  data.frame(x, y, chisq_pval, cramV) }

allCombin <- data.frame(X1 = c("loan_default"),
                        X2 = names(datainputreduced))
# apply function to each variable combination
allCombin_res = map2_df(allCombin$X1, allCombin$X2, fCramerFunction)


features <- c("ltv_bin",
              "Employment.Type",
              "Employee_code_ID",
              "NO.OF_INQUIRIES",
              "asset_cost_bin",
              "Current_pincode_ID",
              "PERFORM_CNS.SCORE.DESCRIPTION",
              "PRI.SANCTIONED.AMOUNT_bin",
              "BorrowerAge_bin")

# create unique combinations of column names
# sorting will help getting a better plot (upper triangular)
df_comb = data.frame(t(combn(sort(c(features,"loan_default")), 2)), stringsAsFactors = F)

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

#creating indices
trainIndex <- createDataPartition(datainputreduced$loan_default,p=0.3,list=FALSE)

#splitting data into training/testing data using the trainIndex object
training1_TRAIN <- datainputreduced[trainIndex,] #training data (75% of data)
training1_TEST <- datainputreduced[-trainIndex,] #testing data (25% of data)


var_simple_glm = reformulate(termlabels = features, 
                             response = "loan_default")



simple_logit_model = glm(var_simple_glm, data = training1_TRAIN , family = binomial(link = "logit"))



training1_TEST$Score = predict(simple_logit_model, newdata = training1_TEST, type = "response")
test_roc_simple = roc(training1_TEST$loan_default ~ training1_TEST$Score, plot = TRUE, print.auc = TRUE)

summary(simple_logit_model) # display results

#decouper mon score par decile et croiser avec le taux de def et faire rep graphique
#cout de bon classement et mauvais classement (risque d'être conservateur ) erreur type 1 & type2 

