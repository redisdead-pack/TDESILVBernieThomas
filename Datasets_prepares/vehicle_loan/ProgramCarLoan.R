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
library(ggstatsplot)
library(gridExtra)
#seed for replication
set.seed(7)







# set up so that all variables of tibbles are printed
options(dplyr.width = Inf)

#make the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
list.files('./data')

datainput = read_delim('./data/trainRed.csv', col_names = TRUE, delim = ',')
#write.csv(datainput[createDataPartition(dataWrangled$loan_default,p=0.1,list=FALSE),],'./data/trainRed.csv')



#detecter outliers https://statistique-et-logiciel-r.com/comment-detecter-les-outliers-avec-r/
#https://www.r-bloggers.com/how-to-remove-outliers-in-r/

#https://www.kaggle.com/arpitjain007/logistic-regression

dataWrangled <- datainput  %>%
  mutate(Employment.Type = replace_na(datainput$Employment.Type, "None")) %>%
  mutate(State_ID =  as.factor(State_ID)) %>%
  mutate(branch_id =  as.factor(branch_id)) %>%
  mutate(loan_default =  as.factor(loan_default)) %>%
  mutate(VoterID_flag =  as.factor(VoterID_flag)) 


summary(as.factor(dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION))
        
dataWrangled <- dataWrangled %>%
  mutate(AAA = as.integer((as.numeric(str_match_all( datainput$AVERAGE.ACCT.AGE,"\\d+(?=yrs)")) *12 
         + as.numeric(str_match_all( datainput$AVERAGE.ACCT.AGE,"\\d+(?=mon)"))))) %>%
  mutate(CHL = (as.numeric(str_match_all( datainput$CREDIT.HISTORY.LENGTH,"\\d+(?=yrs)")) *12 
         + as.numeric(str_match_all( datainput$CREDIT.HISTORY.LENGTH,"\\d+(?=mon)"))))
  

summary(as.factor(dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION))

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
summary(as.factor(dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION))

thisyear = year(Sys.Date())
dataWrangled <- dataWrangled %>%
  mutate(BorrowerAge = thisyear-year(dmy(Date.of.Birth))) 
dataWrangled <- dataWrangled %>%
  mutate(NbrMonthRelation = 12*(thisyear-year(dmy(DisbursalDate)) + month(dmy(DisbursalDate)))) 

partition(skim(dataWrangled))

##############################################################
###--------------------------------------------------------###
###----------------- Analyse univarié ---------------###
hist(dataWrangled$BorrowerAge, main = "Age des clients", 
     xlab = "Age", ylab = "Effectif")
boxplot(dataWrangled$BorrowerAge, main = "Age des clients", 
        ylab = "Age")
#le truc en matrix de kaggle


datainput%>%
  select (ltv,
          disbursed_amount,
          Date.of.Birth,
          supplier_id,
          Employment.Type,
          PERFORM_CNS.SCORE,
          PERFORM_CNS.SCORE.DESCRIPTION,
          loan_default,
          AVERAGE.ACCT.AGE,
          CREDIT.HISTORY.LENGTH)%>%
  select_if(is.numeric) %>%
  gather(cols, value) %>%
  ggplot(aes(x = value)) + geom_histogram() + facet_wrap( .~cols, scales = "free"  ,ncol = 2)

plot(density(dataWrangled$BorrowerAge, na.rm = TRUE), main = "Age de l'emprunteur")
#http://www.sthda.com/french/wiki/ggplot2-combiner-plusieurs-graphiques-sur-la-m-me-page-logiciel-r-et-visualisation-de-donn-es

boxplot(dataWrangled$BorrowerAge, col = grey(0.8), main = "Age de l'emprunteur", 
              ylab = "Heures")
abline(h = median(dataWrangled$BorrowerAge, na.rm = TRUE), col = "navy", lty = 2)
text(1.35, median(dataWrangled$BorrowerAge, na.rm = TRUE) + 0.15, "Médiane", 
     col = "navy")
Q1 <- quantile(dataWrangled$BorrowerAge, probs = 0.25, na.rm = TRUE)
abline(h = Q1, col = "darkred")
text(1.35, Q1 + 0.15, "Q1 : premier quartile", col = "darkred", 
     lty = 2)
Q3 <- quantile(dataWrangled$BorrowerAge, probs = 0.75, na.rm = TRUE)
abline(h = Q3, col = "darkred")
text(1.35, Q3 + 0.15, "Q3 : troisième quartile", col = "darkred", 
     lty = 2)
arrows(x0 = 0.7, y0 = quantile(dataWrangled$BorrowerAge, probs = 0.75, na.rm = TRUE), 
       x1 = 0.7, y1 = quantile(dataWrangled$BorrowerAge, probs = 0.25, na.rm = TRUE), 
       length = 0.1, code = 3)
text(0.7, Q1 + (Q3 - Q1)/2 + 0.15, "h", pos = 2)
mtext("L'écart inter-quartile h contient 50 % des individus", 
      side = 1)
abline(h = Q1 - 1.5 * (Q3 - Q1), col = "darkgreen")
text(1.35, Q1 - 1.5 * (Q3 - Q1) + 0.15, "Q1 -1.5 h", col = "darkgreen", 
     lty = 2)
abline(h = Q3 + 1.5 * (Q3 - Q1), col = "darkgreen")
text(1.35, Q3 + 1.5 * (Q3 - Q1) + 0.15, "Q3 +1.5 h", col = "darkgreen", 
     lty = 2)

bx <- ggplot2::ggplot(dataWrangled, ggplot2::aes(x=BorrowerAge, color=BorrowerAge)) +
  ggplot2::geom_boxplot() + 
  theme(legend.position = "none")


bxDef <- ggplot2::ggplot(dataWrangled, ggplot2::aes(x=BorrowerAge, y = loan_default)) +
  ggplot2::geom_boxplot() + 
  theme(legend.position = "none")

dpDef <- ggplot(dataWrangled, aes(x=BorrowerAge, y = loan_default )) +
  geom_violin()+
  geom_boxplot(width=0.1)
grid.arrange( bx, bxDef, dpDef ,ncol=2)

# library(Hmisc)
# hist.data.frame(datainput, nclass = "compute")
##############################################################
###--------------------------------------------------------###
###----------------- Analyse of correlation ---------------###

#cas du score et regroupement
# https://juba.github.io/tidyverse/04-bivarie.html#croisement-de-deux-variables-qualitatives
tcd <- table(datainput$PERFORM_CNS.SCORE.DESCRIPTION, datainput$loan_default)
lprop(tcd)
tcd <- table(dataWrangled$PERFORM_CNS.SCORE.DESCRIPTION, dataWrangled$loan_default)
lprop(tcd)

chisq.test(tcd)
chisq.residuals(tcd)
cramer.v(tcd)

#faire graph distrub supperposés 0-1 sur variavle continue et significative / non signif




##split
# Resulting bins have an equal number of observations in each group
dataWrangled[, "wt2"] <- bin_data(dataWrangled$NbrYearRelation, bins=3, binType = "quantile")
#https://nextjournal.com/eda/discretize-cont-var


######################################################################
## Calculate bin breaks for numeric variables with respect to their relationships with the outcome variable Churn
library(scorecard)
library(ggplot2)
library(ggplotify)
library(plotly)

bins = woebin(dataWrangled[, c('ltv', 
                                   'disbursed_amount',
                                   'asset_cost',
                                   'loan_default',
                                   'PRI.CURRENT.BALANCE',
                                   'PRI.SANCTIONED.AMOUNT',
                                   'PRI.DISBURSED.AMOUNT',
                                   'PERFORM_CNS.SCORE.DESCRIPTION',
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
woebin_plot(bins$PERFORM_CNS.SCORE.DESCRIPTION)$PERFORM_CNS.SCORE.DESCRIPTION

## Compare results with conditional probability density plot
ggplotly(ggplot(dataWrangled, aes_string(dataWrangled$ltv_bin, fill = dataWrangled$loan_default)) + 
           geom_density(position='fill', alpha = 0.5) + 
           xlab('ltv') + labs(fill='loan_default') +
           theme(legend.text=element_text(size=10), 
                 axis.title=element_text(size=10)))

dataWrangled = woebin_ply(dataWrangled, bins, to = 'bin')

#####################################################

##############################################################
###--------------------------------------------------------###
###----------------- Analyse of correlation ---------------###


# function to get chi square p value and Cramers V
fCramerFunction = function(x,y) {
  #message(sprintf(" %s || %s", x, y))
  tbl = dataWrangled %>% slice(1:7000) %>% select(x,y) %>% table()
  chisq_pval = round(chisq.test(tbl)$p.value, 2)
  cramV = round(cramer.v(tbl), 2) 
  data.frame(x, y, chisq_pval, cramV) }

allCombin <- data.frame(X1 = c("loan_default"),
                        X2 = names(dataWrangled))
# apply function to each variable combination
allCombin_res = map2_df(allCombin$X1, allCombin$X2, fCramerFunction)

features <- c(#"supplier_id",
"ltv_bin",
"PERFORM_CNS.SCORE.DESCRIPTION_bin",
"disbursed_amount_bin",
"State_ID",
"PRI.OVERDUE.ACCTS",
"NO.OF_INQUIRIES",
"DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS",
#"PRI.CURRENT.BALANCE_bin",
"PRI.SANCTIONED.AMOUNT_bin",
#"PRI.DISBURSED.AMOUNT_bin",
"BorrowerAge_bin")
#"PERFORM_CNS.SCORE")

# features <- c("ltv_bin",
#               "disbursed_amount_bin",
#               "Employment.Type",
#               "Employee_code_ID",
#               #"NO.OF_INQUIRIES",
#               "asset_cost_bin",
#               "Current_pincode_ID",
#               "PERFORM_CNS.SCORE.DESCRIPTION",
#               "PRI.SANCTIONED.AMOUNT_bin",
#               "BorrowerAge_bin")

dataWrangled <- dataWrangled %>%
  mutate(as.factor(State_ID)) %>%
  mutate(as.factor(Employee_code_ID)) %>%
  mutate(as.factor(ltv_bin)) %>%
  mutate(as.factor(disbursed_amount_bin)) %>%
  mutate(as.factor(asset_cost_bin)) %>%
  mutate(as.factor(PERFORM_CNS.SCORE.DESCRIPTION_bin)) %>%
  mutate(as.factor(PRI.SANCTIONED.AMOUNT_bin)) %>%
  mutate(as.factor(BorrowerAge_bin)) 

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
trainIndex <- createDataPartition(dataWrangled$loan_default,p=0.80,list=FALSE)

#splitting data into training/testing data using the trainIndex object
training1_TRAIN <- dataWrangled[trainIndex,] #training data (75% of data)
training1_TEST <- dataWrangled[-trainIndex,] #testing data (25% of data)


var_simple_glm = reformulate(termlabels = features, 
                             response = "loan_default")



simple_logit_model = glm(var_simple_glm, data = training1_TRAIN , family = binomial(link = "logit"))



training1_TEST$Score = predict(simple_logit_model, newdata = training1_TEST, type = "response")
test_roc_simple = roc(training1_TEST$loan_default ~ training1_TEST$Score, plot = TRUE, print.auc = TRUE)

summary(simple_logit_model) # display results
#Il faudrait faire un hist entre les bons et les mauvais http://www.sthda.com/french/wiki/ggplot2-histogramme-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees
#decouper mon score par decile et croiser avec le taux de def et faire rep graphique
#cout de bon classement et mauvais classement (risque d'être conservateur ) erreur type 1 & type2 

