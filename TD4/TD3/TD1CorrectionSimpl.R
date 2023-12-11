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


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
datainput = read_delim('./data/trainTD3Simpl.csv', col_names = TRUE, delim = ',')
skim(datainput)

datainput <- datainput %>% 
  dplyr::rename("AAAloan_default" = "loan_default")


dataWrangled <- datainput  %>%
  mutate(Employment.Type = replace_na(datainput$Employment.Type, "None")) %>%
  mutate(State_ID =  as.factor(State_ID)) %>%
  mutate(AAAloan_default =  as.factor(AAAloan_default)) %>%
  mutate(VoterID_flag =  as.factor(VoterID_flag)) %>%
  mutate(supplier_id =  as.factor(supplier_id)) %>%
  mutate(manufacturer_id =  as.factor(manufacturer_id)) %>%
  mutate(Current_pincode_ID =  as.factor(Current_pincode_ID)) %>%
  mutate(MobileNo_Avl_Flag =  as.factor(MobileNo_Avl_Flag)) %>%
  mutate(Aadhar_flag =  as.factor(Aadhar_flag)) %>%
  mutate(Driving_flag =  as.factor(Driving_flag)) %>%
  mutate(Passport_flag =  as.factor(Passport_flag)) 

#Regroupement de modalit?s
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

#refresh de la variable avec les nouvelles modalit?s
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
  mutate(NbrMonthRelation = time_length(interval(start = dmy(DisbursalDate), end = today()), unit = "months"))

#liste des variables a discretiser : toutes sauf les id, la cible, et les variables non transformees
to_bin <- names(dataWrangled)
to_bin <- to_bin[!to_bin %in% c("AAAloan_default","UniqueID","Current_pincode_ID", "Date.of.Birth", "DisbursalDate", "Employee_code_ID", "AVERAGE.ACCT.AGE", "CREDIT.HISTORY.LENGTH")]

bins = woebin(dataWrangled[, c(to_bin,'AAAloan_default')], y = 'AAAloan_default',check_cate_num = F, bin_num_limit = 3)

woebin_plot(bins$ltv)$ltv
woebin_plot(bins$disbursed_amount)$disbursed_amount
woebin_plot(bins$PRI.CURRENT.BALANCE)$PRI.CURRENT.BALANCE
woebin_plot(bins$PRI.DISBURSED.AMOUNT)$PRI.DISBURSED.AMOUNT
woebin_plot(bins$PRIMARY.INSTAL.AMT)$PRIMARY.INSTAL.AMT
woebin_plot(bins$Employment.Type)$Employment.Type
woebin_plot(bins$AAA)$AAA
woebin_plot(bins$BorrowerAge)$BorrowerAge
woebin_plot(bins$PERFORM_CNS.SCORE.DESCRIPTION)$PERFORM_CNS.SCORE.DESCRIPTION
woebin_plot(bins$PRI.OVERDUE.ACCTS)$PRI.OVERDUE.ACCTS
woebin_plot(bins$CHL)$CHL
woebin_plot(bins$supplier_id)$supplier_id

dataWrangled = woebin_ply(dataWrangled, bins, to = 'bin')

## Remarque : l'algorithme regroupe parfois toutes les valeurs en une seule modalite
# Ce qui veut dire que la variable n'est pas discriminante
# On elimine ces variables

#variables discretisees 
binned <- paste0(to_bin,"_bin")
binned <- binned[binned %in% names(dataWrangled)]
nbval <-NULL
for (var in binned) {
  dt <- as.data.frame(dataWrangled)
  nbmod <- length(unique(dt[,var]))
  nbval <- rbind(nbval,cbind(var,nbmod))
}
to_drop <- nbval[nbval[,2] == 1,1]

dataWrangled <- select(dataWrangled,-c(to_drop))

binned <- binned[!binned %in% to_drop]
dataiv <- as.data.frame(dataWrangled)
dataiv <- dataiv[,c("AAAloan_default",binned)]

## Fonction de calcul d'un v de cramer
fCramerFunction = function(x,y) {
  #message(sprintf(" %s || %s", x, y))
  tbl = dataWrangled %>% select(x,y) %>% table()
  chisq_pval = round(chisq.test(tbl)$p.value, 2)
  cramV = round(cramer.v(tbl), 2) 
  data.frame(x, y, chisq_pval, cramV) }

# create unique combinations of column names
# sorting will help getting a better plot (upper triangular)
df_comb = data.frame(t(combn(sort(c(binned,"AAAloan_default")), 2)), stringsAsFactors = F)

# apply function to each variable combination
df_res = purrr::map2_df(df_comb$X1, df_comb$X2, fCramerFunction)

# plot results
df_res %>%
  #ggplot(aes(x,y,fill=chisq_pval))+
  ggplot(aes(x,y,fill=cramV))+
  geom_tile()+
  geom_text(aes(x,y,label=cramV))+
  scale_fill_gradient(low="white", high="red")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 60, hjust = 1))

## Selection des couples avec un V de Cramer des plus de 20%
couples_correl <- df_res[df_res$cramV > 0.3,]

## Dans les couples identifies : le defaut est int?gr?. On enl?ve les lignes contenant le defaut
couples_correl <- couples_correl[couples_correl$x != "AAAloan_default" & couples_correl$y != "AAAloan_default",]

#On isole les correlations avec le defaut
correl_def <- df_res[df_res$y == "AAAloan_default" | df_res$x == "AAAloan_default",]
correl_def$variable <- correl_def$x 
correl_def$variable[correl_def$y != "AAAloan_default"] <- correl_def$y[correl_def$y != "AAAloan_default"]
correl_def <- correl_def[,c("cramV","variable")]
names(correl_def) <- c("correl_def","variable")

#Table de selection : couples correl?s et lien avec le defaut de chaque membre du couple
couples_correl <- merge.data.frame(couples_correl,correl_def,by.x = "x",by.y = "variable",all.x = T)
couples_correl <- merge.data.frame(couples_correl,correl_def,by.x = "y",by.y = "variable",all.x = T)

## Selection parmi les couples
couples_correl$selection <- "x"
couples_correl$selection[couples_correl$correl_def.y > couples_correl$correl_def.x] <- "y"


#Liste des variables a eliminer du modele
dropx <- unique(couples_correl$x[couples_correl$selection=="y"])
dropy <-  unique(couples_correl$y[couples_correl$selection=="x"])
dropall <- unique(c(dropx,dropy))

#on enregistre les variables a maintenir
features <- binned[!binned %in% dropall]

# plot results
df_res_new <- df_res[df_res$x %in% c("AAAloan_default",features) & df_res$y %in% c("AAAloan_default",features),]

df_res_new %>% 
  #ggplot(aes(x,y,fill=chisq_pval))+
  ggplot(aes(x,y,fill=cramV))+
  geom_tile()+
  geom_text(aes(x,y,label=cramV))+
  scale_fill_gradient(low="white", high="red")+
  theme_classic()+ theme(axis.text.x = element_text(angle = 60, hjust = 1))

dataWrangled$branch_id_bin <- as.factor(dataWrangled$branch_id_bin)
levels(dataWrangled$branch_id_bin) <- paste0("branch_group",1:length(levels(dataWrangled$branch_id_bin)))
dataWrangled$branch_id_bin <- factor(dataWrangled$branch_id_bin)

dataWrangled$supplier_id_bin <- as.factor(dataWrangled$supplier_id_bin)
levels(dataWrangled$supplier_id_bin) <- paste0("supplier_group",1:length(levels(dataWrangled$supplier_id_bin)))
dataWrangled$supplier_id_bin <- factor(dataWrangled$supplier_id_bin)

dataWrangled$manufacturer_id_bin <- as.factor(dataWrangled$manufacturer_id_bin)
levels(dataWrangled$manufacturer_id_bin) <- paste0("manuf_group",1:length(levels(dataWrangled$manufacturer_id_bin)))
dataWrangled$manufacturer_id_bin <- factor(dataWrangled$manufacturer_id_bin)

dataWrangled$State_ID_bin <- as.factor(dataWrangled$State_ID_bin)
levels(dataWrangled$State_ID_bin) <- paste0("state_group",1:length(levels(dataWrangled$State_ID_bin)))
dataWrangled$State_ID_bin <- factor(dataWrangled$State_ID_bin)

#creating indices
trainIndex <- createDataPartition(dataWrangled$AAAloan_default,p=0.75,list=FALSE)

#splitting data into training/testing data using the trainIndex object
training1_TRAIN <- dataWrangled[trainIndex,] #training data (75% of data)
training1_TEST <- dataWrangled[-trainIndex,] #testing data (25% of data)


#creation d'une formule du type defaut ~ var1+var2...
var_simple_glm = reformulate(termlabels = c("BorrowerAge_bin",
                                            "disbursed_amount_bin",
                                            "PERFORM_CNS.SCORE.DESCRIPTION_bin", 
                                            "supplier_id_bin", 
                                            "Employment.Type_bin",
                                            "State_ID_bin"), 
                             response = "AAAloan_default")


#ajustement de la r?gression
simple_logit_model = glm(var_simple_glm, data = training1_TRAIN , family = binomial(link = "logit"))


#calcul de la probabilit? (ensuite nomm?e score) sur la base test
training1_TEST$Score = predict(simple_logit_model, newdata = training1_TEST, type = "response")

#Calcul d'une courbe roc
test_roc_simple = roc(training1_TEST$AAAloan_default ~ training1_TEST$Score, plot = TRUE, print.auc = TRUE)

#Coefficients de la r?gression
summary(simple_logit_model) # display results