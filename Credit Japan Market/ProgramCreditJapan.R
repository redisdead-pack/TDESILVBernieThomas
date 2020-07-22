# load useful libraries
library(pacman)
library(tidyverse)
library(skimr)
library(dplyr)
library(caret)
library(pROC)
library(recipes) # could also load the tidymodels package
library(corrplot)
library(lsr)
library(ggcorrplot)
library(Amelia)

#seed for replication
set.seed(7)

# set up so that all variables of tibbles are printed
options(dplyr.width = Inf)

###--------------------------------------------------------###
###------------------------LOAD DATA-----------------------###
#make the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
list.files('./Data')

datainput = read_delim('./Data/crx.data', col_names = FALSE, delim =  ',', trim_ws = TRUE, na = c("", "NA", "?"))

#display information
names(datainput)
skim(datainput)
#Connaitre le nombre de valeurs différentes dans chaque colonnes
sapply(datainput, function(x) length(unique(x)))
missmap(datainput, main = "Missing values vs observed")

#rename 
colnames(datainput) <- c("X1",  "X2",  "X3",  "X4",  "X5",  "X6",  "X7",  "X8",  "X9",  "X10",
                         "X11", "X12", "X13", "X14", "X15", "Approval")


#transform (https://juba.github.io/tidyverse/07-import.html )
# (https://juba.github.io/tidyverse/09-recodages.html)
# (http://larmarange.github.io/analyse-R/recodage.html)
datainput <- datainput %>%   
  mutate(Approval = recode(Approval , `+` = 1L, `-` = 0L, .default = 1L)) %>%
  mutate(X11 = as.integer(X11)) %>%
  mutate(X14 = as.integer(X14))

skim(datainput)
###--------------------------------------------------------###



###--------------------------------------------------------###
###----------------- Analyse of correlation ---------------###
datainputcor <- datainput  %>%
  select (-c(Approval))

# function to get chi square p value and Cramers V
fCramerFunction = function(x,y) {
  tbl = datainputcor %>% select(x,y) %>% table()
  chisq_pval = round(chisq.test(tbl)$p.value, 4)
  cramV = round(cramersV(tbl), 4) 
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


###--------------------------------------------------------###

###--------------------------------------------------------###
###----------------------MODELING--------------------------###
#start preparing

#Option 2. Supprimer les lignes dont la valeur de cette colonne est NA
datainput <- subset(datainput,
                    !is.na(datainput$X1))
datainput <- subset(datainput,
                    !is.na(datainput$X2))
datainput <- subset(datainput,
                    !is.na(datainput$X14))
datainput <- subset(datainput,
                    !is.na(datainput$X7))
datainput <- subset(datainput,
                    !is.na(datainput$X6))
datainput <- subset(datainput,
                    !is.na(datainput$X5))
datainput <- subset(datainput,
                    !is.na(datainput$X7))
missmap(datainput, main = "Missing values vs observed")

rec<-recipe(~.,data = datainput)%>% 
  add_role(Approval,new_role = 'outcome') %>% 
  remove_role(Approval, old_role='predictor') %>% 
  prep(retain=T)

training1 = juice(rec)

#creating indices
trainIndex <- createDataPartition(training1$Approval,p=0.75,list=FALSE)

#splitting data into training/testing data using the trainIndex object
training1_TRAIN <- training1[trainIndex,] #training data (75% of data)
training1_TEST <- training1[-trainIndex,] #testing data (25% of data)

summary(rec)

##--------------- logistic regression--------------------##
#ALL Datas
var_simple_glm = c("X1",  "X2",  "X3",  "X4",  "X6",  "X7",  "X8",  "X9",
                   "X11", "X12", "X13", "X14", "X15")
model_glm = glm(reformulate(termlabels = var_simple_glm, response = "Approval"),
                data = training1_TRAIN, maxit=50)
summary(model_glm) # display results
exp(coef(model_glm)) # exponentiated coefficients

predict(model_glm, type="response") # predicted values
residuals(model_glm, type="deviance") # residuals

test_prob = round(predict(model_glm, newdata = training1_TEST, type = "response"), 0) 
test_roc = roc(training1_TEST$Approval ~ test_prob, plot = TRUE, print.auc = TRUE)

##Simple model
var_simple_glm = c("X6","X9","X11", "X14")
simple_logit_model = glm(reformulate(termlabels = var_simple_glm, response = "Approval"),
                         data = training1_TRAIN, family = binomial(link = "logit"))
test_prob_simple = round(predict(simple_logit_model, newdata = training1_TEST, type = "response"), 0) 
test_roc_simple = roc(training1_TEST$Approval ~ test_prob_simple, plot = TRUE, print.auc = TRUE)

anova(model_glm, var_simple_glm, test="Chisq")


##--------------- Other Model --------------------##
library(randomForest)
model_random_forest <- randomForest(Approval ~ ., data=training1_TRAIN, maxnodes=5, ntree=30)
print(model_random_forest)
test_prob_rf = predict(model_random_forest, newdata = training1_TEST, type = "response")
test_roc_simple = roc(training1_TEST$Approval ~ test_prob_rf, plot = TRUE, print.auc = TRUE)
###--------------------------------------------------------###