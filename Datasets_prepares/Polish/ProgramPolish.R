# load useful libraries
library(pacman)
library(tidyverse)
library(skimr)
library(dplyr)
library(caret)
library(pROC)
library(recipes) # could also load the tidymodels package
library(corrplot)

library(data.table) # data mgmt
library(gtools) # combination
library(ggplot2) # graphics
library(plotly) # interactive graphics

#seed for replication
set.seed(7)

# set up so that all variables of tibbles are printed
options(dplyr.width = Inf)

#make the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
list.files('./data')

datainput = read_delim('./data/polish_data.csv', col_names = TRUE, delim = ';')
dicoheader = read_delim('./data/dico_ratio.csv', col_names = TRUE, delim = ';')
colnames <- names(datainput)

#rename header
# for (i in 1:length(names(datainput))){
#   names(datainput)[names(datainput) == as.character(dicoheader[i,2])] <- as.character(dicoheader[i,1])
# }

#display information
names(datainput)
partition(skim(datainput))

#rename
colnames(datainput) <- c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","Approval")
# datainput <- datainput  %>%
#   mutate(X4 = as.character(X4)) %>%
#   mutate(X6 = as.character(X6)) %>%
#   mutate(X4 = recode(X4 , `1` = "p", `2` = "g", `3` = "gg", .default = "D")) %>%
#   mutate(X6 = recode(X6 ,`1` = "ff", `2` = "dd", `3` = "j", `4` = "bb", `5` = "v", `6` = "n", `7` = "o", `8` = "h", `9` = "z", .default = "D"))



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

#if you want to have a genuine correlation plot for factors or mixed-type, you can also use model.matrix 
#to one-hot encode all non-numeric variables. This is quite different than calculating Cramér's V as 
#it will consider your factor as separate variables, as many regression models do.
model.matrix(~0+., data=datainputcor) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
###--------------------------------------------------------###




#start preparing
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


model_glm = glm(Approval ~., 
                data = training1_TRAIN, 
                family = "binomial")
summary(model_glm) # display results
confint(model_glm) # 95% CI for the coefficients
exp(coef(model_glm)) # exponentiated coefficients
exp(confint(model_glm)) # 95% CI for exponentiated coefficients
predict(model_glm, type="response") # predicted values
residuals(model_glm, type="deviance") # residuals

test_prob = round(predict(model_glm, newdata = training1_TEST, type = "response"), 0) 
test_roc = roc(training1_TEST$Approval ~ test_prob, plot = TRUE, print.auc = TRUE)
