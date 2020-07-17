# load useful libraries
library(pacman)
library(tidyverse)
library(skimr)
library(dplyr)
library(caret)
library(pROC)
library(recipes) # could also load the tidymodels package
library(corrplot)

#seed for replication
set.seed(7)

# set up so that all variables of tibbles are printed
options(dplyr.width = Inf)

#make the working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
list.files('./Data')

datainput = read_delim('./Data/australian.dat', col_names = FALSE, delim = ' ')

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

#Analyse of correlation
datainputcor <- datainput  %>%
  select (-c(Approval))
mcor <- cor(datainputcor)
symnum(mcor, abbr.colnames=FALSE)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)
# Générer des couleurs
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = mcor, col = col, symm = TRUE)

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
