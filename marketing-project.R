#setwd("~/Downloads/marketing")
mydata <- read.csv('D://SCU//Quarter 2//Marketing Analytics//New 2020 files//ForStudents_MarketingAnalytics_2020//Marketing Analytics Project//bank-additional-full.csv')

#install.packages("tidyverse")
#install.packages('gmodels')
#install.packages('ggmosaic'  )
#install.packages('corrplot')
#install.packages('caret')
#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('C50')


library(gmodels) # Cross Tables [CrossTable()]
library(ggmosaic) # Mosaic plot with ggplot [geom_mosaic()]
library(corrplot) # Correlation plot [corrplot()]
library(ggpubr) # Arranging ggplots together [ggarrange()]
library(cowplot) # Arranging ggplots together [plot_grid()]
library(caret) # ML [train(), confusionMatrix(), createDataPartition(), varImp(), trainControl()]
library(ROCR) # Model performance [performance(), prediction()]
library(plotROC) # ROC Curve with ggplot [geom_roc()]
library(pROC) # AUC computation [auc()]
library(PRROC) # AUPR computation [pr.curve()]
library(rpart) # Decision trees [rpart(), plotcp(), prune()]
library(rpart.plot) # Decision trees plotting [rpart.plot()]
library(ranger) # Optimized Random Forest [ranger()]
#library(lightgbm) # Light GBM [lgb.train()]
library(xgboost) # XGBoost [xgb.DMatrix(), xgb.train()]
library(MLmetrics) # Custom metrics (F1 score for example)
library(tidyverse) # Data manipulation
#library(doMC) # Parallel processing
#registerDoMC(cores = 10)

library(gmodels)
library(tidyverse)
library(ggplot2)
library('stargazer')
library(ggmosaic)
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)
library(C50)

# default theme for ggplot
theme_set(theme_bw())

# setting default parameters for mosaic plots
mosaic_theme = theme(axis.text.x = element_text(angle = 90,
                                                hjust = 1,
                                                vjust = 0.5),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())

# setting default parameters for crosstables
fun_crosstable = function(df, var1, var2){
  # df: dataframe containing both columns to cross
  # var1, var2: columns to cross together.
  CrossTable(df[, var1], df[, var2],
             prop.r = T,
             prop.c = F,
             prop.t = F,
             prop.chisq = F,
             dnn = c(var1, var2))
}

# plot weighted lm/leoss regressions with frequencies
fun_gg_freq = function(var){
  # var: which column from bank_data to use in regressions
  
  # computing weights first...
  weight = table(bank_data[, var]) %>% 
    as.data.frame %>% 
    mutate(x = as.numeric(as.character(Var1))) %>% 
    select(-Var1) %>% 
    rename(weight = Freq)
  
  # ... then frequencies
  sink(tempfile())
  freq = fun_crosstable(bank_data, var, "y")$prop.r %>% 
    as.data.frame %>% 
    mutate(x = as.numeric(as.character(x)))
  sink()
  
  # assembling
  both = freq %>% 
    left_join(weight, by = "x") %>% 
    filter(weight > 50 & y == 1)
  
  # plotting
  gg = both %>% 
    ggplot() +
    aes(x = x,
        y = Freq,
        weight = weight) +
    geom_point(aes(size = weight)) +
    geom_smooth(aes(colour = "blue"), method = "loess") +
    geom_smooth(aes(colour = "red"), method = "lm", se = F) +
    coord_cartesian(ylim = c(-0.1, 1)) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "pt")) +
    xlab(var) +
    ylab("") +
    scale_x_continuous(position = "top") +
    scale_colour_manual(values = c("blue", "red"),
                        labels = c("loess", "lm")) +
    labs(colour = "Regression")
  
  return(gg)
}

# re-ordering levels from factor variable
fun_reorder_levels = function(df, variable, first){
  # df: dataframe containing columns to transform into factors
  # variable: variable to transform into factor
  # first: first level of the variable to transform.
  
  remaining = unique(df[, variable])[which(unique(df[, variable]) != first)]
  x = factor(df[, variable], levels = c(first, remaining))
  return(x)
}

# plotting importance from predictive models into two panels
fun_imp_ggplot_split = function(model){
  # model: model used to plot variable importances
  
  if (class(model)[1] == "ranger"){
    imp_df = model$variable.importance %>% 
      data.frame("Overall" = .) %>% 
      rownames_to_column() %>% 
      rename(variable = rowname) %>% 
      arrange(-Overall)
  } else {
    imp_df = varImp(model) %>%
      rownames_to_column() %>% 
      rename(variable = rowname) %>% 
      arrange(-Overall)
  }
  
  # first panel (half most important variables)
  gg1 = imp_df %>% 
    slice(1:floor(nrow(.)/2)) %>% 
    ggplot() +
    aes(x = reorder(variable, Overall), weight = Overall, fill = -Overall) +
    geom_bar() +
    coord_flip() +
    xlab("Variables") +
    ylab("Importance") +
    theme(legend.position = "none")
  
  imp_range = ggplot_build(gg1)[["layout"]][["panel_params"]][[1]][["x.range"]]
  imp_gradient = scale_fill_gradient(limits = c(-imp_range[2], -imp_range[1]),
                                     low = "#132B43", 
                                     high = "#56B1F7")
  
  # second panel (less important variables)
  gg2 = imp_df %>% 
    slice(floor(nrow(.)/2)+1:nrow(.)) %>% 
    ggplot() +
    aes(x = reorder(variable, Overall), weight = Overall, fill = -Overall) +
    geom_bar() +
    coord_flip() +
    xlab("") +
    ylab("Importance") +
    theme(legend.position = "none") +
    ylim(imp_range) +
    imp_gradient
  
  # arranging together
  gg_both = plot_grid(gg1 + imp_gradient,
                      gg2)
  
  return(gg_both)
}

# plotting two performance measures
fun_gg_cutoff = function(score, obs, measure1, measure2) {
  # score: predicted scores
  # obs: real classes
  # measure1, measure2: which performance metrics to plot
  
  predictions = prediction(score, obs)
  performance1 = performance(predictions, measure1)
  performance2 = performance(predictions, measure2)
  
  df1 = data.frame(x = performance1@x.values[[1]],
                   y = performance1@y.values[[1]],
                   measure = measure1,
                   stringsAsFactors = F) %>% 
    drop_na()
  df2 = data.frame(x = performance2@x.values[[1]],
                   y = performance2@y.values[[1]],
                   measure = measure2,
                   stringsAsFactors = F) %>% 
    drop_na()
  
  # df contains all the data needed to plot both curves
  df = df1 %>% 
    bind_rows(df2)
  
  # extracting best cut for each measure
  y_max_measure1 = max(df1$y, na.rm = T)
  x_max_measure1 = df1[df1$y == y_max_measure1, "x"][1]
  
  y_max_measure2 = max(df2$y, na.rm = T)
  x_max_measure2 = df2[df2$y == y_max_measure2, "x"][1]
  
  txt_measure1 = paste("Best cut for", measure1, ": x =", round(x_max_measure1, 3))
  txt_measure2 = paste("Best cut for", measure2, ": x =", round(x_max_measure2, 3))
  txt_tot = paste(txt_measure1, "\n", txt_measure2, sep = "")
  
  # plotting both measures in the same plot, with some detail around.
  gg = df %>% 
    ggplot() +
    aes(x = x,
        y = y,
        colour = measure) +
    geom_line() +
    geom_vline(xintercept = c(x_max_measure1, x_max_measure2), linetype = "dashed", color = "gray") +
    geom_hline(yintercept = c(y_max_measure1, y_max_measure2), linetype = "dashed", color = "gray") +
    labs(caption = txt_tot) +
    theme(plot.caption = element_text(hjust = 0)) +
    xlim(c(0, 1)) +
    ylab("") +
    xlab("Threshold")
  
  return(gg)
}

# creating classes according to score and cut
fun_cut_predict = function(score, cut) {
  # score: predicted scores
  # cut: threshold for classification
  
  classes = score
  classes[classes > cut] = 1
  classes[classes <= cut] = 0
  classes = as.factor(classes)
  
  return(classes)  
}

# computing AUPR
aucpr = function(obs, score){
  # obs: real classes
  # score: predicted scores
  
  df = data.frame("pred" = score,
                  "obs" = obs)
  
  prc = pr.curve(df[df$obs == 1, ]$pred,
                 df[df$obs == 0, ]$pred)
  
  return(prc$auc.davis.goadrich)
}

# plotting PR curve
gg_prcurve = function(df) {
  # df: df containing models scores by columns and the last column must be
  #     nammed "obs" and must contain real classes.
  
  # init
  df_gg = data.frame("v1" = numeric(), 
                     "v2" = numeric(), 
                     "v3" = numeric(), 
                     "model" = character(),
                     stringsAsFactors = F)
  
  # individual pr curves
  for (i in c(1:(ncol(df)-1))) {
    x1 = df[df$obs == 1, i]
    x2 = df[df$obs == 0, i]
    prc = pr.curve(x1, x2, curve = T)
    
    df_prc = as.data.frame(prc$curve, stringsAsFactors = F) %>% 
      mutate(model = colnames(df)[i])
    
    # combining pr curves
    df_gg = bind_rows(df_gg,
                      df_prc)
    
  }
  
  gg = df_gg %>% 
    ggplot() +
    aes(x = V1, y = V2, colour = model) +
    geom_line() +
    xlab("Recall") +
    ylab("Precision")
  
  return(gg)
}

stargazer(mydata, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

mydata$y <- ifelse(mydata$y == "no",0,1)
mydata$previous <- ifelse(mydata$previous == 0,0,1)

#Summary statistics / Checking variables to be included in the model

colSums(is.na(mydata)) #No Na's in the data
summary(mydata) 

# age
mydata$age <- ifelse(mydata$age > 47, 'high',ifelse(mydata$age < 32, 'low', 'mid'))

table(mydata$age)

age_table <- table(mydata$age, mydata$y)
age_tab <- as.data.frame(prop.table(age_table, 1))
colnames(age_tab) <-  c("age", "y", "perc")


ggplot(data = age_tab, aes(x = age, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', alpha = 2/3) + 
  xlab("Age")+
  ylab("Percent")


#married
marriage_table <- table(mydata$marital, mydata$y)
marriage_tab <- as.data.frame(prop.table(marriage_table, 1))
colnames(marriage_tab) <-  c("marital", "y", "perc")

ggplot(data = marriage_tab, aes(x = marital, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', alpha = 2/3) + 
  xlab("Marital")+
  ylab("Percent")

#previous

previous_table <- table(mydata$previous, mydata$y)
previous_tab <- as.data.frame(prop.table(previous_table, 1))
colnames(previous_tab) <-  c("previous", "y", "perc")

ggplot(data = previous_tab, aes(x = previous, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', alpha = 2/3) + 
  xlab("previous")+
  ylab("Percent")

#pdays

#Creating to dummy variable
mydata <- mydata %>% 
  mutate(pdays_dummy = if_else(pdays == 999, "0", "1"))

pdays_table <- table(mydata$pdays_dummy, mydata$y)
pdays_tab <- as.data.frame(prop.table(pdays_table, 1))
colnames(pdays_tab) <-  c("marital", "y", "perc")

ggplot(data = pdays_tab, aes(x = marital, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', alpha = 2/3) + 
  xlab("Pdays_Dummy")+
  ylab("Percent")

#Education variable:
education_table <- table(mydata$education, mydata$y)
education_tab <- as.data.frame(prop.table(education_table, 1))
colnames(education_tab) <-  c("education", "y", "perc")

ggplot(data = education_tab, aes(x = education, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', alpha = 2/3) + 
  xlab("education")+
  ylab("Percent")


#Job
job_table <- table(mydata$job, mydata$y)
job_tab <- as.data.frame(prop.table(job_table, 1))
colnames(job_tab) <-  c("job", "y", "perc")

ggplot(data = job_tab, aes(x = job, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', alpha = 2/3) + 
  xlab("job")+
  ylab("Percent")

#Month
month_table <- table(mydata$month, mydata$y)
month_tab <- as.data.frame(prop.table(month_table, 1))
colnames(month_tab) <-  c("month", "y", "perc")

ggplot(data = month_tab, aes(x = month, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', alpha = 2/3) + 
  xlab("month")+
  ylab("Percent")

#Loan - (Not to be included in the model)
loan_table <- table(mydata$loan, mydata$y)
loan_tab <- as.data.frame(prop.table(loan_table, 1))
colnames(loan_tab) <-  c("loan", "y", "perc")

ggplot(data = loan_tab, aes(x = loan, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', alpha = 2/3) + 
  xlab("loan")+
  ylab("Percent")

#Housing - (Not to be included in the model)
housing_table <- table(mydata$housing, mydata$y)
housing_tab <- as.data.frame(prop.table(housing_table, 1))
colnames(housing_tab) <-  c("housing", "y", "perc")

ggplot(data = housing_tab, aes(x = housing, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', alpha = 2/3) + 
  xlab("housing")+
  ylab("Percent")

#Campaign
mydata$campaign <- ifelse(mydata$campaign > 3, 'high',ifelse(mydata$campaign <= 1, 'low', 'mid'))

table(mydata$campaign)

campaign_table <- table(mydata$campaign, mydata$y)
campaign_tab <- as.data.frame(prop.table(campaign_table, 1))
colnames(campaign_tab) <-  c("campaign", "y", "perc")

ggplot(data = campaign_tab, aes(x = campaign, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', alpha = 2/3) + 
  xlab("campaign")+
  ylab("Percent")

summary(mydata$duration)

#Duration
mydata$duration <- ifelse(mydata$duration > 180, 'high','low')

table(mydata$duration)

duration_table <- table(mydata$duration, mydata$y)
duration_tab <- as.data.frame(prop.table(duration_table, 1))
colnames(duration_tab) <-  c("duration", "y", "perc")

ggplot(data = duration_tab, aes(x = duration, y = perc, fill = y)) + 
  geom_bar(stat = 'identity',alpha = 2/3) + 
  xlab("duration")+
  ylab("Percent")


#Categorizing education variables
mydata$education <- ifelse(mydata$education == "basic.9y" | mydata$education == "basic.6y" | mydata$education == "basic.4y" | mydata$education == "illiterate","low",ifelse(mydata$education == "high.school"| mydata$education == "professional.course","mid",ifelse(mydata$education == "university.degree","high",'unknown')))

table(mydata$education)

#Categorized education plot
education_table <- table(mydata$education, mydata$y)
education_tab <- as.data.frame(prop.table(education_table, 1))
colnames(education_tab) <-  c("education", "y", "perc")

ggplot(data = education_tab, aes(x = education, y = perc, fill = y)) + 
  geom_bar(stat = 'identity', alpha = 2/3) + 
  xlab("education")+
  ylab("Percent")


# PT 1 Are there certain job types making more termed deposits? Were they targeted more?
#Model 1
modeljob <- glm(y ~ job+month+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m,data=mydata,family="binomial")

summary(modeljob)

stargazer(modeljob,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))


# PT 2  Did the clients have to be contacted multiple times before they made the deposit?
model2 <- glm(y ~ job+marital+education+default+contact+duration+poutcome+
                   pdays_dummy+campaign+month+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m,data=mydata,family="binomial")

summary(model2)
table(mydata$previous)
stargazer(modeljob,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))

## Model 3
model3 <- glm(y ~ education+default+contact+duration+poutcome+
                   pdays_dummy+month+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m,data=mydata,family="binomial")


summary(model3)

#Dont include-Model 4 with interaction1
model4 <- glm(y ~ education+default+contact+duration+poutcome+
                   poutcome*duration+pdays_dummy+campaign+month+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m,data=mydata,family="binomial")

summary(model4)
stargazer(model4,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))


#Model 5 - Final 
logistic_model <- glm(y ~ education+default+duration+poutcome+
                campaign*duration+pdays_dummy+campaign+month+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m,data=mydata,family="binomial")

summary(logistic_model)

fun_imp_ggplot_split(logistic_model)

#model without previous



# PT 3 How many contacts were made in the past for this client and what was the outcome of the previous campaigns? This would help answer how should those customers be contacted in the future
modelprevious <- glm(y ~ previous+age+job+marital+education+default+housing+loan+month+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m,data=mydata,family="binomial")

stargazer(modelprevious,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))


# PT4 Is there a particular pattern observed for variables such as marital status and education? Does having a particular marital status and education affect how term deposits are made?
modelmarried<- glm(y ~ marital+month+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m,data=mydata,family="binomial")

stargazer(modelmarried,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))

modeleducation<- glm(y ~ education+month+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m,data=mydata,family="binomial")

stargazer(modeleducation,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001))

#Train and test data

mydata = subset(mydata, select = -c(y.no))
head(mydata)


set.seed(1234)

ind = createDataPartition(mydata$y,
                          times = 1,
                          p = 0.8,
                          list = F)
bank_train = mydata[ind, ]
bank_test = mydata[-ind, ]

train_lm = glm(y ~ education+default+duration+poutcome+
                    campaign*duration+pdays_dummy+campaign+month+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m,data=bank_train,family="binomial")

summary(train_lm)

#Prediction scores
logistic_train_score = predict(train_lm,
                               newdata = bank_train,
                               type = "response")

logistic_test_score = predict(train_lm,
                              newdata = bank_test,
                              type = "response")

#cut identification
measure_train = fun_gg_cutoff(logistic_train_score, bank_train$y, 
                              "acc", "f")
measure_train + geom_vline(xintercept = c(0.2, 0.5), 
             linetype = "dashed")

#Defining threshold
logistic_train_cut = 0.25
logistic_train_class = fun_cut_predict(logistic_train_score, logistic_train_cut)
# matrix

logistic_train_confm = confusionMatrix(logistic_train_class, as.factor(bank_train$y), 
                                       positive = "1",
                                       mode = "everything")
logistic_train_confm

#Validation
measure_test = fun_gg_cutoff(logistic_test_score, bank_test$y, 
                             "acc", "f")
measure_test +
  geom_vline(xintercept = c(logistic_train_cut, 0.5), 
             linetype = "dashed")

#Test_data_accuracy
logistic_test_class = fun_cut_predict(logistic_test_score, logistic_train_cut)
# matrix
logistic_test_confm = confusionMatrix(logistic_test_class, as.factor(bank_test$y), 
                                      positive = "1",
                                      mode = "everything")
logistic_test_confm

#Decision Tree

modelLookup("rpart")

#


tune_grid = expand.grid(
  cp = seq(from = 0, to = 0.01, by = 0.001)
)

tune_control = trainControl(
  method = "cv", # cross-validation
  number = 10, # with n folds 
  summaryFunction = prSummary,
  verboseIter = FALSE, # no training log
  allowParallel = FALSE # FALSE for reproducible results 
)

rpart1_tune = train(
  as.factor(y) ~ education+default+duration+poutcome
  +pdays_dummy+campaign+month+emp.var.rate+
    cons.price.idx+cons.conf.idx+euribor3m,
  data = mydata,
  metric = "F",
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "rpart",
  control = rpart.control(minsplit=1600)
)


tree = rpart(y ~ education+default+duration+poutcome
             +pdays_dummy+campaign+month+emp.var.rate+
               cons.price.idx+cons.conf.idx+euribor3m ,
             data = bank_train,
             cp = rpart1_tune$bestTune)
rpart.plot(tree)


fun_imp_ggplot_split(tree)

######Trial##############
tune_grid1 = expand.grid(
  cp = seq(from = 0, to = 0.01, by = 0.001)
)

tune_control1 = trainControl(
  method = "cv", # cross-validation
  number = 10, # with n folds 
  summaryFunction = prSummary,
  verboseIter = FALSE, # no training log
  allowParallel = FALSE # FALSE for reproducible results 
)

rpart1_tune1 = train(
  as.factor(y) ~ .,
  data = mydata,
  metric = "F",
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "rpart",
  control = rpart.control(minsplit=1600)
)



tree1 = rpart(y ~ .,
             data = bank_train,
             cp = rpart1_tune$bestTune)
rpart.plot(tree1)


fun_imp_ggplot_split(tree1)


bank_train_2 = subset(bank_train, select = c(as.factor(y),education,default,duration,poutcome
                                             ,pdays_dummy,campaign,month,emp.var.rate,
                                             cons.price.idx,cons.conf.idx,euribor3m))
bank_test_2 = subset(bank_test, select = c(as.factor(y),education,default,duration,poutcome
                                           ,pdays_dummy,campaign,month,emp.var.rate,
                                           cons.price.idx,cons.conf.idx,euribor3m))


#Predicted Scores
tree_train_score = predict(tree, bank_train_2, type = "vector")

tree_test_score = predict(tree, bank_test_2, type = "vector")

#CUt identification
measure_train = fun_gg_cutoff(tree_train_score, bank_train$y, 
                              "acc", "f")
measure_train +
  geom_vline(xintercept = c(0.25, 0.5), 
             linetype = "dashed")

#COnfusion matrix - train data
tree_train_cut = 0.25
tree_train_class = fun_cut_predict(tree_train_score, tree_train_cut)

tree_train_confm = confusionMatrix(as.factor(tree_train_class), as.factor(bank_train$y), 
                                   positive = "1",
                                   mode = "everything")
tree_train_confm

#Validation
measure_test = fun_gg_cutoff(tree_test_score, bank_test$y, 
                             "acc", "f")
measure_test +
  geom_vline(xintercept = c(tree_train_cut, 0.5), 
             linetype = "dashed")

tree_test_class = fun_cut_predict(tree_test_score, tree_train_cut)
# matrix
tree_test_confm = confusionMatrix(as.factor(tree_test_class), as.factor(bank_test$y), 
                                  positive = "1",
                                  mode = "everything")
tree_test_confm

##Random forest ----- Optional--------------------###################

modelLookup("ranger")

tune_grid = expand.grid(
  mtry = c(1:(floor(ncol(mydata) * 0.7))),
  splitrule = c("gini", "extratrees"),
  min.node.size = 1000
)

tune_control = trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  summaryFunction = prSummary,
  verboseIter = FALSE, # no training log
  allowParallel = FALSE # FALSE for reproducible results 
)

ranger_tune = train(
  as.factor(y) ~ education+default+duration+poutcome
  +pdays_dummy+campaign+month+emp.var.rate+
    cons.price.idx+cons.conf.idx+euribor3m,
  data = mydata,
  metric = "F",
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "ranger"
)

ggplot(ranger_tune) +
  theme(legend.position = "bottom")

rf = ranger(as.factor(y) ~ education+default+duration+poutcome
            +pdays_dummy+campaign+month+emp.var.rate+
              cons.price.idx+cons.conf.idx+euribor3m,
            data = bank_train,
            num.trees = 1000,
            importance = "impurity",
            splitrule = ranger_tune$bestTune$splitrule,
            mtry = 10,
            min.node.size = ranger_tune$bestTune$min.node.size,
            write.forest = T,
            probability = T)

print(rf)

fun_imp_ggplot_split(rf)


#Predict Scores
rf_train_score = predict(rf,
                         data = bank_train)$predictions[, 2]

rf_test_score = predict(rf,
                        data = bank_test)$predictions[, 2]

#Cut identification
measure_train = fun_gg_cutoff(rf_train_score, bank_train$y, 
                              "acc", "f")
measure_train +
  geom_vline(xintercept = c(0.3, 0.5), 
             linetype = "dashed")

#Confusion matrix
rf_train_cut = 0.21
rf_train_class = fun_cut_predict(rf_train_score, rf_train_cut)
# matrix
rf_train_confm = confusionMatrix(rf_train_class, as.factor(bank_train$y), 
                                 positive = "1",
                                 mode = "everything")
rf_train_confm

### Validation (cut cost and confusion matrix)


measure_test = fun_gg_cutoff(rf_test_score, bank_test$y, 
                             "acc", "f")
measure_test +
  geom_vline(xintercept = c(rf_train_cut, 0.5), 
             linetype = "dashed")


rf_test_class = fun_cut_predict(rf_test_score, rf_train_cut)
# matrix
rf_test_confm = confusionMatrix(rf_test_class, as.factor(bank_test$y), 
                                positive = "1",
                                mode = "everything")
rf_test_confm



##Light Gradient Boost

bank_train_X_lgb = as.matrix(lgb.prepare_rules(bank_train %>% 
                                                 select(-y))[[1]])
bank_test_X_lgb = as.matrix(lgb.prepare_rules(bank_test %>% 
                                                select(-y))[[1]])
bank_train_Y_lgb = as.matrix(bank_train %>%
                               select(y) %>% 
                               mutate(y = as.numeric(as.character(y))))
bank_test_Y_lgb = as.matrix(bank_test %>%
                              select(y) %>% 
                              mutate(y = as.numeric(as.character(y))))

bank_train_lgb = lgb.Dataset(bank_train_X_lgb, 
                             label = bank_train_Y_lgb)
bank_test_lgb = lgb.Dataset(bank_test_X_lgb, 
                            label = bank_test_Y_lgb)

params_lgb = list(
  objective = "binary", # type of exercise
  metric = "auc", # metric to be evaluated 
  num_iterations = 500, # number of boosting iterations
  early_stopping_rounds = 200, # ill stop training if one metric of one validation data doesn't improve
  learning_rate = 0.1, # shrinkage rate
  max_depth = 4, # max depth for tree model (used to deal with over-fitting when data is small)
  num_leaves = 7, # max number of leaves (nodes) in one tree
  # scale_pos_weight = (1 - table(bank_train$y)[[2]]/length(bank_train$y)) * 100, # weight for positive class
  is_unbalance = T,
  min_data_in_leaf = 10, # min number of data in one leaf (used to deal with over-fitting)
  feature_fraction = 0.9, # randomly select part of the features on each iteration
  bagging_fraction = 0.9, # randomly select part of the data without resampling
  bagging_freq = 1, # if != 0, enables bagging, performs bagging at every k iteration
  num_threads = 6 # number of cpu cores (not threads) to use
)

lgb <- lgb.train(
  params = params_lgb,
  data = bank_train_lgb,
  valids = list(train = bank_train_lgb, 
                test = bank_test_lgb),
  verbose = 1, # show results?
  eval_freq = 50 # show metric every how many iterations?
)






#XGBoost

modelLookup("xgbTree")

bank_X = as.matrix(lgb.prepare_rules(mydata %>% 
                                                 select(-y))[[1]])
bank_Y = bank_data$y

nrounds = 1000

tune_grid = expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control = trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  summaryFunction = prSummary,
  verboseIter = FALSE, # no training log
  allowParallel = FALSE # FALSE for reproducible results 
)

xgb_tune = train(
  x = bank_X,
  y = bank_Y,
  metric = "F",
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = FALSE
)

ggplot(xgb_tune) +
  theme(legend.position = "bottom")

## AUC-ROC - Train data

data.frame("Model" = c(
                       "Logistic regression",
                       "Decision tree",
                       "Random Forest"),
           "AUROC" = c(auc(bank_train$y, logistic_train_score),
                       auc(bank_train$y, tree_train_score),
                       auc(bank_train$y, rf_train_score)),
           "AUPR" = c(aucpr(bank_train$y, logistic_train_score),
                      aucpr(bank_train$y, tree_train_score),
                      aucpr(bank_train$y, rf_train_score)),
           "Cut" = c(logistic_train_cut,
                     tree_train_cut,
                     rf_train_cut),
           "Accuracy" = c(logistic_train_confm[["overall"]][["Accuracy"]],
                          tree_train_confm[["overall"]][["Accuracy"]],
                          rf_train_confm[["overall"]][["Accuracy"]]),
           "F1" = c(logistic_train_confm[["byClass"]][["F1"]],
                    tree_train_confm[["byClass"]][["F1"]],
                    rf_train_confm[["byClass"]][["F1"]]),
           stringsAsFactors = F)


#Validation- Train data
score_train = data.frame("logistic complex" = logistic_train_score,
                        "tree complex" = tree_train_score,
                        "random forest" = rf_train_score,
                        "obs" = as.numeric(bank_train$y))

roc_train = score_train %>%
  gather(key = "Method", value = "score", -obs) %>% 
  ggplot() +
  aes(d = obs,
      m = score,
      color = Method) +
  geom_roc(labels = F, pointsize = 0, size = 0.6) +
  xlab("Specificity") +
  ylab("Sensitivity") +
  ggtitle("ROC Curve", subtitle = "Validation dataset")

prcurve_train = gg_prcurve(score_train) + ggtitle("PR Curve", subtitle = "Validation dataset")

curves_train = ggarrange(roc_train, prcurve_train, 
                        common.legend = T,
                        legend = "bottom")
print(curves_train)


## AUC-ROC - Test data


data.frame("Model" = c(
  "Logistic regression",
  "Decision tree",
  "Random Forest"),
  "AUROC" = c(auc(bank_test$y, logistic_test_score),
              auc(bank_test$y, tree_test_score),
              auc(bank_test$y, rf_test_score)),
  "AUPR" = c(aucpr(bank_test$y, logistic_test_score),
             aucpr(bank_test$y, tree_test_score),
             aucpr(bank_test$y, rf_test_score)),
  "Cut" = c(logistic_train_cut,
            tree_train_cut,
            rf_train_cut),
  "Accuracy" = c(logistic_test_confm[["overall"]][["Accuracy"]],
                 tree_test_confm[["overall"]][["Accuracy"]],
                 rf_test_confm[["overall"]][["Accuracy"]]),
  "F1" = c(logistic_test_confm[["byClass"]][["F1"]],
           tree_test_confm[["byClass"]][["F1"]],
           rf_test_confm[["byClass"]][["F1"]]),
  stringsAsFactors = F)

#Validation - ROC-PR Curve
score_test = data.frame("logistic complex" = logistic_test_score,
                        "tree complex" = tree_test_score,
                        "random forest" = rf_test_score,
                        "obs" = as.numeric(bank_test$y))

roc_test = score_test %>%
  gather(key = "Method", value = "score", -obs) %>% 
  ggplot() +
  aes(d = obs,
      m = score,
      color = Method) +
  geom_roc(labels = F, pointsize = 0, size = 0.6) +
  xlab("Specificity") +
  ylab("Sensitivity") +
  ggtitle("ROC Curve", subtitle = "Validation dataset")

prcurve_test = gg_prcurve(score_test) + ggtitle("PR Curve", subtitle = "Validation dataset")

curves_test = ggarrange(roc_test, prcurve_test, 
                        common.legend = T,
                        legend = "bottom")
print(curves_test)
