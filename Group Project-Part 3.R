Best_Home <- read.csv('BestHomePart3.csv', header = T, as.is = T)
View(Best_Home)##See other variables
#part 1
#remove the ProductID column 
Best_Home$ProductID <- NULL
Best_Home$Brand <- NULL

cor(Best_Home$WeeklyDemand, Best_Home$Required_capacity)
plot(Best_Home$WeeklyDemand ~ Best_Home$Required_capacity)
cor(Best_Home$WeeklyDemand, Best_Home$Purchasing_cost)
plot(Best_Home$WeeklyDemand ~ Best_Home$Purchasing_cost)
cor(Best_Home$WeeklyDemand, Best_Home$Selling_price)
plot(Best_Home$WeeklyDemand ~ Best_Home$Selling_price)
cor(Best_Home$WeeklyDemand, Best_Home$WeeklyDiscount)
plot(Best_Home$WeeklyDemand ~ Best_Home$WeeklyDiscount)

boxplot(Best_Home$WeeklyDemand~Best_Home$Assembly_status)
#significant 
boxplot(Best_Home$WeeklyDemand~Best_Home$Class)
#A litlle Significant
boxplot(Best_Home$WeeklyDemand~Best_Home$Material_type)
#Insignificant

training_size <- 0.7 * nrow(Best_Home)
training_index <- sample(x = 1:nrow(Best_Home), size =  training_size, replace = FALSE)
training_set <- Best_Home[training_index, ]
valid_set <- Best_Home[-training_index, ]

#part 2
##The reasons behind two decision tree
##According to the boxplot, we can see Class have slight relationship
##with WeeklyDemand. Therefore, we build two models.
##One includes Class and another does not.
###TWO LINEAR REGRESSION
model1 <- lm(formula = WeeklyDemand ~ Assembly_status + Required_capacity + Purchasing_cost + Selling_price + WeeklyDiscount, data = training_set)
summary(model1)

#adjusted r-squared is 0.7022, variables are all significant 

model2 <- lm(formula = WeeklyDemand ~ Class + Assembly_status + Required_capacity + Purchasing_cost + Selling_price + WeeklyDiscount, data = training_set)
summary(model2)
#adjusted R-square is 0.954
#individual variable are not all significant 

###TWO DECISION TREE
#Decision tree 1
install.packages('rpart')
library(rpart)
cl_tree1 <- rpart(formula = WeeklyDemand ~ Assembly_status + Required_capacity + Purchasing_cost + Selling_price + WeeklyDiscount, training_set,method='anova')
install.packages('rpart.plot')
library(rpart.plot)
prp(cl_tree1, type = 1, extra = 1)

#Decision tree 2
cl_tree2 <- rpart(formula = WeeklyDemand ~ Assembly_status + Purchasing_cost + Selling_price + WeeklyDiscount,training_set,method='anova')
prp(cl_tree2,type = 1, extra = 1)


#predictive performance of linear regression model 1

pred_valid_model1 <- predict(object = model1, newdata = valid_set)
library("forecast")
accuracy(pred_valid_model1, valid_set$WeeklyDemand)

pred_train_model1 <- predict(object = model1, newdata = training_set)
accuracy(pred_train_model1, training_set$WeeklyDemand)
#Training RSME = 24.71287 , Validation RSME = 25.42106 , the model is not overfitting


#Compare the prediction performance linear regressions model2
pred_valid_model2 <- predict(object = model2, newdata = valid_set)
library("forecast")
accuracy(pred_valid_model2, valid_set$WeeklyDemand)

pred_train_model2 <- predict(object = model2, newdata = training_set)
accuracy(pred_train_model2, training_set$WeeklyDemand)

#training RMSE = 10.21796  , predictive RMSE = 10.21796 
#Since training RMSE is significantly larger than predictive RMSE
#The model2 regression is overfitting
##Therefore, we should choose the model1

#Predictive Performance of Decision Tree 1
pred_valid_tree1 <- predict(object = cl_tree1,newdata = valid_set)
accuracy(pred_valid_tree1,valid_set$WeeklyDemand)

accuracy(pred_valid_tree1, valid_set$WeeklyDemand)
all_errors <- valid_set$WeeklyDemand - pred_valid_tree1
boxplot(all_errors, main= "Box plot of Prediction Errors1")

#Predictive Performance of Decision Tree 2
pred_valid_tree2 <- predict(object = cl_tree2,newdata = valid_set)
accuracy(pred_valid_tree2,valid_set$WeeklyDemand)

accuracy(pred_valid_tree2, valid_set$WeeklyDemand)
all_errors <- valid_set$WeeklyDemand - pred_valid_tree2
boxplot(all_errors, main= "Box plot of Prediction Errors2")

##We decide to choose tree1 
# Stopping Rules
# minsplit
cl_tree1_mins <- rpart(formula = WeeklyDemand ~ Assembly_status 
                       + Required_capacity + Purchasing_cost + Selling_price + WeeklyDiscount,
                       minsplit = 20, data = training_set, method = "anova")
prp(cl_tree1_mins)
# Prediction performance
pred_valid_dtree1 <- predict(object = cl_tree1_mins, newdata = valid_set)
accuracy(pred_valid_dtree1, valid_set$WeeklyDemand)

pred_train_dtree2 <- predict(object = cl_tree1_mins, newdata = training_set)
accuracy(pred_train_dtree2, training_set$WeeklyDemand)

#maxdepth
cl_tree1max <- rpart(formula = WeeklyDemand ~ Assembly_status + 
                       Required_capacity + Purchasing_cost + Selling_price + WeeklyDiscount,
                     maxdepth = 30, data = training_set, method = "anova")
prp(cl_tree1max)
#Prediction performance
pred_valid_tree13 <- predict(object = cl_tree1max, newdata = valid_set)
accuracy(pred_valid_tree13, valid_set$WeeklyDemand)

pred_train_tree14 <- predict(object = cl_tree1max, newdata = training_set)
accuracy(pred_train_tree14, training_set$WeeklyDemand)
### Compare liner to decision tree
pred_valid_tree1 <- predict(object = cl_tree1,newdata = valid_set)
accuracy(pred_valid_tree1,valid_set$WeeklyDemand)
pred_train_tree1 <- predict(object = cl_tree1,newdata = train_set)
accuracy(pred_valid_tree1,train_set$WeeklyDemand)

pred_valid_model1 <- predict(object = model1, newdata = valid_set)
accuracy(pred_valid_model1, valid_set$WeeklyDemand)

pred_train_model1 <- predict(object = model1, newdata = training_set)
accuracy(pred_train_model1, training_set$WeeklyDemand)

### Problem 2
Best_Home$DemandLevel <- ifelse(Best_Home$WeeklyDemand >= quantile(Best_Home$WeeklyDemand, 0.75), 1, 0)
Best_Home$DemandLevel
#Part 1
cor(Best_Home$DemandLevel, Best_Home$Required_capacity)
plot(Best_Home$DemandLevel ~ Best_Home$Required_capacity)
cor(Best_Home$DemandLevel, Best_Home$Purchasing_cost)
plot(Best_Home$DemandLevel ~ Best_Home$Purchasing_cost)
cor(Best_Home$DemandLevel, Best_Home$Selling_price)
plot(Best_Home$DemandLevel ~ Best_Home$Selling_price)
cor(Best_Home$DemandLevel, Best_Home$WeeklyDiscount)
plot(Best_Home$DemandLevel ~ Best_Home$WeeklyDiscount)
cor(Best_Home$DemandLevel, Best_Home$WeeklyDemand)
plot(Best_Home$DemandLevel ~ Best_Home$WeeklyDemand)

boxplot(Best_Home$DemandLevel~Best_Home$Assembly_status)
#not significant 
boxplot(Best_Home$DemandLevel~Best_Home$Class)
#not Significant
boxplot(Best_Home$DemandLevel~Best_Home$Material_type)
#not significant

training_size <- 0.7 * nrow(Best_Home)
training_index <- sample(x = 1:nrow(Best_Home), size =  training_size, replace = FALSE)
training_set <- Best_Home[training_index, ]
valid_set <- Best_Home[-training_index, ]


# Part 2
##We could just change the available predictor to 
##build two different models to chose the best one.
# Logisitc Regression Models
p2model1 <- glm(formula = DemandLevel ~ Required_capacity+ WeeklyDemand 
                + Selling_price + WeeklyDiscount, data = training_set, family = "binomial")
summary(p2model1)

p2model2 <- glm(formula = DemandLevel ~  Required_capacity + Purchasing_cost
                + Selling_price + WeeklyDiscount + WeeklyDemand, data = training_set, family = "binomial")
summary(p2model2)

#Predictive Performance Model 1
library(caret)
pred_valid_p2model1 <- predict(object = p2model1,newdata = valid_set,type = "response")
cutoff <- 0.5
pred_sur1 <- ifelse(pred_valid_p2model1>cutoff,1,0)
confusionMatrix(as.factor(pred_sur1), as.factor(valid_set$DemandLevel),positive='1')

#Predictive Performance Model 2
pred_valid_p2model2 <- predict(object = p2model2,newdata = valid_set,type = "response")
cutoff <- 0.5
pred_sur2 <- ifelse(pred_valid_p2model2>cutoff,1,0)
confusionMatrix(as.factor(pred_sur2), as.factor(valid_set$DemandLevel),positive='1')
##According to accuracy, we decide to choose Model 1

#Decision Trees
decision_tree1 <- rpart(formula = DemandLevel ~ Required_capacity + Purchasing_cost  
                        + Selling_price + WeeklyDiscount + WeeklyDemand, 
                 training_set,method='class')

prp(decision_tree1, type = 1, extra = 1)

#predictive performance
library(caret)
pred_valid_2tree1 <- predict(object = decision_tree1 ,newdata = valid_set,type='class')
confusionMatrix(as.factor(pred_valid_2tree1), 
                as.factor(valid_set$DemandLevel))

#Decision tree 2
decision_tree2 <- rpart(formula = DemandLevel ~ Required_capacity + Purchasing_cost  
                        + Selling_price + WeeklyDiscount, 
                        training_set,method='class')
prp(decision_tree2,type =1, extra = 1)

#predictive performance
library(caret)
pred_valid_2tree2 <- predict(object = decision_tree2 ,newdata = valid_set,type='class')
confusionMatrix(as.factor(pred_valid_2tree2), 
                as.factor(valid_set$DemandLevel))
##According to the Accuracy, we decide to choose Decision tree 1.