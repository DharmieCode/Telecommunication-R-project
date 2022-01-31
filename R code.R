# Libraries
library(foreign)
library(ggplot2)

TeleCustomer <- read.spss("TeleCustomer.sav", use.value.labels = TRUE, to.data.frame = TRUE)
View(TeleCustomer)
# attach
attach(TeleCustomer)

### DATA CLEANING
# missing values
sum(is.na(TeleCustomer))

# plot the missing map
library(Amelia)
missmap(TeleCustomer, col = c("black", "grey"))

# remove irrelevant columns (logtoll, logequi, logwire and logcard)
colnames(TeleCustomer)
TeleCustomer2<-TeleCustomer[,-c(36,37,38,39)]
# check missingness
missmap(TeleCustomer2, col = c("black", "grey"))

# check for missing values
sum(is.na(TeleCustomer2))


# main factors
# tenure, age, marital, address, employ, retire, gender, reside, custcat, and churn
TeleCustomer_data <-TeleCustomer2[ ,c(2,3,4,5,8,9,10,11,37,38)]
colnames(TeleCustomer_data)


### DATA VISUALIZATION
# create correlation plot
# check for data types
str(TeleCustomer_data)


# Customer churn count
ggplot(TeleCustomer_data, aes(x=churn)) + geom_bar(width = 0.5, fill="blue") + 
  ggtitle("                  Customer churn status by count") +
  geom_text(stat = "count", aes(label=stat(count)), vjust=0.5) +
  theme_classic()

# visualize age
# density plot
ggplot(TeleCustomer_data, aes(x=age)) + 
  geom_density(fill="coral") +
  ggtitle("                  Density plot for age")

# Categorize age
summary(TeleCustomer_data$age)
TeleCustomer_data$agecat <- ifelse(TeleCustomer_data$age>=18 & TeleCustomer_data$age<=35,"young adult",
                                   ifelse(TeleCustomer_data$age>35 & TeleCustomer_data$age<=55,"middle age", "older adult"))
table(TeleCustomer_data$agecat)
# plot
ggplot(TeleCustomer_data, aes(x=churn, fill=agecat)) + geom_bar(position = position_dodge()) + 
  ggtitle("                  Customer churn status by Age_group") +
  geom_text(stat = "count", aes(label=stat(count)), 
            position = position_dodge(width = 1), vjust=0.5) +
  theme_classic()

# address
str(TeleCustomer_data)
# ggplot
plot_address<-ggplot(TeleCustomer_data, aes(x=address)) + 
  geom_density(fill="green") +
  ggtitle("Density plot for address")

# employment
str(TeleCustomer_data)
# ggplot
plot_employ<-ggplot(TeleCustomer_data, aes(x=employ)) + 
  geom_density(fill="blue") +
  ggtitle("Density plot for employ")

# place plots in the same space (employ and address)
require(gridExtra)
grid.arrange(plot_address, plot_employ, ncol=2)

# Gender
ggplot(TeleCustomer_data, aes(x=churn, fill=gender)) + geom_bar(position = position_dodge()) + 
  ggtitle("                  Customer churn status by Gender") +
  geom_text(stat = "count", aes(label=stat(count)), 
            position = position_dodge(width = 1), vjust=0.5) +
  theme_classic()


# Reside
summary(TeleCustomer_data$reside)
# reside category
TeleCustomer_data$residecat <- ifelse(TeleCustomer_data$reside<3,"average","not average")
# plot
plot_reside<-plot_reside<-ggplot(TeleCustomer_data, aes(x=reside)) + 
  geom_density(fill="red") +
  ggtitle("                  Density plot for reside")
plot_residecat<-ggplot(TeleCustomer_data, aes(x=churn, fill=residecat)) + geom_bar(position = position_dodge()) + 
  ggtitle("                  Customer churn status by reside") +
  geom_text(stat = "count", aes(label=stat(count)), 
            position = position_dodge(width = 1), vjust=0.5) +
  theme_classic()
# place plots in the same space
require(gridExtra)
grid.arrange(plot_reside, plot_residecat, ncol=2)

# retire
ggplot(TeleCustomer_data, aes(x=churn, fill=retire)) + geom_bar(position = position_dodge()) + 
  ggtitle("                  Customer churn status by retire") +
  geom_text(stat = "count", aes(label=stat(count)), 
            position = position_dodge(width = 1), vjust=0.5) +
  theme_classic()


# marital
colnames(TeleCustomer_data)
table(TeleCustomer_data$marital)
# ggplot
ggplot(TeleCustomer_data, aes(x=churn, fill=marital)) + geom_bar(position = position_dodge()) + 
  ggtitle("                  Customer churn status by marital status") +
  geom_text(stat = "count", aes(label=stat(count)), 
            position = position_dodge(width = 1), vjust=0.5) +
  theme_classic()


# Customer category
ggplot(TeleCustomer_data, aes(x=churn, fill=custcat)) + geom_bar(position = position_dodge()) + 
  ggtitle("                  Customer churn status by Customer category") +
  geom_text(stat = "count", aes(label=stat(count)), 
            position = position_dodge(width = 1), vjust=0.5) +
  theme_classic()



### PREDICTION
# create train and test dataset
train_test_split = function(TeleCustomer_data, fraction =0.8, train = TRUE){     
  total_row= nrow(TeleCustomer_data)                                             
  train_rows= fraction*total_row                                      
  sample=1:train_rows
  if(train==TRUE){
    return(TeleCustomer_data[sample,])
  }else{
    return(TeleCustomer_data[-sample,])
  }
}

# train data
train <- train_test_split(TeleCustomer_data, 0.8, train = TRUE)
# test data
test <- train_test_split(TeleCustomer_data, 0.8, train = FALSE)


# Decision tree
# library 
library(rpart)
library(rpart.plot)

# Build tree model
# fit <- rpart(churn ~ tenure+age+marital+address+employ+retire+gender+reside+custcat, train, method ="class")
# using the significant variables and tenure
fit <- rpart(churn ~ tenure+address+employ+custcat, train, method ="class")

# Plot the decision tree
rpart.plot(fit)

# prediction
# using the test data
predicted_unseen<-predict(fit, test, type = 'class')

# table 
# count the number of patients that died and survived, then compare to the correct result
table_mat<- table(test$churn, predicted_unseen)
table_mat

# Accuracy
# compute accuracy of the test set
dt_accuracy<- sum(diag(table_mat))/ sum(table_mat)

# print the accuracy of the test set
print(paste("The accuracy is :", dt_accuracy))


### LOGIT
# library
library(dplyr)

# Logistic regression
data_rescale <- mutate_if(TeleCustomer_data,
                          is.numeric,
                          list(~as.numeric(scale(.))))
summary(data_rescale)

r_train<- train_test_split(data_rescale, 0.7, train = TRUE)
r_test<- train_test_split(data_rescale, 0.7, train = FALSE)
logit <- glm(churn ~ tenure+age+marital+address+employ+retire+gender+reside+custcat, data = r_train, family = "binomial")
summary(logit)
lr_predict<- predict(logit, r_test, type = "response")
# confusion matrix
table_mat2 <- table(r_test$churn, lr_predict>0.50)
table_mat2
lr_accuracy= sum(diag(table_mat2))/sum(table_mat2)
paste("The accuracy is :", lr_accuracy)


# Naive Bayes
library(e1071)
# model
nb_model<- naiveBayes(churn ~ tenure+age+marital+address+employ+retire+gender+reside+custcat, data = train)
# predict
nb_predict <- predict(nb_model, test)
# table
table_mat3<-table(test$churn, nb_predict)
table_mat3
# accuracy
nb_accuracy= sum(diag(table_mat3))/sum(table_mat3)
paste("The accuracy is :", nb_accuracy)







