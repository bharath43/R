library(dplyr)
setwd("D:\\c\\Downloads")
getwd()

#Reading train and test datasets:
  
train=read.csv("bank-full_train.csv",stringsAsFactors = FALSE,header = T ) 
test=read.csv("bank-full_test.csv",stringsAsFactors = FALSE,header = T ) 

#Step 1: Imputing NA values in the datasets.

apply(train,2,function(x)sum(is.na(x)))
#There exist no NA values in train dataset.
apply(test,2,function(x)sum(is.na(x)))
#There exist no NA values in test dataset.

#Step 2:Data Preparation

#Combining both train n test datasets prior to data preparation.
test$y=NA
train$data='train'
test$data='test'
all_data=rbind(train,test)
apply(all_data,2,function(x)sum(is.na(x)))

#Lets see the structure and datatypes of the combined dataset.
glimpse(all_data) 
#Creating dummy variables by combining similar categories for variable job(char type)
t=table(all_data$job)
sort(t)
final=round(prop.table(table(all_data$job,all_data$y),1)*100,1)
final
s=addmargins(final,2) #add margin across Y
sort(s[,1])
all_data=all_data %>% 
  mutate(job_1=as.numeric(job %in% c("self-employed","unknown","technician")), 
         job_2=as.numeric(job %in% c("services","housemaid","entrepreneur")),
         job_3=as.numeric(job %in% c("management","admin")),
         job_4=as.numeric(job=="student"),
         job_5=as.numeric(job=="retired"),
         job_6=as.numeric(job=="unemployed")) %>% 
  select(-job)

glimpse(all_data)

#Making dummies for variable marital
t=table(all_data$marital)
sort(t)

all_data=all_data %>% 
  mutate(divorced=as.numeric(marital %in% c("divorced")),
         single=as.numeric(marital %in% c("single"))
  ) %>% 
  select(-marital)
glimpse(all_data)
#Making dummies for variable education
t=table(all_data$education)
sort(t)
all_data=all_data %>% 
  mutate(edu_primary=as.numeric(education %in% c("primary")),
         edu_sec=as.numeric(education %in% c("secondary")),
         edu_tert=as.numeric(education %in% c("tertiary"))
  ) %>% 
  select(-education)
glimpse(all_data)
 
#Making dummies for varible default
table(all_data$default)

all_data$default=as.numeric(all_data$default=="yes")
#Making dummies for variable housing
table(all_data$housing)
all_data$housing=as.numeric(all_data$housing=="yes")
glimpse(all_data)
#Making dummies for variable loan
table(all_data$loan)
all_data$loan=as.numeric(all_data$loan=="yes")
glimpse(all_data)
#Making dummies for variable contact
t=table(all_data$contact)
sort(t)
all_data=all_data %>% 
  mutate(co_cellular=as.numeric(contact %in% c("cellular")),
         co_tel=as.numeric(contact %in% c("telephone"))
  ) %>% 
  select(-contact)
glimpse(all_data)
#Making dummies for variable month
table(all_data$month)
#lets convert into percentage across months.
finalmnth=round(prop.table(table(all_data$month,all_data$y),1)*100,1)
sss=addmargins(finalmnth,2) #adding margin across Y
sort(sss[,1])
all_data=all_data %>% 
  mutate(month_1=as.numeric(month %in% c("aug","jun","nov","jan","jul")), 
         month_2=as.numeric(month %in% c("dec","sep")),
         month_3=as.numeric(month=="mar"),
         month_4=as.numeric(month=="oct"),
         month_5=as.numeric(month=="apr"),
         month_6=as.numeric(month=="feb")) %>% 
  select(-month)
glimpse(all_data)

#Making dummies for variable outcome
t=table(all_data$poutcome)
sort(t)
all_data=all_data %>% 
  mutate(poc_success=as.numeric(poutcome %in% c("success")),
         poc_failure=as.numeric(poutcome %in% c("failure")),
         poc_other=as.numeric(poutcome %in% c("other"))
  )%>% 
select(-poutcome)
glimpse(all_data)

#Thus data preparation is done and we will now seperate both test n train data.
glimpse(all_data)
table(all_data$y)

table(train$y)

all_data$y=as.numeric(all_data$y=="yes")
table(all_data$y)

glimpse(all_data)
#Separating test and train:
train=all_data %>% 
filter(data=='train') %>% 
select(-data) #31647,34
test=all_data %>% 
filter(data=='test') %>% 
select(-data,-y)
#Lets view the structure of test n train datasets:
glimpse(train)   
glimpse(test) 
#Now lets divide the train dataset in the ratio 75:25.
set.seed(5)
s=sample(1:nrow(train),0.75*nrow(train))
train_75=train[s,] #23735,34
test_25=train[-s,]#7912,34

#Step 3: Model Building
#We will use train for logistic regression model building and use train_25 to test the performance of the model thus built.
#Lets build logistic regression model on train dataset.
library(car)
for_vif=lm(y~.,data=train)
summary(for_vif)

#In order to take care of multi collinearity,we remove variables whose VIF>5,as follows:
t=vif(for_vif)
sort(t,decreasing = T)[1:5]
#Removing variable edu_sec
for_vif=lm(y~.-edu_sec,data=train)
t=vif(for_vif)
sort(t,decreasing = T)[1:5]
summary(for_vif)
#Now lets remove edu-sec from train dataset
colnames(train) 
fit_train=train %>% 
select(-edu_sec)
colnames(fit_train) 
#Lets build model on fit_train dataset:
fit=glm(y~.,family = "binomial",data=fit_train) 
summary(fit) 
#Now lets remove all variables whose p value is >0.05 using step function.
fit=step(fit)
#Lets check the remaining significant variables
names(fit$coefficients) 

#Lets build final logistic model on significant variables on dataset fit_train

fit_final=glm(y~balance + housing + loan + duration + campaign + ID + 
                job_3 + job_5 + divorced + single + edu_primary + 
                co_cellular + co_tel + month_1 + month_2 + month_3 + month_4 + 
                month_5 + month_6 + poc_success + poc_failure + poc_other ,data=fit_train,family="binomial")
summary(fit_final)

#Thus logistic regression model is successfully built.
#Now lets make predict scores
train$score=predict(fit_final,newdata = train,type="response")
#lets see how the score behaves.
install.packages("ggplot2")
library(ggplot2)
ggplot(train,aes(y=y,x=score,color=factor(y)))+
  geom_point()+geom_jitter()


#Step 4. Finding Cutoff value and Perfomance measurements of the model.
#Lets find cutoff based on these probability scores.

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)
cutoffs=seq(0,1,length=100)
for (i in cutoffs){
  predicted=as.numeric(train$score>i)
  
  TP=sum(predicted==1 & train$y==1)
  FP=sum(predicted==1 & train$y==0)
  FN=sum(predicted==0 & train$y==1)
  TN=sum(predicted==0 & train$y==0)
  cutoff_data=rbind(cutoff_data,c(i,TP,FP,FN,TN))
}
#lets remove the dummy data cotaining top row in data frame cutoff_data
cutoff_data=cutoff_data[-1,]
#we now have 100 obs in df cutoff_data
#Lets calculate the performance measures:sensitivity,specificity,accuracy, KS and precision.
cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP, #total positives and negatives
         Sn=TP/P, #sensitivity
         Sp=TN/N, #specificity
         KS=abs((TP/P)-(FP/N)),
         Accuracy=(TP+TN)/(P+N),
         Lift=(TP/P)/((TP+FP)/(P+N)),
         Precision=TP/(TP+FP),
         Recall=TP/P
  ) %>% 
  select(-P,-N)
#Lets view cutoff dataset:
  #Lets find cutoff value based on ks MAXIMUM.
KS_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]
KS_cutoff

#Step 5.Predict the final output on test dataset.(whether the client subscribe or no to term deposit)
#Lets predict test scores
test$score=predict(fit_final,newdata =test,type = "response")#on final test dataset.
#Predicting whether the client has subscribed or no in final test dataset.
test$left=as.numeric(test$score>KS_cutoff)#if score is greater dan cutoff then true(1) else false(0)
table(test$left)
#Thus final prediction is as follows:
test$leftfinal=factor(test$left,levels = c(0,1),labels=c("no","yes"))
table(test$leftfinal)
#writing into csv file final output test$leftfinal
write.csv(test$leftfinal,"Bharath_Reddy_Banking_Project.csv")
#Thus 3396 customers out of 13564 subscribe to term deposit according to the model.

#Step 6:Creating confusion matrix and find how good our model is (by predicting on test_25 dataset)
test_25$score=predict(fit_final,newdata =test_25,type = "response")
table(test_25$y,as.numeric(test_25$score>KS_cutoff))
table(test_25$y)

#here TP=770,TN=5888,FP=,FN=
#Accuracy=(TP+TN)/(P+N):
a=(770+5888)/7912
a
#Hence error will be:
1-a
#Error is 15.85%.(according to ks method)
#Lets plot the ROC curve:
library(pROC)
roccurve=roc(test_25$y,test_25$score) #real outcome and predicted score is plotted
roccurve
plot(roccurve)
#Thus area under the ROC curve is:
auc(roccurve) 