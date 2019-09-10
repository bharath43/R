# R
Projects on R.

Banking Project

A Portugese bank is rolling out term deposit for its customers. They have in the past connected to their customer base through phone calls. Results for these previous campaigns were recorded and have been provided to the current campaign manager to use the same in making this campaign more effective.

Challenges that the manager faces are following:

* Customers have recently started to complain that bank’s marketing staff bothers them with irrelevant product calls and this should        immediately stop

* There is no prior framework for her decide and choose which customer to call and which one to leave alone

She has decided to use past data to automate this decision, instead of manually choosing through each and every customer. Previous campaign data which has been made available to her; contains customer characteristics , campaign characteristics, previous campaign information as well as whether customer ended up subscribing to the product as a result of that campaign or not. Using this she plans to develop a statistical model which given this information predicts whether customer in question will subscribe to the product or not. A successful model which is able to do this, will make her campaign efficiently targeted and less bothering to uninterested customers.

We have given you two datasets , bank-full_train.csv and bank-full_test.csv . You need to use data bank-full_train to build predictive model for response variable “y”. bank-full_test data contains all other factors except “y”, you need to predict that using the model that you developed and submit your predicted values in a csv files.
# Install the necessary packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("cvTools")
install.packages("pROC")
install.packages("car")
library(dplyr)
library(tidyr)
library(cvTools)
library(pROC)
library(car)

getwd()
# Read the csv files
bank_train=  read.csv("bank-full_train.csv", header= T, stringsAsFactors = F)
bank_test =    read.csv("bank-full_test.csv", header= T, stringsAsFactors = F)

ld_bank=bank_train
ld_bank_test=bank_test
glimpse(bank_test)

# Function to create dummy variables
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

ld_bank$Success.campaign=NA

table(ld_bank$age)

# converting  1 and 0 of yes or no value
ld_bank$Success.campaign= as.numeric(ld_bank$y=='yes')


ld_bank=ld_bank %>%  
  select(-y)

# find out categorical data
names(ld_bank)[sapply(ld_bank,function(x) is.character(x))]

# create dummy variables
cat_cols=c("job","marital","education","default","housing","loan",
           "contact","month","poutcome")
for(cat in cat_cols){
  ld_bank=CreateDummies(ld_bank,cat,100)
}


ld_bank=ld_bank %>%
  select(-ID)

for_vif=lm(Success.campaign~.,data=ld_bank)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(Success.campaign~.-month_may,data=ld_bank)
sort(vif(for_vif),decreasing = T)[1:3]

for_vif=lm(Success.campaign~.-month_may-job_blue_collar,data=ld_bank)
sort(vif(for_vif),decreasing = T)[1:3]


log_fit<- glm(Success.campaign~.-month_may-job_blue_collar ,data=ld_bank, family = "binomial")

log_fit= step(log_fit)

log_fit

formula(log_fit)

log_fit<- glm(Success.campaign ~ balance + day + duration + campaign + job_student + 
                job_housemaid + job_retired + job_admin. + job_technician + 
                job_management + marital_married + education_primary + education_tertiary + 
                housing_yes + loan_no + contact_unknown + contact_cellular + 
                month_mar + month_sep + month_oct + month_jan + month_feb + 
                month_apr + month_nov + month_jun + month_aug + month_jul + 
                poutcome_other + poutcome_failure + poutcome_unknown ,data=ld_bank, family = "binomial")
summary(log_fit)

val.score=predict(log_fit,newdata = ld_bank, type= 'response')
auc_score=auc(roc(ld_bank$Success.campaign,val.score))
auc_score


real=ld_bank$Success.campaign

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,KS=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(val.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  
  
  KS=(TP/P)-(FP/N)
  
  
  
  cutoff_data=rbind(cutoff_data,c(cutoff,KS))
}

View(cutoff_data)

cutoff_data=cutoff_data[-1,]

cutoff_data

max(cutoff_data$KS)


my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

#
rm(ld_bank_test)
ld_bank_test=bank_test

ld_bank_test=ld_bank_test %>%
  select(-ID)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  print(categories)
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

ld_bank_test$Success.campaign=NA

table(ld_bank_test$age)


# find out categorical data
names(ld_bank_test)[sapply(ld_bank_test,function(x) is.character(x))]
# create dummy variables
cat_cols=c("job","marital","education","default","housing","loan",
           "contact","month","poutcome")
for(cat in cat_cols){
  ld_bank_test=CreateDummies(ld_bank_test,cat,200)
}

bank_test[,'job']

ld_bank_test$job_student=as.numeric(bank_test[,'job']=='student')
ld_bank_test$month_oct=as.numeric(bank_test[,'month']=='oct')
ld_bank_test$month_sep=as.numeric(bank_test[,'month']=='sep')
ld_bank_test$month_mar=as.numeric(bank_test[,'month']=='mar')

test.prob.score= predict(log_fit,newdata = ld_bank_test,type='response')

test.predicted=as.numeric(test.prob.score>my_cutoff)

auc_score=auc(roc(ld_bank$Success.campaign,test.predicted))

auc_score

write.csv(test.predicted,"Bharath_Reddy_Banking_Project.csv",row.names = F)
