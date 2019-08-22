install.packages("dplyr")
install.packages("tidyr")
install.packages("randomForest")
install.packages("cvTools")
install.packages("pROC")
library(dplyr)
library(tidyr)
library(randomForest)
library(cvTools)
library(pROC)

#Reading Train and Test File
s_train=read.csv("store_train.csv",stringsAsFactors = FALSE)
s_test=read.csv("store_test.csv",stringsAsFactors = FALSE)

#Creating Dummies Function
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

s_test$store = NA
s_train$data='train'
s_test$data='test'
s=rbind(s_train,s_test)

#Drop Features
s$countyname = NULL
s$countytownname = NULL
s$Areaname = NULL
s$storecode = NULL

glimpse(s)
s = CreateDummies(s,"store_Type",100)
s = CreateDummies(s,"state_alpha",50)

#NA
for(col in names(s)){
  if(sum(is.na(s[,col]))>0 & !(col %in% c("data","store"))){
    s[is.na(s[,col]),col]=mean(s[s$data=='train',col],na.rm=T)
  }
}

lapply(s,function(x) sum(is.na(x)))

s_train=s %>% filter(data=='train') %>% select(-data)
s_test=s %>% filter(data=='test') %>% select (-data,-store)

##################################################################################################
##################################################################################################

s_train$store = as.factor(s_train$store)
param=list(mtry=c(5,10,15,20,25,35,40,45),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10)
)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}

num_trials=50
my_params=subset_paras(param,num_trials)

my_params

myauc=0

## Cvtuning
for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  params=my_params[i,]
  
  k=cvTuning(randomForest,store~., 
             data =s_train,
             tuning =params,
             folds = cvFolds(nrow(s_train), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    myauc=score.this
    print(myauc)
    best_params=params
  }
  print('DONE')
}

#myauc = 0.8078341
#mtry ntree maxnodes nodesize
#35   200      100       10

best_params=data.frame(mtry=35,
                       ntree=200,
                       maxnodes=100,
                       nodesize=10)

## Model on the entire training data

s.final=randomForest(store~.,
                     mtry=best_params$mtry,
                     ntree=best_params$ntree,
                     maxnodes=best_params$maxnodes,
                     nodesize=best_params$nodesize,
                     data=s_train)

test.score=predict(s.final,newdata = s_test,type='prob')[,2]
write.csv(test.score,'Bharath_Reddy_Retail_Project.csv',row.names = F)     
