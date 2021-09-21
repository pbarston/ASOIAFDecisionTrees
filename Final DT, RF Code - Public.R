#load and install
library(tidyverse) 
library(ggplot2)
library(caret)
library(e1071)
library(car)
library(pROC)
library(dplyr)
library(rpart)
library(rpart.plot)

#load the data
thrones=read.csv("./Data/character-deaths.csv",na.strings = NA)
battles = read.csv("./Data/battles.csv") #for later
predictions = read.csv("./Data/character-predictions.csv") #for later

#check out initial structure 
str(thrones) 

#remove the "House" leading string from allegiances
#before:
table(thrones$Allegiances)
#after:
thrones$Allegiances=str_remove(thrones$Allegiances,"House ")
table(thrones$Allegiances)

#create the "intro.chapter" column - to fill in later
colnames(thrones)[which(names(thrones) == "Book.Intro.Chapter")] <- "Intro.Chapter"
#one last check
str(thrones)

#manual, character-based cleaning
#all done via ASOIAF wiki - others could be out there!
#change those who need to be dead in book 2
thrones$Book.of.Death[thrones$Name=="Cressen"]=2
thrones$Book.of.Death[thrones$Name=="Shyra Errol"]=2
#manually update the intro NAs
thrones$Intro.Chapter[thrones$Name=="Aemon Costayne"]=33
thrones$Intro.Chapter[thrones$Name=="Aemon Estermont"]=71
thrones$Intro.Chapter[thrones$Name=="Alyn Estermont"]=71
thrones$Intro.Chapter[thrones$Name=="Bearded Ben"]=47
thrones$Intro.Chapter[thrones$Name=="Big Walder Frey"]=60
thrones$Intro.Chapter[thrones$Name=="Cuger"]=42
thrones$Intro.Chapter[thrones$Name=="Garse Goodbrook"]=71
thrones$Intro.Chapter[thrones$Name=="Groleo"]=9
thrones$Intro.Chapter[thrones$Name=="Joss Stilwood"]=31
thrones$Intro.Chapter[thrones$Name=="Muttering Bill"]=83
thrones$Intro.Chapter[thrones$Name=="Orphan Oss"]=34
thrones$Intro.Chapter[thrones$Name=="Tytos Frey"]=53
#update the book death NAs
thrones$Book.of.Death[thrones$Name=="Lennocks"]=3
thrones$Book.of.Death[thrones$Name=="Pate (Old)"]=3
#update the chapter death NAs
thrones$Death.Chapter[thrones$Name=="Garse Goodbrook"]=82
thrones$Death.Chapter[thrones$Name=="Lysa Tully"]=81
thrones$Death.Chapter[thrones$Name=="Merrett Frey"]=82
thrones$Death.Chapter[thrones$Name=="Petyr Frey"]=82
thrones$Death.Chapter[thrones$Name=="Tytos Frey"]=82
thrones$Death.Chapter[thrones$Name=="Alester Florent"]=47
thrones$Death.Chapter[thrones$Name=="Aenys Frey"]=74
thrones$Death.Chapter[thrones$Name=="Kevan Lannister"]=73
thrones$Death.Chapter[thrones$Name=="Pycelle"]=73
thrones$Death.Chapter[thrones$Name=="Shyra Errol"]=71

#need to impute how long a character is around for
#first, create the df with all the chapter counts
chapters=data.frame("Book Name" = c("GoT","CoK","SoS","FfC","DwD"),
                    "Chapter Count" = c(74,71,83,47,74))

#create the "book of intro" column
thrones = thrones %>%
  mutate(Book.of.Intro=ifelse(GoT>0,1,
                              ifelse(CoK>0,2,
                                     ifelse(SoS>0,3,
                                            ifelse(FfC>0,4,5
                                            )))))
#change Harmune book of intro
#harmune needs to go here - book of intro column doesn't exist in beginning
thrones$Book.of.Intro[thrones$Name=="Harmune"]=3
thrones$SoS[thrones$Name=="Harmune"]=1


#now, create our target "length of time in the series" column
#first, find the sum of chapters
sum(chapters$Chapter.Count)
#nothing can exceed that 349
#create the max possible value
max.possible = sum(chapters$Chapter.Count)
#add 1 to each column to account for prologue = 0 in the dataset but not the wiki
thrones = thrones %>%
  mutate(Intro.Chapter=Intro.Chapter+1,
         Death.Chapter=Death.Chapter+1)
#create the chapter column amounts
count.GoT=chapters$Chapter.Count[chapters$Book.Name=="GoT"]
count.CoK=chapters$Chapter.Count[chapters$Book.Name=="CoK"]
count.SoS=chapters$Chapter.Count[chapters$Book.Name=="SoS"]
count.FfC=chapters$Chapter.Count[chapters$Book.Name=="FfC"]
count.DwD=chapters$Chapter.Count[chapters$Book.Name=="DwD"]

#then populate the new column
thrones = thrones %>%
  mutate(sequential.intro=ifelse(GoT>0,Intro.Chapter, #can also just make "GoT" into "book of intro"
                                 ifelse(CoK>0,count.GoT+Intro.Chapter,
                                        ifelse(SoS>0,count.GoT+count.CoK+Intro.Chapter,
                                               ifelse(FfC>0,count.GoT+count.CoK+count.SoS+Intro.Chapter,count.GoT+count.CoK+count.SoS+count.FfC+Intro.Chapter
                                               )))))

#now create sequential death (not the same equation same as the other one)
thrones = thrones %>%
  mutate(sequential.death=ifelse(Book.of.Death==1,Death.Chapter, #can also just make this "book of intro"
                                 ifelse(Book.of.Death==2,count.GoT+Death.Chapter,
                                        ifelse(Book.of.Death==3,count.GoT+count.CoK+Death.Chapter,
                                               ifelse(Book.of.Death==4,count.GoT+count.CoK+count.SoS+Death.Chapter,count.GoT+count.CoK+count.SoS+count.FfC+Death.Chapter
                                               )))))


#now create the length of time - the target column
thrones = thrones %>%
  mutate(chapter.span=ifelse(!is.na(Death.Chapter),
                             sequential.death - sequential.intro,
                             max.possible - sequential.intro))
#now create the status column
thrones = thrones %>%
  mutate(is.dead=ifelse(!is.na(Death.Chapter),
                        1,0)) #do not make this a factor!


#Beginning to construct the Decision Tree
#1. Make the target variable a factor
thrones$is.dead=as.factor(thrones$is.dead)

#make any qualitative variables into factors 
thrones$Allegiances=as.factor(thrones$Allegiances) #allegiance
thrones$Gender=as.factor(thrones$Gender) #gender
thrones$Nobility=as.factor(thrones$Nobility) #nobility
thrones$Book.of.Intro=as.factor(thrones$Book.of.Intro) #book of intro
#leave numeric variables as is

#create the datasets you want -
#this includes dropping name of character - not relevant

#dataset 1 - all variables
thrones.dtree.full=thrones %>%
  select(-c(Name,
            #drop anything related to death
            Death.Year,
            Book.of.Death,
            Death.Chapter,
            Intro.Chapter,
            sequential.death,
            #drop book columns that can be summarized in book of intro
            GoT,
            CoK,
            SoS,
            FfC,
            DwD))

str(thrones.dtree.full)

#dataset 2 - all variables as categorical
#to start, create a histogram of chapter span
#where do most characters fall?
ggplot(data=thrones.dtree.full,aes(x=chapter.span)) + geom_histogram() + 
  geom_vline(data=thrones.dtree.full,aes(xintercept=median(chapter.span)))
#validate
median(thrones.dtree.full$chapter.span)
#estimate is roughly 125 chapters
#draw the distinction there
thrones.dtree.catty=thrones.dtree.full %>%
  mutate(long.life=ifelse(chapter.span>125,T,F))

#histogram of sequential intro
#where do most characters fall?
ggplot(data=thrones.dtree.full,aes(x=sequential.intro)) + geom_histogram() + 
  geom_vline(data=thrones.dtree.full,aes(xintercept=median(sequential.intro)))
#validate
median(thrones.dtree.full$sequential.intro)
#estimate is chapter 140
#draw the distinction there
thrones.dtree.catty = thrones.dtree.catty %>%
  mutate(late.intro=ifelse(sequential.intro>140,T,F))

#drop the numeric variables for this one
thrones.dtree.catty = thrones.dtree.catty %>%
  select(-c(sequential.intro,chapter.span))
str(thrones.dtree.catty)


#dataset 3 - just the categorical variables in general
thrones.dtree.slim = thrones.dtree.full  %>%
  select(-c(sequential.intro,chapter.span))
str(thrones.dtree.slim)

#set seed and create training vs validation
set.seed(1000)
trainIndex_PBtree1=createDataPartition(thrones.dtree.full$is.dead,
                                      p=0.75, #proportion of data to be made training
                                      list=FALSE, #should the results be in a list?
                                      times = 1)
set.seed(1001)
trainIndex_PBtree2=createDataPartition(thrones.dtree.catty$is.dead,
                                       p=0.75, #proportion of data to be made training
                                       list=FALSE, #should the results be in a list?
                                       times = 1)
set.seed(1002)
trainIndex_PBtree3=createDataPartition(thrones.dtree.slim$is.dead,
                                       p=0.75, #proportion of data to be made training
                                       list=FALSE, #should the results be in a list?
                                       times = 1)

#create training data
thrones.dtree.train=thrones.dtree.full[trainIndex_PBtree1,]
#create validation data
thrones.dtree.validation=thrones.dtree.full[-trainIndex_PBtree1,]
#this is saying, from the thrones.dtree.full DF, grab all the rows that match the train index, and all the columns

#create first tree model - full
tree.model_PB_full=train(is.dead~.,
                    data=thrones.dtree.train,
                    method="rpart",
                    na.action=na.pass)
tree.model_PB_full #cp is complexity parameter. if cost of splitting node above, then tree stops
#so smaller CP is biased towards smaller trees

prp(tree.model_PB_full$finalModel,type = 2,extra = 106,varlen = 0, yesno = 2)
#1st # is binary predicted class
#2nd # is % of values in that node that have actual value of 1
#3rd # is % of values in that node as a percentage of total set

#assessing model accuracy
#full
#1. confusion matrix
prediction=predict(tree.model_PB_full,newdata = thrones.dtree.validation,na.action=na.pass)
confusionMatrix(prediction,thrones.dtree.validation$is.dead,positive="1")
table(thrones.dtree.validation$is.dead)

#2. ROC
tree.probabilities=predict(tree.model_PB_full,
                           newdata=thrones.dtree.validation,
                           type="prob",
                           na.action = na.pass)
tree.roc=roc(predictor=tree.probabilities$'1',
             response=thrones.dtree.validation$is.dead,
             levels=levels(thrones.dtree.validation$is.dead))
plot(tree.roc)
auc(tree.roc)

#Dataset #2
# all categorical variables
#create training data
thrones.dtree.train=thrones.dtree.catty[trainIndex_PBtree2,]
#create validation data
thrones.dtree.validation=thrones.dtree.catty[-trainIndex_PBtree2,]

#create second tree model - all categorical
tree.model_PB_catty=train(is.dead~.,
                         data=thrones.dtree.train,
                         method="rpart",
                         na.action=na.pass)
tree.model_PB_catty

prp(tree.model_PB_catty$finalModel,type = 2,extra = 106, varlen = 0, yesno = 2)
#1st # is binary predicted class
#2nd # is % of values in that node that have actual value of 1
#3rd # is % of values in that node as a percentage of total set

#assessing model accuracy
#categoricals
#1. confusion matrix
prediction=predict(tree.model_PB_catty,newdata = thrones.dtree.validation,na.action=na.pass)
confusionMatrix(prediction,thrones.dtree.validation$is.dead,positive="1")
table(thrones.dtree.validation$is.dead)

#2. ROC
tree.probabilities=predict(tree.model_PB_catty,
                           newdata=thrones.dtree.validation,
                           type="prob",
                           na.action = na.pass)
tree.roc=roc(predictor=tree.probabilities$'1',
             response=thrones.dtree.validation$is.dead,
             levels=levels(thrones.dtree.validation$is.dead))
plot(tree.roc)
auc(tree.roc)

#Dataset Three
# Just original categorical variables
#create training data
thrones.dtree.train=thrones.dtree.slim[trainIndex_PBtree3,]
#create validation data
thrones.dtree.validation=thrones.dtree.slim[-trainIndex_PBtree3,]

#create third tree model - all categorical originally
tree.model_PB_slim=train(is.dead~.,
                          data=thrones.dtree.train,
                          method="rpart",
                          na.action=na.pass)
tree.model_PB_slim

prp(tree.model_PB_slim$finalModel,type = 2,extra = 106,varlen = 0, yesno = 2)
#1st # is binary predicted class
#2nd # is % of values in that node that have actual value of 1
#3rd # is % of values in that node as a percentage of total set

#Evaluating Model Accuracy
#categorical only
#1. confusion matrix
prediction=predict(tree.model_PB_slim,newdata = thrones.dtree.validation,na.action=na.pass)
confusionMatrix(prediction,thrones.dtree.validation$is.dead,positive="1")
table(thrones.dtree.validation$is.dead)

#2. ROC
tree.probabilities=predict(tree.model_PB_slim,
                           newdata=thrones.dtree.validation,
                           type="prob",
                           na.action = na.pass)
tree.roc=roc(predictor=tree.probabilities$'1',
             response=thrones.dtree.validation$is.dead,
             levels=levels(thrones.dtree.validation$is.dead))
plot(tree.roc)
auc(tree.roc)


#Random Forests 
#A random forest model is an ensemble of classification or regression trees.
#The forest models were developed to overcome the instability that a single classification or regression tree exhibits with minor perturbations of the training data.

#Trees in the forest differ from each other in two ways:
#Training data for a tree is a sample from all observations.
#Input variables considered for splitting a node are randomly selected from available inputs. Only the variable most associated with the target is split for that node.

#to start: make one giant DF, leveraging stability of RF
thrones.rf = cbind(thrones.dtree.full,thrones.dtree.catty$long.life,thrones.dtree.catty$late.intro)
#change the column names
colnames(thrones.rf)[8] = "long.Life"
colnames(thrones.rf)[9] = "late.intro"

#validate
table(thrones.rf$long.Life)
table(thrones.dtree.catty$long.life)
table(thrones.rf$late.intro)
table(thrones.dtree.catty$late.intro)

#create the train index
#here, we switch up - have it run three times
#and grab a particular resample for each model
set.seed(1003)
trainIndex_PBrf1=createDataPartition(thrones.rf$is.dead,
                                       p=0.75, #proportion of data to be made training
                                       list=TRUE, #should the results be in a list?
                                       times = 3)

#Default RF
#create training data
thrones.rf.train=thrones.rf[trainIndex_PBrf1$Resample1,]
#create validation data
thrones.rf.validation=thrones.rf[-trainIndex_PBrf1$Resample1,]

rf_default=train(is.dead~.,
                 data = thrones.rf.train,
                 method="rf",
                 metric="Accuracy",
                 ntrees=15,
                 importance=TRUE
)
#leaving # of variables for each tree unspecified, 
#we will check in later on that
print(rf_default)
#xxx samples (should match train amount)


# Evaluate model performance
prediction <- predict(rf_default,newdata =  thrones.rf.validation)
confusionMatrix(prediction,thrones.rf.validation$is.dead,positive="1")


#Second - let's fine tune mtry
#new training set
#create training data
thrones.rf2.train=thrones.rf[trainIndex_PBrf1$Resample2,]
#create validation data
thrones.rf2.validation=thrones.rf[-trainIndex_PBrf1$Resample2,]


#go with random mtry values
tuneGrid=expand.grid(.mtry=c(sample(1:21,5)))

rf_mtry_random=train(is.dead~.,
                 data = thrones.rf2.train,
                 method="rf",
                 metric="Accuracy",
                 ntrees=15,
                 tuneGrid=tuneGrid, 
                 importance=TRUE
)
print(rf_mtry_random)

# Evaluate model performance
prediction <- predict(rf_mtry_random,newdata = thrones.rf2.validation)
confusionMatrix(prediction,thrones.rf2.validation$is.dead,positive="1")


#Third - let's try all possible mtry levels
#create training data
thrones.rf3.train=thrones.rf[trainIndex_PBrf1$Resample3,]
#create validation data
thrones.rf3.validation=thrones.rf[-trainIndex_PBrf1$Resample3,]

# More detailed model tuning to search the best mtry
tuneGrid <- expand.grid(.mtry=c(1:21)) #naming convention

rf_mtry_all <- train(is.dead~.,
                 data=thrones.rf3.train,
                 method='rf',
                 metric='Accuracy',
                 tuneGrid=tuneGrid,
                 importance=TRUE,
                 ntree=15)
print(rf_mtry_all)


# Evaluate model performance
prediction <- predict(rf_mtry_all,thrones.rf3.validation)
confusionMatrix(prediction,thrones.rf3.validation$is.dead,positive="1")

#variable importance section
varImp(rf_default)
varImp(rf_mtry_random)
varImp(rf_mtry_all) 
varImp(tree.model_PB_slim)
