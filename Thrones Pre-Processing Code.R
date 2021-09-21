#load and install
library(survival) #for computing survival analyses
library(survminer) #summarizing and visualizing the results of survival analysis
library(tidyverse) 
library(ggplot2)

#load the data
thrones=read.csv("./Data/character-deaths.csv")
battles = read.csv("./Data/battles.csv") #for later
predictions = read.csv("./Data/character-predictions.csv") #for later

#check out initial structure 
str(thrones) #do not make anything a factor!

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
#final checks: 
str(thrones)
table(thrones$is.dead)

