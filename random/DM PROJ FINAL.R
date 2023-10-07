install.packages("arules")
install.packages("tidyverse")
install.packages("dplyr")
library(arules)
library(dplyr)
library(tidyverse)


#C:\Users\HP\Downloads\SLEEP QUALITY.csv
#C:\Users\HP\Downloads\SLEEP QUALITY.csv (1)

#RUNNING AN EXISTING IN-BUILT ALGORITHM (APRIORI)
df<-read.csv(file="https://raw.githubusercontent.com/riyaeliza123/sleepstress/main/SLEEP%20QUALITY.csv")
View(df)
colnames(df)

#renaming columns
colnames(df) <- c('Timestamp','Gender','Age','Major','Diagnosed?','Treatment','Bedtime','Fall_asleep','Wakeup_time','Sleep_time','Sleep.30min','Sleep.wakeUpMiddle','Sleep.bathroom','Sleep.snore','Sleep.cold','Sleep.hot','Sleep.badDreams','Sleep.pain','Stress.assignment','Stress.exams','Stress.ketchup','Stress.newPerson','Stress.help','Stress.confronting','Stress.noHelp')
View(df)

#label encoding
df$Sleep.30min=factor(df$Sleep.30min, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c('no','once','twice','more'))
df$Sleep.wakeUpMiddle=factor(df$Sleep.wakeUpMiddle, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c('no','once','twice','more'))
df$Sleep.bathroom=factor(df$Sleep.bathroom, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c('no','once','twice','more'))
df$Sleep.snore=factor(df$Sleep.snore, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c('no','once','twice','more'))
df$Sleep.cold=factor(df$Sleep.cold, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c('no','once','twice','more'))
df$Sleep.hot=factor(df$Sleep.hot, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c('no','once','twice','more'))
df$Sleep.badDreams=factor(df$Sleep.badDreams, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c('no','once','twice','more'))
df$Sleep.pain=factor(df$Sleep.pain, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c('no','once','twice','more'))
df


df.trial<-data.frame(df[4:6],df[19:25])
View(df.trial)

rules <- apriori(df.trial, parameter = list(supp = 0.3, conf = 0.9, target = "rules"))
inspect(rules)

#CONCLUSION:It is a stretch to correlate basic stress quotients to indications of mental illnesses, so moving to a more basic, direct approach

#USING INDEXING TO DERIVE NEW CONCLUSIONS
#Manipulated dataframe is dfs

dfs<-read.csv(file="https://raw.githubusercontent.com/riyaeliza123/sleepstress/main/SLEEP%20QUALITY.csv")
View(dfs)

#CALCULATE SLEEP INDEX
#renaming columns
colnames(dfs) <- c('Timestamp','Gender','Age','Major','Diagnosed?','Treatment','Bedtime','Fall_asleep','Wakeup_time','Sleep_time','Sleep.30min','Sleep.wakeUpMiddle','Sleep.bathroom','Sleep.snore','Sleep.cold','Sleep.hot','Sleep.badDreams','Sleep.pain','Stress.assignment','Stress.exams','Stress.ketchup','Stress.newPerson','Stress.help','Stress.confronting','Stress.noHelp')
View(dfs)

temp<-dfs #copying dfs into temp to manipulate dfs
View(temp)

#label encoding->dfs manipulated, all sleep related columns encoded
dfs$Sleep.30min=factor(dfs$Sleep.30min, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c(1,2,3,4))
dfs$Sleep.wakeUpMiddle=factor(dfs$Sleep.wakeUpMiddle, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c(1,2,3,4))
dfs$Sleep.bathroom=factor(dfs$Sleep.bathroom, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c(1,2,3,4))
dfs$Sleep.snore=factor(dfs$Sleep.snore, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c(1,2,3,4))
dfs$Sleep.cold=factor(dfs$Sleep.cold, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c(1,2,3,4))
dfs$Sleep.hot=factor(dfs$Sleep.hot, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c(1,2,3,4))
dfs$Sleep.badDreams=factor(dfs$Sleep.badDreams, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c(1,2,3,4))
dfs$Sleep.pain=factor(dfs$Sleep.pain, levels=c('Not during the past month','Less than once a week','Once or twice a week','Three or more times a week'), labels=c(1,2,3,4))

dfs$Fall_asleep=factor(dfs$Fall_asleep, levels=c('under 30 minutes','30 minutes','1 hour','1.5 hours', '2 hours', 'More time than that'), labels=c(1,2,3,4,5,6))
dfs$Sleep_time=factor(dfs$Sleep_time, levels=c('less than 4 hours of sleep','4-6 hours','7-8 hours','more than 8 hours'), labels=c(1,2,3,4))

View(dfs)

#creating df.sleep->containing all sleep related columns

df.sleep<-data.frame(dfs[8],dfs[10:18])
View(df.sleep)

colnames(df.sleep)
df.sleep_num<???lapply(df.sleep,as.numeric) #convert to numeric list

df.sleep<-data.frame(df.sleep_num) #convert back to dataframe

df.sleep$INDEX_sleep<- rowSums(df.sleep, na.rm=TRUE)
View(df.sleep)

#normalizing index--> max=41, min=1 as 1-8->1 , 9-16->2, 17-25->3, 26-33->4, 34-41->5
ctr<-0;
for(i in df.sleep$INDEX_sleep)
{
  ctr<-ctr+1;
  print(ctr)
  if (i<9)
  {
    df.sleep$normINDEX_sleep[ctr]<-1
  }
  else if (i<17)
  {
    df.sleep$normINDEX_sleep[ctr]<-2
  }
  else if (i<26)
  {
    df.sleep$normINDEX_sleep[ctr]<-3
  }
  else if(i<34)
  {
    df.sleep$normINDEX_sleep[ctr]<-4
  }
  else if(i>33)
  {
    df.sleep$normINDEX_sleep[ctr]<-5
  }
}

View(df.sleep)

#CALCULATE STRESS INDEX

#encoding->dfs manipulated, stress related columns are encoded
dfs$Stress.assignment=factor(dfs$Stress.assignment, levels=c('not stressed','mild','moderate','severe','very severe'), labels=c(1,2,3,4,5))
dfs$Stress.exams=factor(dfs$Stress.exams, levels=c('not stressed','mild','moderate','severe','very severe'), labels=c(1,2,3,4,5))
dfs$Stress.ketchup=factor(dfs$Stress.ketchup, levels=c('not stressed','mild','moderate','severe','very severe'), labels=c(1,2,3,4,5))
dfs$Stress.newPerson=factor(dfs$Stress.newPerson, levels=c('not stressed','mild','moderate','severe','very severe'), labels=c(1,2,3,4,5))
dfs$Stress.help=factor(dfs$Stress.help, levels=c('not stressed','mild','moderate','severe','very severe'), labels=c(1,2,3,4,5))
dfs$Stress.confronting=factor(dfs$Stress.confronting, levels=c('not stressed','mild','moderate','severe','very severe'), labels=c(1,2,3,4,5))
dfs$Stress.noHelp=factor(dfs$Stress.noHelp, levels=c('not stressed','mild','moderate','severe','very severe'), labels=c(1,2,3,4,5))
View(dfs)

#creating df.stress->containing all stress related columns
df.stress<-data.frame(dfs[19:25])

df.stress_num<???lapply(df.stress,as.numeric) #convert to numeric list
summary(df.stress_num)

df.stress<-data.frame(df.stress_num) #convert back to dataframe

df.stress$INDEX_stress<- rowSums(df.stress, na.rm=TRUE)
View(df.stress)

#normalizing index--> max=35, min=1: 1-7->1, 8-14->2, 15-21->3, 22-28->4, 29-35->5
ctr<-0;
for(i in df.stress$INDEX_stress)
{
  ctr<-ctr+1;
  
  if (i<8)
  {
    df.stress$normINDEX_stress[ctr]<-1
  }
  else if (i<15)
  {
    df.stress$normINDEX_stress[ctr]<-2
  }
  else if (i<22)
  {
    df.stress$normINDEX_stress[ctr]<-3
  }
  else if(i<29)
  {
    df.stress$normINDEX_stress[ctr]<-4
  }
  else if(i>28)
  {
    df.stress$normINDEX_stress[ctr]<-5
  }
}

View(df.stress)

#merging 2 dataframes to find total index, dataframe is final_index
final_index<-data.frame(df.sleep$normINDEX_sleep, df.stress$normINDEX_stress)
View(final_index)

final_index$TOTAL_INDEX<- rowSums(final_index, na.rm=TRUE)
View(final_index)
summary(final_index$TOTAL_INDEX)

#GETTING FINAL DATAFRAME TO DERIVE CONCLUSIONS

final<-data.frame(dfs[2:4],final_index,dfs[5:6])
colnames(final)

colnames(final) <- c('Gender','Age','Major','Sleep_index','Stress_index','Total','Diagnosed','Treatment')
View(final)

#rules <- apriori(final, parameter = list(supp = 0.3, conf = 0.9, target = "rules"))
#inspect(rules)

# Merge sleep_index, stress_index, total_index ---> dataframe called complete
complete<-data.frame(temp,final[4:6])
View(complete)

#QUESTIONS FOR CONCLUSIONS:
#1. Diagnosed people-> what is their stress and sleep pattern; Who else(non-diagnosed) has 
#   same pattern as diagnosed people-->can indicate possibility of mental illness
 diag_yes<-filter(complete, Diagnosed.=='Yes')
 View(diag_yes)
 #avg total score=?
 mean(diag_yes$Total) #6.7778
 #take a list of non-diagnosed above score 6
 notdiag_7<-filter(complete, Diagnosed.=='No', Total>6)
 View(notdiag_7)
 count(notdiag_7) #24
 
 #non-diagnosed=116-9=107 [ie; total-diagnosed]
 #24 non-diagnosed show same behavior as diagnosed
 24*100/107 #22.42
 print(paste("That is 22.42% of non diagnosed people show signs of mental distress"))
 #that is, 22.42% of non diagnosed people show signs of mental distress
 
#2. Analyse people with irregular sleep patters (that is; sleep between 1am-3am , sleep for upto 6 hrs )
 
 latesleepers<-filter(complete, Bedtime=='1am-3am', Sleep_time %in% c("less than 4 hours of sleep", "4-6 hours"))
 View(latesleepers)
 
 count(latesleepers) #19
 #19/116 i.e; 16.37% have less sleep
 #check how many have total index above 6, meaning show signs of mental distress
 
 count(filter(latesleepers,Total>6)) #7
 count(filter(latesleepers,Total>6, Diagnosed.=="Yes")) #1
 #7/19 show signs of mental distress. i.e; 36.87% of late sleepers show signs of mental distress, out of which 
 #14.28% have been diagnosed
 
#3. Diagnosed(9) not equal to treated(12)-> who, why?, patterns
 treatment_yes<-filter(complete, Treatment=='Yes')
 View(treatment_yes)
 # less data about treated people, but if we ran a survey just for them, asking more details like time and
 # circumstance of diagnosis, we can draw better conclusions 
 
#4. More than 2 hrs to fall asleep->stress pattern? Demographic?
 moresleep<-filter(complete, Fall_asleep=='More time than that')
 View(moresleep)
 
#visualization
#Visualization of the stats calculated
  # 1.Diagnosed v/s undiagnosed
    z<-as.integer(count(diag_yes))
    y<-as.integer(count(dfs)-z)
    x<-c(z,y)
    x
    labels<-c("Diagnosed","Un-diagnosed")
    pie(x,labels, main="Diagnosed v/s undiagnosed in the total strength")
    
  # 2.In non-diagnosed-> what percentage show signs of mental illness
    total<-y
    possible_ill<-as.integer(count(notdiag_7))
    fit<-as.integer(total-possible_ill)
    x<-c(fit, possible_ill)
    labels<-c("Low", "High")
    pie(x,labels, main="Potential for mental illness among non-diagnosed individuals")
    
  # 3. Analysis of late-sleepers : latesleepers showing signs of mental distress
    total<-as.integer(count(latesleepers))
    distressed<-as.integer(count(filter(latesleepers,Total>6)))
    not<-total-distressed
    x<-c(distressed,not)
    labels<-c("Distressed","Not Distressed")
    pie(x,labels, main="Late sleepers showing signs of mental distress")
    
# Visualization of diagnosed with respect to gender
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    
    gender_frame<-data.frame(final$Diagnosed,final$Gender)
    for(i in gender_frame)
    {
      gender_frame$Count<-1
    }
    colnames(gender_frame)<-c("Diagnosed","Gender","Count")
    View(gender_frame)
    
    summarised<-gender_frame %>% group_by(Gender, Diagnosed) %>% summarise(Total=sum(Count))
    #View(summarised)
    
    ggplot(summarised, aes(fill=Diagnosed, y=Total, x=Gender)) + geom_bar(position="fill", stat="identity")


 


