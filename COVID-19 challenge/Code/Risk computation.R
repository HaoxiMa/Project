setwd("/Users/mahaoxi/Desktop/competition/Useful dataset")

library(ggplot2)
library(tidyr)
library(dplyr)

data<-read.csv("dataset_MODEL1.csv")
data<-data[,-1]

boxplot(data$Adjusted.death_rate,plot = FALSE)

data$Risk<-rep(0,nrow(data))
data$Risk[which(data$Adjusted.death_rate>=0 & data$Adjusted.death_rate<8.5)]<-1#low
data$Risk[which(data$Adjusted.death_rate>=8.5 & data$Adjusted.death_rate<16)]<-2#medium
data$Risk[which(data$Adjusted.death_rate>=16)]<-3#high

write.csv(data,"Risk_neig.csv")

mhx<-data %>% group_by(Risk) %>% 
  summarize(mean.Adjusted.case_rate=mean(Adjusted.case_rate),
            mean.Adjusted.test_rate=mean(Adjusted.testing_rate),
             Socioeconomic=mean(mean.Socioeconomic),
            `HouseholdComposition and Disability`=mean(mean.HouseholdComposition.and.Disability),
            `Minority and language`=mean(mean.Minority.Status.and.Language),
            `HousingType and transport`=mean(mean.Housing.Type.and.Transportation),
            mean.diabetes=mean(Diabetes_Percent),
            mean.Population=mean(Population),
            mean.Latin=mean(Latin),
            mean.White=mean(White),
            mean.Black=mean(Black),
            mean.NativeAmer=mean(Native.American),
            mean.Asian=mean(Asian),
            mean.Area=mean(Area))
mhx<-as.data.frame(mhx)


write.csv(mhx,"Risk_summary.csv")



