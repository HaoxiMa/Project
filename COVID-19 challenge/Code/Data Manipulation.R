setwd("/Users/mahaoxi/Desktop/competition/Useful dataset")

library(mice)
library(dplyr)
library(tidyr)
library(stringr)

#Manipulate SVI dataset
raw_SVI<-read.csv("California_SVI.csv")
SVI<-raw_SVI %>% filter(COUNTY =="Los Angeles")
SVI<-SVI[,c("FIPS","RPL_THEME1","RPL_THEME2","RPL_THEME3","RPL_THEME4","RPL_THEMES","AREA_SQMI")]
names(SVI)<-c("FIPS","Socioeconomic","Household Composition and Disability","Minority Status and Language",
              "Housing Type and Transportation","SVI","Area.SQMI")

#Manipulate Population and race dataset
raw_census<-read.csv("Census_population_race.csv")
other<-raw_census$HPIPOP_D+raw_census$OTHPOP_D+raw_census$MMRPOP_D
raw_census<-cbind(raw_census[,c(2:8)],other)
names(raw_census)<-c("Tract.Number","Population","Latin","White","Black","Native American","Asian","Other population")

#Left join Population_race dataset with spatial dataset
raw_negi<-read.csv("LA_spatial.csv")
cen_negi<-left_join(raw_census,raw_negi,by="Tract.Number")
md.pattern(cen_negi)
paste_FIPS<-function(S_FIPS){
  paste("6037",S_FIPS,sep="")
}
L_FIPS<-unlist(lapply(cen_negi$Tract.Number,paste_FIPS))
cen_negi$Tract.Number<-L_FIPS
negi<-cen_negi[,c(1:8,11,14,13)]
names(negi)[1]<-"FIPS"
negi<-negi %>% arrange(Neighborhood)
negi<-negi[-c(1011,1012),]
md.pattern(negi)

#Add SVI imformation into the dataset
SVI$FIPS<-as.character(SVI$FIPS)
Combine1<-merge(SVI,negi,by="FIPS")
md.pattern(Combine1)
#delete some worng va
Combine1<-Combine1 %>% arrange(Neighborhood) %>% filter(Socioeconomic!=-999,`Household Composition and Disability`!=-999,
                                                        `Minority Status and Language`!=-999,
                                                        `Housing Type and Transportation`!=-999,
                                                        SVI!=-999)
#Summarize all features by negiborhoods
Combine1<-Combine1 %>% group_by(Neighborhood) %>%
  mutate(mean.Socioeconomic=mean(Socioeconomic,na.rm = TRUE),
         mean.HouseholdComposition.and.Disability= mean(`Household Composition and Disability`,na.rm =TRUE ),
         mean.Minority.Status.and.Language = mean(`Minority Status and Language`,na.rm=TRUE),
         mean.Housing.Type.and.Transportation = mean(`Housing Type and Transportation`,na.rm = TRUE),
         mean.SVI = mean(SVI,na.rm = TRUE),
         Area = sum(Area.SQMI,na.rm = TRUE),
         Population = sum(Population,na.rm = TRUE),
         Latin = sum(Latin,na.rm = TRUE),
         White = sum(White,na.rm=TRUE),
         Black = sum(Black,na.rm = TRUE),
         Native.American=sum(`Native American`,na.rm = TRUE),
         Asian = sum(Asian,na.rm = TRUE),
         Other.population= sum(`Other population`,na.rm = TRUE))
Combine1<-Combine1[!duplicated(Combine1$Neighborhood),]   
#Deal with the missing value ---using mean value to replace
Combine1[c(16,88,95,103,104,118),8]<-mean(Combine1$Population[-c(16,88,95,103,104,118)])
Combine1[c(16,88,95,103,104,118),9]<-mean(Combine1$Latin[-c(16,88,95,103,104,118)])
Combine1[c(16,88,95,103,104,118),10]<-mean(Combine1$White[-c(16,88,95,103,104,118)])
Combine1[c(16,88,95,103,104,118),11]<-mean(Combine1$Black[-c(16,88,95,103,104,118)])
Combine1[c(16,88,95,103,104,118),12]<-mean(Combine1$`Native American`[-c(16,88,95,103,104,118)])
Combine1[c(16,88,95,103,104,118),13]<-mean(Combine1$Asian[-c(16,88,95,103,104,118)])
Combine1[c(16,88,95,103,104,118),14]<-mean(Combine1$`Other population`[-c(16,88,95,103,104,118)])    
Summary_data<-Combine1[,c(15,1,8:14,18:21,16,17,23)]
#Calculate race proportion by neighborhood
Summary_data$Latin<-round(Summary_data$Latin/Summary_data$Population,digits=3)
Summary_data$White<-round(Summary_data$White/Summary_data$Population,digits=3)
Summary_data$Black<-round(Summary_data$Black/Summary_data$Population,digits=3)
Summary_data$`Native American`<-round(Summary_data$`Native American`/Summary_data$Population,digits=3)
Summary_data$Asian<-round(Summary_data$Asian/Summary_data$Population,digits=3)
Summary_data$`Other population`<-round(Summary_data$`Other population`/Summary_data$Population,digits=3)
Summary_data[,c(10:13)]<-round(Summary_data[,c(10:13)],3)

#Manipulate Data and case dataset
case<-read.csv("LA_CASE.csv")
Data1<-Summary_data[!duplicated(Summary_data$Neighborhood),]
Data1$Population<-round(Data1$Population)
case<-case[,c(2,5,8,11)]
names(case)<-c("Neighborhood","Adjusted.case_rate","Adjusted.death_rate","Adjusted.testing_rate")
delete.city<-function(city){
  gsub("City of ","",city)
}
D.city<-unlist(lapply(case$Neighborhood,delete.city))
case$Neighborhood<-D.city
delete.LA<-function(LA){
  gsub("Los Angeles - ","",LA)
}
D.LA<-unlist(lapply(case$Neighborhood,delete.LA))
case$Neighborhood<-D.LA
delete.corp<-function(corp){
  gsub("Unincorporated - ","",corp)
}
D.corp<-unlist(lapply(case$Neighborhood,delete.corp))
case$Neighborhood<-D.corp

#merge two dataset 
Summary_new<-merge(case,Data1)
Summary_new<-Summary_new[,c(1,5,2:4,13:16,6:12,17:19)]
md.pattern(Summary_new)

#now we are going to add diabetes data
name_zipcode<-read.csv("Name_zipcode.csv")#This dataset help correspond neigborhood's name to zipcode
name_zipcode<-name_zipcode[,c(2,4)]
diabetes<-read.csv("diabetes.csv",header=TRUE)
for (negi in Summary_new$Neighborhood) {
  mhxx<-name_zipcode %>% filter(str_detect(Neighborho,negi))
  zipcode<-mhxx[,1]
  diaa<-diabetes[which(diabetes$ZIPCODE %in% zipcode),]
  percent<-sum(diaa$Percent_*diaa$Population)/sum(diaa$Population)
  Summary_new[which(Summary_new$Neighborhood==negi),20]<-percent
}
names(Summary_new)[20]<-"Diabetes_Percent"
Summary_new[is.na(Summary_new$Diabetes_Percent),20]<-mean(Summary_new$Diabetes_Percent,na.rm=TRUE) 
Summary_new$Diabetes_Percent<-round(Summary_new$Diabetes_Percent,digits = 3)
Summary_new<-Summary_new[,c(1:9,20,10:19)]

write.csv(Summary_new,"dataset_MODEL1.csv")

#now we get the dataset for risk prediction 






