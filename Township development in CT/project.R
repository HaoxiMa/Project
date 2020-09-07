setwd("/Users/mahaoxi/Desktop/project/Township development in CT")
crime<-read.csv("crime.csv")
register_google(key="AIzaSyAwG6H0vMmhNDmFJnJC6-KV0KwWbAYUl5M")

library(XML)
#retrive table from internet
library(dplyr)
library(stringr)
#data refine and string modify
library(mice)
#check missing value
library(ggplot2)
library(ggmap)
#draw spatial map
library(factoextra)
#do clustering

#retrive table from website
myurl<-"https://en.wikipedia.org/wiki/List_of_Connecticut_locations_by_per_capita_income"
#data comes from 2011-2015 American Community Survey 5-Year Estimates
download.file(url=myurl, destfile = "CTincomes.html", method = "curl")
doc<-htmlParse("CTincomes.html")
table<-getNodeSet(doc,"//table")
tb<-readHTMLTable(table[[3]])

#refine income table
income<-tb[-1,-1]
nam<-c("Town","Type","County","Per.income",
       "Median.household.income","Median.family.income","Population","Num.households")
rownames(income)<-1:nrow(income)
names(income)<-nam

#get lat and lon
total_crime<-crime %>% filter(str_detect(Crime.Type,"Total Crime")) %>% 
  filter(str_detect(Measure.Type,"Rate"))
towns<-total_crime$Town
paste_town<-function(town){
  paste(town,", CT, US",sep="")
}
loc<-unlist(lapply(towns,paste_town))
total_crime$Town<-loc
town_unique<-unique(total_crime$Town)
Lat_Lon<-lapply(town_unique,geocode) 
lat_lon<-unlist(Lat_Lon)
lat_lon<-as.vector(lat_lon)
ma_lat_lon<-matrix(lat_lon,ncol=2,byrow=TRUE)
town_lat_lon<-data.frame(Town=town_unique,Longitude=ma_lat_lon[,1],Latitude=ma_lat_lon[,2])
crime.lat<-merge(total_crime,town_lat_lon)

#merge income and crime.lat
income$Town<-lapply(income$Town,paste_town)
crime.income<-merge(crime.lat,income)
crime.income$Town<-factor(crime.income$Town)
str(crime.income)#County and Type have redundancy levels
crime.income<-crime.income[crime.income$County!="",]
crime.income$County<-factor(crime.income$County)
crime.income<-crime.income[crime.income$Type!="",]
crime.income$Type<-factor(crime.income$Type)
crime.income<-arrange(crime.income,Town,Year)
md.pattern(crime.income)

#density plot for towns' distribution 
CTMap<-qmap("Connecticut",zoom=8,legend="topleft")
CTheat<-CTMap+geom_density2d(data=crime.income,aes(x=Longitude,y=Latitude),size=0.3)+
  stat_density2d(data=crime.income,aes(x=Longitude,y=Latitude,fill=..level..,alpha=..level..),
                 size=0.01,bins=20,geom="polygon")+
  scale_fill_gradient(low="yellow",high="violet")+
  scale_alpha(range=c(0.1,0.2),guide=FALSE)+
  ggtitle("The Heat Map of Towns' distribution in Connecticut")+
  theme(legend.position = "bottom")
CTheat

#add cities location to heat plot
city<-crime.income[!duplicated(crime.income$Town),]
city<-city %>% filter(Type=="City")
ctheat<-CTheat+geom_point(aes(x=Longitude,y=Latitude),data=city,color="red",shape=1)
ctheat

#clustering 
#summarize dataset
crime.income$Per.income<-as.numeric(gsub("[\\$,]","",crime.income$Per.income))
crime.income$Median.household.income<-as.numeric(gsub("[\\$,]","",crime.income$Median.household.income))
crime.income$Median.family.income<-as.numeric(gsub("[\\$,]","",crime.income$Median.family.income))
crime.income$Population<-as.numeric(gsub(",","",crime.income$Population))
crime.income$Num.households<-as.numeric(gsub(",","",crime.income$Num.households))
names(crime.income)
data.cluster<-crime.income[c("Town","Value","Per.income","Median.household.income","Median.family.income",
                             "Population","Num.households")]
data.cluster<-data.cluster %>% group_by(Town) %>% mutate(meancrime = mean(Value))
data.cluster<-data.cluster[,-2]
data.cluster<-data.cluster[!duplicated(data.cluster$Town),]
str(data.cluster)
data.cluster<-as.data.frame(data.cluster)
rownames(data.cluster)<-data.cluster$Town
data.cluster<-data.cluster[,-1]
#do clustering
data.cluster<-scale(data.cluster)
kmeans<-kmeans(data.cluster,centers=3,nstart=25)
str(kmeans)
fviz_cluster(kmeans,data=data.cluster,geom="point")
#summarize
Towns<-rownames(data.cluster)
cluster<-rep(0,length(Towns))
cluster[kmeans$cluster==1]<-2;cluster[kmeans$cluster==2]<-3;cluster[kmeans$cluster==3]<-1
index<-data.frame(Town=Towns,Cluster=cluster)
crime.income<-merge(crime.income,index)
crime.income$Cluster<-factor(crime.income$Cluster)

#plot for classified towns
crime.income2<-crime.income[!duplicated(crime.income$Town),]
crime.income2<-arrange(crime.income2,Cluster)
crime.income2 %>% group_by(Cluster) %>% summarize(mean.crime = mean(Value),mean.Per.income = mean(Per.income),
                                                  mean.M.H.=mean(Median.household.income),
                                                  mean.M.F=mean(Median.family.income),
                                                  mean.Popu=mean(Population),
                                                  mean.N.H=mean(Num.households))
CTscatter<-CTMap+geom_point(aes(x=Longitude,y=Latitude,color=Cluster,shape=Cluster),data=crime.income)+
  ggtitle("The Scatter plot of classified Towns in Connecticut")+
  theme(legend.position = "bottom")
CTscatter

#cluster summarize
table(crime.income2$Type,crime.income2$Cluster)








