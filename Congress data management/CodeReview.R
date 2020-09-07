#Haoxi Ma  
#Just run the codes step by step
#Finish assigments and draw a plot for states corresponding to #congress member

setwd("/Users/mahaoxi/Desktop/project/Congress data management")

library(ggplot2)
library(mice)
library(plotrix)
library(maps)
library(dplyr)


load("Congress99.RData")
#test missing data in mice package
md.pattern(dat99)
str(dat99)
names(dat99)
#note: there is no missing value but have some "" value
#in facotr, this correspond to redundancy levels

#Arrange voting data in two tables 
Table1<-dat99[,c("vote_legislator.text","vote_legislator.party","vote_legislator.state",
                 "roll_number")]
names(Table1)<-c("Name","Party","State","roll-number")
Table2<-dat99[,c("roll_number","issue","question","title_or_description","vote_result")]
names(Table2)<-c("roll-number","issue number","question","title/description","votes")

#question1
congress.member<-Table1[!duplicated(Table1$Name),]
str(congress.member)#there is "" value in Party and state
levels(congress.member$Party)
congress.member<-congress.member[congress.member$Party!="",]
congress.member<-congress.member[congress.member$State!="",]
#delete redundancy levels
congress.member$Party<-factor(congress.member$Party)
congress.member$State<-factor(congress.member$State)
table(congress.member$Party)

#question2
ggplot(data=congress.member,aes(x=State))+geom_bar(aes(fill=Party))
table(congress.member[c("Party","State")])

#question3
#function that when same vote_result in R and D, give x=1 else x=0
Sel<-function(x){
  dat.D<-dat99 %>% filter(roll_number==x) %>% filter(vote_legislator.party=="D")
  dat.R<-dat99 %>% filter(roll_number==x) %>% filter(vote_legislator.party=="R")
  tab.D<-table(dat.D["vote_result"]);tab.R<-table(dat.R["vote_result"])
  ifelse(tab.D["Yea"]>tab.D["Nay"],result.D<-"Agree",result.D<-"Disagree")
  ifelse(tab.R["Yea"]>tab.R["Nay"],result.R<-"Agree",result.R<-"Disagree")
  ifelse(result.D!=result.R,x<-0,x<-1)
}
sketch<-seq(1,611,1);sketch<-unlist(lapply(sketch,Sel));sketch
index<-seq(1,611,1)[sketch==0];
#so take 609 roll-number
dat609.D<-dat99 %>% filter(roll_number==609) %>% filter(vote_legislator.party=="D")
dat609.R<-dat99 %>% filter(roll_number==609) %>% filter(vote_legislator.party=="R")
table(dat609.D["vote_result"]);table(dat609.R["vote_result"])
#so in D there are 207 people and in R, there are 217 people

#question4-draw pie3D 
#1
levels(dat99$result)
res<-dat99[dat99$result!="",]
res$result<-factor(res$result)
res<-res[!duplicated(res$roll_number),]
tabular<-as.data.frame(table(res$result))
num_P<-tabular$Freq[which(tabular$Var1=="P")]
num_F<-tabular$Freq[which(tabular$Var1=="F")]
num_other<-sum(tabular$Freq[which(tabular$Var1!="P"&tabular$Var1!="F")])
number<-c(num_P,num_F,num_other);Name<-c("Passed","Failed","Other")
pct<-round(number/sum(number)*100,digit=1)
lbls<-paste(Name,pct) %>% paste("%",sep="")
pie3D(number,labels=lbls,col=rainbow(length(lbls)),main="Vote Results",theta = pi/4)
#2
table(sketch)#76

#question5
str(Table2)
Table2<-Table2[which(Table2$`issue number`!="" & Table2$votes!=""),]
Table2$`issue number`<-factor(Table2$`issue number`)
Table2$votes<-factor(Table2$votes)
iss.votes<-as.data.frame.matrix(table(Table2[c("issue number","votes")]))
iss.votes

#draw a plot for states corresponding to #congress member
member<-as.data.frame(table(congress.member$State))
names(member)<-c("region","number")
#order the region
member$region<-factor(member$region,levels=state.abb)
levels(member$region)<-state.name
member$region<-tolower(as.character(member$region))
map_state<-map_data("state")
num.member<-left_join(map_state,member,by="region")
mhx<-ggplot(num.member,aes(long,lat,group=group))+geom_polygon(aes(fill=number),color="white")+
      scale_fill_viridis_c(option="C")+labs(title="#Congress members of states")+
      theme(axis.text = element_blank(),axis.ticks = element_blank(),
            axis.title = element_blank())
mhx











