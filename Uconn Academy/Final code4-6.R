setwd("/Users/mahaoxi/Desktop/Uconn Academy/Phase2 datasets")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(carData)
library(car)#Anova
library(asbio)#pairwise
library(pastecs)#descriptive analysis
library(exact2x2)#fisher exact test
library(forestplot)
library(corrplot)


#######Q4(a)
ADPA<-read.csv("ADPA.csv")
str(ADPA)

#Check whether TRTP is equal to TRTA
table(ADPA$TRTP==ADPA$TRTA)

#Data manipulate
ADPA$TRTA<-factor(ADPA$TRTA,
                      levels=c("Placebo","Test drug 70mg","Test drug 140mg","Test drug 210mg","Test drug 280mg"))
New_ADPA<-ADPA %>% select(SUBJID,TRTA,AVISITN,PARAMCD,CHG,PCHGCA1N,PCHGCA2N,PCHGCA3N,PCHGCA4N)

#plot PASI score
Plot_PASI<-New_ADPA %>% select(SUBJID,TRTA,AVISITN,PARAMCD,CHG) %>% filter(PARAMCD=="PASI")
Plot_PASI$Improve<-ifelse((Plot_PASI$CHG>0),1,0)
Plot_PASI<-Plot_PASI %>% group_by(TRTA,AVISITN) %>% mutate(Percent_Improvement=mean(Improve)) %>% ungroup() %>% 
  distinct(TRTA,AVISITN,.keep_all = TRUE)
Plot_PASI$label<-scales::percent(Plot_PASI$Percent_Improvement,accuracy = 0.01)
Plot_PASI$label[Plot_PASI$AVISITN==2]<-NA
gg_PASI<-ggplot(Plot_PASI,aes(x=AVISITN,y=Percent_Improvement*100))+
  geom_line(aes(col=TRTA),size=1.5)+geom_point(aes(col=TRTA),size=3)+
  labs(title="VISIT vs Improvement Percent Of PASI Score",subtitle="Classifed by doses",x="VISIT",y="Percent of Responders")+
  geom_text_repel(aes(label=label),box.padding=0.35, point.padding = 0.5) +
  theme_light()+theme(legend.position="top",legend.title = element_blank())
plot(gg_PASI)

#plot PASI50
Plot_PASI50<-New_ADPA %>% select(SUBJID,TRTA,AVISITN,PARAMCD,PCHGCA1N) %>% filter(PARAMCD=="PASI")
Plot_PASI50$PCHGCA1N[Plot_PASI50$AVISITN==2]<-0
Plot_PASI50<-Plot_PASI50 %>% group_by(TRTA,AVISITN) %>% mutate(Percent_Improvement50=mean(PCHGCA1N)) %>% ungroup() %>% 
  distinct(TRTA,AVISITN,.keep_all = TRUE)
Plot_PASI50$label<-scales::percent(Plot_PASI50$Percent_Improvement50,accuracy = 0.01)
Plot_PASI50$label[Plot_PASI50$AVISITN==2]<-NA
Plot_PASI50$label[c(7,12,17,22)]<-NA #just for better looking
gg_PASI50<-ggplot(Plot_PASI50,aes(x=AVISITN,y=Percent_Improvement50*100))+
  geom_line(aes(col=TRTA),size=1.5)+geom_point(aes(col=TRTA),size=3)+
  labs(title="VISIT vs Improvement Percent Of PASI 50",subtitle="Classifed by doses",x="VISIT",y="Percent of Responders")+
  geom_text_repel(aes(label=label),box.padding=0.35, point.padding = 0.5) +
  theme_light()+theme(legend.position="top",legend.title = element_blank())
plot(gg_PASI50)

#plot PASI75
Plot_PASI75<-New_ADPA %>% select(SUBJID,TRTA,AVISITN,PARAMCD,PCHGCA2N) %>% filter(PARAMCD=="PASI")
Plot_PASI75$PCHGCA2N[Plot_PASI75$AVISITN==2]<-0
Plot_PASI75<-Plot_PASI75 %>% group_by(TRTA,AVISITN) %>% mutate(Percent_Improvement75=mean(PCHGCA2N)) %>% ungroup() %>% 
  distinct(TRTA,AVISITN,.keep_all = TRUE)
Plot_PASI75$label<-scales::percent(Plot_PASI75$Percent_Improvement75,accuracy = 0.01)
Plot_PASI75$label[Plot_PASI75$AVISITN==2]<-NA
Plot_PASI75$label[c(7,8,12,13,17,18,22,23)]<-NA #just for better looking
gg_PASI75<-ggplot(Plot_PASI75,aes(x=AVISITN,y=Percent_Improvement75*100))+
  geom_line(aes(col=TRTA),size=1.5)+geom_point(aes(col=TRTA),size=3)+
  labs(title="VISIT vs Improvement Percent Of PASI 75",subtitle="Classifed by doses",x="VISIT",y="Percent of Responders")+
  geom_text_repel(aes(label=label),box.padding=0.35, point.padding = 0.5) +
  theme_light()+theme(legend.position="top",legend.title = element_blank())
plot(gg_PASI75)

#plot PASI90
Plot_PASI90<-New_ADPA %>% select(SUBJID,TRTA,AVISITN,PARAMCD,PCHGCA3N) %>% filter(PARAMCD=="PASI")
Plot_PASI90$PCHGCA3N[Plot_PASI90$AVISITN==2]<-0
Plot_PASI90<-Plot_PASI90 %>% group_by(TRTA,AVISITN) %>% mutate(Percent_Improvement90=mean(PCHGCA3N)) %>% ungroup() %>% 
  distinct(TRTA,AVISITN,.keep_all = TRUE)
Plot_PASI90$label<-scales::percent(Plot_PASI90$Percent_Improvement90,accuracy = 0.01)
Plot_PASI90$label[Plot_PASI90$AVISITN==2]<-NA
Plot_PASI90$label[c(4,7,8,9,12,13,17,18,22,23)]<-NA #just for better looking
gg_PASI90<-ggplot(Plot_PASI90,aes(x=AVISITN,y=Percent_Improvement90*100))+
  geom_line(aes(col=TRTA),size=1.5)+geom_point(aes(col=TRTA),size=3)+
  labs(title="VISIT vs Improvement Percent Of PASI 90",subtitle="Classifed by doses",x="VISIT",y="Percent of Responders")+
  geom_text_repel(aes(label=label),box.padding=0.35, point.padding = 0.5) +
  theme_light()+theme(legend.position="top",legend.title = element_blank())
plot(gg_PASI90)

#plot PASI100
Plot_PASI100<-New_ADPA %>% select(SUBJID,TRTA,AVISITN,PARAMCD,PCHGCA4N) %>% filter(PARAMCD=="PASI")
Plot_PASI100$PCHGCA4N[Plot_PASI100$AVISITN==2]<-0
Plot_PASI100<-Plot_PASI100 %>% group_by(TRTA,AVISITN) %>% mutate(Percent_Improvement100=mean(PCHGCA4N)) %>% ungroup() %>% 
  distinct(TRTA,AVISITN,.keep_all = TRUE)
Plot_PASI100$label<-scales::percent(Plot_PASI100$Percent_Improvement100,accuracy = 0.01)
Plot_PASI100$label[Plot_PASI100$AVISITN==2]<-NA
Plot_PASI100$label[c(4,7,8,9,12,13,14,17,18,19,22,23)]<-NA #just for better looking
gg_PASI100<-ggplot(Plot_PASI100,aes(x=AVISITN,y=Percent_Improvement100*100))+
  geom_line(aes(col=TRTA),size=1.5)+geom_point(aes(col=TRTA),size=3)+
  labs(title="VISIT vs Improvement Percent Of PASI 100",subtitle="Classifed by doses",x="VISIT",y="Percent of Responders")+
  geom_text_repel(aes(label=label),box.padding=0.35, point.padding = 0.5) +
  theme_light()+theme(legend.position="top",legend.title = element_blank())
plot(gg_PASI100)

#plot sPGA
Plot_sPGA<-New_ADPA %>% select(SUBJID,TRTA,AVISITN,PARAMCD,CHG) %>% filter(PARAMCD=="sPGA")
Plot_sPGA$Improve<-ifelse((Plot_sPGA$CHG>0),1,0)
Plot_sPGA<-Plot_sPGA %>% group_by(TRTA,AVISITN) %>% mutate(Percent_Improvement=mean(Improve)) %>% ungroup() %>% 
  distinct(TRTA,AVISITN,.keep_all = TRUE)
Plot_sPGA$label<-scales::percent(Plot_sPGA$Percent_Improvement,accuracy = 0.01)
Plot_sPGA$label[Plot_sPGA$AVISITN==2]<-NA
gg_sPGA<-ggplot(Plot_sPGA,aes(x=AVISITN,y=Percent_Improvement*100))+
  geom_line(aes(col=TRTA),size=1.5)+geom_point(aes(col=TRTA),size=3)+
  labs(title="VISIT vs Improvement Percent Of sPGA Score",subtitle="Classifed by doses",x="VISIT",y="Percent of Responders")+
  geom_text_repel(aes(label=label),box.padding=0.35, point.padding = 0.5) +
  theme_light()+theme(legend.position="top",legend.title = element_blank())
plot(gg_sPGA)

########Q4(b)
#Test all PASI score
Test_all_PASI<-New_ADPA %>% select(SUBJID,TRTA,AVISITN,PARAMCD,PCHGCA1N,PCHGCA2N,PCHGCA3N,PCHGCA4N) %>%
  filter(PARAMCD=="PASI",AVISITN==6)
str(Test_all_PASI)

#Summary of PASI
table(Test_all_PASI$TRTA)
table(Test_all_PASI$TRTA,Test_all_PASI$PCHGCA1N)
table(Test_all_PASI$TRTA,Test_all_PASI$PCHGCA2N)
table(Test_all_PASI$TRTA,Test_all_PASI$PCHGCA3N)
table(Test_all_PASI$TRTA,Test_all_PASI$PCHGCA4N)
CI<-function(S,N){
  p<-(S+2)/(N+4)
  left<-p-1.96*sqrt(p*(1-p)/(N+4))
  right<-p+1.96*sqrt(p*(1-p)/(N+4))
  print(left)
  print(right)
}
CI(6,38);CI(20,39);CI(35,39);CI(36,40);CI(34,42)
CI(0,38);CI(13,39);CI(30,39);CI(33,40);CI(28,42)
CI(7,39);CI(28,39);CI(30,40);CI(24,42)
CI(4,39);CI(15,39);CI(25,40);CI(12,42)

#Fisher exact test
PASI50_70mg<-matrix(c(20, 19, 6, 32), ncol = 2,byrow = TRUE) 
fisher.test(PASI50_70mg, alternative = "greater", conf.int=FALSE)
PASI50_140mg<-matrix(c(35, 4, 6, 32), ncol = 2,byrow = TRUE) 
fisher.test(PASI50_140mg, alternative = "greater", conf.int=FALSE)
PASI50_210mg<-matrix(c(36, 4, 6, 32), ncol = 2,byrow = TRUE) 
fisher.test(PASI50_210mg, alternative = "greater", conf.int=FALSE)
PASI50_280mg<-matrix(c(34, 8, 6, 32), ncol = 2,byrow = TRUE) 
fisher.test(PASI50_280mg, alternative = "greater", conf.int=FALSE)

PASI75_70mg<-matrix(c(13, 26, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI75_70mg, alternative = "greater", conf.int=FALSE)
PASI75_140mg<-matrix(c(30, 9, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI75_140mg, alternative = "greater", conf.int=FALSE)
PASI75_210mg<-matrix(c(33, 7, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI75_210mg, alternative = "greater", conf.int=FALSE)
PASI75_280mg<-matrix(c(28, 14, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI75_280mg, alternative = "greater", conf.int=FALSE)

PASI90_70mg<-matrix(c(7, 32, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI90_70mg, alternative = "greater", conf.int=FALSE)
PASI90_140mg<-matrix(c(28, 11, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI90_140mg, alternative = "greater", conf.int=FALSE)
PASI90_210mg<-matrix(c(30, 10, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI90_210mg, alternative = "greater", conf.int=FALSE)
PASI90_280mg<-matrix(c(24, 18, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI90_280mg, alternative = "greater", conf.int=FALSE)

PASI100_70mg<-matrix(c(4, 35, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI100_70mg, alternative = "greater", conf.int=FALSE)
PASI100_140mg<-matrix(c(15, 24, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI100_140mg, alternative = "greater", conf.int=FALSE)
PASI100_210mg<-matrix(c(25, 15, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI100_210mg, alternative = "greater", conf.int=FALSE)
PASI100_280mg<-matrix(c(12, 30, 0, 38), ncol = 2,byrow = TRUE) 
fisher.test(PASI100_280mg, alternative = "greater", conf.int=FALSE)

#Summary of sPGA
Test_all_sPGA<-ADPA %>% select(SUBJID,TRTA,AVISITN,PARAMCD,AVAL) %>% filter(PARAMCD=="sPGA",AVISITN==6)
Test_all_sPGA$Almost<-ifelse((Test_all_sPGA$AVAL==0|Test_all_sPGA$AVAL==1),1,0)
table(Test_all_sPGA$TRTA,Test_all_sPGA$Almost)

CI(1,38);CI(10,39);CI(33,39);CI(32,40);CI(29,42)
sPGA_almost_70<-matrix(c(10, 29, 1, 37), ncol = 2,byrow = TRUE) 
fisher.test(sPGA_almost_70, alternative = "greater", conf.int=FALSE)
sPGA_almost_140<-matrix(c(33, 6, 1, 37), ncol = 2,byrow = TRUE) 
fisher.test(sPGA_almost_140, alternative = "greater", conf.int=FALSE)
sPGA_almost_210<-matrix(c(32, 8, 1, 37), ncol = 2,byrow = TRUE) 
fisher.test(sPGA_almost_210, alternative = "greater", conf.int=FALSE)
sPGA_almost_280<-matrix(c(29, 13, 1, 37), ncol = 2,byrow = TRUE) 
fisher.test(sPGA_almost_280, alternative = "greater", conf.int=FALSE)

#Pairwise comparsion
#PASI50
fit1<-aov(PCHGCA1N~TRTA,data=Test_all_PASI)
summary(fit1)
result1<-pairw.anova(y=Test_all_PASI$PCHGCA1N, x=Test_all_PASI$TRTA, method="tukey", MSE=0.147, df.err=193)

#Forest Plot
cochrane_from_rmeta1 <- 
  structure(list(
    diff  = as.numeric(as.character(result1$summary$Diff)), 
    lower = as.numeric(as.character(result1$summary$Lower)),
    upper = as.numeric(as.character(result1$summary$Upper))),
    .Names = c("diff", "lower", "upper"),
    row.names = c(NA, -10L),
    class = "data.frame")
tabletext1<-c("Placebo-drug70mg","Placebo-drug140mg","drug70mg-drug140mg","Placebo-drug210mg","drug70mg-drug210mg"
             ,"drug140mg-drug210mg","Placebo-drug280mg","drug70mg-drug280mg","drug140mg-drug280mg","drug210mg-drug280mg")
forestplot(tabletext1, 
           cochrane_from_rmeta1,new_page = TRUE, col=fpColors(box="royalblue",line="darkblue"))

#PASI75
fit2<-aov(PCHGCA2N~TRTA,data=Test_all_PASI)
summary(fit2)
result2<-pairw.anova(y=Test_all_PASI$PCHGCA2N, x=Test_all_PASI$TRTA, method="tukey", MSE=0.159, df.err=193)
cochrane_from_rmeta2 <- 
  structure(list(
    diff  = as.numeric(as.character(result2$summary$Diff)), 
    lower = as.numeric(as.character(result2$summary$Lower)),
    upper = as.numeric(as.character(result2$summary$Upper))),
    .Names = c("diff", "lower", "upper"),
    row.names = c(NA, -10L),
    class = "data.frame")
tabletext2<-c("Placebo-drug70mg","Placebo-drug140mg","drug70mg-drug140mg","Placebo-drug210mg","drug70mg-drug210mg"
              ,"drug140mg-drug210mg","Placebo-drug280mg","drug70mg-drug280mg","drug140mg-drug280mg","drug210mg-drug280mg")
forestplot(tabletext2, 
           cochrane_from_rmeta2,new_page = TRUE, col=fpColors(box="royalblue",line="darkblue"))

#PASI90
fit3<-aov(PCHGCA3N~TRTA,data=Test_all_PASI)
summary(fit3)
result3<-pairw.anova(y=Test_all_PASI$PCHGCA3N, x=Test_all_PASI$TRTA, method="tukey", MSE=0.163, df.err=193)
cochrane_from_rmeta3 <- 
  structure(list(
    diff  = as.numeric(as.character(result3$summary$Diff)), 
    lower = as.numeric(as.character(result3$summary$Lower)),
    upper = as.numeric(as.character(result3$summary$Upper))),
    .Names = c("diff", "lower", "upper"),
    row.names = c(NA, -10L),
    class = "data.frame")
tabletext3<-c("Placebo-drug70mg","Placebo-drug140mg","drug70mg-drug140mg","Placebo-drug210mg","drug70mg-drug210mg"
              ,"drug140mg-drug210mg","Placebo-drug280mg","drug70mg-drug280mg","drug140mg-drug280mg","drug210mg-drug280mg")
forestplot(tabletext3, 
           cochrane_from_rmeta3,new_page = TRUE, col=fpColors(box="royalblue",line="darkblue"))

#PASI100
fit4<-aov(PCHGCA4N~TRTA,data=Test_all_PASI)
summary(fit4)
result4<-pairw.anova(y=Test_all_PASI$PCHGCA4N, x=Test_all_PASI$TRTA, method="tukey", MSE=0.1594, df.err=193)
cochrane_from_rmeta4 <- 
  structure(list(
    diff  = as.numeric(as.character(result4$summary$Diff)), 
    lower = as.numeric(as.character(result4$summary$Lower)),
    upper = as.numeric(as.character(result4$summary$Upper))),
    .Names = c("diff", "lower", "upper"),
    row.names = c(NA, -10L),
    class = "data.frame")
tabletext4<-c("Placebo-drug70mg","Placebo-drug140mg","drug70mg-drug140mg","Placebo-drug210mg","drug70mg-drug210mg"
             ,"drug140mg-drug210mg","Placebo-drug280mg","drug70mg-drug280mg","drug140mg-drug280mg","drug210mg-drug280mg")
forestplot(tabletext4, 
           cochrane_from_rmeta4,new_page = TRUE, col=fpColors(box="royalblue",line="darkblue"))

#sPGA score of 0 or 1
fit5<-aov(Almost~TRTA,data=Test_all_sPGA)
summary(fit5)
pairw.anova(y=Test_all_sPGA$Almost, x=Test_all_sPGA$TRTA, method="tukey", MSE=0.15, df.err=193)
result5<-pairw.anova(y=Test_all_sPGA$Almost, x=Test_all_sPGA$TRTA, method="tukey", MSE=0.163, df.err=193)
cochrane_from_rmeta5 <- 
  structure(list(
    diff  = as.numeric(as.character(result5$summary$Diff)), 
    lower = as.numeric(as.character(result5$summary$Lower)),
    upper = as.numeric(as.character(result5$summary$Upper))),
    .Names = c("diff", "lower", "upper"),
    row.names = c(NA, -10L),
    class = "data.frame")
tabletext5<-c("Placebo-drug70mg","Placebo-drug140mg","drug70mg-drug140mg","Placebo-drug210mg","drug70mg-drug210mg"
              ,"drug140mg-drug210mg","Placebo-drug280mg","drug70mg-drug280mg","drug140mg-drug280mg","drug210mg-drug280mg")
forestplot(tabletext5, 
           cochrane_from_rmeta5,new_page = TRUE, col=fpColors(box="royalblue",line="darkblue"))

########Q5
ADSL<-read.csv("ADSL.csv")

#data manipulate
adpavis6<-ADPA[which(ADPA$AVISITN==6&ADPA$PARAMN==10),]
adspgavis6<-ADPA[which(ADPA$AVISITN==6&ADPA$PARAMN==20),]
adpavis6$alm<-ifelse(adpavis6$AVAL==0|adpavis6$AVAL==1,1,0)
adpavis6$clear<-ifelse(adpavis6$AVAL==0,1,0)
data3<-as.data.frame(cbind(adpavis6$USUBJID,ADSL$AGE,ADSL$SEX,
                           ADSL$WEIGHT,adpavis6$TRTAN,adpavis6$BASE,adpavis6$PCHGCA1N,
                           adpavis6$PCHGCA2N,adpavis6$PCHGCA3N,adpavis6$PCHGCA4N,adpavis6$alm,adpavis6$clear))
names(data3)<-c("ID","age","sex","weight","treatment","baseline","pasi50",
                "pasi75","pasi90","pasi100","almost","clear")

#compare subgroups for which patients are benefiting more
#age,separate as three groups
#PASI50
data3$age1<-ifelse(data3$age<40,1,
                   ifelse(data3$age<60,2,
                          ifelse(data3$age<80,3,0)))
less4050<-(data3$pasi50[which(data3$age1==1)])
count11<-round(sum(less4050)/length(less4050)*100,2)
count12<-100-count11
less6050<-(data3$pasi50[which(data3$age1==2)])
count21<-round(sum(less6050)/length(less6050)*100,2)
count22<-100-count21
more6050<-(data3$pasi50[which(data3$age1==3)])
count31<-round(sum(more6050)/length(more6050)*100,2)
count32<-100-count31
x=rep(c('20-40','40-60','60-80'),each=2)
y=rep(c('PASI=1','PASI=0'),times=3)
z<-c(count11,count12,count21,count22,count31,count32)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('age category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI50 percentage comparison among age groups')

#PASI75
less4075<-(data3$pasi75[which(data3$age1==1)])
count11<-round(sum(less4075)/length(less4075)*100,2)
count12<-100-count11
less6075<-(data3$pasi75[which(data3$age1==2)])
count21<-round(sum(less6075)/length(less6075)*100,2)
count22<-100-count21
more6075<-(data3$pasi75[which(data3$age1==3)])
count31<-round(sum(more6075)/length(more6075)*100,2)
count32<-100-count31
x=rep(c('20-40','40-60','60-80'),each=2)
y=rep(c('PASI=1','PASI=0'),times=3)
z<-c(count11,count12,count21,count22,count31,count32)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('age category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI75 percentage comparison among age groups')

#PASI90
less4090<-(data3$pasi90[which(data3$age1==1)])
count11<-round(sum(less4090)/length(less4090)*100,2)
count12<-100-count11
less6090<-(data3$pasi90[which(data3$age1==2)])
count21<-round(sum(less6090)/length(less6090)*100,2)
count22<-100-count21
more6090<-(data3$pasi90[which(data3$age1==3)])
count31<-round(sum(more6090)/length(more6090)*100,2)
count32<-100-count31
x=rep(c('20-40','40-60','60-80'),each=2)
y=rep(c('PASI=1','PASI=0'),times=3)
z<-c(count11,count12,count21,count22,count31,count32)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('age category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI90 percentage comparison among age groups')

#PASI100
less40100<-(data3$pasi100[which(data3$age1==1)])
count11<-round(sum(less40100)/length(less40100)*100,2)
count12<-100-count11
less60100<-(data3$pasi100[which(data3$age1==2)])
count21<-round(sum(less60100)/length(less60100)*100,2)
count22<-100-count21
more60100<-(data3$pasi100[which(data3$age1==3)])
count31<-round(sum(more60100)/length(more60100)*100,2)
count32<-100-count31
x=rep(c('20-40','40-60','60-80'),each=2)
y=rep(c('PASI=1','PASI=0'),times=3)
z<-c(count11,count12,count21,count22,count31,count32)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('age category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI100 percentage comparison among age groups')

#SPGA almost clear or clear
less40alm<-(data3$almost[which(data3$age1==1)])
count11<-round(sum(less40alm)/length(less40alm)*100,2)
count12<-100-count11
less60alm<-(data3$almost[which(data3$age1==2)])
count21<-round(sum(less60alm)/length(less60alm)*100,2)
count22<-100-count21
more60alm<-(data3$almost[which(data3$age1==3)])
count31<-round(sum(more60alm)/length(more60alm)*100,2)
count32<-100-count31
x=rep(c('20-40','40-60','60-80'),each=2)
y=rep(c('SPGA=1','SPGA=0'),times=3)
z<-c(count11,count12,count21,count22,count31,count32)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('age category')+
  ylab('SPGA(almost or clear) percentage(%)')+
  ggtitle('SPGA percentage comparison among age groups')

#SPGA clear
less40clear<-(data3$clear[which(data3$age1==1)])
count11<-round(sum(less40clear)/length(less40clear)*100,2)
count12<-100-count11
less60clear<-(data3$clear[which(data3$age1==2)])
count21<-round(sum(less60clear)/length(less60clear)*100,2)
count22<-100-count21
more60clear<-(data3$clear[which(data3$age1==3)])
count31<-round(sum(more60clear)/length(more60clear)*100,2)
count32<-100-count31
x=rep(c('20-40','40-60','60-80'),each=2)
y=rep(c('SPGA=1','SPGA=0'),times=3)
z<-c(count11,count12,count21,count22,count31,count32)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('age category')+
  ylab('SPGA(clear) percentage(%)')+
  ggtitle('SPGA percentage comparison among age groups')

#sex,1=male,2=female
#PASI50
data3$sex1<-ifelse(data3$sex<=1,'M','F')
male50<-(data3$pasi50[which(data3$sex==1)])
count11<-round(sum(male50)/length(male50)*100,2)
count12<-100-count11
female50<-(data3$pasi50[which(data3$sex==2)])
count21<-round(sum(female50)/length(female50)*100,2)
count22<-100-count21
x=rep(c('Male','Female'),each=2)
y=rep(c('PASI=1','PASI=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('sex category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI50 percentage comparison among sex groups')

#PASI75
male75<-(data3$pasi75[which(data3$sex==1)])
count11<-round(sum(male75)/length(male75)*100,2)
count12<-100-count11
female75<-(data3$pasi75[which(data3$sex==2)])
count21<-round(sum(female75)/length(female75)*100,2)
count22<-100-count21
x=rep(c('Male','Female'),each=2)
y=rep(c('PASI=1','PASI=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('sex category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI75 percentage comparison among sex groups')

#PASI90
male90<-(data3$pasi90[which(data3$sex==1)])
count11<-round(sum(male90)/length(male90)*100,2)
count12<-100-count11
female90<-(data3$pasi90[which(data3$sex==2)])
count21<-round(sum(female90)/length(female90)*100,2)
count22<-100-count21
x=rep(c('Male','Female'),each=2)
y=rep(c('PASI=1','PASI=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('sex category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI90 percentage comparison among sex groups')

#PASI100
male100<-(data3$pasi100[which(data3$sex==1)])
count11<-round(sum(male100)/length(male100)*100,2)
count12<-100-count11
female100<-(data3$pasi100[which(data3$sex==2)])
count21<-round(sum(female100)/length(female100)*100,2)
count22<-100-count21
x=rep(c('Male','Female'),each=2)
y=rep(c('PASI=1','PASI=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('sex category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI100 percentage comparison among sex groups')

#SPGA almost clear or clear
malealm<-(data3$almost[which(data3$sex==1)])
count11<-round(sum(malealm)/length(malealm)*100,2)
count12<-100-count11
femalealm<-(data3$almost[which(data3$sex==2)])
count21<-round(sum(femalealm)/length(femalealm)*100,2)
count22<-100-count21
x=rep(c('Male','Female'),each=2)
y=rep(c('SPGA=1','SPGA=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('age category')+
  ylab('SPGA(almost clear and clear) percentage(%)')+
  ggtitle('SPGA percentage comparison among sex groups')

#SPGA clear
maleclear<-(data3$clear[which(data3$sex==1)])
count11<-round(sum(maleclear)/length(maleclear)*100,2)
count12<-100-count11
femaleclear<-(data3$clear[which(data3$sex==2)])
count21<-round(sum(femaleclear)/length(femaleclear)*100,2)
count22<-100-count21
x=rep(c('Male','Female'),each=2)
y=rep(c('SPGA=1','SPGA=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('age category')+
  ylab('SPGA(clear) percentage(%)')+
  ggtitle('SPGA percentage comparison among sex groups')

#weight>100 and weight<=100
#PASI50
data3$weight1<-ifelse(data3$weight<=100,1,2)
we150<-(data3$pasi50[which(data3$weight1==1)])
count11<-round(sum(we150)/length(we150)*100,2)
count12<-100-count11
we250<-(data3$pasi50[which(data3$weight1==2)])
count21<-round(sum(we250)/length(we250)*100,2)
count22<-100-count21
x=rep(c('<=100','>100'),each=2)
y=rep(c('PASI=1','PASI=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('weight category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI50 percentage comparison among weight groups')

#PASI75
we175<-(data3$pasi75[which(data3$weight1==1)])
count11<-round(sum(we175)/length(we175)*100,2)
count12<-100-count11
we275<-(data3$pasi75[which(data3$weight1==2)])
count21<-round(sum(we275)/length(we275)*100,2)
count22<-100-count21
x=rep(c('<=100','>100'),each=2)
y=rep(c('PASI=1','PASI=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('weight category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI75 percentage comparison among weight groups')

#PASI90
we190<-(data3$pasi90[which(data3$weight1==1)])
count11<-round(sum(we190)/length(we190)*100,2)
count12<-100-count11
we290<-(data3$pasi90[which(data3$weight1==2)])
count21<-round(sum(we290)/length(we290)*100,2)
count22<-100-count21
x=rep(c('<=100','>100'),each=2)
y=rep(c('PASI=1','PASI=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('weight category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI90 percentage comparison among weight groups')

#PASI100
we1100<-(data3$pasi100[which(data3$weight1==1)])
count11<-round(sum(we1100)/length(we1100)*100,2)
count12<-100-count11
we2100<-(data3$pasi100[which(data3$weight1==2)])
count21<-round(sum(we2100)/length(we2100)*100,2)
count22<-100-count21
x=rep(c('<=100','>100'),each=2)
y=rep(c('PASI=1','PASI=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('weight category')+
  ylab('PASI percentage(%)')+
  ggtitle('PASI100 percentage comparison among weight groups')

#SPGA almost clear or clear
we1alm<-(data3$almost[which(data3$weight1==1)])
count11<-round(sum(we1alm)/length(we1alm)*100,2)
count12<-100-count11
we2alm<-(data3$almost[which(data3$weight1==2)])
count21<-round(sum(we2alm)/length(we2alm)*100,2)
count22<-100-count21
x=rep(c('<=100','>100'),each=2)
y=rep(c('SPGA=1','SPGA=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('weight category')+
  ylab('SPGA(almost and clear) percentage(%)')+
  ggtitle('SPGA percentage comparison among weight groups')

#SPGA clear
we1clear<-(data3$clear[which(data3$weight1==1)])
count11<-round(sum(we1clear)/length(we1clear)*100,2)
count12<-100-count11
we2clear<-(data3$clear[which(data3$weight1==2)])
count21<-round(sum(we2clear)/length(we2clear)*100,2)
count22<-100-count21
x=rep(c('<=100','>100'),each=2)
y=rep(c('SPGA=1','SPGA=0'),times=2)
z<-c(count11,count12,count21,count22)
df=data.frame(x=x,y=y,z=z)
ggplot(data=df,mapping=aes(x=factor(x),y=z,fill=y))+
  geom_bar(stat='identity',width=0.4,position='stack')+
  geom_text(mapping=aes(label=z),size=4,
            vjust=3.5,hjust=0.5,position=position_stack())+
  theme(legend.position = "top")+xlab('weight category')+
  ylab('SPGA(clear) percentage(%)')+
  ggtitle('SPGA percentage comparison among weight groups')

#Analysis of primary endpoint
Primary_data_1<-ADPA %>% select(SUBJID,TRTA,AVISITN,PARAMCD,PCHG) %>% filter(PARAMCD=="PASI",AVISITN==6) %>%
  select(-c(AVISITN,PARAMCD))
Primary_data_2<-ADSL %>% select(SUBJID,AGE,SEX,WEIGHT,PASI,SPGA)

#merge two dataset
Primary_data<-left_join(Primary_data_2,Primary_data_1)
str(Primary_data)

#data descriptive
table(Primary_data$SEX)
stat.desc(Primary_data[,-c(3,7)])

#ANCOVA
Primary_data$AGE<-Primary_data$AGE-mean(Primary_data$AGE)
Primary_data$WEIGHT<-Primary_data$WEIGHT-mean(Primary_data$WEIGHT)
Primary_data$PASI<-Primary_data$PASI-mean(Primary_data$PASI)
model1<-lm(PCHG~AGE+SEX+WEIGHT+TRTA+PASI+SEX*TRTA,data=Primary_data)
summary(model1)
Anova(model1,type=3)

#check whether adequate
model2<-lm(PCHG~AGE+SEX+WEIGHT+TRTA+PASI+SEX*TRTA+AGE*TRTA+WEIGHT*TRTA+PASI*TRTA+AGE*SEX+WEIGHT*SEX+PASI*SEX,data=Primary_data)
anova(model1,model2)

#Pairwise comparsion
library(lsmeans)#pairwise comparsion
library(multcomp)#pairwise comparsion
marginal<-lsmeans(model1,~SEX:TRTA)
CLD<-cld(marginal,alpha=0.05,Letters=letters,adjust="tukey");CLD

#Analysis of Secondary endpoint
Secondary_PASI_1<-New_ADPA %>% dplyr::select(SUBJID,TRTA,AVISITN,PARAMCD,PCHGCA1N,PCHGCA2N,PCHGCA3N,PCHGCA4N) %>%
  dplyr::filter(PARAMCD=="PASI",AVISITN==6)
Secondary_PASI_2<-ADSL %>% dplyr::select(SUBJID,AGE,SEX,WEIGHT,PASI)

#merge two dataset
Secondary_PASI<-left_join(Secondary_PASI_2,Secondary_PASI_1)
str(Secondary_PASI)

#PASI75 at week 12
model3<-glm(PCHGCA2N~AGE+SEX+WEIGHT+PASI+TRTA,data = Secondary_PASI,family = "binomial")
summary(model3)
Anova(model3,type=3)
Age_20_40<-Secondary_PASI %>% filter(AGE>=20&AGE<40)
table(Age_20_40$TRTA,Age_20_40$PCHGCA2N)
Age1<-matrix(c(13, 2, 18, 1), ncol = 2,byrow = TRUE) 
fisher.test(Age1, alternative = "greater", conf.int=FALSE)
Age_40_60<-Secondary_PASI %>% filter(AGE>=40&AGE<60)
table(Age_40_60$TRTA,Age_40_60$PCHGCA2N)
Age2<-matrix(c(13, 4, 14, 4), ncol = 2,byrow = TRUE) 
fisher.test(Age2, alternative = "greater", conf.int=FALSE)
Age_60_80<-Secondary_PASI %>% filter(AGE>=60&AGE<80)
table(Age_60_80$TRTA,Age_60_80$PCHGCA2N)
Age3<-matrix(c(4, 2, 1, 2), ncol = 2,byrow = TRUE) 
fisher.test(Age3, alternative = "greater", conf.int=FALSE)

#PASI50 at week 12
model4<-glm(PCHGCA1N~AGE+SEX+WEIGHT+PASI+TRTA,data = Secondary_PASI,family = "binomial")
summary(model4)
Anova(model4,type=3)

#PASI90 at week 12
model5<-glm(PCHGCA3N~AGE+SEX+WEIGHT+PASI+TRTA,data = Secondary_PASI,family = "binomial")
summary(model5)
Anova(model5,type=3)

#Subgroup age fisher test
Age_20_40<-Secondary_PASI %>% filter(AGE>=20&AGE<40)
table(Age_20_40$TRTA,Age_20_40$PCHGCA3N)
Age1<-matrix(c(12, 4, 17, 2), ncol = 2,byrow = TRUE) 
fisher.test(Age1, alternative = "greater", conf.int=FALSE)
Age_40_60<-Secondary_PASI %>% filter(AGE>=40&AGE<60)
table(Age_40_60$TRTA,Age_40_60$PCHGCA3N)
Age2<-matrix(c(13, 4, 12, 6), ncol = 2,byrow = TRUE) 
fisher.test(Age2, alternative = "greater", conf.int=FALSE)
Age_60_80<-Secondary_PASI %>% filter(AGE>=60&AGE<80)
table(Age_60_80$TRTA,Age_60_80$PCHGCA3N)
Age3<-matrix(c(3, 3, 1, 2), ncol = 2,byrow = TRUE) 
fisher.test(Age3, alternative = "greater", conf.int=FALSE)

#PASI100 at week 12
model6<-glm(PCHGCA4N~AGE+SEX+WEIGHT+PASI+TRTA,data = Secondary_PASI,family = "binomial")
summary(model6)
Anova(model6,type=3)

#sPGA of clear or almost clear at week 12
Secondary_CA_1<-ADPA %>% dplyr::select(SUBJID,TRTA,AVISITN,PARAMCD,AVAL) %>% dplyr::filter(PARAMCD=="sPGA",AVISITN==6) %>%
  select(-c(AVISITN,PARAMCD))
Secondary_CA_2<-ADSL %>% dplyr::select(SUBJID,AGE,SEX,WEIGHT,SPGA)

#merge two dataset
Secondary_CA<-left_join(Secondary_CA_2,Secondary_CA_1)
Secondary_CA$Clear_or_almost<-ifelse((Secondary_CA$AVAL==0|Secondary_CA$AVAL==1),1,0)
Secondary_CA$Clear_or_almost<-as.factor(Secondary_CA$Clear_or_almost)
str(Secondary_CA)

#logistic
model3<-glm(Clear_or_almost~AGE+SEX+WEIGHT+SPGA+TRTA,data = Secondary_CA,family = "binomial")
summary(model3)
Anova(model3,type=3)

#subgroup fisher exact test
Age_20_40_<-Secondary_CA %>% filter(AGE>=20&AGE<40)
table(Age_20_40$TRTA,Age_20_40$Clear_or_almost)
Age1<-matrix(c(13, 3, 18, 1), ncol = 2,byrow = TRUE) 
fisher.test(Age1, alternative = "greater", conf.int=FALSE)
Age_40_60<-Secondary_CA %>% filter(AGE>=40&AGE<60)
table(Age_40_60$TRTA,Age_40_60$Clear_or_almost)
Age2<-matrix(c(15, 2, 13, 5), ncol = 2,byrow = TRUE) 
fisher.test(Age2, alternative = "greater", conf.int=FALSE)
Age_60_80<-Secondary_CA %>% filter(AGE>=60&AGE<80)
table(Age_60_80$TRTA,Age_60_80$Clear_or_almost)
Age3<-matrix(c(5, 1, 1, 2), ncol = 2,byrow = TRUE) 
fisher.test(Age3, alternative = "greater", conf.int=FALSE)

#sPGA of clear at week 12
Secondary_C<-Secondary_CA[,-ncol(Secondary_CA)]
Secondary_C$clear<-ifelse((Secondary_C$AVAL==0),1,0)
Secondary_C$clear<-as.factor(Secondary_C$clear)
str(Secondary_C)

#logistic
model4<-glm(clear~AGE+SEX+WEIGHT+SPGA+TRTA,data = Secondary_C,family = "binomial")
summary(model4)
Anova(model4,type=3)

###########Q6
Baseline_cha<-ADSL %>% dplyr::select(SUBJID,AGE,SEX,WEIGHT,PASI,SPGA)
Prim_Sec<-ADPA %>% dplyr::filter(PARAMCD=="PASI",AVISITN=="6") %>%
  dplyr::select(SUBJID,PCHG,PCHGCA1N,PCHGCA2N,PCHGCA3N,PCHGCA4N)
Data1<-left_join(Baseline_cha,Prim_Sec)
Data2<-cbind(Secondary_C[,c(1,8)],Secondary_CA[,c(8)])
Cool_data<-left_join(Data1,Data2)
names(Cool_data)<-c("ID","Age","Sex","Weight","Baseline PASI","Baseline sPGA","Percent improvement of PASI",
                    "PASI50","PASI75","PASI90","PASI100","Clear of sPGA","Clear or almost clear of sPGA")

#Corplot
data<-Cool_data[-1]
str(data)
#convert factor into numeric to plot correlation matrix and corplot
data1<-data
index<-sapply(data1,is.factor)
data1[index]<-sapply(data1[index],as.numeric)
correlation<-cor(data1[-7])
#correlation plot
corrplot(correlation, method = "number",type="lower")




