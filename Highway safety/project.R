setwd("/Users/mahaoxi/Desktop/5505/project")
ST3_original<-read.csv("3ST.csv",header = TRUE, sep = ",")
major_original<-ST3_original$AADT_MAJOR
minor_original<-ST3_original$AADT_MINOR

#########make a whole comparsion fisrt
crashes<-ST3_original$PDO+ST3_original$KA+ST3_original$BC
pairs(~ST3_original$AADT_MAJOR+ST3_original$AADT_MINOR+ST3_original$LIGHTING+ST3_original$APPROACH_LEFTTURN+
        ST3_original$APPROACH_RIGHTTURN+ST3_original$SKEW_ANGLE+crashes,
      data=ST3_original,main="Scatter matrix for all factors")

#########analyze angle itself:
#delete the outliers of angle
boxplot(ST3_original$SKEW_ANGLE,main="Boxplot of skew angle");box4<-boxplot(ST3_original$SKEW_ANGLE,plot=FALSE)
range(box4$out);range(ST3_original$SKEW_ANGLE)
ST3<-ST3_original[which(ST3_original$SKEW_ANGLE<70),]
angle<-ST3$SKEW_ANGLE

#construct the histogram of the angle
hist(angle,freq = FALSE)
lines(density(angle))

#judge the distribution of the angle
shapiro.test(angle)#not normal
gam<-rgamma(380,1.296,15.36)
data_quantile<-quantile(angle,ppoints(380))
gam_quantile<-quantile(gam,ppoints(380))
plot(data_quantile,gam_quantile,xlab="Empirical Quantiles",ylab="Theoretical Quantiles",
     main="Gamma QQplot for angle")

#########analyze major and other quantity factors:
#delete major's outliers:
boxplot(major_original,main="Boxplot of AADT for major roads ");box1<-boxplot(major_original,plot=FALSE)
range(box1$out);range(major_original)
ST3<-ST3_original[which(250<=ST3_original$AADT_MAJOR & ST3_original$AADT_MAJOR<15900),]

#classify the major data:
major<-ST3$AADT_MAJOR
quantile(major)
range(major)
major_lowest<-major[which(250<major&major<2400)];major_low<-major[which(2400<major&major<4200)]
major_high<-major[which(4200<major&major<7100)];major_highest<-major[which(7100<major&major<15600)]

####analyze relationship between major and lighting:
#bar chart for major when lighting is 1:
st_light1<-ST3[which(ST3$LIGHTING==1),]
major_light1<-st_light1$AADT_MAJOR
major_lowest_light1<-major_light1[which(250<major_light1&major_light1<2400)]
major_low_light1<-major_light1[which(2400<major_light1&major_light1<4200)]
major_high_light1<-major_light1[which(4200<major_light1&major_light1<7100)]
major_highest_light1<-major_light1[which(7100<major_light1&major_light1<15600)]
x<-c(length(major_lowest_light1),length(major_low_light1),length(major_high_light1),length(major_highest_light1))
names<-c("Lowest","Low","High","Highest")
par(mfrow=c(1,2))
barplot(x,names.arg = names,main="Barplot of major daily traffic with lighting equal to 1")

#bar chart for major when lightening is 0:
st_light0<-ST3[which(ST3$LIGHTING==0),]
major_light0<-st_light0$AADT_MAJOR
major_lowest_light0<-major_light0[which(250<major_light0&major_light0<2400)]
major_low_light0<-major_light0[which(2400<major_light0&major_light0<4200)]
major_high_light0<-major_light0[which(4200<major_light0&major_light0<7100)]
major_highest_light0<-major_light0[which(7100<major_light0&major_light0<15600)]
x<-c(length(major_lowest_light0),length(major_low_light0),length(major_high_light0),length(major_highest_light0))
names<-c("Lowest","Low","High","Highest")
barplot(x,names.arg = names,main="Barplot of major daily traffic with lighting equal to 0")

#in order to verify the relationship between lighting and crashes:
####delete the outliers of crashes:
k<-ST3_original$PDO+ST3_original$KA+ST3_original$BC
par(mfrow=c(1,1))
boxplot(k,main="Boxplot of sum of the crashes");box2<-boxplot(k,plot=FALSE)
range(box2$out);range(k)
ST3<-ST3_original[which((ST3_original$PDO+ST3_original$KA+ST3_original$BC)<8),]

#lighting=1
st31<-ST3[which(ST3$LIGHTING==1),]
x<-st31$PDO+st31$KA+st31$BC
hist(x,main="Overlaid histogram of whole crashes with lighting equal to 0 and 1",freq=FALSE,
     col = rgb(0,0,1,0.2),xlab="whole crashes",ylim=c(0,0.7))
#lighting=0
st32<-ST3[which(ST3$LIGHTING==0),]
y<-st32$PDO+st32$KA+st32$BC
hist(y,add=TRUE,col = rgb(0,1,1,0.2),freq=FALSE)
legend("topright",c('whole crashes with lighting equal to 0','whole crashes with lighting equal to 1'),
       col=c(rgb(0,0,1,0.2),rgb(0,1,1,0.2)),lwd=8,text.width=3)
boxplot(x,y,notch=TRUE,names=c("Lighting is equal to 1","Lighting is equal to 0")
        ,main="Whole crahses side-by-side notched boxplot for Lighting is equal to 1 and 0")



####analyze relationship between major and left turn:
#bar chart for major when left is 1:
st_left1<-ST3[which(ST3$APPROACH_LEFTTURN==1),]
major_left1<-st_left1$AADT_MAJOR
major_lowest_left1<-major_left1[which(250<major_left1&major_left1<2400)]
major_low_left1<-major_left1[which(2400<major_left1&major_left1<4200)]
major_high_left1<-major_left1[which(4200<major_left1&major_left1<7100)]
major_highest_left1<-major_left1[which(7100<major_left1&major_left1<15600)]
x<-c(length(major_lowest_left1),length(major_low_left1),length(major_high_left1),length(major_highest_left1))
names<-c("Lowest","Low","High","Highest")
par(mfrow=c(1,2))
barplot(x,names.arg = names,main="Barplot of major daily traffic with left turn equal to 1")

#bar chart for major when left is 0:
st_left0<-ST3[which(ST3$APPROACH_LEFTTURN==0),]
major_left0<-st_left0$AADT_MAJOR
major_lowest_left0<-major_left0[which(250<major_left0&major_left0<2400)]
major_low_left0<-major_left0[which(2400<major_left0&major_left0<4200)]
major_high_left0<-major_left0[which(4200<major_left0&major_left0<7100)]
major_highest_left0<-major_left0[which(7100<major_left0&major_left0<15600)]
x<-c(length(major_lowest_left0),length(major_low_left0),length(major_high_left0),length(major_highest_left0))
names<-c("Lowest","Low","High","Highest")
barplot(x,names.arg = names,main="Barplot of major daily traffic with left turn equal to 0")

#in order to verify the relationship between left turn and crashes:
#left=1
st31<-ST3[which(ST3$APPROACH_LEFTTURN==1),]
x<-st31$PDO+st31$KA+st31$BC
par(mfrow=c(1,1))
hist(x,main="Overlaid histogram of crashes with left turn equal to 1 and 0",col=rgb(0,0,1,0.2),freq = FALSE,
     ylim=c(0,1),xlab="whole crashes")
#right=0
st32<-ST3[which(ST3$APPROACH_LEFTTURN==0),]
y<-st32$PDO+st32$KA+st32$BC
hist(y,add=TRUE,col = rgb(0,1,1,0.2),breaks=5,freq = FALSE)
legend("topright",c('whole crashes with left turn equal to 0','whole crashes with left turn equal to 1'),
       col=c(rgb(0,0,1,0.2),rgb(0,1,1,0.2)),lwd=8,text.width=2)
boxplot(x,y,notch=TRUE,names=c("Left-turn is equal to 1","Left-turn is equal to 0")
        ,main="Whole crahses side-by-side notched boxplot for left-turn is equal to 1 and 0")

####analyze relationship between major and right turn:
#bar chart for major when right turn is 1:
st_right1<-ST3[which(ST3$APPROACH_RIGHTTURN==1),]
major_right1<-st_right1$AADT_MAJOR
major_lowest_right1<-major_right1[which(250<major_right1&major_right1<2400)]
major_low_right1<-major_right1[which(2400<major_right1&major_right1<4200)]
major_high_right1<-major_right1[which(4200<major_right1&major_right1<7100)]
major_highest_right1<-major_right1[which(7100<major_right1&major_right1<15600)]
x<-c(length(major_lowest_right1),length(major_low_right1),length(major_high_right1),length(major_highest_right1))
names<-c("Lowest","Low","High","Highest")
par(mfrow=c(1,2))
barplot(x,names.arg = names,main="Barplot of major daily traffic with right turn equal to 1")

#bar chart for major when right turn is 0:
st_right0<-ST3[which(ST3$APPROACH_RIGHTTURN==0),]
major_right0<-st_right0$AADT_MAJOR
major_lowest_right0<-major_right0[which(250<major_right0&major_right0<2400)]
major_low_right0<-major_right0[which(2400<major_right0&major_right0<4200)]
major_high_right0<-major_right0[which(4200<major_right0&major_right0<7100)]
major_highest_right0<-major_right0[which(7100<major_right0&major_right0<15600)]
x<-c(length(major_lowest_right0),length(major_low_right0),length(major_high_right0),length(major_highest_right0))
names<-c("Lowest","Low","High","Highest")
barplot(x,names.arg = names,main="Barplot of major daily traffic with right turn equal to 0")

#in order to verify the relationship between right turn and crashes:
#right=1
st31<-ST3[which(ST3$APPROACH_RIGHTTURN==1),]
x<-st31$PDO+st31$KA+st31$BC
par(mfrow=c(1,1))
hist(x,main="Overlaid histogram of crashes with right turn equal to 1 and 0",col=rgb(0,0,1,0.2),freq = FALSE,
     ylim=c(0,1.5),breaks=4,xlim=c(0,7),xlab="whole crashes")
#right=0
st32<-ST3[which(ST3$APPROACH_LEFTTURN==0),]
y<-st32$PDO+st32$KA+st32$BC
hist(y,add=TRUE,col = rgb(0,1,1,0.2),breaks=6,freq = FALSE)
legend("topright",c('whole crashes with right turn equal to 0','whole crashes with right turn equal to 1'),
       col=c(rgb(0,0,1,0.2),rgb(0,1,1,0.2)),lwd=8,text.width=2)
boxplot(x,y,notch=TRUE,names=c("Right-turn is equal to 1","Right-turn is equal to 0")
        ,main="Whole crahses side-by-side notched boxplot for Right-turn is equal to 1 and 0")

####analyze relationship between major and angle:
ST31<-ST3[which(ST3$AADT_MAJOR<2400),]
ST32<-ST3[which(2400<ST3$AADT_MAJOR&ST3$AADT_MAJOR<4200),]
ST33<-ST3[which(4200<ST3$AADT_MAJOR&ST3$AADT_MAJOR<7100),]
ST34<-ST3[which(7100<ST3$AADT_MAJOR&ST3$AADT_MAJOR<15600),]
boxplot(ST31$SKEW_ANGLE,ST32$SKEW_ANGLE,ST33$SKEW_ANGLE,ST34$SKEW_ANGLE,names=c("Lowest","Low","High","Highest"),
        ylab="Skewed Angle",notch = TRUE,
        main="Skew angle side-by-side notched boxplot for AADT for major roads")
wilcox.test(ST32$SKEW_ANGLE,ST34$SKEW_ANGLE,alternative = "two.sided")

#judge the relationship between the angle and crashes
angle_original<-ST3_original$SKEW_ANGLE;cras_original<-ST3_original$PDO+ST3_original$BC+ST3_original$KA;
plot(angle_original,cras_original,main="Scatterplot between the skew angle and whole crashes",
     xlab="skew angle",ylab="whole crashes")
hist(angle_original,freq = FALSE,main="Overlais histogram of Skew angle and whole crashes",
     col=rgb(0,0,1,0.2),xlab="Skew angle and whole crashes",ylim=c(0,0.1),xlim=c(0,90))
hist(cras_original,add=TRUE,freq=FALSE,col=rgb(0,1,1,0.2),breaks=5)
legend("topright",c('skew angle','whole crashes'),
       col=c(rgb(0,0,1,0.2),rgb(0,1,1,0.2)),lwd=8,text.width=10)

#########analyze minor and other quantity factors:
#delete minor's outliers:
par(mfrow=c(1,1))
boxplot(minor_original,main="Boxplot of AADT for minor roads");box3<-boxplot(minor_original,plot=FALSE)
range(box3$out);range(minor_original)
ST3<-ST3_original[which(ST3_original$AADT_MINOR<5300),]

#classify the minor data:
minor<-ST3$AADT_MINOR
quantile(minor)
range(minor)
minor_lowest<-minor[which(20<minor&minor<700)];minor_low<-minor[which(700<minor&minor<1300)]
minor_high<-minor[which(1300<minor&minor<2200)];minor_highest<-minor[which(2200<minor&minor<5200)]

####analyze relationship between minor and lighting:
#bar chart for minor when lighting is 1:
st_light1<-ST3[which(ST3$LIGHTING==1),]
minor_light1<-st_light1$AADT_MINOR
minor_lowest_light1<-minor_light1[which(20<minor_light1&minor_light1<700)]
minor_low_light1<-minor_light1[which(700<minor_light1&minor_light1<1300)]
minor_high_light1<-minor_light1[which(1300<minor_light1&minor_light1<2200)]
minor_highest_light1<-minor_light1[which(2200<minor_light1&minor_light1<5200)]
x<-c(length(minor_lowest_light1),length(minor_low_light1),length(minor_high_light1),length(minor_highest_light1))
names<-c("Lowest","Low","High","Highest")
par(mfrow=c(1,2))
barplot(x,names.arg = names,main="Barplot of minor daily traffic with lighting equal to 1")

#bar chart for minor when lightening is 0:
st_ligth0<-ST3[which(ST3$LIGHTING==0),]
minor_light0<-st_light0$AADT_MINOR
minor_lowest_light0<-minor_light0[which(250<minor_light0&minor_light0<2400)]
minor_low_light0<-minor_light0[which(2400<minor_light0&minor_light0<4200)]
minor_high_light0<-minor_light0[which(4200<minor_light0&minor_light0<7100)]
minor_highest_light0<-minor_light0[which(7100<minor_light0&minor_light0<15600)]
x<-c(length(minor_lowest_light0),length(minor_low_light0),length(minor_high_light0),length(minor_highest_light0))
names<-c("Lowest","Low","High","Highest")
barplot(x,names.arg = names,main="Barplot of minor daily traffic with lighting equal to 0")

####analyze relationship between minor and left turn:
#bar chart for minor when left is 1:
st_left1<-ST3[which(ST3$APPROACH_LEFTTURN==1),]
minor_left1<-st_left1$AADT_MINOR
minor_lowest_left1<-minor_left1[which(20<minor_left1&minor_left1<700)]
minor_low_left1<-minor_left1[which(700<minor_left1&minor_left1<1300)]
minor_high_left1<-minor_left1[which(1300<minor_left1&minor_left1<2200)]
minor_highest_left1<-minor_left1[which(2200<minor_left1&minor_left1<5200)]
x<-c(length(minor_lowest_left1),length(minor_low_left1),length(minor_high_left1),length(minor_highest_left1))
names<-c("Lowest","Low","High","Highest")
par(mfrow=c(1,2))
barplot(x,names.arg = names,main="Barplot of minor daily traffic with left turn equal to 1")

#bar chart for minor when left is 0:
st_left0<-ST3[which(ST3$APPROACH_LEFTTURN==0),]
minor_left0<-st_left0$AADT_MINOR
minor_lowest_left0<-minor_left0[which(20<minor_left0&minor_left0<700)]
minor_low_left0<-minor_left0[which(700<minor_left0&minor_left0<1300)]
minor_high_left0<-minor_left0[which(1300<minor_left0&minor_left0<2200)]
minor_highest_left0<-minor_left0[which(2200<minor_left0&minor_left0<5200)]
x<-c(length(minor_lowest_left0),length(minor_low_left0),length(minor_high_left0),length(minor_highest_left0))
names<-c("Lowest","Low","High","Highest")
barplot(x,names.arg = names,main="Barplot of minor daily traffic with left turn equal to 0")

####analyze relationship between minor and right turn:
#bar chart for major when right turn is 1:
st_right1<-ST3[which(ST3$APPROACH_RIGHTTURN==1),]
minor_right1<-st_right1$AADT_MINOR
minor_lowest_right1<-minor_right1[which(20<minor_right1&minor_right1<700)]
minor_low_right1<-minor_right1[which(700<minor_right1&minor_right1<1300)]
minor_high_right1<-minor_right1[which(1300<minor_right1&minor_right1<2200)]
minor_highest_right1<-minor_right1[which(2200<minor_right1&minor_right1<5200)]
x<-c(length(minor_lowest_right1),length(minor_low_right1),length(minr_high_right1),length(minor_highest_right1))
names<-c("Lowest","Low","High","Highest")
par(mfrow=c(1,2))
barplot(x,names.arg = names,main="Barplot of minor daily traffic with right turn equal to 1")

#bar chart for minor when right turn is 0:
st_right0<-ST3[which(ST3$APPROACH_RIGHTTURN==0),]
minor_right0<-st_right0$AADT_MINOR
minor_lowest_right0<-minor_right0[which(20<minor_right0&minor_right0<700)]
minor_low_right0<-minor_right0[which(700<minor_right0&minor_right0<1300)]
minor_high_right0<-minor_right0[which(1300<minor_right0&minor_right0<2200)]
minor_highest_right0<-minor_right0[which(2200<minor_right0&minor_right0<5200)]
x<-c(length(minor_lowest_right0),length(minor_low_right0),length(minor_high_right0),length(minor_highest_right0))
names<-c("Lowest","Low","High","Highest")
barplot(x,names.arg = names,main="Barplot of minor daily traffic with right turn equal to 0")

####analyze relationship between minor and angle:
par(mfrow=c(1,1))
ST31<-ST3[which(ST3$AADT_MINOR<700),]
ST32<-ST3[which(700<ST3$AADT_MINOR&ST3$AADT_MINOR<1300),]
ST33<-ST3[which(1300<ST3$AADT_MINOR&ST3$AADT_MINOR<2200),]
ST34<-ST3[which(2200<ST3$AADT_MINOR&ST3$AADT_MINOR<5200),]
boxplot(ST31$SKEW_ANGLE,ST32$SKEW_ANGLE,ST33$SKEW_ANGLE,ST34$SKEW_ANGLE,names=c("Lowest","Low","High","Highest"),
        ylab="Skewed Angle",notch = TRUE,main="Skew angle Side-by-side Boxplots of AADT for minor roads")

#########analyze major and minor:
hist(major_original,col=rgb(0,0,1,0.2),xlab="AADT for major and minor roads",
     main="Histogram of Annual Average Major And Minor Daily Traffic",freq = FALSE,ylim=c(0,0.0004))
hist(minor_original,col=rgb(0,1,1,0.2),add=TRUE,breaks = 4,freq=FALSE)
legend("topright",c('Major','Minor'),
       col=c(rgb(0,0,1,0.2),rgb(0,1,1,0.2)),lwd=8,text.width=4000)
boxplot(major_original,minor_original,main="Side-by-side Boxplots for AADT for major roads and minor roads",
        notch=TRUE,
        names=c("Major Roads","Minor Roads"))
plot(major_original,minor_original,main="Scatterplot for AADT for major roads and minor roads",
     xlab="AADT for major roads",ylab="AADT for minor roads")

#########analyze relationship between minor and crashes:
crashes_original<-ST3_original$PDO+ST3_original$KA+ST3_original$BC
par(mfrow=c(1,2))
hist(minor_original,col=rgb(0,0,1,0.2),xlab="AADT for minor roads",
     main="Histogram of AADT for minor roads",freq = FALSE,ylim=c(0,0.0004))
hist(crashes_original,col=rgb(0,1,1,0.2),freq=FALSE,xlab="whole crashes",main="Histogram of whole crashes")

par(mfrow=c(1,1))
ST3<-ST3_original[which(ST3_original$AADT_MINOR<5300),]
ST31<-ST3$PDO+ST3$KA+ST3$BC;boxplot(ST31);box5<-boxplot(ST31,plot=FALSE)
ST33<-ST3[which(ST33$PDO+ST33$KA+ST33$BC<8),]
ST331<-ST33[which(ST33$AADT_MINOR<700),]
ST332<-ST33[which(700<ST33$AADT_MINOR&ST33$AADT_MINOR<1300),]
ST333<-ST33[which(1300<ST33$AADT_MINOR&ST33$AADT_MINOR<2200),]
ST334<-ST33[which(2200<ST33$AADT_MINOR&ST33$AADT_MINOR<5200),]
boxplot(ST331$PDO+ST331$KA+ST331$BC,
        ST332$PDO+ST332$KA+ST332$BC,ST333$PDO+ST333$KA+ST333$BC,
        ST334$PDO+ST334$KA+ST334$BC,names=c("Lowest","Low","High","Highest"),
        ylab="Whole crashes",notch = TRUE,main="Crashes Side-by-side Boxplots for AADT for minor roads")
plot(minor_original,crashes_original)

#########analyze relationship between major and crashes:
par(mfrow=c(1,2))
crashes_original<-ST3_original$PDO+ST3_original$KA+ST3_original$BC
hist(major_original,col=rgb(0,0,1,0.2),xlab="AADT for major roads",
     main="Histogram of AADT for major roads",freq = FALSE)
hist(crashes_original,col=rgb(0,1,1,0.2),freq=FALSE,xlab="whole crashes",main="Histogram of whole crashes")

par(mfrow=c(1,1))
ST3<-ST3_original[which(ST3_original$AADT_MAJOR<15900),]
ST31<-ST3$PDO+ST3$KA+ST3$BC;boxplot(ST31);box5<-boxplot(ST31,plot=FALSE)
ST33<-ST3[which(ST33$PDO+ST33$KA+ST33$BC<8),]
ST331<-ST33[which(ST33$AADT_MAJOR<2400),]
ST332<-ST33[which(2400<ST33$AADT_MAJOR&ST33$AADT_MAJOR<4200),]
ST333<-ST33[which(4200<ST33$AADT_MAJOR&ST33$AADT_MAJOR<7100),]
ST334<-ST33[which(7100<ST33$AADT_MAJOR&ST33$AADT_MAJOR<15600),]
boxplot(ST331$PDO+ST331$KA+ST331$BC,
        ST332$PDO+ST332$KA+ST332$BC,ST333$PDO+ST333$KA+ST333$BC,
        ST334$PDO+ST334$KA+ST334$BC,names=c("Lowest","Low","High","Highest"),
        ylab="whole crashes",main="Crashes Side-by-side Boxplots for AADT for major roads")
plot(major_original,crashes_original,xlab="AADT for major roads",
     ylab="whole crashes",main="Scatterplot for AADT for major roads and whole crashes")

#########analyze major itself:
#construct the histogram of the major
par(mfrow=c(1,1))
hist(major,freq = FALSE,main="Histogram of AADT for major roads",xlab="AADT for major roads",ylab="Density")
lines(density(major))

#judge the distribution of the major
shapiro.test(major)#not normal
library(vcd)
gam<-rgamma(373,2.18,2369)
data_quantile<-quantile(major,ppoints(385))
gam_quantile<-quantile(gam,ppoints(385))
plot(data_quantile,gam_quantile,xlab="Empirical Quantiles",ylab="Theoretical Quantiles",
     main="Gamma QQplot for Major")


#########analyze minor itself:
#construct the histogram of the minor
hist(minor,freq = FALSE,main="Histogram of AADT for minor roads",xlab="AADT for minor roads",ylab="Density")
lines(density(minor))

#judge the distribution of the minor
shapiro.test(minor)#not normal
gam<-rgamma(366,1.76,901.7)
data_quantile<-quantile(minor,ppoints(366))
gam_quantile<-quantile(gam,ppoints(366))
plot(data_quantile,gam_quantile,xlab="Empirical Quantiles",ylab="Theoretical Quantiles",
     main="Gamma QQplot for Minor")

#########analyze whole crashes itself:
par(mfrow=c(1,1))
hist(ST3_original$PDO+ST3_original$KA+ST3_original$BC,
     freq = FALSE,main="Histogram of whole crashes",xlab="Whole crashes",ylab="Density",ylim = c(0,0.3))
lines(density(ST3_original$PDO+ST3_original$KA+ST3_original$BC))


#########analyze the whole model
Z0000<-ST3_original[which(ST3_original$AADT_MAJOR<4200&ST3_original$AADT_MINOR<1300
                          &ST3_original$LIGHTING==0&ST3_original
                          $APPROACH_LEFTTURN==0&ST3_original$APPROACH_RIGHTTURN==0),]
Z0001<-ST3_original[which(ST3_original$AADT_MAJOR<4200&ST3_original$AADT_MINOR<1300
                          &ST3_original$LIGHTING==0&ST3_original
                          $APPROACH_LEFTTURN==0&ST3_original$APPROACH_RIGHTTURN==1),]
Z0010<-ST3_original[which(ST3_original$AADT_MAJOR<4200&ST3_original$AADT_MINOR<1300
                          &ST3_original$LIGHTING==0&ST3_original
                          $APPROACH_LEFTTURN==1&ST3_original$APPROACH_RIGHTTURN==0),]
Z0011<-ST3_original[which(ST3_original$AADT_MAJOR<4200&ST3_original$AADT_MINOR<1300
                          &ST3_original$LIGHTING==0&ST3_original
                          $APPROACH_LEFTTURN==1&ST3_original$APPROACH_RIGHTTURN==1),]
Z0100<-ST3_original[which(ST3_original$AADT_MAJOR<4200&ST3_original$AADT_MINOR<1300
                          &ST3_original$LIGHTING==1&ST3_original
                          $APPROACH_LEFTTURN==0&ST3_original$APPROACH_RIGHTTURN==0),]
Z0101<-ST3_original[which(ST3_original$AADT_MAJOR<4200&ST3_original$AADT_MINOR<1300
                          &ST3_original$LIGHTING==1&ST3_original
                          $APPROACH_LEFTTURN==0&ST3_original$APPROACH_RIGHTTURN==1),]
Z0110<-ST3_original[which(ST3_original$AADT_MAJOR<4200&ST3_original$AADT_MINOR<1300
                          &ST3_original$LIGHTING==1&ST3_original
                          $APPROACH_LEFTTURN==1&ST3_original$APPROACH_RIGHTTURN==0),]
Z0111<-ST3_original[which(ST3_original$AADT_MAJOR<4200&ST3_original$AADT_MINOR<1300
                          &ST3_original$LIGHTING==1&ST3_original
                          $APPROACH_LEFTTURN==1&ST3_original$APPROACH_RIGHTTURN==1),]
Z1000<-ST3_original[which(4200<ST3_original$AADT_MAJOR&ST3_original$AADT_MAJOR<15600
                          &1300<ST3_original$AADT_MINOR&ST3_original$AADT_MINOR<5200
                          &ST3_original$LIGHTING==0&ST3_original
                          $APPROACH_LEFTTURN==0&ST3_original$APPROACH_RIGHTTURN==0),]
Z1001<-ST3_original[which(4200<ST3_original$AADT_MAJOR&ST3_original$AADT_MAJOR<15600
                          &1300<ST3_original$AADT_MINOR&ST3_original$AADT_MINOR<5200
                          &ST3_original$LIGHTING==0&ST3_original
                          $APPROACH_LEFTTURN==0&ST3_original$APPROACH_RIGHTTURN==1),]
Z1010<-ST3_original[which(4200<ST3_original$AADT_MAJOR&ST3_original$AADT_MAJOR<15600
                          &1300<ST3_original$AADT_MINOR&ST3_original$AADT_MINOR<5200
                          &ST3_original$LIGHTING==0&ST3_original
                          $APPROACH_LEFTTURN==1&ST3_original$APPROACH_RIGHTTURN==0),]
Z1011<-ST3_original[which(4200<ST3_original$AADT_MAJOR&ST3_original$AADT_MAJOR<15600
                          &1300<ST3_original$AADT_MINOR&ST3_original$AADT_MINOR<5200
                          &ST3_original$LIGHTING==0&ST3_original
                          $APPROACH_LEFTTURN==1&ST3_original$APPROACH_RIGHTTURN==1),]
Z1100<-ST3_original[which(4200<ST3_original$AADT_MAJOR&ST3_original$AADT_MAJOR<15600
                          &1300<ST3_original$AADT_MINOR&ST3_original$AADT_MINOR<5200
                          &ST3_original$LIGHTING==1&ST3_original
                          $APPROACH_LEFTTURN==0&ST3_original$APPROACH_RIGHTTURN==0),]
Z1101<-ST3_original[which(4200<ST3_original$AADT_MAJOR&ST3_original$AADT_MAJOR<15600
                          &1300<ST3_original$AADT_MINOR&ST3_original$AADT_MINOR<5200
                          &ST3_original$LIGHTING==1&ST3_original
                          $APPROACH_LEFTTURN==0&ST3_original$APPROACH_RIGHTTURN==1),]
Z1110<-ST3_original[which(4200<ST3_original$AADT_MAJOR&ST3_original$AADT_MAJOR<15600
                          &1300<ST3_original$AADT_MINOR&ST3_original$AADT_MINOR<5200
                          &ST3_original$LIGHTING==1&ST3_original
                          $APPROACH_LEFTTURN==1&ST3_original$APPROACH_RIGHTTURN==0),]
Z1111<-ST3_original[which(4200<ST3_original$AADT_MAJOR&ST3_original$AADT_MAJOR<15600
                          &1300<ST3_original$AADT_MINOR&ST3_original$AADT_MINOR<5200
                          &ST3_original$LIGHTING==1&ST3_original
                          $APPROACH_LEFTTURN==1&ST3_original$APPROACH_RIGHTTURN==1),]
z0000<-Z0000$PDO+Z0000$KA+Z0000$BC
z0001<-Z0001$PDO+Z0001$KA+Z0001$BC
z0010<-Z0010$PDO+Z0010$KA+Z0010$BC
z0011<-Z0011$PDO+Z0011$KA+Z0011$BC
z0100<-Z0100$PDO+Z0100$KA+Z0100$BC
z0101<-Z0101$PDO+Z0101$KA+Z0101$BC
z0110<-Z0110$PDO+Z0110$KA+Z0110$BC
z0111<-Z0111$PDO+Z0111$KA+Z0111$BC
z1000<-Z1000$PDO+Z1000$KA+Z1000$BC
z1001<-Z1001$PDO+Z1001$KA+Z1001$BC
z1010<-Z1010$PDO+Z1010$KA+Z1010$BC
z1011<-Z1011$PDO+Z1011$KA+Z1011$BC
z1100<-Z1100$PDO+Z1100$KA+Z1100$BC
z1101<-Z1101$PDO+Z1101$KA+Z1101$BC
z1110<-Z1110$PDO+Z1110$KA+Z1110$BC
z1111<-Z1111$PDO+Z1111$KA+Z1111$BC
boxplot(z0000,z0001,z0010,z0011,z0100,z0101,z0110,z0111,z1000,z1001,z1010,z1011,z1100,z1101,z1110,z1111,
        names=c("0000","0001","0010","0011","0100","0101","0110","0111","1000","1001","1010","1011","1100","1101","1110","1111"),
        main="Whole crashes side-by-side boxplot of factors")

#########analyze the difference of other two sample
SG4_original<-read.csv("4SG.csv",header = TRUE, sep = ",")
ST4_original<-read.csv("4ST.csv",header = TRUE, sep = ",")

#preparation
boxplot(ST3_original$PDO+ST3_original$KA+ST3_original$BC,SG4_original$PDO+SG4_original$KA+SG4_original$BC,
        ST4_original$PDO+ST4_original$KA+ST4_original$BC,notch = TRUE,
        main="Whole crashes Side-by-side boxplot of three kinds of intersections",
        names=c("3ST","4SG","4ST"))
boxplot(ST3_original$AADT_MAJOR,SG4_original$AADT_MAJOR,ST4_original$AADT_MAJOR,notch = TRUE,
        main="AADT for major roads Side-by-side boxplot of three kinds of intersections",
        names=c("3ST","4SG","4ST"))


#compare the mean of AADT of major road and whole crashes in 3ST and 4ST
#AADT of major road
#Jackknife
X<-ST3_original$AADT_MAJOR;Y<-ST4_original$AADT_MAJOR
estsX<-matrix(0,nrow=(1+length(X)),ncol=2)
n<-length(X)
A<-rep(0,n)
cat(mean(X),log(var(X)),"\n")
estsX[1,]<-c(mean(X),log(var(X)))
for (i in 1:n) {
        cat(mean(X[-i]),log(var(X[-i])),"\n")
        estsX[1+i,]<-c(mean(X[-i]),log(var(X[-i])))
        A[i]<-n*estsX[1,1] - (n-1)*estsX[i+1,1]
}
V1<-var(A)/n

estsY<-matrix(0,nrow=(1+length(Y)),ncol=2)
m <-length(Y)
B<-rep(0,m)
cat(mean(Y),log(var(Y)),"\n")
estsY[1,]<-c(mean(Y),log(var(Y)))
for (i in 1:m) {
        #cat(mean(Y[-i]),log(var(Y[-i])),"\n")
        estsY[1+i,]<-c(mean(Y[-i]),log(var(Y[-i])))
        B[i]<-m*estsY[1,1] - (m-1)*estsY[i+1,1]
}
V2<-var(B)/m

Q<-(mean(B) - mean(A))/(sqrt(V1+V2))
cat("Q=",Q,"\n")
#bootstrap
boot<-function(n1,n2,B){
        est<-matrix(0,B,2)
        colnames(est)<-c("mdiff", "vratio")              # column names of matrix est
        out<-vector("list",2)
        names(out)<-c("bootstats","actual")
        actual<-c(mean(X)-mean(Y),var(X)/var(Y))
        names(actual)<-c("mdiff", "vratio")  
        for (i in 1:B){                                  # boot loop
                idx1<-sample(1:n1,n1,replace=TRUE)            # select indices for bootstrap sample for Y1
                idx2<-sample(1:n2,n2,replace=TRUE)            # select indices for bootstrap sample for Y2
                Ystar1<-X[idx1]
                Ystar2<-Y[idx2]   
                est[i,1]<-mean(Ystar1)-mean(Ystar2)             # difference of two means
                est[i,2]<-var(Ystar1)/var(Ystar2)               # ratio of two variances
        }
        out$bootstats<-est
        out$actual<-actual
        return(out)
}

out<-boot(length(X),length(Y),1000)
mdiff<-sort(out$bootstats[,1])
mdiff[1000*0.025];mdiff[1000*0.975]

#whole crashes
#Jackknife
X<-ST3_original$PDO+ST3_original$KA+ST3_original$BC;Y<-ST4_original$PDO+ST4_original$KA+ST4_original$BC
estsX<-matrix(0,nrow=(1+length(X)),ncol=2)
n<-length(X)
A<-rep(0,n)
cat(mean(X),log(var(X)),"\n")
estsX[1,]<-c(mean(X),log(var(X)))
for (i in 1:n) {
        cat(mean(X[-i]),log(var(X[-i])),"\n")
        estsX[1+i,]<-c(mean(X[-i]),log(var(X[-i])))
        A[i]<-n*estsX[1,1] - (n-1)*estsX[i+1,1]
}
V1<-var(A)/n

estsY<-matrix(0,nrow=(1+length(Y)),ncol=2)
m <-length(Y)
B<-rep(0,m)
cat(mean(Y),log(var(Y)),"\n")
estsY[1,]<-c(mean(Y),log(var(Y)))
for (i in 1:m) {
        #cat(mean(Y[-i]),log(var(Y[-i])),"\n")
        estsY[1+i,]<-c(mean(Y[-i]),log(var(Y[-i])))
        B[i]<-m*estsY[1,1] - (m-1)*estsY[i+1,1]
}
V2<-var(B)/m

Q<-(mean(B) - mean(A))/(sqrt(V1+V2))
cat("Q=",Q,"\n")

#compare the median of AADT of major road and whole crashes in 3ST and 4ST
wilcox.test(ST3_original$AADT_MAJOR,ST4_original$AADT_MAJOR,alternative = "two.sided")
wilcox.test(ST3_original$PDO+ST3_original$KA+ST3_original$BC,
            ST4_original$PDO+ST4_original$KA+ST4_original$BC,alternative="two.sided")








