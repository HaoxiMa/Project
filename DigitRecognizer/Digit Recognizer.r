library(ggplot2) #绘图
library(caret) #数据预处理
library(mice)#缺失值处理包
library(kernlab)#机器学习
train = read.csv("/Users/mahaoxi/desktop/project/DigitRecognizer/train.csv")
test = read.csv("/Users/mahaoxi/desktop/project/DigitRecognizer/test.csv")

###查看数据
str(train)#即structure() 查看数据结构
md.pattern(train) ### 查看缺失值

###查看任意样本图像
sample = sample(1:nrow(train),30) ### sample函数随机提取三十行(30个手写数字)
#将去除label列的训练集转化为矩阵
var = t(train[sample,-1])#转置
var_matrix = lapply(1:30,function(x) matrix(var[,x],nrow = 28))
image(var_matrix[[30]],col = grey.colors(255),axes = F)

###数据处理
nzr = nearZeroVar(train[,-1],saveMetrics=T,freqCut=10000/1,uniqueCut=1/7) #剔除方差近似为0的变量(如四个角)
cutvar = rownames(nzr[nzr$nzv==TRUE,]) #被剔除的变量
var = setdiff(names(train),cutvar)#剔除后的变量
train = train[,var]#创建新的train,只含577个变量
var2 = setdiff(names(test),cutvar)
test = test[,var2]#创建新的test,只含577个变量

###二值化处理
label = as.factor(train$label) ### 数据格式变换
train$label = NULL ### 剔除redtrain中的label列
train = train/255 ### 二值化

### pca降维
cov = cov(train) ### 计算协方差矩阵
pc = prcomp(cov) ### 计算特征矩阵
var = pc$sdev^2/sum(pc$sdev^2) ### 计算特征变量方差(sdev=standard deviation)
varcum = cumsum(var) ### 计算特征变量累积方差
result = data.frame(num=1:length(pc$sdev),ex=var,cum=varcum) ### 将结果变为数据框展示
for (x in c(1:length(varcum)))#挑选主成分
{
  if (varcum[x]>0.99)
    break
}
print(x)

### 选取前x个特征向量作为主成分
n = x ### 设定主成分
score = as.matrix(train) %*% pc$rotation[,1:n]
train = cbind(label,as.data.frame(score))

### 修改test数据集格式
test = test/255
test = as.matrix(test) %*% pc$rotation[,1:n]
test = as.data.frame(test)

####模型
svm = train(label~.,data=train,method="svmRadial",
            trControl=trainControl(method="cv",number=5),
            tuneGrid=data.frame(sigma = 0.01104614,C = 3.5))

### 预测结果并输出
pred = predict(svm,test)
prediction = data.frame(ImageId=1:nrow(test),Label=pred)
write.csv(prediction,"/Users/mahaoxi/desktop/project/DigitRecognizer/reslut.csv",row.names=F)





