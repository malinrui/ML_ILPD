#load data
dat<-read.csv('ilpd.csv')
head(dat)
str(dat)
#性别
table(dat$gender)
#条形图
barplot(table(dat$gender),xlab = 'gender',ylab = '频数',main='性别频数统计
    ')
##直方图
hist(dat$age)
shapiro.test(dat$age)
library(pastecs)
library(psych)
describe.by(dat$age)##偏度

##总胆红素
library(ggplot2)
ggplot(data = dat,aes(x=age,y=TB))+geom_point(aes(col=gender,shape=gender))
##ratio
dat$age_cut<-cut(dat$age,seq(0,100,by = 10),right = F )
library(dplyr)



AGE_RATIO<-dat %>% group_by(age_cut,Y)%>% summarise(
  age_ratio = mean(A.G.Ratio.,na.rm = T)
)
write.csv(AGE_RATIO,'AGE_RATIO.csv')
##可视化
ggplot(data = AGE_RATIO,aes(x=age_cut,y=age_ratio))+geom_point(aes(col=factor(Y),shape=factor(Y)))
##患者和年龄
write.csv(table(dat$Y,dat$age_cut),'age_ill.csv') 
barplot(table(dat$Y,dat$age_cut),legend.text = T) 
#相关分析
library(corrplot)
write.csv(round(cor(dat[,3:9]),3),'corr.csv')
corrplot(cor(dat[,3:9]))

##t检验
t.test(dat[dat$gender=='Female','TB'],dat[dat$gender=='Male','TB'])
AGE_RATIO
summary(aov(data = AGE_RATIO,age_ratio~age_cut+Y))
##
chisq.test(table(dat$Y,dat$gender))

##机器学习
set.seed(112)
dat$gender<-as.factor(dat$gender)
dat$Y<-as.factor(dat$Y)
train.id<-sample(1:nrow(dat),0.8*nrow(dat),replace = FALSE)
train_dat<-dat[train.id,-12]
test_dat<-dat[-train.id,-12]
##决策树模型
set.seed(12)
library(tree)
cls.tree <- tree(Y ~ ., train_dat)
summary(cls.tree)
cls.tree
plot(cls.tree)
text(cls.tree)
summary(cls.tree)

#predict
tree.pred=predict(cls.tree,newdata = test_dat,type='class')
# Confusion matrix
(tree.table=table(tree.pred,test_dat$Y))

76/sum(tree.table)


# logistic回归

set.seed(123)

train_dat$Y<-ifelse(train_dat$Y==1,0,1)
test_dat$Y<-ifelse(test_dat$Y==1,0,1)
##logistic
glm_fit1<-glm(data = train_dat,formula = Y~.,binomial(link="logit"))
#混淆矩阵
table(ifelse(predict(glm_fit1,newdata = test_dat,type = 'response')>=0.5,1,0),test_dat$Y)


#随机森林模型

set.seed(123)

library(randomForest)
train_dat<-na.omit(train_dat)
train_dat$Y<-as.factor(train_dat$Y)
rf_fit1<-randomForest(Y~.,data = train_dat )


#混淆矩阵
table(predict(rf_fit1,newdata = test_dat,type = 'response'),test_dat$Y)

