data = readRDS('Longnecker.rds')

#remove row with mostly missing data
data = data[-which(is.na(data[,2])),]

#add indicator for albumin measurement
data$al.meas = !is.na(data$albumin)

#drop raw albumin score
data = data[-13]

#center as factor
data$center  = as.factor(data$center)

#ignore missing values (MICE will replace this)
data2 = na.omit(data)

#censor obvious outliers
data2 = data2[data2$gestational_age < 50,]

library(car)
lm.test = lm(gestational_age~.,data= data2)

#look at variance inflation factors
vif(lm.test)

#look at lm assumptions, pretty violated
plot(lm.test$fitted.values,lm.test$residuals)
qqnorm(lm.test$residuals)
abline(0,1)

#correlation of non-factor covariates (I like this plot a lot, illustrates pca value)
png('cor_cs1.png',width = 800, height = 600)
par(oma = c(3,4,0,0))
library(fields)
image.plot(x=1:21,y=1:21,z=cor(data2[,-c(14,22)]), yaxt = 'n',ylab='', xaxt = 'n',xlab='',main='Correlation Between Model Variables')
axis(2,at=1:21,labels = names(data2[,-c(14,22)]),las=2)
axis(1,at=1:21,labels = names(data2[,-c(14,22)]),las=2)
dev.off()

#ses correlations
cor(data2[,c(15:17)])

#pca on pcbs
pc_pcb = prcomp(data2[,c(2:12)],scale=T)
pc_pcb
library(xtable)
print(xtable(pc_pcb$rotation[,c(1,2,8)]))

#shows %variance accounted for
summary(pc_pcb)

#skree plot for visualizaion
library(factoextra)
fviz_eig(pc_pcb)

pcb_x = as.data.frame(pc_pcb$x)

#select pcs to use
library(MASS)
pcb.lm = lm(data2$gestational_age~.,data=pcb_x)
summary(pcb.lm)
select.pcb = stepAIC(pcb.lm,direction="both")
summary(select.pcb)

#recommend 1,2,8 for reasons (could justify omitting any but 1 though)

#pca for ses scores
pc_ses = prcomp(data2[,15:17])
pc_ses
summary(pc_ses)
ses_x = as.data.frame(pc_ses$x)

ses_lm = lm(data2$gestational_age~.,data=ses_x)

summary(ses_lm)

select.ses = stepAIC(ses_lm,direction="both")
summary(select.ses)

#recommend 1

#boxcox transorm, in case we want to use
plot(density(data2$gestational_age))
x0 = seq(25,50,length=1000)
lines(x0,dnorm(x0,mean(data2$gestational_age,sd(data2$gestational_age))),col='red')

y <- data2$gestational_age
#different results if using a different cutoff
#y <- data2$gestational_age[data2$gestational_age < 45]

miny <- min(y)
maxy <- max(y)
unity <- (y - miny) / (maxy-miny) + 1e-6

bc = boxcox(unity~1)
lambda <- bc$x[which.max(bc$y)]

box_tran <- function(dat,lam){
  if( lam != 0){             ## use power transformation in lam != 0
    (dat^lam - 1) / lam
  }else{                     ## use log(data) if lam =0
    log(dat)
  }
}

boxy <- box_tran(unity,lambda)

s <- sd(boxy)
m <- mean(boxy)
y <- (boxy - m) / s
plot(density(y,adjust=1.5))
x = seq(-4,5,length = 1000)
#plot(density(y,adjust=2))
#x = seq(-3,3,length=1000)
lines(x,dnorm(x,0,1),col='red')

#might be better to trim more aggressively than 50 weeks (44 and under perhaps)