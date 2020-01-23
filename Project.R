# import packages
library(quantreg)
library(tidyverse)
library(mice)

# import data
df = readRDS("Longnecker.rds")

## TIDY DATA

# majority of albumin is NA and doesn't seem importatnt so drop column 
df_ss = subset(df, select=-c(albumin))
# add missing indicator
df_ss$ai_ind = ifelse(is.na(df$albumin),0,1)

# explore missing data
colSums(is.na(df_ss))
df_ss[is.na(df_ss$pcb_028),]
# All of the PCB NAs in one row. Drop this entry. 
df_ss = df_ss[!is.na(df_ss$pcb_028),]

# double check there are no more NA
sum(colSums(is.na(df_ss)))

# drop gestational age above 50
df_ss = df_ss[df_ss$gestational_age<50,]

# bin maternal age into high risk, low risk
df_ss$mat_age_18un = ifelse(df_ss$maternal_age<=18,1,0)
df_ss$mat_age_35up = ifelse(df_ss$maternal_age>=35,1,0)
df_ss$mat_age_norisk = ifelse(df_ss$maternal_age<35 & df_ss$maternal_age>18,1,0)
# drop maternal age
df_ss = subset(df_ss,select=-c(maternal_age))

# mice on score variables
imp <- mice(df_ss)
df_ss <- complete(imp)


# pca on pcb and score

#pca on pcbs
pcb1_ind = 2
pcb2_ind = which(names(df_ss)=="pcb_203")
pc_pcb = prcomp(df_ss[,c(pcb1_ind:pcb2_ind)],scale=T)
pc_pcb

#shows %variance accounted for
summary(pc_pcb)

#skree plot for visualizaion
library(factoextra)
fviz_eig(pc_pcb)

pcb_x = as.data.frame(pc_pcb$x)

#select pcs to use
library(MASS)
pcb.lm = lm(df_ss$gestational_age~.,data=pcb_x)
summary(pcb.lm)
select.pcb = stepAIC(pcb.lm,direction="both")
summary(select.pcb)

#recommend 1,2,8 for reasons (could justify omitting any but 1 though)

#pca for ses scores
pc_ses = prcomp(df_ss[,15:17])
pc_ses
summary(pc_ses)
ses_x = as.data.frame(pc_ses$x)

ses_lm = lm(df_ss$gestational_age~.,data=ses_x)

summary(ses_lm)

select.ses = stepAIC(ses_lm,direction="both")
summary(select.ses)

# recommend 1


# qr

# placeholder for variables
# combime pcb
pcb1_ind = 2
pcb2_ind = which(names(df_ss)=="pcb_203")
# remove individual pcb from df_ss and create sum column
df_ss = df_ss[,-c(pcb1_ind:pcb2_ind)]

df_ss$pcb_pc1 = pcb_x[,1]
df_ss$pcb_pc2 = pcb_x[,2]
df_ss$pcb_pc8 = pcb_x[,8]

# add pca for score variables
df_ss$score = ses_x[,1]

# reformat race and center
# indicators for race
races = unique(df_ss$race)
races
df_ss$race_w = ifelse(df_ss$race==races[1],1,0)
df_ss$race_b = ifelse(df_ss$race==races[2],1,0)
df_ss$race_o = ifelse(df_ss$race==races[3],1,0)
# drop race
df_ss = subset(df_ss,select=-c(race))
# indicators for center
centers = unique(df_ss$center)
centers
df_ss$center_1 = ifelse(df_ss$center==centers[1],1,0)
df_ss$center_2 = ifelse(df_ss$center==centers[2],1,0)
df_ss$center_3 = ifelse(df_ss$center==centers[3],1,0)
df_ss$center_4 = ifelse(df_ss$center==centers[4],1,0)
df_ss$center_5 = ifelse(df_ss$center==centers[5],1,0)
df_ss$center_6 = ifelse(df_ss$center==centers[6],1,0)
df_ss$center_7 = ifelse(df_ss$center==centers[7],1,0)
df_ss$center_8 = ifelse(df_ss$center==centers[8],1,0)
df_ss$center_9 = ifelse(df_ss$center==centers[9],1,0)
df_ss$center_10 = ifelse(df_ss$center==centers[10],1,0)
df_ss$center_11 = ifelse(df_ss$center==centers[11],1,0)
df_ss$center_12 = ifelse(df_ss$center==centers[12],1,0)
# drop center
df_ss = subset(df_ss,select=-c(center))


# drop variables we don't want as covariates
df_ss = subset(df_ss,select=-c(score_income,score_education,score_occupation,ai_ind,center_1,race_w,mat_age_norisk))

# explore identifiability
des_mat = stats::model.matrix(gestational_age ~ ., data=df_ss)
qr(des_mat)$rank
ncol(df_ss)

# linear model
lm_model = lm(gestational_age ~ ., data=df_ss)
summary(lm_model)

# qr 
quants = seq(from=.1,to=.9,by=.1)
#quants = c(.1,.25,.5,.75,.9)
nq = length(quants)
qr_model = rq(gestational_age ~ ., data=df_ss, tau=quants)
qr_summ = summary(qr_model,se="rank")


par(mfrow = c(1,2))
# plot fitted
# df_ss_ordered = df_ss %>% mutate(index = 1:nrow(df_ss)) %>% 
#   arrange(dde,pcb_pc1,pcb_pc2,pcb_pc8) %>% 
#   select(index)
# data = qr_model$fitted.values[c(df_ss_ordered$index),]
data = qr_model$fitted.values

#dde
logx = log(df_ss$dde)
png('fitted_dde_cs1.png',width=600,height=400)
plot(logx,data[,nq],col=2,ylim=range(data), bty='L',
     ylab="Age",
     main = "Fitted Values",
     xlab ="log(DDE)",
     pch=20)
for ( j in 2:3){
  points(logx,data[,c(9,5,1)][,j],col=j+1,
         pch=20)
}
par(xpd=TRUE)
legend("topright", inset = c(-.05,0),
       legend=as.character(rev(quants[c(1,5,9)])), title="Quantile",
       pch=16,col = 2:4)
par(xpd=F)
dev.off()

#pcb_pc1
logx = (df_ss$pcb_pc1)
png('fitted_pcb1_cs1.png',width=600,height=400)
plot(logx,data[,nq],col=2,ylim=range(data), bty='L',
     ylab="Age",
     main = "Fitted Values",
     xlab ="PCB_PC1",
     pch=20)
for ( j in 2:3){
  points(logx,data[,c(9,5,1)][,j],col=j+1,
         pch=20)
}
par(xpd=TRUE)
legend("topright", inset = c(-.05,0),
       legend=as.character(rev(quants[c(1,5,9)])), title="Quantile",
       pch=16,col = 2:4)
par(xpd=F)
dev.off()

#pcb_pc2
logx = df_ss$pcb_pc2
png('fitted_pcb2_cs1.png',width=600,height=400)
plot(logx,data[,nq],col=2,ylim=range(data), bty='L',
     ylab="Age",
     main = "Fitted Values",
     xlab ="PCB_PC2",
     pch=20)
for ( j in 2:3){
  points(logx,data[,c(9,5,1)][,j],col=j+1,
         pch=20)
}
par(xpd=TRUE)
legend("topright", inset = c(-.05,0),
       legend=as.character(rev(quants[c(1,5,9)])), title="Quantile",
       pch=16,col = 2:4)
par(xpd=F)
dev.off()

#pcb_pc8
logx = df_ss$pcb_pc8
png('fitted_pcb8_cs1.png',width=600,height=400)
plot(logx,data[,nq],col=2,ylim=range(data), bty='L',
     ylab="Age",
     main = "Fitted Values",
     xlab ="PCB_PC8",
     pch=20)
for ( j in 2:3){
  points(logx,data[,c(9,5,1)][,j],col=j+1,
         pch=20)
}
par(xpd=TRUE)
legend("topright", inset = c(-.05,0),
       legend=as.character(rev(quants[c(1,5,9)])), title="Quantile",
       pch=16,col = 2:4)
par(xpd=F)
dev.off()




# plot coefs of important variables
coefs = qr_model$coefficients
cis = lapply(1:nq,
             function(x) qr_summ[x][[1]]$coefficients[,2:3])

# dde
coefs_dde = coefs[which(rownames(coefs) == "dde"),]
cis_dde = sapply(1:nq, 
                 function(x) cis[[x]][which(rownames(coefs) == "dde"),])

png('coef_dde_cs1.png',width=1000,height=600)
plot(quants,coefs_dde,
     type="b",lwd=1.5,lty=1,col="black",pch=16,
     xlab = "quantiles",ylab = expression(beta[dde]),
     main = "Coefficient of DDE",
     ylim = range(coefs_dde,cis_dde),xaxt='n')
axis(1,at=quants)
lines(quants,cis_dde[1,],lty=2)
lines(quants,cis_dde[2,],lty=2)
abline(h=0,col="red")
dev.off()

# pcb
coefs_pcb = coefs[which(rownames(coefs) == "pcb_pc1"),]
cis_pcb = sapply(1:nq, 
                 function(x) cis[[x]][which(rownames(coefs) == "pcb_pc1"),])

png('coef_pcb1_cs1.png',width=1000,height=600)
plot(quants,coefs_pcb,
     type="b",lwd=1.5,lty=1,col="black",pch=16,
     xlab = "quantiles",ylab = expression(beta[pcb]),
     main = "Coefficient of PCB, PC1",
     ylim = range(coefs_pcb,cis_pcb),xaxt='n')
axis(1,at=quants)
lines(quants,cis_pcb[1,],lty=2)
lines(quants,cis_pcb[2,],lty=2)
abline(h=0,col="red")
dev.off()

coefs_pcb = coefs[which(rownames(coefs) == "pcb_pc2"),]
cis_pcb = sapply(1:nq, 
                 function(x) cis[[x]][which(rownames(coefs) == "pcb_pc2"),])

png('coef_pcb2_cs1.png',width=1000,height=600)
plot(quants,coefs_pcb,
     type="b",lwd=1.5,lty=1,col="black",pch=16,
     xlab = "quantiles",ylab = expression(beta[pcb]),
     main = "Coefficient of PCB, PC2",
     ylim = range(coefs_pcb,cis_pcb),xaxt='n')
axis(1,at=quants)
lines(quants,cis_pcb[1,],lty=2)
lines(quants,cis_pcb[2,],lty=2)
abline(h=0,col="red")
dev.off()

coefs_pcb = coefs[which(rownames(coefs) == "pcb_pc8"),]
cis_pcb = sapply(1:nq, 
                 function(x) cis[[x]][which(rownames(coefs) == "pcb_pc8"),])

png('coef_pcb8_cs1.png',width=1000,height=600)
plot(quants,coefs_pcb,
     type="b",lwd=1.5,lty=1,col="black",pch=16,
     xlab = "quantiles",ylab = expression(beta[pcb]),
     main = "Coefficient of PCB, PC8",
     ylim = range(coefs_pcb,cis_pcb),xaxt='n')
axis(1,at=quants)
lines(quants,cis_pcb[1,],lty=2)
lines(quants,cis_pcb[2,],lty=2)
abline(h=0,col="red")
dev.off()

#pc_loadings
pc_pcb

