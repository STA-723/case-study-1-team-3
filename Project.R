# import packages
library(quantreg)

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
df_ss$mat_age_risk = ifelse(df_ss$maternal_age>=35 | df_ss$maternal_age<=18,1,0)
# drop maternal age
df_ss = subset(df_ss,select=-c(maternal_age))


# mice on score variables



# pca on pcb


# qr

# placeholder for variables
# combime pcb
pcb1_ind = 2
pcb2_ind = which(names(df_ss)=="pcb_203")
# remove individual pcb from df_ss and create sum column
df_ss = df_ss[,-c(pcb1_ind:pcb2_ind)]
df_ss$pcb_sum = rowSums(pcb_mat)

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
df_ss = subset(df_ss,select=-c(score_income,score_education,ai_ind,center_1,race_w))

# explore identifiability
des_mat = stats::model.matrix(gestational_age ~ ., data=df_ss)
qr(des_mat)$rank
ncol(df_ss)

# linear model
lm_model = lm(gestational_age ~ ., data=df_ss)
summary(lm_model)

# qr 
quants = seq(from=.1,to=.9,by=.1)
nq = length(quants)
qr_model = rq(gestational_age ~ ., data=df_ss, tau=quants)
qr_summ = summary(qr_model,se="rank")

# plot fitted
data = qr_model$fitted.values
plot(data[,1],col=2,ylim=range(data), bty='L',
     ylab="Age",
     main = "Fitted Values")
for ( j in 2:(nq)){
  points(data[,j],col=j+1)
}
par(xpd=TRUE)
legend("topright", inset=c(-0.15,0),
       legend=as.character(quants), title="Quantile",
       pch=16,col = c(2:nq))
par(xpd=F)

# plot coefs of important variables
coefs = qr_model$coefficients
cis = lapply(1:nq,
             function(x) qr_summ[x][[1]]$coefficients[,2:3])

# dde
coefs_dde = coefs[which(rownames(coefs) == "dde"),]
cis_dde = sapply(1:nq, 
                 function(x) cis[[x]][which(rownames(coefs) == "dde"),])
plot(quants,coefs_dde,
     type="b",lwd=1.5,lty=1,col="black",pch=16,
     xlab = "quantiles",ylab = expression(beta[dde]),
     main = "Coefficient of DDE",
     ylim = range(coefs_dde,cis_dde))
lines(quants,cis_dde[1,],lty=2)
lines(quants,cis_dde[2,],lty=2)
abline(h=0,col="red")

# pcb
coefs_pcb = coefs[which(rownames(coefs) == "pcb_sum"),]
cis_pcb = sapply(1:nq, 
                 function(x) cis[[x]][which(rownames(coefs) == "pcb_sum"),])
plot(quants,coefs_pcb,
     type="b",lwd=1.5,lty=1,col="black",pch=16,
     xlab = "quantiles",ylab = expression(beta[pcb]),
     main = "Coefficient of PCB",
     ylim = range(coefs_pcb,cis_pcb))
lines(quants,cis_pcb[1,],lty=2)
lines(quants,cis_pcb[2,],lty=2)
abline(h=0,col="red")



