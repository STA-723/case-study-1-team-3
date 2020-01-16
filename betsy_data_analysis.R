library(mice, warn.conflicts = FALSE)
library(PerformanceAnalytics, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(reshape2)


# import data
df = readRDS("Longnecker.rds")
head(df)

df[rowSums(is.na(df)) == 0,]

# majority of albumin is NA and doesn't seem importatnt so drop column 
df_ss = subset(df, select=-c(albumin))
# add missing indicator
df_ss$ai_ind = ifelse(is.na(df$albumin),0,1)

# impute the rest using mean- quite a few missing of woman's data
colSums(is.na(df_ss))
df_ss[is.na(df_ss$pcb_028),]
# All of the PCB NAs in one row. Drop this entry. 
df_ss = df_ss[!is.na(df_ss$pcb_028),]

# now, the rest of the NAs are contained in 'score' columns. 
# use single imputation- write func
mean_impute = function(col){
  col[is.na(col)] = mean(col,na.rm=T)
  return(col)
}
df_ss$score_education = mean_impute(df_ss$score_education)
df_ss$score_income = mean_impute(df_ss$score_income)
df_ss$score_occupation = mean_impute(df_ss$score_occupation)
# examining plots of these-
hist(df$score_income)
hist(df$score_education)
hist(df$score_occupation)
# score income looks uniform, but the rest don't. 
# Maybe this subset of data was chosen so this is the case? Doesn't seem alarming to me.


# double check there are no more NA
sum(colSums(is.na(df_ss)))

# indicators for race
races = unique(df_ss$race)
races
df_ss$race_w = ifelse(df_ss$race==races[1],1,0)
df_ss$race_b = ifelse(df_ss$race==races[2],1,0)
df_ss$race_o = ifelse(df_ss$race==races[3],1,0)
# drop race
df_ss = subset(df_ss,select=-c(race))

# explore albumin indicator. can this column be dropped?
# look at ratios of means of the variables with ai_ind = 0 or 1
colMeans(df_ss[df_ss$ai_ind==0,])/colMeans(df_ss[df_ss$ai_ind==1,])
# biggest difference from 1 is race_other which has low sample size. 
# note- explored relationship with center on first day of class. no relationship found.
# decision- drop ai_ind
df_ss = subset(df_ss,select = -c(ai_ind))

# cholestoral and triglycerides correlation
plot(df_ss$cholesterol)
plot(df_ss$triglycerides)
chart.Correlation(df_ss[,c("cholesterol","triglycerides")])
t.test(df_ss$cholesterol,y=df_ss$triglycerides)
# conclusion- inconclusive.

# explore age and outliers ( i.e. gest age of 90 )
# maternal age
plot(df_ss$maternal_age)
range(df_ss$maternal_age)
# conclusion- no obvious outliers or problems
# gestational age
df_ss %>% group_by(center) %>% summarise(mean(gestational_age))
df_ss[df_ss$gestational_age>=50,]
# drop these entries
df_ss = df_ss[df_ss$gestational_age<50,]


# combime pcb
pcb1_ind = 2
pcb2_ind = which(names(df_ss)=="pcb_203")
boxplot(df_ss[,pcb1_ind:pcb2_ind])
# remove individual pcb from df_ss and create 2 columns- sum, mean. 
# Will run sensitivity analysis later
pcb_mat = df_ss[,pcb1_ind:pcb2_ind]
df_ss = df_ss[,-c(pcb1_ind:pcb2_ind)]
df_ss$pcb_mean = rowMeans(pcb_mat)
df_ss$pcb_sum = rowSums(pcb_mat)

# explore differences between medical centers and all variables
center_mean_df = df_ss %>% group_by(center) %>% summarise_all("mean")
ggplot(melt(center_mean_df[,c("center","score_occupation","score_income","score_education")],id.vars="center"),
       aes(x=center,y=value,col=center))+
  geom_point()+
  facet_wrap(~variable)


# run basic linear reg
df_ss$center = as.factor(as.character(df_ss$center))
lm_mean = lm(gestational_age~.,subset(df_ss,select=-c(pcb_sum)))
lm_sum = lm(gestational_age~.,subset(df_ss,select=-c(pcb_mean)))

lm_mean_aic = step(lm_mean)
lm_sum_aic = step(lm_sum)
summary(lm_mean_aic)
summary(lm_sum_aic)

# run specific lm
lm_betsy = lm(gestational_age~dde+triglycerides+cholesterol+pcb_sum+race_w+race_b+race_o-1,df_ss)
summary(lm_betsy)

lm_simple = lm(gestational_age~dde+triglycerides+cholesterol+pcb_sum,df_ss)
summary(lm_simple)
## cholesterol and pcb not sig. Everything of small magnitude. 

lm_simple = lm(gestational_age~dde+triglycerides+cholesterol+pcb_sum+center-1,df_ss)
summary(lm_simple)


