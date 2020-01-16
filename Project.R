
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

# IMPUTE SCORE MISSING DATA
# use single imputation- write func
mean_impute = function(col){
  col[is.na(col)] = mean(col,na.rm=T)
  return(col)
}
df_ss$score_education = mean_impute(df_ss$score_education)
df_ss$score_income = mean_impute(df_ss$score_income)
df_ss$score_occupation = mean_impute(df_ss$score_occupation)

# double check there are no more NA
sum(colSums(is.na(df_ss)))

# drop gestational age above 50
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


