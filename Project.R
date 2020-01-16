# import packages


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


