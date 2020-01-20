long <- readRDS("~/Desktop/Perinatal/Longnecker.rds")
long$smoking_status <- factor(long$smoking_status)
long$center <- factor(long$center)
# Define pcb variable as sum of PCB concentrations
long$pcb <- apply(long[,2:12], MARGIN = 1, FUN = sum)
# Define preterm as GA deficit from 37
# long$preterm <- sapply(long$gestational_age, FUN = function(age) max(0, 37-age))
long$pcb[long$gestational_age ,]

plot(data.frame(score_education, score_income, score_occupation))

long$risk_age <- factor(long$m)

ga_mod <- lm(gestational_age ~ 
               center + smoking_status + score_education + score_income + score_occupation + 
               race + cholesterol + triglycerides +  
               pcb + dde + race:pcb + race:dde + race:cholesterol + race:triglycerides, 
             data = long)
