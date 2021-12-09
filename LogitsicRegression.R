### logistic regression example

library(caret)
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outpu
# Performs stratified random split of the data set
# choose variables from cohort 2017
vari_apr = c("CIP_APR_updated","HS_APR_UPdated","HOURS_BROUGHT_TO_UNIVERSITY", "HSGPA_tenths", "GENDER")
FTIC_RISK_APR_DF_vari <- FTIC_RISK_APR_DF %>%  mutate(APR = factor(APR)) %>% select(APR, vari_apr)
TrainingIndex <- createDataPartition(FTIC_RISK_APR_DF_vari$APR, p=0.7, list = FALSE)
TrainingSet <- FTIC_RISK_APR_DF_vari[TrainingIndex,] # Training Set
TestingSet <- FTIC_RISK_APR_DF_vari[-TrainingIndex,] # Test Set
# Build model using all factors and labels
set.seed(111)
admitted_glm <- glm(APR ~ ., data = TrainingSet, family="binomial")
summary(admitted_glm)
exp(coef(admitted_glm))
set.seed(222)
admitted_glm_re <- glm(APR ~APR_PROGRAM+APR_HSNAME+GENDER+codeHSGPA+PELL_VERF*codeHSGPA+HOURS_BROUGHT_TO_UNIVERSITY*codeHSGPA+PELL_VERF*GENDER+ETHNICITY*codeHSGPA, data = TrainingSet, family="binomial")
summary(admitted_glm_re)
ts
P_value <- broom::tidy(admitted_glm)
#coefficient
coef1 <- exp(coef(admitted_glm))
coef_table <- knitr::kable(coef1)
coef_table
vari_imp <- knitr::kable(caret::varImp(admitted_glm))
vari_imp

#accuracy train data 
p_1 <- predict(admitted_glm, TrainingSet, type="response")
pred_1 <- ifelse(p_1>0.5, 1,0)
tab_1 <- table(Predicted=pred_1, Actural=TrainingSet$APR)
round(sum(diag(tab_1))/sum(tab_1),4) #80.15%
#test data
p_2 <- predict(admitted_glm, TestingSet, type="response")
pred_2 <- ifelse(p_2>0.5, 1,0)
tab_2 <- table(Predicted=pred_2, Actural=TestingSet$APR)
round(sum(diag(tab_2))/sum(tab_2),4) #78.6
### goodness of fit
with(admitted_glm, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail=F))#p-value==1.86439e-162
#anova(admitted_glm, admitted_glm_re, test = "Chisq")

## prediction for new ftic
NEWFTIC <- trim_apr_data2_V0 %>% filter(Cohort == 2021) 

new_FTIC_data <- NEWFTIC %>% 
    mutate(APR= factor(APR)) %>% 
    select(APR, vari_apr1) #%>% filter(!is.na(OFFER_GPA))

#call model
RISK_APR_MODEL <- readRDS("RISK_APR_MODEL_glmV0.rds")
#prediction
PREDICTION_NEWFTIC <- round(predict(RISK_APR_MODEL, new_FTIC_data, type="response"),4)
hist(PREDICTION_NEWFTIC)

PREDICTION_DF <- cbind(NEWFTIC, PREDICTION_NEWFTIC)
PREDICTION_DF$codeProb_APR <- ifelse(PREDICTION_DF$PREDICTION_NEWFTIC>=0.90,"low-risk",
                                     ifelse(PREDICTION_DF$PREDICTION_NEWFTIC>=0.60,"moderate-risk","high-risk"))



xtabs(~PREDICTION_DF$codeProb_APR)
# export resutls
write.csv(PREDICTION_DF,"OUTPUT_PREDICTION_NEWFTIC2021V0.csv") # removed 11 since missing HSGPA
colSums(is.na(PREDICTION_DF))


