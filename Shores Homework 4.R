
library(tidycensus)
library(tidyverse)

dec_pl_vars <- load_variables(year =2020, dataset = "dp")

p1_2020 <- get_decennial(year = 2020,
                         sumfile = "dp",
                         table = "P1",
                         geography = "county")

p1_2020_sf <- get_decennial(year = 2020,
                            sumfile = "dp",
                            table = "P1",
                            geography = "county",
                            geometry = TRUE)

SexAge_2020 <- get_decennial(year = 2020,
                              sumfile = "dp",
                              variables = "DP1_0001P",
                              geography = "county",
                              state = "OH")

Households_2020 <- get_decennial(year = 2020,
                             sumfile = "dp",
                             variables = "DP1_0132P",
                             geography = "county",
                             state = "OH")

acs5_subject_vars <- load_variables(year = 2021, dataset = "acs5/subject", cache = TRUE)

unique(acs5_subject_vars$concept)

s2702_vars <- acs5_subject_vars %>% filter(grepl("S2702", name)) %>% rename(variable = name)

oh_counties_prcnt_uninsured_agesex<- get_acs( year = 2021,
                                                    variables =  c(`Age and Sex` = "S0101_C01_001"),
                                                    survey = "S2702",
                                                    geography = "county",
                                                    state = "OH",
                                                    output = "wide") 
                                                                 
library(tidyverse)
Cancer <- read_csv("data.csv")

#Log model 
diag.fit <- lm(diagnosis_binary~texture_mean+radius_mean+perimeter_mean, data = Cancer)
pred.diag <- round(predict(diag.fit,newdata=Cancer))

glm1 <- glm(diagnosis_binary~texture_mean+radius_mean+perimeter_mean, data = Cancer, family = binomial)
summary(glm1)
glm1.pred <- round(predict(glm1, type = "response"))

plot(Cancer$texture_mean,Cancer$radius_mean,Cancer$perimeter_mean,col=Cancer$diagnosis+2,pch=19,cex=.7)
points(Cancer$texture_mean,Cancer$radius_mean,Cancer$perimeter_mean,col=glm1.pred+2,cex=1.5)

crosstab_glm1 <- table(glm1.pred, body$diagnosis_binary)
colnames(crosstab_glm1) <- c("Predicts 0", "Predicts 1")
rownames(crosstab_glm1) <- c("True 0", "True 1")
crosstab_glm1

#linear model

fit <- lm(texture_mean+radius_mean+perimeter_mean ~ diagnosis_binary, data = Cancer)
fit
abline(fit)

fit.pred <- round(predict(fit, type = "response"))

summary(fit)

#compare ROC curves

library(mosaic)

simple_roc <- function(labels, scores){
  
  labels <- labels[order(scores, decreasing=TRUE)]
  
  data.frame(FPR=cumsum(!labels)/sum(!labels),TPR=cumsum(labels)/sum(labels), labels)
  
}

roc_glm1 <- simple_roc(Cancer$diagnosis_binary, glm1.pred)
plot(roc_glm1[,1:2], xlab='False Positive', ylab='True Positive',col='green')

roc_fit <- simple_roc(Cancer$diagnosis_binary, fit.pred)
plot(roc_fit[,1:2], xlab='False Positive', ylab='True Positive',col='green')

library(gridExtra)


