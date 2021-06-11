#================================ Part II =====================================
#================ Decriptive Characteristics & Regression Models ==============

#========================== Working directory =================================
setwd("C:/Users/hilar/Desktop/HZ/School/CHL5250 - Special Topics in Biostats/A3")
getwd()

library(car)

#=================================== Dateset ==================================

cohort <- read.csv("Data.cleaned.csv")
str(cohort)


#============================ Descriptive Statistics =========================
nametag <- c("Count", "Proportion")

# Age:
str(cohort$age)
cohort$age <- as.factor(cohort$age)
str(cohort$age)
age_sum <- summary(cohort$age)
age_sum <- as.matrix(age_sum)
age_prop <- round(prop.table(age_sum), 4)
age <- cbind(age_sum, age_prop[,1])
age <- data.frame(age)
names(age) <- nametag

# Sex:
str(cohort$sex)
cohort$sex <- as.factor(cohort$sex)
str(cohort$sex)
sex_sum <- summary(cohort$sex)
sex_sum <- as.matrix(sex_sum)
sex_prop <- round(prop.table(sex_sum), 4)
sex <- cbind(sex_sum, sex_prop[,1])
sex <- data.frame(sex)
names(sex) <- nametag

# Ethnicity:
str(cohort$racial_origin)
cohort$racial_origin <- as.factor(cohort$racial_origin)
str(cohort$racial_origin)
ethnic_sum <- summary(cohort$racial_origin)
ethnic_sum <- as.matrix(ethnic_sum)
ethnic_prop <- round(prop.table(ethnic_sum), 4)
ethnic <- cbind(ethnic_sum, ethnic_prop[,1])
ethnic <- data.frame(ethnic)
names(ethnic) <- nametag

# Education:
str(cohort$education)
cohort$education <- as.factor(cohort$education)
str(cohort$education)
edu_sum <- summary(cohort$education)
edu_sum <- as.matrix(edu_sum)
edu_prop <- round(prop.table(edu_sum), 4)
edu <- cbind(edu_sum, edu_prop[,1])
edu <- data.frame(edu)
names(edu) <- nametag
rownames(edu)[1:4] <- c("< secondary", 
                        "secondary grad.", 
                        "other post-sec", 
                        "post-sec. grad")

# Household income:
str(cohort$income)
cohort$income <- as.factor(cohort$income)
str(cohort$income)
income_sum <- summary(cohort$income)
income_sum <- as.matrix(income_sum)
income_prop <- round(prop.table(income_sum), 4)
income <- cbind(income_sum, income_prop[,1])
income <- data.frame(income)
names(income) <- nametag
rownames(income)[1:5] <- c("no income", "< 15000", "15000-29999", "30000-49999", 
                           "50000-79999")

# BMI:
str(cohort$BMI)
cohort$BMI <- as.factor(cohort$BMI)
str(cohort$BMI)
bmi_sum <- summary(cohort$BMI)
bmi_sum <- as.matrix(bmi_sum)
bmi_prop <- round(prop.table(bmi_sum), 4)
bmi <- cbind(bmi_sum, bmi_prop[,1])
bmi <- data.frame(bmi)
names(bmi) <- nametag

# Smoking:
str(cohort$smoker)
cohort$smoker <- as.factor(cohort$smoker)
str(cohort$smoker)
smk_sum <- summary(cohort$smoker)
smk_sum <- as.matrix(smk_sum)
smk_prop <- round(prop.table(smk_sum), 4)
smk <- cbind(smk_sum, smk_prop[,1])
smk <- data.frame(smk)
names(smk) <- nametag
rownames(smk)[1:6] <- c("daily", "occasionally", "not a smoker", "former daily", 
                        "former occasionally", "never smoked")

# Alcohol:
str(cohort$drinker)
cohort$drinker <- as.factor(cohort$drinker)
str(cohort$drinker)
drink_sum <- as.matrix(summary(cohort$drinker))
drink_prop <- round(prop.table(drink_sum), 4)
drink <- cbind(drink_sum, drink_prop[,1])
drink <- data.frame(drink)
names(drink) <- nametag
rownames(drink)[1:4] <- c("regular", "occasionally", "former", "never")

# High blood pressure:
str(cohort$bp)
cohort$bp <- as.factor(cohort$bp)
str(cohort$bp)
bp_sum <- as.matrix(summary(cohort$bp))
bp_prop <- round(prop.table(bp_sum), 4)
bp <- cbind(bp_sum, bp_prop[,1])
bp <- data.frame(bp)
names(bp) <- nametag

# Diabetes:
str(cohort$diabetes)
cohort$diabetes <- as.factor(cohort$diabetes)
str(cohort$diabetes)
t2d_sum <- as.matrix(summary(cohort$diabetes))
t2d_prop <- round(prop.table(t2d_sum), 4)
t2d <- cbind(t2d_sum, t2d_prop[,1])
t2d <- data.frame(t2d)
names(t2d) <- nametag

# Access to regular medical doctor:
str(cohort$doctor)
cohort$doctor <- as.factor(cohort$doctor)
str(cohort$doctor)
med_sum <- as.matrix(summary(cohort$doctor))
med_prop <- round(prop.table(med_sum), 4)
med <- cbind(med_sum, med_prop[,1])
med <- data.frame(med)
names(med) <- nametag

# Physical activity:
str(cohort$PAI)
cohort$PAI <- as.factor(cohort$PAI)
str(cohort$PAI)
activity_sum <- as.matrix(summary(cohort$PAI))
activity_prop <- round(prop.table(activity_sum), 4)
activity <- cbind(activity_sum, activity_prop[,1])
activity <- data.frame(activity)
names(activity) <- nametag
rownames(activity)[1:3] <- c("active", "moderate", "inactive")

# Daily consumption:
str(cohort$daily_consumption)
cohort$daily_consumption <- as.factor(cohort$daily_consumption)
str(cohort$daily_consumption)
consump_sum <- as.matrix(summary(cohort$daily_consumption))
consump_prop <- round(prop.table(consump_sum), 4)
consump <- cbind(consump_sum, consump_prop[,1])
consump <- data.frame(consump)
names(consump) <- nametag

# Marital status:
str(cohort$marital_status)
cohort$marital_status <- as.factor(cohort$marital_status)
str(cohort$marital_status)
marital_sum <- as.matrix(summary(cohort$marital_status))
marital_prop <- round(prop.table(marital_sum), 4)
marital <- cbind(marital_sum, marital_prop[,1])
marital <- data.frame(marital)
names(marital) <- nametag
rownames(marital)[1:4] <- c("marrried", "common-law", "widow", "single")

# Recency of immigration:
str(cohort$length_of_time)
cohort$length_of_time <- as.factor(cohort$length_of_time)
str(cohort$length_of_time)
cic_sum <- as.matrix(summary(cohort$length_of_time))
cic_prop <- round(prop.table(cic_sum), 4)
cic <- cbind(cic_sum, cic_prop[,1])
cic <- data.frame(cic)
names(cic) <- nametag
rownames(cic)[1:3] <- c(">10 years", "not immigrant", "recent immigrant") 

# Region:
str(cohort$province)
cohort$province <- as.factor(cohort$province)
str(cohort$province)
region_sum <- as.matrix(summary(cohort$province))
region_prop <- round(prop.table(region_sum), 4)
region <- cbind(region_sum, region_prop[,1])
region <- data.frame(region)
names(region) <- nametag

# Combining all covariates into one table:
empt <- c("","")
age <- rbind(empt, age)
rownames(age)[1] <- "Age"

sex <- rbind(empt, sex)
rownames(sex)[1] <- "Sex"

ethnic <- rbind(empt, ethnic)
rownames(ethnic)[1] <- "Ethnicity"

edu <- rbind(empt, edu)
rownames(edu)[1] <- "Education"

income <- rbind(empt, income)
rownames(income)[1] <- "Household income"

bmi <- rbind(empt, bmi)
rownames(bmi)[1] <- "BMI"

smk <- rbind(empt, smk)
rownames(smk)[1] <- "Smoker"

drink <- rbind(empt, drink)
rownames(drink)[1] <- "Drinker"

bp <- rbind(empt, bp)
rownames(bp)[1] <- "High blood pressure"

t2d <- rbind(empt, t2d)
rownames(t2d)[1] <- "Diabetes"

med <- rbind(empt, med)
rownames(med)[1] <- "Medical access"

activity <- rbind(empt, activity)
rownames(activity)[1] <- "Physical activity"

marital <- rbind(empt, marital)
rownames(marital)[1] <- "Marital status"

cic <- rbind(empt, cic)
rownames(cic)[1] <- "Recency of immigration"

region <- rbind(empt, region)
rownames(region)[1] <- "Living region"

# Table of Descriptive Statistics:
descript <- rbind(age, sex, ethnic, edu, income, bmi, smk, drink, bp, t2d, med, 
                  activity, marital, cic, region)

knitr::kable(descript, caption = "Table 1. Descriptive Characteristics")
write.csv(descript, "Descriptive statistics.csv")


#============================ Reference levels ===============================
# Outcome:
str(cohort$heart_disease)
cohort$heart_disease <- as.factor(cohort$heart_disease)
summary(cohort$heart_disease)
cohort$heart_disease <- relevel(cohort$heart_disease, ref = "No")
cohort$heart_disease <- ifelse(cohort$heart_disease == "No", 0, 
                               ifelse(cohort$heart_disease == "Yes", 1, NA))

# Main exposure:
str(cohort$arthritis)
cohort$arthritis <- as.factor(cohort$arthritis)
summary(cohort$arthritis)
cohort$arthritis <- relevel(cohort$arthritis, ref = "No")

# Covariates:
cohort$age <- relevel(cohort$age, ref = "35-49")
cohort$sex <- relevel(cohort$sex, ref = "female")
cohort$racial_origin <- relevel(cohort$racial_origin, ref = "white")
cohort$education <- relevel(cohort$education, ref = 4)
cohort$income <- relevel(cohort$income, ref = 4)
cohort$BMI <- relevel(cohort$BMI, ref = "overweight")
cohort$smoker <- relevel(cohort$smoker, ref = 6)
cohort$drinker <- relevel(cohort$drinker, ref = 1)
cohort$bp <- relevel(cohort$bp, ref = "No")
cohort$diabetes <- relevel(cohort$diabetes, ref = "No")
cohort$doctor <- relevel(cohort$doctor, ref = "Yes")
cohort$PAI <- relevel(cohort$PAI, ref = 3)
cohort$marital_status <- relevel(cohort$marital_status, ref = 1)
cohort$length_of_time <- relevel(cohort$length_of_time, ref = "not_immigrant")
cohort$daily_consumption <- relevel(cohort$daily_consumption, ref="3-6")

str(cohort$COPD)
cohort$COPD <- as.factor(cohort$COPD)
cohort$COPD <- relevel(cohort$COPD, ref="No")


#========================= Multivariable logistic model =======================

AIC_0 <- c()
cohort_non <- na.omit(cohort)

# Multivariable logistic regression model with sex:
full.mod <- glm(heart_disease ~ arthritis + age + sex + racial_origin + education 
                + income + BMI + smoker + drinker + bp + diabetes + doctor + PAI,
                data = cohort_non, weights = weight/mean(weight), family = "binomial",
                na.action = na.omit)
summary(full.mod)

full.mod.step <- step(full.mod, direction = "backward")
summary(full.mod.step)

AIC_0 <- rbind(AIC_0, c("Sex", full.mod$aic))


# Multivariable logistic regression with immigration: 
full.mod1 <- glm(heart_disease ~ arthritis + age + sex + racial_origin + education 
                 + income + BMI + smoker + drinker + bp + diabetes + doctor + PAI 
                 + length_of_time,
                 data = cohort_non, weights = weight/mean(weight), family = "binomial",
                 na.action = na.omit)
summary(full.mod1)
full.mod.step1 <- step(full.mod1, direction = "backward")
summary(full.mod.step1)

AIC_0 <- rbind(AIC_0, c("Immigration", full.mod1$aic))

# Multivariable logistic regression with marital status: 
full.mod2 <- glm(heart_disease ~ arthritis + age + sex + racial_origin + education 
                 + income + BMI + smoker + drinker + bp + diabetes + doctor + PAI 
                 + marital_status,
                 data = cohort_non, weights = weight/mean(weight), family = "binomial",
                 na.action = na.omit)
summary(full.mod2)
full.mod.step2 <- step(full.mod2, direction = "backward")
summary(full.mod.step2)

AIC_0 <- rbind(AIC_0, c("Marital Status", full.mod2$aic))


# Multivariable logistic regression with sex, marital status & immigration: 
full.mod3 <- glm(heart_disease ~ arthritis + age + sex + racial_origin + education 
                 + income + BMI + smoker + drinker + bp + diabetes + doctor + PAI 
                 + marital_status + length_of_time,
                 data = cohort_non, weights = weight/mean(weight), family = "binomial",
                 na.action = na.omit)
summary(full.mod3)

AIC_0 <- rbind(AIC_0, c("Full", full.mod3$aic))

full.mod.step3 <- step(full.mod3, direction = "backward")
summary(full.mod.step3)


# Multivariable logistic regression with sex removed: 
full.mod4 <- glm(heart_disease ~ arthritis + age + racial_origin + education 
                 + income + BMI + smoker + drinker + bp + diabetes + doctor + PAI,
                 data = cohort_non, weights = weight/mean(weight), family = "binomial",
                 na.action = na.omit)
summary(full.mod4)

AIC_0 <- rbind(AIC_0, c("Base", full.mod4$aic))


AIC_0 <- data.frame(AIC_0)
names(AIC_0) <- c("Model", "AIC")
AIC_0$AIC <- round(as.numeric(AIC_0$AIC), digits = 4)

write.csv(AIC_0, "Multivariable Models.csv")


#======================= Multivariate Model with Interaction ==================
# Based on the final model selected by backward selection:
# heart_disease ~ arthritis + age + sex + racial_origin + education + income + 
#   smoker + drinker + bp + diabetes + doctor + length_of_time

# Interaction with arthritis:
AIC <- c()
out <- c()

## Age:
mod1 <- glm(heart_disease ~ arthritis + age + sex + 
              racial_origin + education + income + smoker + drinker + bp + 
              diabetes + doctor + length_of_time + arthritis*age,
            data = cohort, weights = weight/mean(weight), family = "binomial",
            na.action = na.omit)
summary(mod1)
type3_1 <- Anova(mod1, type="III")
out <- rbind(out, c("Age", mod1$aic, type3_1$`Pr(>Chisq)`[13]))


## Sex:
mod2 <- glm(heart_disease ~ arthritis + age + sex + 
              racial_origin + education + income + smoker + drinker + bp + 
              diabetes + doctor + length_of_time + arthritis*sex,
            data = cohort, weights = weight/mean(weight), family = "binomial",
            na.action = na.omit)
summary(mod2)
type3_2 <- Anova(mod2, type="III")
out <- rbind(out, c("Sex", mod2$aic, type3_2$`Pr(>Chisq)`[13]))

## Ethnicity:
mod3 <- glm(heart_disease ~ arthritis + age + sex + 
              racial_origin + education + income + smoker + drinker + bp + 
              diabetes + doctor + length_of_time + arthritis*racial_origin,
            data = cohort, weights = weight/mean(weight), family = "binomial",
            na.action = na.omit)
summary(mod3)
type3_3 <- Anova(mod3, type="III")
out <- rbind(out, c("Ethnicity", mod3$aic, type3_3$`Pr(>Chisq)`[13]))

## Education:
mod4 <- glm(heart_disease ~ arthritis + age + sex + 
              racial_origin + education + income + smoker + drinker + bp + 
              diabetes + doctor + length_of_time + arthritis*education,
            data = cohort, weights = weight/mean(weight), family = "binomial",
            na.action = na.omit)
summary(mod4)
AIC <- rbind(AIC, c("Education", mod4$aic))
type3_4 <- Anova(mod4, type="III")
out <- rbind(out, c("Education", mod4$aic, type3_4$`Pr(>Chisq)`[13]))

## Household income:
mod5 <- glm(heart_disease ~ arthritis + age + sex + 
              racial_origin + education + income + smoker + drinker + bp + 
              diabetes + doctor + length_of_time + arthritis*income,
            data = cohort, weights = weight/mean(weight), family = "binomial",
            na.action = na.omit)
summary(mod5)
AIC <- rbind(AIC, c("Income", mod5$aic))
type3_5 <- Anova(mod5, type="III")
out <- rbind(out, c("Income", mod5$aic, type3_5$`Pr(>Chisq)`[13]))

## Smoking:
mod6 <- glm(heart_disease ~ arthritis + age + sex + 
              racial_origin + education + income + smoker + drinker + bp + 
              diabetes + doctor + length_of_time + arthritis*smoker,
            data = cohort, weights = weight/mean(weight), family = "binomial",
            na.action = na.omit)
summary(mod6)
AIC <- rbind(AIC, c("Smoking", mod6$aic))
type3_6 <- Anova(mod6, type="III")
out <- rbind(out, c("Smoking", mod6$aic, type3_6$`Pr(>Chisq)`[13]))

## Drinker:
mod7 <- glm(heart_disease ~ arthritis + age + sex + 
              racial_origin + education + income + smoker + drinker + bp + 
              diabetes + doctor + length_of_time + arthritis*drinker,
            data = cohort, weights = weight/mean(weight), family = "binomial",
            na.action = na.omit)
summary(mod7)
AIC <- rbind(AIC, c("Drinking", mod7$aic))
type3_7 <- Anova(mod7, type="III")
out <- rbind(out, c("Drinking", mod7$aic, type3_7$`Pr(>Chisq)`[13]))

## Blood pressure:
mod8 <- glm(heart_disease ~ arthritis + age + sex + 
              racial_origin + education + income + smoker + drinker + bp + 
              diabetes + doctor + length_of_time + arthritis*bp,
            data = cohort, weights = weight/mean(weight), family = "binomial",
            na.action = na.omit)
summary(mod8)
AIC <- rbind(AIC, c("High BP", mod8$aic))
type3_8 <- Anova(mod8, type="III")
out <- rbind(out, c("High BP", mod8$aic, type3_8$`Pr(>Chisq)`[13]))

## Diabetes:
mod9 <- glm(heart_disease ~ arthritis + age + sex + 
              racial_origin + education + income + smoker + drinker + bp + 
              diabetes + doctor + length_of_time + arthritis*diabetes,
            data = cohort, weights = weight/mean(weight), family = "binomial",
            na.action = na.omit)
summary(mod9)
AIC <- rbind(AIC, c("Diabetes", mod9$aic))
type3_9 <- Anova(mod9, type="III")
out <- rbind(out, c("Diabetes", mod9$aic, type3_9$`Pr(>Chisq)`[13]))

## Medical access:
mod10 <- glm(heart_disease ~ arthritis + age + sex + 
               racial_origin + education + income + smoker + drinker + bp + 
               diabetes + doctor + length_of_time + arthritis*doctor,
             data = cohort, weights = weight/mean(weight), family = "binomial",
             na.action = na.omit)
summary(mod10)
AIC <- rbind(AIC, c("Medical access", mod10$aic))
type3_10 <- Anova(mod10, type="III")
out <- rbind(out, c("Medical access", mod10$aic, type3_10$`Pr(>Chisq)`[13]))

## Length of immigration:
mod11 <- glm(heart_disease ~ arthritis + age + sex + 
               racial_origin + education + income + smoker + drinker + bp + 
               diabetes + doctor + length_of_time + arthritis*length_of_time,
             data = cohort, weights = weight/mean(weight), family = "binomial",
             na.action = na.omit)
summary(mod11)
AIC <- rbind(AIC, c("Immigration status", mod11$aic))
type3_11 <- Anova(mod11, type="III")
out <- rbind(out, c("Immigration status", mod11$aic, type3_11$`Pr(>Chisq)`[13]))

# Create the table for the output with AIC & Type III effect analysis P-value:
out[,2] <- round(as.numeric(out[,2]), 4)
out <- data.frame(out)
names(out) <- c("Interaction with arthritis", "AIC", "Type III P-value")

write.csv(out, "Models with Interaction.csv")

# Smallest AIC presented with the interaction between arthritis & ethnicity:
out[out$AIC==min(out$AIC),]




# #================================ IGNORE the part below =========================
# #================================= Clustering =================================
# # Clustering:
# library(geeM)
# 
# cohort_non <- na.omit(cohort) 
# 
# gee.mod <- geeglm(heart_disease ~ arthritis + age + sex + racial_origin + education 
#                   + income + BMI + smoker + drinker + bp + diabetes + doctor + PAI,
#                   data = cohort_non, weights = weight/mean(weight), family = "binomial",
#                   id = length_of_time, corstr = "exchangeable")
# 
# 
# #============================ Regression Model with Survglm ===================
# library(survey)
# 
# cohort$length_of_time <- ifelse(cohort$length_of_time == "not_immigrant", 0,
#                                 ifelse(cohort$length_of_time == "immigrated_more_than_10_years_ago", 1,
#                                        ifelse(cohort$length_of_time == "recent_immigrant", 2, NA)))
# 
# surv.mod <- svyglm(heart_disease ~ arthritis + age + sex + racial_origin + education 
#                    + income + BMI + smoker + drinker + bp + diabetes + doctor + PAI,
#                    svydesign(ids = ~length_of_time, 
#                              # probs=1/(cohort_non$weight/mean(cohort_non$weight)), 
#                              weights = cohort_non$weight/mean(cohort_non$weight),
#                              data = cohort_non),
#                    data = cohort_non, family = "binomial")
# summary(surv.mod)




#####======================= Imputation analysis=====================#########
require(mice)
require(pROC)
require(ggplot2)
######################### final model (training & test) #########################
final.mod <- glm(heart_disease ~ arthritis + age + sex + racial_origin + education + income + 
                 smoker + drinker + bp + diabetes + doctor + length_of_time,
                 weights = weight/mean(weight),
                 family = "binomial", data = cohort, na.action = na.omit)
summary(final.mod)

## Missing Ratio of income
sum(is.na(cohort$income)) / dim(cohort)[1]

## Divide total sample set into train & test sets, Ratio of test set & train set = 1:3
smp_size <- floor(0.75 * nrow(cohort))
set.seed(3000614)
train_ind <- sample(seq_len(nrow(cohort)), size = smp_size)

# Train model using the unimputed training set
train.set.true <- na.omit(cohort[train_ind, ])

# final model
final.mod.true <- glm(heart_disease ~ arthritis + age + sex + racial_origin + education + income + 
                        smoker + drinker + bp + diabetes + doctor + length_of_time,
                      weights = weight/mean(weight),
                      family = "binomial", data = train.set.true )
test.pred.final <- predict(final.mod.true, na.omit(cohort[-train_ind, ]), type = "response")

roc.true.final <- pROC::roc(response = na.omit(cohort[-train_ind,])$heart_disease , 
                            predictor = test.pred.final);roc.true.final
roc.list.final <- list() 
roc.list.final[[1]] <- roc.true.final

rmse.final <- c()
rmse.final[1] <- sqrt(mean((final.mod.true$residuals)^2))

aic.final <- c()
aic.final[1] <- final.mod.true$aic

coef.list.final <- list()
coef.list.final[[1]] <- final.mod.true$coefficients

# Train model using the training set where income is imputed
# Imputation method is chosen to be: "true","polyreg","polr","lda","cart",
# "pmm","rf","norm"
method.list <- c("true","polyreg","polr","lda","cart",
                 "pmm","rf","norm")

for (i in 2:length(method.list) ) {
  print(paste("Let's test",method.list[i],"imputation model~"))
  
  init <- mice(cohort[1:10,], maxit=0) 
  meth <- init$method
  predM <- init$predictorMatrix
  non.income.var <- (names(meth) != "income")
  meth[non.income.var] <- ""
  ## Impute data with method.list[i] method
  meth["income"] <- method.list[i]
  
  train.set.imp <- mice(cohort[train_ind, ], m=1, printFlag=TRUE, maxit = 10, 
                        seed=3000614,method = meth,predictorMatrix = predM)
  train.set.imp <- complete(train.set.imp)
  train.set.imp <- na.omit(train.set.imp)
  
  final.mod.imp <- glm(heart_disease ~ arthritis + age + sex + racial_origin + education + income + 
                       smoker + drinker + bp + diabetes + doctor + length_of_time,
                       weights = weight/mean(weight),
                       family = "binomial", data = train.set.imp)
  imp.pred.value <- predict(final.mod.imp, na.omit(cohort[-train_ind, ]), type = "response")
  
  # ROC
  roc.imp <- pROC::roc(response = na.omit(cohort[-train_ind,])$heart_disease , 
                       predictor = imp.pred.value);roc.imp
  roc.list.final[[i]] <- roc.imp
  
  # RMSE
  rmse.final[i] <- sqrt(mean((final.mod.imp$residuals)^2))
  # AIC
  aic.final[i] <- final.mod.imp$aic
  # Coefficients
  coef.list.final[[i]] <- final.mod.imp$coefficients
}

## Plot the ROC curves using ggplot2, since "polyreg"&"polr" have similar results, we plot "polr" only.
roc.data.final <- data.frame(specificities = c(1-roc.list.final[[1]]$specificities,
                                               1-roc.list.final[[3]]$specificities,
                                               1-roc.list.final[[4]]$specificities,
                                               1-roc.list.final[[5]]$specificities,
                                               1-roc.list.final[[6]]$specificities,
                                               1-roc.list.final[[7]]$specificities,
                                               1-roc.list.final[[8]]$specificities),
                             sensitivities = c(roc.list.final[[1]]$sensitivities,
                                               roc.list.final[[3]]$sensitivities,
                                               roc.list.final[[4]]$sensitivities,
                                               roc.list.final[[5]]$sensitivities,
                                               roc.list.final[[6]]$sensitivities,
                                               roc.list.final[[7]]$sensitivities,
                                               roc.list.final[[8]]$sensitivities),
                             type = c(rep("Original Data",length(roc.list.final[[1]]$sensitivities)),
                                      rep("Polyreg Imputation",length(roc.list.final[[3]]$sensitivities)),
                                      rep("LDA Imputation",length(roc.list.final[[4]]$sensitivities)),
                                      rep("CART Imputation",length(roc.list.final[[5]]$sensitivities)),
                                      rep("PMM Imputation",length(roc.list.final[[6]]$sensitivities)),
                                      rep("RF Imputation",length(roc.list.final[[7]]$sensitivities)),
                                      rep("Norm Imputation",length(roc.list.final[[8]]$sensitivities))
                             )
)
ggplot(roc.data.final, aes(x = specificities, y = sensitivities, color = type, linetype= type)) + 
  geom_line(size = 1,alpha = 0.5)+ xlab("False positive probability") + 
  ylab("True positive probability") + theme(legend.position="none") +
  annotate(geom="text", x=0.8, y=0.05, label=paste("Original data AUC =",round(roc.list.final[[1]]$auc,4) ),color="#F8766D",size = 5)+
  annotate(geom="text", x=0.8, y=0.1, label=paste("Polyreg Imputation AUC =",round(roc.list.final[[3]]$auc,4) ),color="#7CAE00",size = 5)+
  annotate(geom="text", x=0.8, y=0.15, label=paste("LDA Imputation AUC =",round(roc.list.final[[4]]$auc,4) ),color="#00BFC4",size = 5)+
  annotate(geom="text", x=0.8, y=0.2, label=paste("CART Imputation AUC =",round(roc.list.final[[5]]$auc,4) ),color="#DB72FB",size = 5)+
  annotate(geom="text", x=0.8, y=0.25, label=paste("PMM Imputation AUC =",round(roc.list.final[[6]]$auc,4) ),color="#31a354",size = 5)+
  annotate(geom="text", x=0.8, y=0.3, label=paste("RF Imputation AUC =",round(roc.list.final[[7]]$auc,4) ),color="#c51b8a",size = 5)+
  annotate(geom="text", x=0.8, y=0.35, label=paste("Norm Imputation AUC =",round(roc.list.final[[8]]$auc,4) ),color="#756bb1",size = 5)+
  geom_abline(intercept =0 , slope = 1,  linetype= 2, alpha = 0.5)

# Create the table outputting the coefficients, RMSE & AIC for each methods
stat.imp.final=data.frame(Original.tr=coef.list.final[[1]],
                          Polyreg=coef.list.final[[3]],
                          LDA=coef.list.final[[4]],
                          CART=coef.list.final[[5]],
                          PMM=coef.list.final[[6]],
                          RF=coef.list.final[[7]],
                          Norm=coef.list.final[[8]])
stat.imp.final=rbind(stat.imp.final,rmse.final,aic.final)
rownames(stat.imp.final)=c(rownames(stat.imp.final)[1:26],"RMSE","AIC")
all.dat=c(final.mod$coefficients,sqrt(mean((final.mod$residuals)^2)),final.mod$aic)
stat.imp.final=cbind(stat.imp.final,Original.all=all.dat)
write.csv(stat.imp.final,file="imp_tr.csv")



######################### final model (all data) #########################
rmse.final2 <- c()
rmse.final2[1] <- sqrt(mean((final.mod$residuals)^2))

aic.final2 <- c()
aic.final2[1] <- final.mod$aic

coef.list.final2 <- list()
coef.list.final2[[1]] <- final.mod$coefficients

# Train model using the training set where income is imputed
# Imputation method is chosen to be: "true","polyreg","polr","lda","cart",
# "pmm","rf","norm"
method.list <- c("true","polyreg","polr","lda","cart",
                 "pmm","rf","norm")

for (i in 2:length(method.list) ) {
  print(paste("Let's test",method.list[i],"imputation model~"))
  
  init <- mice(cohort[1:10,], maxit=0) 
  meth <- init$method
  predM <- init$predictorMatrix
  non.income.var <- (names(meth) != "income")
  meth[non.income.var] <- ""
  ## Impute data with method.list[i] method
  meth["income"] <- method.list[i]
  
  cohort.imp <- mice(cohort, m=1, printFlag=TRUE, maxit = 10, 
                     seed=3000614,method = meth,predictorMatrix = predM)
  cohort.imp <- complete(cohort.imp)
  cohort.imp <- na.omit(cohort.imp)
  
  final.mod.imp <- glm(heart_disease ~ arthritis + age + sex + racial_origin + education + income + 
                       smoker + drinker + bp + diabetes + doctor + length_of_time,
                       weights = weight/mean(weight),
                       family = "binomial", data = cohort.imp, na.action = na.omit)
  
  # RMSE
  rmse.final2[i] <- sqrt(mean((final.mod.imp$residuals)^2))
  # AIC
  aic.final2[i] <- final.mod.imp$aic
  # Coefficients
  coef.list.final2[[i]] <- final.mod.imp$coefficients
}

# Create the table outputting the coefficients, RMSE & AIC for each methods
stat.imp.final2=data.frame(Original=coef.list.final2[[1]],
                           Polyreg=coef.list.final2[[3]],
                           LDA=coef.list.final2[[4]],
                           CART=coef.list.final2[[5]],
                           PMM=coef.list.final2[[6]],
                           RF=coef.list.final2[[7]],
                           Norm=coef.list.final2[[8]])
stat.imp.final2=rbind(stat.imp.final2,rmse.final2,aic.final2)
rownames(stat.imp.final2)=c(rownames(stat.imp.final2)[1:26],"RMSE","AIC")
write.csv(stat.imp.final2,file="imp_all_dat.csv")



# ====================== Visualize missing data pattern ======================
# Visualize missing data pattern
library(VIM)
data <- data.frame(cohort$heart_disease,
                   cohort$arthritis,
                   cohort$age,
                   cohort$sex,
                   cohort$racial_origin,
                   cohort$education,
                   cohort$income,
                   cohort$smoker,
                   cohort$drinker,
                   cohort$bp,
                   cohort$diabetes,
                   cohort$doctor,
                   cohort$length_of_time,
                   cohort$weight)
ylab=c("heart","arthritis","age","sex","racial","education",
       "income","smoker","drinker","bp","diabetes","doctor","time","weight")
matrixplot(data,labels=ylab) # red - missing values

