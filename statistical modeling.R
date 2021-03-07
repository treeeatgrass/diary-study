library(lme4)

##logistic HLM modeling
#calculate ICC using the null model without any variables.
m0 <- glmer(device5 ~ (1|ID), 
              data = df, family = binomial(link = "logit"),
              control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 0)
#Step 1: add level1 variables with covariate
m1 <- glmer(device5 ~ UnhappySocial + Cpos + Cneg +(1|ID), 
              data = df, family = binomial(link = "logit"),
              control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 0)
#Step 2: add level2 variables to test main effects
m2 <- glmer(device5 ~ UnhappySocial + Cpos + Cneg + CsoAnxiety + (1|ID),
              data = df, family = binomial(link = "logit"),
              control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 0)
#Step 3: add cross-level interaction terms
m3 <- glmer(device5 ~ UnhappySocial + 
                Cpos + Cneg + CsoAnxiety +
                Cpos*CsoAnxiety + Cneg*CsoAnxiety + (1|ID),
              data = df, family = binomial(link = "logit"),
              control = glmerControl(optimizer = "bobyqa"),
              nAGQ = 0)  

#comparing models to detect the simplest model with best data-model fit
anova(m0, m1)
anova(m1, m2)
anova(m2, m3)
summary(m1)
