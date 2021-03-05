library(tidyverse)

#check the class and values of all variables
class(lx$POS5.2)
lx$POS5.2
class(lx$NEG5.2)
lx$NEG5.2
class(lx$CsoAnxiety)
lx$CsoAnxiety
class(lx$Cage)
lx$Cage
class(lx$gen)
lx$gen
class(lx$UnhappySocial)
lx$UnhappySocial
class(lx$device5)
lx$device5

##descriptive statistical analyses

#calculate associations between two categorical variables
chisq.test(lx$gen,lx$UnhappySocial)
chisq.test(lx$gen,lx$device5)
chisq.test(lx$UnhappySocial,lx$device5)

#calculate associations between one binary variable and one continuous variable
group1 <- 
  lx %>% 
  filter(gen == 1) %>% 
  select(CsoAnxiety) %>% 
  pull()

group2 <- 
  lx %>% 
  filter(gen == 0) %>% 
  select(CsoAnxiety) %>% 
  pull()

fit1 <- t.test(group1, group2)
fit1

#calculate association between two continuous variables.
cor1 <- cor.test(lx$age, lx$soAnxiety, method = "pearson")
cor1

#calculate the mean and SD of a continuous variable for each group of a categorical variable
group_by(lx, gen) %>% 
  summarise(
    count = n(),
    `mean(CsoAnxiety)` = sprintf("%0.2f", mean(CsoAnxiety)),
    sd = sd(age, na.rm = TRUE)
  )

