library(tidyverse)

#center continuous variables
level1 <- level1 %>% 
  mutate(Cpos = POS5.2 - mean(POS5.2)) %>% 
  mutate(Cneg = NEG5.2 - mean(NEG5.2))  

level2 <- level2 %>% 
  mutate(CsoAnxiety = soAnxiety - mean(soAnxiety)) %>% 
  mutate(Cage = Age - mean(Age))  
  
#select certain variables from level2 data to join level1 data.
df1 <-  level1 %>% 
  select(ID, Cpos, Cneg, UnhappySocial, device5, HappySocial:device6)

df2 <- level2 %>% 
  select(ID, CsoAnxiety, Cage, gender, Hukou,Grade)

df <- df1 %>% 
  select(-HappySocial, -device6) %>% 
  left_join(df2, by = "ID")


  
#filter out responses with missing data on the independent variables
 df <- df %>%
  filter(!is.na(Cpos)) %>% 
  filter(!is.na(Cneg)) %>%
  filter(!is.na(UnhappySocial)) %>% 
  filter(!is.na(CsoAnxiety)) %>% 
  filter(!is.na(Cage)) %>%
  filter(!is.na(gender)) 

#check the class and values of all variables
class(df$Cpos)
df$Cpos
class(df$Cneg)
df$Cneg
class(df$CsoAnxiety)
df$CsoAnxiety
class(df$Cage)
df$Cage
class(df$gender)
df$gender
class(df$UnhappySocial)
df$UnhappySocial
class(df$device5)
df$device5

##descriptive statistical analyses
#calculate associations between two categorical variables
chisq.test(df$gender,df$UnhappySocial)
chisq.test(df$gender,df$device5)
chisq.test(df$UnhappySocial,df$device5)

#calculate associations between one binary variable and one continuous variable
group1 <- 
  df %>% 
  filter(gender == 1) %>% 
  select(CsoAnxiety) %>% 
  pull()

group2 <- 
  df %>% 
  filter(gender == 0) %>% 
  select(CsoAnxiety) %>% 
  pull()

fit1 <- t.test(group1, group2)
fit1

#calculate association between two continuous variables.
cor1 <- cor.test(df$Cage, df$CsoAnxiety, method = "pearson")
cor1

#calculate the mean and SD of a continuous variable for each group of a categorical variable
group_by(df, gender) %>% 
  summarise(
    count = n(),
    `mean(CsoAnxiety)` = sprintf("%0.2f", mean(CsoAnxiety)),
    sd = sd(Cage, na.rm = TRUE)
  )
