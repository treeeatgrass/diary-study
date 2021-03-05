# #install.packages("dplyr")
# install.packages("readr")
# install.packages("tidyverse")
# install.packages("lme4")

library(tidyverse)
library(lme4)
library(foreign)

#load excel and SPSS data into R environment
level2 <- read.spss("level2data.sav", use.value.label=FALSE, to.data.frame=TRUE)
level1 <- read_csv("data1.csv")



##create variables for analyses
#compute a variable from existing columnes
level1 <- level1 %>% 
  mutate(POS5.2 = rowMeans(lx[,c('UnhappyEvent_interested','UnhappyEvent_excited','UnhappyEvent_proud')], na.rm=TRUE)) %>% 
  mutate(NEG5.2 = rowMeans(lx[,c('UnhappyEvent_sad','UnhappyEvent_ashamed','UnhappyEvent_irritable','UnhappyEvent_nervous',
                                 'UnhappyEvent_guilty','UnhappyEvent_scared','UnhappyEvent_afraid')], na.rm=TRUE)) %>% 
#center a variable around the grand mean
  mutate(Cage = age - mean(age)) %>%
  mutate(CsoAnxiety = soAnxiety - mean(soAnxiety)) %>% 
#recode a categorical variable into a new binary variable
  mutate(UnhappySocial = case_when(a5_1encode == 21 ~ 1L,
                               a5_1encode == 22 ~ 1L,
                               a5_1encode == 23 ~ 1L,
                               a5_1encode == 11 ~ 0L,
                               a5_1encode == 12 ~ 0L,
                               a5_1encode == 13 ~ 0L,
                               a5_1encode == 3 ~ 0L,
                               a5_1encode == 4 ~ 0L)) %>% 
  mutate(device5 = case_when(a5_3device == 11 ~ 1L,
                             a5_3device == 12 ~ 0L,
                             a5_3device == 21 ~ 0L)) %>% 
#recode a numeric variable to a new binary variable
  mutate(gen = recode(
    gender,
    `1` = 0L, `2` = 1L
  ))

#filter out responses with missing data on the independent variables
level1 <- level1 %>%
  filter(!is.na(POS5.2)) %>% 
  filter(!is.na(NEG5.2)) %>%
  filter(!is.na(UnhappySocial)) %>% 
  filter(!is.na(CsoAnxiety)) %>% 
  filter(!is.na(Cage)) %>%
  filter(!is.na(gen)) 

#select certain variables from level2 data to join level1 data.
df1 <-  level1 %>% 
  select(ID, POS5.2, NEG5.2, CsoAnxiety, UnhappySocial, device5, ha_social:ha_study)

df2 <- level2 %>% 
  select(ID, Hukou,Grade)

df <- df1 %>% 
  select(-ha_social, -ha_study) %>% 
  left_join(df2, by = "ID")
