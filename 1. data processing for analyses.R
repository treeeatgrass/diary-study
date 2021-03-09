library(here)
library(tidyverse)
library(lme4)

here()
#load excel data into R environment
here("level1data.csv")
here("level2data.csv")

level1 <- read.csv(here("level1data.csv"))
level2 <- read.csv(here("level2data.csv"))

##create variables for analyses
#level1 data processing: compute a variable from existing columns
level1 <- level1 %>% 
  mutate(POS5.2 = rowMeans(level1[,c('UnhappyEvent_interested','UnhappyEvent_excited','UnhappyEvent_proud')], na.rm=TRUE)) %>% 
  mutate(NEG5.2 = rowMeans(level1[,c('UnhappyEvent_sad','UnhappyEvent_ashamed','UnhappyEvent_irritable','UnhappyEvent_nervous',
                                 'UnhappyEvent_guilty','UnhappyEvent_scared','UnhappyEvent_afraid')], na.rm=TRUE)) %>% 
  mutate(POS6.2 = rowMeans(level1[,c('HappyEvent_interested','HappyEvent_excited','HappyEvent_proud')], na.rm=TRUE)) %>% 
  mutate(NEG6.2 = rowMeans(level1[,c('HappyEvent_sad','HappyEvent_ashamed','HappyEvent_irritable','HappyEvent_nervous',
                                 'HappyEvent_guilty','HappyEvent_scared','HappyEvent_afraid')], na.rm=TRUE)) %>% 
#recode a categorical variable into a new binary variable
  mutate(UnhappySocial = case_when(UnhappyEvent == 21 ~ 1L,
                                   UnhappyEvent == 22 ~ 1L,
                                   UnhappyEvent == 23 ~ 1L,
                                   UnhappyEvent == 11 ~ 0L,
                                   UnhappyEvent == 12 ~ 0L,
                                   UnhappyEvent == 13 ~ 0L,
                                   UnhappyEvent == 3 ~ 0L,
                                   UnhappyEvent == 4 ~ 0L)) %>% 
  mutate(device5 = case_when(UnhappyEventRegulation_phone == 11 ~ 1L,
                             UnhappyEventRegulation_phone == 12 ~ 0L,
                             UnhappyEventRegulation_phone == 21 ~ 0L)) %>% 
  mutate(HappySocial = case_when(HappyEvent == 21 ~ 1L,
                                   HappyEvent == 22 ~ 1L,
                                   HappyEvent == 23 ~ 1L,
                                   HappyEvent == 11 ~ 0L,
                                   HappyEvent == 12 ~ 0L,
                                   HappyEvent == 13 ~ 0L,
                                   HappyEvent == 3 ~ 0L,
                                   HappyEvent == 4 ~ 0L)) %>% 
  mutate(device6 = case_when(HappyEventRegulation_phone == 11 ~ 1L,
                             HappyEventRegulation_phone == 12 ~ 0L,
                             HappyEventRegulation_phone == 21 ~ 0L))


#Level2 data processing: compute a variable from existing columns
level2 <- level2 %>% 
  mutate(online = rowMeans(level2[,c('OnlineSocialSupport1','OnlineSocialSupport2','OnlineSocialSupport3','OnlineSocialSupport4',
                                 'OnlineSocialSupport5','OnlineSocialSupport6','OnlineSocialSupport7','OnlineSocialSupport8',
                                 'OnlineSocialSupport9','OnlineSocialSupport10','OnlineSocialSupport11')], na.rm=TRUE)) %>% 
  mutate(offline = rowMeans(level2[,c('OfflineSocialSupport1','OfflineSocialSupport2','OfflineSocialSupport3',
                                      'OfflineSocialSupport4','OfflineSocialSupport5','OfflineSocialSupport6',
                                      'OfflineSocialSupport7','OfflineSocialSupport8','OfflineSocialSupport9',
                                      'OfflineSocialSupport10','OfflineSocialSupport11')], na.rm=TRUE)) %>% 
  mutate(soAnxiety = rowMeans(level2[,c('SocialAnxiety1','SocialAnxiety2','SocialAnxiety3','SocialAnxiety4',
                                        'SocialAnxiety5','SocialAnxiety6','SocialAnxiety7','SocialAnxiety8',
                                        'SocialAnxiety9','SocialAnxiety10')], na.rm = TRUE)) %>% 
  mutate(soEfficacy = rowMeans(level2[,c('SocialSelfEfficacy1','SocialSelfEfficacy2','SocialSelfEfficacy3','SocialSelfEfficacy4',
                                        'SocialSelfEfficacy5','SocialSelfEfficacy6')], na.rm = TRUE)) %>% 
  mutate(addiction = rowMeans(level2[,c('Addiction1','Addiction2','Addiction3','Addiction4','Addiction5',
                                       'Addiction6','Addiction7','Addiction8','Addiction9','Addiction10')], na.rm = TRUE)) %>% 
  mutate(paInvolve = rowMeans(level2[,c('ParentalSchoolInvolvement1','ParentalSchoolInvolvement2','ParentalSchoolInvolvement3',
                                        'ParentalSchoolInvolvement4','ParentalSchoolInvolvement5','ParentalSchoolInvolvement6',
                                        'ParentalSchoolInvolvement7','ParentalSchoolInvolvement8','ParentalSchoolInvolvement9',
                                        'ParentalSchoolInvolvement10')], na.rm = TRUE)) %>% 
  mutate(selfControl = rowMeans(level2[,c('SelfControl1','SelfControl2','SelfControl3','SelfControl4','SelfControl5',
                                          'SelfControl6')], na.rm = TRUE)) %>% 
  mutate(emoEfficacy = rowMeans(level2[,c('EmotionRegulationEfficacy1','EmotionRegulationEfficacy2','EmotionRegulationEfficacy3',
                                          'EmotionRegulationEfficacy4','EmotionRegulationEfficacy5','EmotionRegulationEfficacy6',
                                          'EmotionRegulationEfficacy7','EmotionRegulationEfficacy8','EmotionRegulationEfficacy9',
                                          'EmotionRegulationEfficacy10','EmotionRegulationEfficacy11','EmotionRegulationEfficacy12')], na.rm = TRUE)) %>% 
  #recode a numeric variable to a new binary variable
  mutate(gender = recode(
    Gender,
    `1` = 0L, `2` = 1L
  ))
