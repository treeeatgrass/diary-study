library(tidyverse)

df <- df %>% 
  mutate(posbi = ifelse(Cpos <=0, 0L, 1L)) %>% 
  mutate(negbi = ifelse(Cneg <=0, 0L, 1L)) %>% 
  mutate(posb = recode(
    posbi,
    `1` = "present",`0` = "absent"
  )) %>% 
  mutate(negb = recode(
    negbi,
    `1` = "present",`0` = "absent"
  ))

ggplot(data = df)+
  geom_histogram(aes(x = Cpos))

pos <- ggplot(df, aes(x = CsoAnxiety, y = device5)) + 
  geom_point(color="red", position = position_jitter(height = .02)) +
  geom_smooth(mapping = aes(linetype = posb),
              method="glm", method.args=list(family="binomial"))+
  xlab("Social anxiety")+ ylab("Used a device when unhappy")
print(pos)

neg <- ggplot(df, aes(x = CsoAnxiety, y = device5)) + 
  geom_point(color="red", position = position_jitter(height = .02)) +
  geom_smooth(mapping = aes(linetype = negb),
              method="glm", method.args=list(family="binomial"))+
  xlab("Social anxiety")+ ylab("Used a device when unhappy")
print(neg)
