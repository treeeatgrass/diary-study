setwd("C:/Users/DELL/Documents/research/data/2019data")

install.packages ("devtools")
install.packages ("careless")

library(devtools)
library('careless')

data <- read.csv("C:/Users/DELL/Documents/research/data/2019data/beforels.csv")
careless_ls <- longstring(data)
x3 <- data.frame(data,careless_ls)
write.csv(x3,file = 'twoyear_ls.csv',row.names = FALSE)
