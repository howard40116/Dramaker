
####ÄÝ©ÊÅu¥­(¼ÆÁØ«õ±¸P41)

library(tidyr)
domatrix3 = read.csv(paste0("C:/Users/Student/Desktop/moneyball.CSV"))
domatrix3 %>% spread(keyword, Freq)