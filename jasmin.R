library(tidyverse)

dat <- read.csv("sydneyRF.csv")

str(dat)
unique(dat$total.closed)
dat[dat$total.closed=="total closed",]

dat <- dat %>% filter(!(total.closed ==0 & total.open ==0))
databoth <- dat %>% filter(!(total.closed ==0 | total.open ==0))

databoth <- databoth %>% mutate(diff = total.open-total.closed)

#plot of data where rows have both open and closed data
databoth %>% group_by(genSpec, sp) %>% summarize(mndiff = mean(diff)) %>% 
  mutate(trt = paste(genSpec, sp)) %>%
  ggplot(aes(y=mndiff, color=trt)) + geom_boxplot()

 
#plot of data where rows have EITHER open and closed as well as 
#both open and closed data  
dat %>% group_by(genSpec, sp) %>% 
  summarize(mndiff= mean(total.open) - mean(total.closed))%>%   
  mutate(trt = paste(genSpec, sp)) %>%
  ggplot(aes(y=mndiff, color=trt)) + geom_boxplot()
