library(dplyr)
library(readr)
library(ggplot2)
library(betareg)
library(broom)

#read in the data and convert some string columns to factors
catchments <- read_csv('catchments.csv') %>%
  mutate(spp = as.factor(spp),condition = as.factor(condition))

#have a look
head(catchments)

#plot
ggplot(catchments,aes(x=age,y=flow_reduction,colour=spp:condition)) +
  geom_point()

#fit a beta regression
catch_reg <- betareg(flow_reduction ~ age  + age*spp, data = catchments)

#view results
tidy(catch_reg)

#get table  of obs and fitted values
catch_fit <- augment(catch_reg)

#plot fitted models
ggplot(catch_fit,aes(x=age,colour=spp)) +
  geom_line(aes(y=.fitted)) +
  geom_point(aes(y=flow_reduction)) +
  ylab('flow reduction') +
  theme_bw()
