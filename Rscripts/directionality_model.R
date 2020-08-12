#profit driver for recommended catch too high?

#clear workspace
rm(list = ls())
graphics.off()

#data and working directory
setwd("~/Dropbox/quota_matching")
datadir <- "~/Dropbox/quota_matching/balance"
datadir2 = "~/Dropbox/quota_matching/data/"

#libaries
library(tidyverse)
library(sandwich)
library(lmtest)

#read data
data = read.csv("QuotaCatchAnalyses2018.csv")
targeting = read.csv("targeting/regression_results_31_01.csv")
targeting = targeting %>% select(period, Sp, M2) %>% rename(coeff1 = M2)

c <- read.csv(file.path(datadir, "balance.csv"), as.is=T)
c = c %>%
  filter(period > 2001)

cev = read.csv(paste0(datadir2, "lease_ex_vessel.csv"))
cost = read.csv(paste0(datadir2, "cost.csv"))%>%
  select(cost, period)

cev = cev%>%
  rename(period = year)

#choke indicator
c = c %>%left_join(cost)%>%
  mutate(lease_exvessel_corrected = lease_exvessel*(1/(1-cost))) %>%
  mutate(choke = ifelse(lease_exvessel_corrected >= 1, 1, 0))

#arbitrage indicator
prof= cev %>%
  filter(type == "lease_cv")%>%
  rename(prof = value)%>%
  select(-type)



data$period = data$Year

tac = data %>%
  select(Sp, period, Allocated_plus_compensated)

tac$time.period = ifelse(tac$period < 2012, "before", "post2012")

#combine different data
data = tac %>% 
  left_join(c)%>%
  left_join(prof)%>%
  left_join(targeting)



#scale values
data$prof = scale(data$prof)
data$Allocated_plus_compensated = scale(data$Allocated_plus_compensated)
data$coeff1 = scale(data$coeff1)
data$lease_exvessel_corrected = scale(data$lease_exvessel_corrected)

balance = data %>% filter(!is.na(balance)) %>%
  filter(Sp != "Cod")


#corect parameter
balance$balance3 = balance$balance/2 + 0.5 
#logit model
m = glm(balance$balance3 ~   balance$choke + as.factor(balance$period) +balance$Allocated_plus_compensated + balance$coeff1*balance$prof , family=binomial(link=logit))


#model coefficients
coeftest(m, vcov = NeweyWest(m, lag = 1))

