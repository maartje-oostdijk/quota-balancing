#clear workspace
rm(list = ls())
graphics.off()
#working directory
setwd("~/Dropbox/quota_matching")
#data directories
datadir = "~/Dropbox/quota_matching/data/"
datadir2 <- "~/Dropbox/quota_matching/balance"

#libaries
require(tidyverse)
require(sjstats)
require(glmmTMB)

#read data
all = read.csv(file.path(datadir,"vessel_matching_data.csv"), as.is=T)
targeting = read.csv("targeting/regression_results_31_01.csv")
bal <- read.csv(file.path(datadir2, "balance.csv"), as.is=T)#contains choke indicator
cost = read.csv(paste0(datadir, "cost.csv"))%>%#costs
  select(cost, period)


#choke indicator
bal = bal %>%
  filter(period > 2001)%>%left_join(cost)%>%
  mutate(lease_exvessel_corrected = lease_exvessel*(1/(1-cost))) %>%
  mutate(choke = as.factor(ifelse(lease_exvessel_corrected >= 1, 1, 0)))
#targeting indicator
targeting = targeting %>% select(period, Sp, M2) %>% 
  rename(coeff1 = M2)
targeting$coeff1[is.na(targeting$coeff1)] = 0

#select variables used
all = all %>% 
  select(Sp, period, owner_id, ship_id, total_CE_holder, CR, TAC, percTAC, permit, allocated, Compensations, transfers_prev_year, transfers_cur_year, transfers_vessels, allowed_catch, catch, ship_type, gross_ton, transformed, CE_transformed, mean) %>%
  rename(mean_lease_ind = mean)

all = all %>% left_join(targeting, by = c("Sp", "period"))%>%
  left_join(bal)

all = subset(all, period>2001 & Sp != "Cod")#subset from when we have most data
all = subset(all, ship_type!= "Only Lumpfish quota")

all$mean_lease_ind[all$Sp == "small_redfish" & all$period == 2016] = 17.33


# prof index
mean.lease = all %>% group_by(permit, period, Sp) %>% 
  filter(!duplicated(mean_lease_ind)) %>% select("permit", "period", "Sp", "mean_lease_ind")
#get unique CR per species per year
unique.CR = all %>% group_by(Sp, period) %>% summarise(CR = unique(CR))

#second part of ratio
# we could segregate the permits here, so calculate and average per permit per year (this is different from all the other calculations as it's per permit and others only per species)
average = mean.lease %>% left_join(unique.CR, by= c("period", "Sp")) %>% filter(!is.na(mean_lease_ind)) %>% mutate(ls.cr = mean_lease_ind/CR) %>%
  group_by(permit, period) %>% mutate(mean.ls.cr = mean(ls.cr)) %>% mutate(prof = ls.cr-mean.ls.cr) %>%
  select(permit, period, Sp, prof, ls.cr)

all = all %>% left_join(average, by = c("Sp", "period", "permit"))

all$coeff1[is.na(all$coeff1)] = 0

#give the lease value for small redfish the same value as in 2015 as there are no real non barter leases in 2016
averages = all %>% group_by(period) %>% # calculate averages
  filter(!is.na(mean_lease_ind))%>%
  summarise(average_lease = mean(mean_lease_ind), average_CR = mean(CR), average_TAC= mean(TAC), average_coeff1=mean(coeff1))

# multiple vessels
nr.vessel = all%>% group_by(owner_id, period) %>% summarise(nr.vessels = length(unique(ship_id)))
all = nr.vessel %>% left_join(all, by = c("owner_id", "period"))
all$multiple_vessel = ifelse(all$nr.vessels>1, "multiple vessels", "single vessel")

#summarise ship types
all$ship_type[all$ship_type =="Quota vessel"] = "Quota permit"
all$ship_type[all$ship_type =="Trawler"] = "Quota permit"
all$ship_type[all$ship_type =="Quota vessel"] = "Quota permit"
all$ship_type[all$ship_type =="Hook permit"] = "Hook and line boat"

all = subset(all, ship_type!= "Uncategorised" & ship_type!= "With refreshable rights")

data_change = all %>% ungroup %>% select(period, Sp, TAC)%>%
  distinct()%>%
  group_by(Sp) %>%
  mutate(perc_diff_TAC = TAC - lag(TAC))#change in TAC

all = all %>% 
  left_join(data_change)


all = all %>% 
  filter(!is.na(perc_diff_TAC) & !is.na(prof) & !is.na(choke))

#calculate catch quota 
all$catch_quota = all$allocated + all$Compensations + all$transfers_vessels
#adjust ship type
all$ship_type = ifelse(all$ship_type=="Small quota boat" | all$ship_type=="Hook and line boat", "small_boat", "quota_permit")
#rule changes
all$rule_change_2012 = ifelse(all$period < 2012, "before", "after")

#factors as factors
all$ship_id = as.factor(all$ship_id)#vessel identifier
all$multiple_vessel= as.factor(all$multiple_vessel)#multiple vessels?
all$period= as.factor(all$period)#fishing year
all$ship_type= as.factor(all$ship_type)#vessel type (small or large boat)
all$rule_change_2012 =as.factor(all$rule_change_2012)#management changes in fishing year 2011 2012
all$Sp = as.factor(all$Sp)#species factor

all$multiple_vessel = ifelse(all$multiple_vessel=="single vessel", "a single vessel", "multiple vessels")
all$rule_change_2012 = ifelse(all$rule_change_2012=="before", "1. before", "after")


#scale all numeric variables for model
all$total_CE_holder = scale(all$total_CE_holder)
all$coeff1= scale(all$coeff1)#targeting
all$ls.cr = scale(all$ls.cr)#arbitrage indicator
all$TAC = scale(all$TAC)#total allowable catch
all$perc_diff_TAC = scale(all$perc_diff_TAC)#perc diff TAC (not used)
all$gross_ton = as.numeric(all$gross_ton)#gross tonnage
all$gross_ton = scale(all$gross_ton)#gross tonnage
all$catch_quota = all$catch_quota + 0.1#to correct small values Gamma distribution
all$catch = all$catch + 0.1

#subset positive values
all = subset(all, catch >0 & catch_quota > 0)


#model all species
m= glmmTMB( catch ~  + ar1(period + 0 | ship_id) + (1|period) +(1|Sp)+ choke +ship_type + rule_change_2012+ TAC+ ls.cr*coeff1+
              + gross_ton + total_CE_holder + multiple_vessel, family=Gamma(link="log"),  data = all, offset=log(catch_quota))



#simulate residuals
res_1 = simulateResiduals(m_null)
#test residuals
testResiduals(res_1) #
plot(res_1)


sum = data.frame(summary(m)$coefficients$cond)


confidence= data.frame(predictors = rownames(sum[c(2:dim(sum)[1]),]), estimate = confint(m)[c(2:dim(sum)[1]), 3], upper = confint(m)[c(2:dim(sum)[1]), 2], lower = confint(m)[c(2:dim(sum)[1]), 1],  probability= sum$Pr...z..[c(2:dim(sum)[1])])

rownames(confidence) <- c("choke indicator", "permit type (small boat)", "rule change (after)", "TAC", "arbitrage indicator", "targeting indicator",  "gross tonnage", "demersal quota holdings", "multiple vessels", "targeting indicator : arbitrage potential")

confidence$probability[is.na(confidence$probability)] = 0
confidence$predictors = c("choke indicator", "permit type (small boat)", "rule change (after)", "TAC", "arbitrage indicator", "targeting indicator",  "gross tonnage", "demersal quota holdings", "multiple vessels", "targeting indicator : arbitrage potential")
confidence$effect = ifelse(confidence$estimate <0  & confidence$probability <0.05 , "negative", "non-significant")
confidence$effect = ifelse(confidence$estimate >0  & confidence$probability <0.05, "positive", confidence$effect)
confidence$effect = ifelse(confidence$lower == 0 & confidence$estimate ==0 &confidence$upper ==0, "", confidence$effect)

confidence$real_order = c(3,6, 10, 4, 2, 5, 7, 8, 9, 11)

confidence = confidence%>% arrange(real_order)%>%
  mutate(species = "multi-species-model")%>%
  rename(predictor=predictors)


#plot confidence intervals
ggplot(confidence, aes( x= rev(real_order), y = estimate, ymax = upper, ymin = lower, colour= effect)) +
  geom_pointrange(position=position_dodge(width=c(0.3)))+
  theme_bw() + scale_x_continuous(breaks = rev(confidence$real_order), labels = confidence$predictor) + coord_flip() + geom_hline(yintercept=0, linetype="dashed") +
  scale_color_manual(values=c("blue","grey", "red")) + xlab("predictor")
#save confidence intervals
write.csv(confidence, "~/Dropbox/quota_matching/data/confidence_interval_single_model.csv")









