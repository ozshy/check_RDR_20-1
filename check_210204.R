# check_210204.R, shifitng to new id
# check_200305.R For NFTV blog adding use of cash by age (end of file)
# check_190917.R adding trans var "check_dep_src" (source of checking accout deposit)
# check_190823.R adding income variables from newly merged dataset. Removing restriction to type==expenditure
# check_190822.R adding new sections on income (types), Line 870
# check_190808.R sent to Claire as new version
# check_190721b.R corresponds to check_190721 sent to Claire
# check_190718.R adding weight_1 (simple, used) and weight_2
#
# The weights are based on the 2018 Current Population Survey. As always, I recommend doing analysis using a variety of weights, including unweighted, to see how much the results depend on the weighting scheme. The first weights, "weight_1", are simpler: they match on gender, income, education, and age and do not incorporate Marco's base weights, which correspond to likelihood of sampling based on geography. The second weights, "weight_2" do incorporate the base weights and also race and size of the households. For the latter, I artificially forced weights less than 0.25 up to 0.25 so that the spread of the weights is not too large
#
# check_190628.R Rearranging R to conform to Claire's check_draft_190627.docx, which I will revise as check_draft_190628_Oz.docx. 
# check_190627.R restricting analysis to payment==1 & type==1
# Note: check_190627.R corresponds to check_additional_tables_20190627.docx  before switching to claire's version also from 190627. Save this one!
#
# Set working directory below (Note: Yours may differ)
setwd("~/Papers/Papers_non-academic/check/check_coding")
dir()
library(dplyr)
library(tidyr)# function "spread()"
library(formattable)# formats tables to allow trailing zeros (w/ numeric format)
#library(plotrix)# weighted histograms
library(lubridate) # extracts day of the week
library(spatstat) # for weighted.median
#library(ggplot2); theme_set(theme_bw())
# Read data
d1 = readRDS("check_merged_2017_2018_210204.rds")
dim(d1)# num transactions
length(unique(d1$id))# num of resp
names(d1)
# 
table(d1$payment) # 0 = transactions with missing pi
table(d1$type) # 1 = expenditure, 2=transfer3=income
#Restricting the data to:
# d2 = subset(d1, payment == 1 & type == "expenditure") # use all types b/c some type==transfers contain some check bill payments (we guess)
d2 = subset(d1, payment == 1)# for income and deposit analysis use d1 not d2 
nrow(d2) # num trans in the combined 2017-18 sample
length(unique(d2$id)) # num resp
nrow(d1) -  nrow(d2) # num trans lost by this restriction
length(unique(d1$id)) - length(unique(d2$id)) # num resp lost
#
d2$id = as.factor(d2$id)
str(d2$id)
str(d2$weight_1) # simplified weights: less variance => use this one
str(d2$weight_2) # complex weights: higher variance, combined with Marco's
#
# Rescaling weight_1 and weight_2 with new variables: w1 and w2
dim(d2)
nrow(d2)
sum(d2$weight_1)
sum(d2$weight_2)
d2$w1 = d2$weight_1
d2$w2 = d2$weight_2
d2$w1 = nrow(d2)*d2$w1/sum(d2$w1) # rescaling: num tras = sum of weights
d2$w2 = nrow(d2)*d2$w2/sum(d2$w2)
sum(d2$w1)
sum(d2$w2)
head(d2$w1) #comparing scaled to before scaling below
head(d2$weight_1)
head(d2$w2)
head(d2$weight_2)

### Table 1 in paper: Summary of data source (Instroduction)
(num_trans_17 = nrow(filter(d2, year==2017)))# number of 10/17 trans
(num_resp_17 = length(unique(filter(d2, year==2017)$id)))# number of 10/17 resp
(num_trans_18 = nrow(filter(d2, year==2018)))# number of 10/18 trans
(num_resp_18 = length(unique(filter(d2, year==2018)$id)))# number of 10/18 resp
(num_trans_17_18 = nrow(d2))
(num_resp_17_18 = length(unique(d2$id)))
#
# Looking at check trans
(check_total_17 = nrow(subset(d2, year==2017 & pi==2)))# total 2017 checks written
(check_frac_17 = check_total_17/num_trans_17)
(check_total_18 = nrow(subset(d2, year==2018 & pi==2)))# total 2018 checks written
(check_frac_18 = check_total_18/num_trans_18)
# check trans 2017 2018 combined
(check_total_17_18 = nrow(subset(d2, pi==2)))# total checks 2017-18
(check_frac_17_18 = check_total_17_18/num_trans_17_18)
#
# making table of data summary (Table 1)
(data_var.vec = c("Number_of_unique_respondents", "Number_of_total_payments", "Number_of_check_payments", "Percentage_of_check_payments (%)"))
(data_17.vec = c(num_resp_17, num_trans_17, check_total_17, 100*check_frac_17))
(data_18.vec = c(num_resp_18, num_trans_18, check_total_18, 100*check_frac_18))
(data_17_18.vec = c(num_resp_17_18, num_trans_17_18, check_total_17_18, 100*check_frac_17_18))

(data_summary.df = data.frame("Variable" = data_var.vec, "Sample_2017" = data_17.vec, "Sample_2018" = data_18.vec , "Merged_2017_18" = data_17_18.vec  ))
write.table(format(data_summary.df, digits=3), file = "data_summary.txt", sep = ",", quote = F, row.names = F)
dir()# end of first table (all checks, bills and non-bills)

# Information provided below Table 1 (data summary)
# Indexing resp according to diary year
resp_17 = unique(d2[d2$year=="2017",]$id)
length(resp_17)#  num resp in 2017
resp_18 = unique(d2[d2$year=="2018",]$id)
length(resp_18)#  num resp in 2017
resp_17_18 = intersect(resp_17, resp_18)
length(resp_17_18)# num resp who participated in both 2017 & 2018
resp_17_only = setdiff(resp_17, resp_17_18)
length(resp_17_only)# num resp participated only in 2017
resp_18_only = setdiff(resp_18, resp_17_18)
length(resp_18_only)# num resp participated only in 2018

### Start building Table 2: Check payments merged 2017/18 started on Line = 100 ends Line 475
# Computing monthly personal check volume per respondent 
(check_var.vec = c("Monthly_volume_per_respondent_(w)", "Monthly_value_per_respondent_(w)", "Average_check_value_(w)", "Median_check_value_(w)", "Minimum_check_value", "Maximum_check_value", "Number_of_check_observations", "Percetage_of_observations_(w)_(%)"))
(check_total_17 = nrow(subset(d2, year==2017 & pi==2)))# total 2017 checks written
(check_per_resp_17 = check_total_17/length(resp_17))# num checks per resp 2017
(check_monthly_per_resp_17 = 31*check_per_resp_17/3)
# redo above weighted
check_by_resp_17_w = subset(d2, year==2017 & pi==2) %>% group_by(id) %>% summarize("check_total_17_w" = sum(w1)) # sum of weights of resp who wrote checks
(check_total_17_w = sum(check_by_resp_17_w$check_total_17_w))
(check_per_resp_17_w = sum(check_total_17_w)/length(resp_17))# num checks per resp 2017 weighted
(check_monthly_per_resp_17_w = 31*check_per_resp_17_w/3) # weighted
#
(check_total_18 = nrow(subset(d2, year==2018 & pi==2)))# total 2018 checks written
(check_per_resp_18 = check_total_18/length(resp_18))# num checks per resp 2018
(check_monthly_per_resp_18 = 31*check_per_resp_18/3)
# redo above weighted
check_by_resp_18_w = subset(d2, year==2018 & pi==2) %>% group_by(id) %>% summarize("check_total_18_w" = sum(w1)) # sum of weights of resp who wrote checks
(check_total_18_w = sum(check_by_resp_18_w$check_total_18_w))
(check_per_resp_18_w = sum(check_total_18_w)/length(resp_18))# num checks per resp 2018 weighted
(check_monthly_per_resp_18_w = 31*check_per_resp_18_w/3) # weighted
#
# monthly check per resp 2017 2018 combined
(check_total_17_18 = nrow(subset(d2, pi==2)))# total 2017-18
(check_monthly_per_resp_17_18 = (check_monthly_per_resp_17*length(resp_17) + check_monthly_per_resp_18*length(resp_18))/(length(resp_17)+length(resp_18)))
# redo above weighted
(check_total_17_18_w = sum(subset(d2, pi==2)$w1))# used below for bills etc.
(check_monthly_per_resp_17_18_w = (check_monthly_per_resp_17_w*length(resp_17) + check_monthly_per_resp_18_w*length(resp_18))/(length(resp_17)+length(resp_18)))

## Computing monthly personal check VALUE per respondent 
(check_total_val_17 = sum(subset(d2, year==2017 & pi==2)$amnt, na.rm = T))# total 2017 value checks written
(check_val_per_resp_17 = check_total_val_17/length(resp_17))# val per resp
(check_monthly_val_per_resp_17 = 31*check_val_per_resp_17/3)
# redo above weighted
(check_total_val_17_w = sum(subset(d2, year==2017 & pi==2)$amnt * subset(d2, year==2017 & pi==2)$w1, na.rm = T))
(check_val_per_resp_17_w = check_total_val_17_w/length(resp_17))# val per resp
(check_monthly_val_per_resp_17_w = 31*check_val_per_resp_17_w/3)
#
(check_total_val_18 = sum(subset(d2, year==2018 & pi==2)$amnt, na.rm = T))# total 2018 value checks written
(check_val_per_resp_18 = check_total_val_18/length(resp_18))# val per resp
(check_monthly_val_per_resp_18 = 31*check_val_per_resp_18/3)
# redo above weighted
(check_total_val_18_w = sum(subset(d2, year==2018 & pi==2)$amnt * subset(d2, year==2018 & pi==2)$w1, na.rm = T))
(check_val_per_resp_18_w = check_total_val_18_w/length(resp_18))# val per resp
(check_monthly_val_per_resp_18_w = 31*check_val_per_resp_18_w/3)
#
(check_total_val_17_18 = sum(subset(d2, pi==2)$amnt, na.rm = T))# total 2017-18 value checks written
(check_val_per_resp_17_18 = (check_total_val_17 + check_total_val_18)/(length(resp_17)+length(resp_18)))# val per resp
(check_monthly_val_per_resp_17_18 = 31*check_val_per_resp_17_18/3)
# redo above weighted
(check_val_per_resp_17_18_w = (check_total_val_17_w + check_total_val_18_w)/(length(resp_17)+length(resp_18)))# val per resp weighted
(check_monthly_val_per_resp_17_18_w = 31*check_val_per_resp_17_18_w/3)

## Avg, medians, etc. Note: 2017 and 2018 are not reported separately in Table 2
# Avg check value 2017
(check_val_avg_17 = mean(subset(d2, year==2017 & pi==2)$amnt, na.rm = T))# 
# Median check value 2017
(check_val_med_17 = median(subset(d2, year==2017 & pi==2)$amnt, na.rm = T))# 
# Min check value 2017
(check_val_min_17 = min(subset(d2, year==2017 & pi==2)$amnt, na.rm = T))# 
# Max check value 2017
(check_val_max_17 = max(subset(d2, year==2017 & pi==2)$amnt, na.rm = T))# 
# Avg check value 2018
(check_val_avg_18 = mean(subset(d2, year==2018 & pi==2)$amnt, na.rm = T))# 
# Median check value 2018
(check_val_med_18 = median(subset(d2, year==2018 & pi==2)$amnt, na.rm = T))# 
# Min check value 2018
(check_val_min_18 = min(subset(d2, year==2018 & pi==2)$amnt, na.rm = T))# 
# Max check value 2018
(check_val_max_18 = max(subset(d2, year==2018 & pi==2)$amnt, na.rm = T))# 
#
# Avg check value 2017-2018
(check_val_avg_17_18 = mean(subset(d2, pi==2)$amnt, na.rm = T))# 
# Avg check value 2017-2018 Reported on Table 2 (weighted)
(check_val_avg_17_18_w = weighted.mean(subset(d2, pi==2)$amnt, subset(d2, pi==2)$w1, na.rm = T))# 
# Median check value 2017-2018
(check_val_med_17_18 = median(subset(d2, pi==2)$amnt, na.rm = T))# 
# Median check value 2017-2018 (w)
(check_val_med_17_18_w = weighted.median(subset(d2, pi==2)$amnt, subset(d2, pi==2)$w1, na.rm = T))# 
# Min check value 2017-2018
(check_val_min_17_18 = min(subset(d2, pi==2)$amnt, na.rm = T))# 
# Max check value 2017-2018
(check_val_max_17_18 = max(subset(d2, pi==2)$amnt, na.rm = T))# 
#
# Building All checks column as a vector
check_var.vec # lists the order of variables
length(check_var.vec) # num of variables in Table 2
(all_checks.vec = c(check_monthly_per_resp_17_18_w, check_monthly_val_per_resp_17_18_w, check_val_avg_17_18_w, check_val_med_17_18_w, check_val_min_17_18, check_val_max_17_18, check_total_17_18_w, 100))
length(all_checks.vec)
#
## start bill payments (2nd column in Table 2)
(check_total_17_bill = nrow(subset(d2, year==2017 & pi==2 & bill==1)))# total 2017 checks written
(check_per_resp_17_bill = check_total_17_bill/length(resp_17))# num checks per resp 2017
(check_monthly_per_resp_17_bill = 31*check_per_resp_17_bill/3)
# redo above weighted
check_by_resp_17_bill_w = subset(d2, year==2017 & pi==2 & bill==1) %>% group_by(id) %>% summarize("check_total_17_bill_w" = sum(w1)) # sum of weights of resp who wrote checks (for some unknown reason "check_total_17_bill_w" has to re-summed)
head(check_by_resp_17_bill_w$check_total_17_bill_w)
(check_total_17_bill_w = sum(check_by_resp_17_bill_w$check_total_17_bill_w))
(check_per_resp_17_bill_w = sum(check_total_17_bill_w)/length(resp_17))# num checks per resp 2017 weighted
(check_monthly_per_resp_17_bill_w = 31*check_per_resp_17_bill_w/3) # weighted
#
(check_total_18_bill = nrow(subset(d2, year==2018 & pi==2 & bill==1)))# total 2018 checks written
(check_per_resp_18_bill = check_total_18_bill/length(resp_18))# num checks per resp 2018
(check_monthly_per_resp_18_bill = 31*check_per_resp_18_bill/3)
# redo above weighted
check_by_resp_18_bill_w = subset(d2, year==2018 & pi==2 & bill==1) %>% group_by(id) %>% summarize("check_total_18_bill_w" = sum(w1)) # sum of weights of resp who wrote checks (for some unknown reason "check_total_18_bill_w" has to re-summed)
head(check_by_resp_18_bill_w$check_total_18_bill_w)
(check_total_18_bill_w = sum(check_by_resp_18_bill_w$check_total_18_bill_w))
(check_per_resp_18_bill_w = sum(check_total_18_bill_w)/length(resp_18))# num checks per resp 2018 weighted
(check_monthly_per_resp_18_bill_w = 31*check_per_resp_18_bill_w/3) # weighted
#
# monthly bill check per resp 2017 2018 combined
(check_total_17_18_bill = nrow(subset(d2, pi==2 & bill==1)))# total 2017-18
(check_frac_17_18_bill = check_total_17_18_bill/check_total_17_18)# note: frac of bills out of all checks
(check_monthly_per_res_17_18_bill = (check_monthly_per_resp_17_bill*length(resp_17) + check_monthly_per_resp_18_bill*length(resp_18))/(length(resp_17)+length(resp_18)))
# redo the above weighted
(check_total_17_18_bill_w = sum(subset(d2, pi==2 & bill==1)$w1))# total 2017-18 (w)
(check_frac_17_18_bill_w = check_total_17_18_bill_w/check_total_17_18_w)# note: frac of bills out of all checks
(check_monthly_per_res_17_18_bill_w = (check_monthly_per_resp_17_bill_w*length(resp_17) + check_monthly_per_resp_18_bill_w*length(resp_18))/(length(resp_17)+length(resp_18)))
#
## Computing monthly bill check value per respondent 
(check_total_val_17_bill = sum(subset(d2, year==2017 & pi==2 & bill==1)$amnt, na.rm = T))# total 2017 value bill checks written
(check_val_per_resp_17_bill = check_total_val_17_bill/length(resp_17))# val per resp
(check_monthly_val_per_resp_17_bill = 31*check_val_per_resp_17_bill/3)
#
(check_total_val_18_bill = sum(subset(d2, year==2018 & pi==2 & bill==1)$amnt, na.rm = T))# total 2018 value checks written
(check_val_per_resp_18_bill = check_total_val_18_bill/length(resp_18))# val per resp
(check_monthly_val_per_resp_18_bill = 31*check_val_per_resp_18_bill/3)
#
(check_total_val_17_18_bill = sum(subset(d2, pi==2 & bill==1)$amnt, na.rm = T))# total 2017-18 value checks written
(check_val_per_resp_17_18_bill = (check_total_val_17_bill + check_total_val_18_bill)/(length(resp_17)+length(resp_18)))# val per resp
(check_monthly_val_per_resp_17_18_bill = 31*check_val_per_resp_17_18_bill/3)
# redo the above weighted
(check_total_val_17_bill_w = sum(subset(d2, year==2017 & pi==2 & bill==1)$amnt * subset(d2, year==2017 & pi==2 & bill==1)$w1, na.rm = T))# total 2017 value bill checks written (w)
(check_val_per_resp_17_bill_w = check_total_val_17_bill_w/length(resp_17))# val per resp
(check_monthly_val_per_resp_17_bill_w = 31*check_val_per_resp_17_bill_w/3)
#
## Avg, medians, etc
# Avg check value 2017
(check_val_avg_17_bill = mean(subset(d2, year==2017 & pi==2 & bill==1)$amnt, na.rm = T))# 
# Avg check value 2017 (w)
(check_val_avg_17_bill_w = weighted.mean(subset(d2, year==2017 & pi==2 & bill==1)$amnt, subset(d2, year==2017 & pi==2 & bill==1)$w1, na.rm = T))# 
# Median check value 2017
(check_val_med_17_bill = median(subset(d2, year==2017 & pi==2 & bill==1)$amnt, na.rm = T))# 
# Median check value 2017 (w)
(check_val_avg_17_bill_w = weighted.median(subset(d2, year==2017 & pi==2 & bill==1)$amnt, subset(d2, year==2017 & pi==2 & bill==1)$w1, na.rm = T))# 
# Min check value 2017
(check_val_min_17_bill = min(subset(d2, year==2017 & pi==2 & bill==1)$amnt, na.rm = T))# 
# Max check value 2017
(check_val_max_17_bill = max(subset(d2, year==2017 & pi==2 & bill==1)$amnt, na.rm = T))# 
#
(check_total_val_18_bill_w = sum(subset(d2, year==2018 & pi==2 & bill==1)$amnt * subset(d2, year==2018 & pi==2 & bill==1)$w1, na.rm = T))# total 2018 value bill checks written (w)
# Avg check value 2018
(check_val_avg_18_bill = mean(subset(d2, year==2018 & pi==2 & bill==1)$amnt, na.rm = T))# 
# Avg check value 2018 (w)
(check_val_avg_18_bill_w = weighted.mean(subset(d2, year==2018 & pi==2 & bill==1)$amnt, subset(d2, year==2018 & pi==2 & bill==1)$w1, na.rm = T))# 
# Median check value 2018
(check_val_med_18_bill = median(subset(d2, year==2018 & pi==2 & bill==1)$amnt, na.rm = T))# 
# Median check value 2018 (w)
(check_val_avg_18_bill_w = weighted.median(subset(d2, year==2018 & pi==2 & bill==1)$amnt, subset(d2, year==2018 & pi==2 & bill==1)$w1, na.rm = T))# 
# Min check value 2018
(check_val_min_18_bill = min(subset(d2, year==2018 & pi==2 & bill==1)$amnt, na.rm = T))# 
# Max check value 2018
(check_val_max_18_bill = max(subset(d2, year==2018 & pi==2 & bill==1)$amnt, na.rm = T))# 
#
(check_val_per_resp_17_18_bill_w = (check_total_val_17_bill_w + check_total_val_18_bill_w)/(length(resp_17)+length(resp_18)))# val per resp weighted
(check_monthly_val_per_resp_17_18_bill_w = 31*check_val_per_resp_17_18_bill_w/3)
# Avg check value 2017-2018
(check_val_avg_17_18_bill = mean(subset(d2, pi==2 & bill==1)$amnt, na.rm = T))# 
# Avg check value 2017-2018 (w)
(check_val_avg_17_18_bill_w = weighted.mean(subset(d2, pi==2 & bill==1)$amnt, subset(d2,  pi==2 & bill==1)$w1, na.rm = T))# 
# Median check value 2017-2018
(check_val_med_17_18_bill = median(subset(d2, pi==2 & bill==1)$amnt, na.rm = T))# 
# Median check value 2017-2018 (w)
(check_val_med_17_18_bill_w = weighted.median(subset(d2, pi==2 & bill==1)$amnt, subset(d2,  pi==2 & bill==1)$w1, na.rm = T))# 
# Min check value 2017-2018
(check_val_min_17_18_bill = min(subset(d2, pi==2 & bill==1)$amnt, na.rm = T))# 
# Max check value 2017-2018
(check_val_max_17_18_bill = max(subset(d2, pi==2 & bill==1)$amnt, na.rm = T))# 
#
# Building Bill checks column as a vector
check_var.vec # lists the order of variables
length(check_var.vec) # num of variables in Table 2
(bill_checks.vec = c(check_monthly_per_res_17_18_bill_w, check_monthly_val_per_resp_17_18_bill_w, check_val_avg_17_18_bill_w, check_val_med_17_18_bill_w, check_val_min_17_18_bill, check_val_max_17_18_bill, check_total_17_18_bill_w, 100*check_frac_17_18_bill_w))
length(all_checks.vec)

## start P2P check payments (3rd column in Table 2)
(check_total_17_p2p = nrow(subset(d2, year==2017 & pi==2 & merch==16)))# total 2017 checks written
(check_per_resp_17_p2p = check_total_17_p2p/length(resp_17))# num checks per resp 2017
(check_monthly_per_resp_17_p2p = 31*check_per_resp_17_p2p/3)
# redo above weighted
(check_total_val_17_p2p_w = sum(subset(d2, year==2017 & pi==2 & merch==16)$amnt * subset(d2, year==2017 & pi==2 & merch==16)$w1, na.rm = T))# total 2017 value p2p checks written (w)
check_by_resp_17_p2p_w = subset(d2, year==2017 & pi==2 & merch==16) %>% group_by(id) %>% summarize("check_total_17_p2p_w" = sum(w1)) # sum of weights of resp who wrote checks (for some unknown reason "check_total_17_bill_w" has to re-summed)
head(check_by_resp_17_p2p_w$check_total_17_p2p_w)
(check_total_17_p2p_w = sum(check_by_resp_17_p2p_w$check_total_17_p2p_w))
(check_per_resp_17_p2p_w = sum(check_total_17_p2p_w)/length(resp_17))# num checks per resp 2017 weighted
(check_monthly_per_resp_17_p2p_w = 31*check_per_resp_17_p2p_w/3) # weighted
#
(check_total_18_p2p = nrow(subset(d2, year==2018 & pi==2 & merch==16)))# total 2018 checks written
(check_per_resp_18_p2p = check_total_18_p2p/length(resp_18))# num checks per resp 2018
(check_monthly_per_resp_18_p2p = 31*check_per_resp_18_p2p/3)
# redo above weighted
check_by_resp_18_p2p_w = subset(d2, year==2018 & pi==2 & merch==16) %>% group_by(id) %>% summarize("check_total_18_p2p_w" = sum(w1)) # sum of weights of resp who wrote checks (for some unknown reason "check_total_18_p2p_w" has to re-summed)
head(check_by_resp_18_p2p_w$check_total_18_p2p_w)
(check_total_18_p2p_w = sum(check_by_resp_18_p2p_w$check_total_18_p2p_w))
(check_per_resp_18_p2p_w = sum(check_total_18_p2p_w)/length(resp_18))# num checks per resp 2018 weighted
(check_monthly_per_resp_18_p2p_w = 31*check_per_resp_18_p2p_w/3) # weighted
#
# monthly p2p check per resp 2017 2018 combined
(check_total_17_18_p2p = nrow(subset(d2, pi==2 & merch==16)))# total 2017-18
(check_frac_17_18_p2p = check_total_17_18_p2p/check_total_17_18)# note: frac of bills out of all checks
(check_monthly_per_res_17_18_p2p = (check_monthly_per_resp_17_p2p*length(resp_17) + check_monthly_per_resp_18_p2p*length(resp_18))/(length(resp_17)+length(resp_18)))
#
## Computing monthly p2p check value per respondent 
(check_total_val_17_p2p = sum(subset(d2, year==2017 & pi==2 & merch==16)$amnt, na.rm = T))# total 2017 value p2p checks written
(check_val_per_resp_17_p2p = check_total_val_17_p2p/length(resp_17))# val per resp
(check_monthly_val_per_resp_17_p2p = 31*check_val_per_resp_17_p2p/3)
#
(check_total_val_18_p2p = sum(subset(d2, year==2018 & pi==2 & merch==16)$amnt, na.rm = T))# total 2018 value checks written
(check_val_per_resp_18_p2p = check_total_val_18_p2p/length(resp_18))# val per resp
(check_monthly_val_per_resp_18_p2p = 31*check_val_per_resp_18_p2p/3)
#
# redo the above weighted
(check_total_17_18_p2p_w = sum(subset(d2, pi==2 & merch==16)$w1))# total 2017-18 (w)
(check_frac_17_18_p2p_w = check_total_17_18_p2p_w/check_total_17_18_w)# note: frac of p2p out of all checks
(check_monthly_per_res_17_18_p2p_w = (check_monthly_per_resp_17_p2p_w*length(resp_17) + check_monthly_per_resp_18_p2p_w*length(resp_18))/(length(resp_17)+length(resp_18)))
#
(check_total_val_17_18_p2p = sum(subset(d2, pi==2 & merch==16)$amnt, na.rm = T))# total 2017-18 value checks written
(check_val_per_resp_17_18_p2p = (check_total_val_17_p2p + check_total_val_18_p2p)/(length(resp_17)+length(resp_18)))# val per resp
(check_monthly_val_per_resp_17_18_p2p = 31*check_val_per_resp_17_18_p2p/3)
# redo weighted
(check_total_val_18_p2p_w = sum(subset(d2, year==2018 & pi==2 & merch==16)$amnt * subset(d2, year==2018 & pi==2 & merch==16)$w1, na.rm = T))# total 2018 value p2p checks written (w)
(check_val_per_resp_17_18_p2p_w = (check_total_val_17_p2p_w + check_total_val_18_p2p_w)/(length(resp_17)+length(resp_18)))# val per resp weighted
(check_monthly_val_per_resp_17_18_p2p_w = 31*check_val_per_resp_17_18_p2p_w/3)
# 
# Avg check value 2017-2018
(check_val_avg_17_18_p2p = mean(subset(d2, pi==2 & merch==16)$amnt, na.rm = T))# 
# Avg check value 2017-2018 (w)
(check_val_avg_17_18_p2p_w = weighted.mean(subset(d2, pi==2 & merch==16)$amnt, subset(d2,  pi==2 & merch==16)$w1, na.rm = T))# 
#
## Avg, medians, etc
# Avg check value 2017
(check_val_avg_17_p2p = mean(subset(d2, year==2017 & pi==2 & merch==16)$amnt, na.rm = T))# 
(check_val_avg_17_p2p_w = weighted.mean(subset(d2, year==2017 & pi==2 & merch==16)$amnt, subset(d2, year==2017 & pi==2 & merch==16)$w1, na.rm = T))# 
# Median check value 2017
(check_val_med_17_p2p = median(subset(d2, year==2017 & pi==2 & merch==16)$amnt, na.rm = T))# 
(check_val_med_17_p2p_w = weighted.median(subset(d2, year==2017 & pi==2 & merch==16)$amnt, subset(d2, year==2017 & pi==2 & merch==16)$w1, na.rm = T))# 
# Min check value 2017
(check_val_min_17_p2p = min(subset(d2, year==2017 & pi==2 & merch==16)$amnt, na.rm = T))# 
# Max check value 2017
(check_val_max_17_p2p = max(subset(d2, year==2017 & pi==2 & merch==16)$amnt, na.rm = T))# 
# Avg check value 2018
(check_val_avg_18_p2p = mean(subset(d2, year==2018 & pi==2 & merch==16)$amnt, na.rm = T))# 
(check_val_avg_18_p2p_w = weighted.mean(subset(d2, year==2018 & pi==2 & merch==16)$amnt, subset(d2, year==2018 & pi==2 & merch==16)$w1, na.rm = T))# 
# Median check value 2018
(check_val_med_18_p2p = median(subset(d2, year==2018 & pi==2 & merch==16)$amnt, na.rm = T))# 
(check_val_med_18_p2p_w = weighted.median(subset(d2, year==2018 & pi==2 & merch==16)$amnt, subset(d2, year==2018 & pi==2 & merch==16)$w1, na.rm = T))# 
# Min check value 2018
(check_val_min_18_p2p = min(subset(d2, year==2018 & pi==2 & merch==16)$amnt, na.rm = T))# 
# Max check value 2018
(check_val_max_18_p2p = max(subset(d2, year==2018 & pi==2 & merch==16)$amnt, na.rm = T))# 
# Avg check value 2017-2018
(check_val_avg_17_18_p2p = mean(subset(d2, pi==2 & merch==16)$amnt, na.rm = T))# 
(check_val_avg_17_18_p2p_w = weighted.mean(subset(d2, pi==2 & merch==16)$amnt, subset(d2, pi==2 & merch==16)$w1, na.rm = T))# 
# Median check value 2017-2018
(check_val_med_17_18_p2p = median(subset(d2, pi==2 & merch==16)$amnt, na.rm = T))# 
(check_val_med_17_18_p2p_w = weighted.median(subset(d2, pi==2 & merch==16)$amnt, subset(d2, pi==2 & merch==16)$w1, na.rm = T))# 
# Min check value 2017-2018
(check_val_min_17_18_p2p = min(subset(d2, pi==2 & merch==16)$amnt, na.rm = T))# 
# Max check value 2017-2018
(check_val_max_17_18_p2p = max(subset(d2, pi==2 & merch==16)$amnt, na.rm = T))# 
#
# Building P2P checks column as a vector
check_var.vec # lists the order of variables
length(check_var.vec) # num of variables in Table 2
(p2p_checks.vec = c(check_monthly_per_res_17_18_p2p_w, check_monthly_val_per_resp_17_18_p2p_w, check_val_avg_17_18_p2p_w, check_val_med_17_18_p2p_w, check_val_min_17_18_p2p, check_val_max_17_18_p2p, check_total_17_18_p2p_w, 100*check_frac_17_18_p2p_w))
length(p2p_checks.vec)

## start other check payments (4th column in Table 2)
(check_total_17_other = nrow(subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)))# total 2017 checks written
(check_per_resp_17_other = check_total_17_other/length(resp_17))# num checks per resp 2017
(check_monthly_per_resp_17_other = 31*check_per_resp_17_other/3)
# redo above weighted
(check_total_val_17_other_w = sum(subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)$amnt * subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)$w1, na.rm = T))# total 2017 value p2p checks written (w)
check_by_resp_17_other_w = subset(d2, year==2017 & pi==2 & merch != 16 & bill==0) %>% group_by(id) %>% summarize("check_total_17_other_w" = sum(w1)) # sum of weights of resp who wrote checks (for some unknown reason "check_total_17_other_w" has to re-summed)
head(check_by_resp_17_other_w$check_total_17_other_w)
(check_total_17_other_w = sum(check_by_resp_17_other_w$check_total_17_other_w))
(check_per_resp_17_other_w = sum(check_total_17_other_w)/length(resp_17))# num checks per resp 2017 weighted
(check_monthly_per_resp_17_other_w = 31*check_per_resp_17_other_w/3) # weighted
#
(check_total_18_other = nrow(subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)))# total 2018 checks written
(check_per_resp_18_other = check_total_18_other/length(resp_18))# num checks per resp 2018
(check_monthly_per_resp_18_other = 31*check_per_resp_18_other/3)
# redo above weighted
check_by_resp_18_other_w = subset(d2, year==2018 & pi==2 & merch != 16 & bill==0) %>% group_by(id) %>% summarize("check_total_18_other_w" = sum(w1)) # sum of weights of resp who wrote checks (for some unknown reason "check_total_18_other_w" has to re-summed)
head(check_by_resp_18_p2p_w$check_total_18_p2p_w)
(check_total_18_other_w = sum(check_by_resp_18_other_w$check_total_18_other_w))
(check_per_resp_18_other_w = sum(check_total_18_other_w)/length(resp_18))# num checks per resp 2018 weighted
(check_monthly_per_resp_18_other_w = 31*check_per_resp_18_other_w/3) # weighted
#
# monthly bill check per resp 2017 2018 combined
(check_total_17_18_other = nrow(subset(d2, pi==2 & merch != 16 & bill==0)))# total 2017-18
(check_frac_17_18_other = check_total_17_18_other/check_total_17_18)# note: frac of bills out of all checks
(check_monthly_per_res_17_18_other = (check_monthly_per_resp_17_other*length(resp_17) + check_monthly_per_resp_18_other*length(resp_18))/(length(resp_17)+length(resp_18)))
# redo the above weighted
(check_total_17_18_other_w = sum(subset(d2, pi==2 & merch != 16 & bill==0)$w1))# total 2017-18 (w)
(check_frac_17_18_other_w = check_total_17_18_other_w/check_total_17_18_w)# note: frac of other out of all checks
(check_monthly_per_res_17_18_other_w = (check_monthly_per_resp_17_other_w*length(resp_17) + check_monthly_per_resp_18_other_w*length(resp_18))/(length(resp_17)+length(resp_18)))

## Computing monthly bill check value per respondent 
(check_total_val_17_other = sum(subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# total 2017 value bill checks written
(check_val_per_resp_17_other = check_total_val_17_other/length(resp_17))# val per resp
(check_monthly_val_per_resp_17_other = 31*check_val_per_resp_17_other/3)
# redo weighted
(check_total_val_18_other_w = sum(subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$amnt * subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$w1, na.rm = T))# total 2018 value other checks written (w)
(check_val_per_resp_17_18_other_w = (check_total_val_17_other_w + check_total_val_18_other_w)/(length(resp_17)+length(resp_18)))# val per resp weighted
(check_monthly_val_per_resp_17_18_other_w = 31*check_val_per_resp_17_18_other_w/3)
# 
(check_total_val_18_other = sum(subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# total 2018 value checks written
(check_val_per_resp_18_other = check_total_val_18_other/length(resp_18))# val per resp
(check_monthly_val_per_resp_18_other = 31*check_val_per_resp_18_other/3)
#
(check_total_val_17_18_other = sum(subset(d2, pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# total 2017-18 value checks written
(check_val_per_resp_17_18_other = (check_total_val_17_other + check_total_val_18_other)/(length(resp_17)+length(resp_18)))# val per resp
(check_monthly_val_per_resp_17_18_other = 31*check_val_per_resp_17_18_other/3)
# redo weighted
(check_total_val_18_other_w = sum(subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$amnt * subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$w1, na.rm = T))# total 2018 value p2p checks written (w)
(check_val_per_resp_17_18_other_w = (check_total_val_17_other_w + check_total_val_18_other_w)/(length(resp_17)+length(resp_18)))# val per resp weighted
(check_monthly_val_per_resp_17_18_other_w = 31*check_val_per_resp_17_18_other_w/3)
#
## Avg, medians, etc
# Avg check value 2017
(check_val_avg_17_other = mean(subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
(check_val_avg_17_other_w = weighted.mean(subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)$amnt, subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)$w1, na.rm = T))# 
# Median check value 2017
(check_val_med_17_other = median(subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
(check_val_med_17_other_w = weighted.median(subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)$amnt, subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)$w1, na.rm = T))# 
# Min check value 2017
(check_val_min_17_other = min(subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
# Max check value 2017
(check_val_max_17_other = max(subset(d2, year==2017 & pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
# Avg check value 2018
(check_val_avg_18_other = mean(subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
(check_val_avg_18_other_w = weighted.mean(subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$amnt, subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$w1, na.rm = T))# 
# Median check value 2018
(check_val_med_18_other = median(subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
(check_val_med_18_other_w = weighted.median(subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$amnt, subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$w1, na.rm = T))# 
# Min check value 2018
(check_val_min_18_other = min(subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
# Max check value 2018
(check_val_max_18_other = max(subset(d2, year==2018 & pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
# Avg check value 2017-2018
(check_val_avg_17_18_other = mean(subset(d2, pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
(check_val_avg_17_18_other_w = weighted.mean(subset(d2, pi==2 & merch != 16 & bill==0)$amnt, subset(d2, pi==2 & merch != 16 & bill==0)$w1, na.rm = T))# 
# Median check value 2017-2018
(check_val_med_17_18_other = median(subset(d2, pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
(check_val_med_17_18_other_w = weighted.median(subset(d2, pi==2 & merch != 16 & bill==0)$amnt, subset(d2, pi==2 & merch != 16 & bill==0)$w1, na.rm = T))# 
# Min check value 2017-2018
(check_val_min_17_18_other = min(subset(d2, pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
# Max check value 2017-2018
(check_val_max_17_18_other = max(subset(d2, pi==2 & merch != 16 & bill==0)$amnt, na.rm = T))# 
#
# Building other checks column as a vector
check_var.vec # lists the order of variables
length(check_var.vec) # num of variables in Table 2
(other_checks.vec = c(check_monthly_per_res_17_18_other_w, check_monthly_val_per_resp_17_18_other_w, check_val_avg_17_18_other_w, check_val_med_17_18_other_w, check_val_min_17_18_other, check_val_max_17_18_other, check_total_17_18_other_w, 100*check_frac_17_18_other_w))
length(other_checks.vec)

## Finalizing check-use Table 2
(check_use.df = data.frame("Variable" = check_var.vec, "All_checks" = all_checks.vec, "Bill_payments" = bill_checks.vec, "P2P_payments" = p2p_checks.vec, "Other_payments" = other_checks.vec) )
write.table(format(check_use.df, digits=3), file = "check_use.txt", sep = ",", quote = F, row.names = F)
dir()# end of first table (all checks, bills and non-bills)

# Note below Table 2: Overlap between bill payments and P2P (not mutually exclusive!)
(check_total_17_18_p2p = nrow(subset(d2, pi==2 & bill==1 & merch==16)))# num checks classfied as both: bill and P2P. 

### End of cash-use table (Table 2) started on Line = 100 ends Line 475

### Defining check data subset of the entire trans set
check_trans_17 = subset(d2, year==2017 & pi==2)
dim(check_trans_17)# num of check payments in 2017
check_trans_18 = subset(d2, year==2018 & pi==2)
dim(check_trans_18)# num check payments in 2018
check_trans_17_18 = subset(d2, pi==2)
dim(check_trans_17_18)# num check payments in 2017 and 2018

### Figure 1: Histogram w.r.t check value
# head(check_trans_17_18$amnt)
# summary(check_trans_17_18$amnt)
# hist(subset(check_trans_17_18, amnt <= 1000)$amnt, probability = F, col = "lightblue", main = "", xlab = "Check dollar value (below $1000)")
# #
# #hist(subset(check_trans_17_18, amnt <= 200)$amnt, probability = F, col = "lightblue", main = "", xlab = "Check dollar value (below $200)")
# #
# # Notes below Figure 1
# nrow(subset(check_trans_17_18, amnt >= 1000))# num checks $1k+
# nrow(subset(check_trans_17_18, amnt >= 10000))# num checks $10k+
# 
# ### Table 3 starts: Share of check payments by dollar value
nrow(subset(d2, pi==2))# num checks written
summary(subset(d2, pi==2)$amnt)
# begin amount buckets
(amount_buckets.vec = c("[0_25]", "(25_50]", "(50_100]", "(100_250]", "(250_30k]"))
(amount_25 = nrow(subset(d2, pi==2 & amnt <= 25)))
(amount_50 = nrow(subset(d2, pi==2 & amnt > 25 & amnt <= 50)))
(amount_100 = nrow(subset(d2, pi==2 & amnt > 50 & amnt <= 100)))
(amount_250 = nrow(subset(d2, pi==2 & amnt > 100 & amnt <= 250)))
(amount_inf = nrow(subset(d2, pi==2 & amnt > 250)))
# put the above in a vector
(amount_total.vec = c(amount_25, amount_50, amount_100, amount_250, amount_inf))
sum(amount_total.vec)
(amount_frac.vec = amount_total.vec/sum(amount_total.vec))
sum(amount_frac.vec)
# Do the above weighted
(amount_25_w = sum(subset(d2, pi==2 & amnt <= 25)$w1))
(amount_50_w = sum(subset(d2, pi==2 & amnt > 25 & amnt <= 50)$w1))
(amount_100_w = sum(subset(d2, pi==2 & amnt > 50 & amnt <= 100)$w1))
(amount_250_w = sum(subset(d2, pi==2 & amnt > 100 & amnt <= 250)$w1))
(amount_inf_w = sum(subset(d2, pi==2 & amnt > 250)$w1))
# put the above in a vector (w)
(amount_total.vec_w = c(amount_25_w, amount_50_w, amount_100_w, amount_250_w, amount_inf_w))
sum(amount_total.vec_w)
(amount_frac.vec_w = amount_total.vec_w/sum(amount_total.vec_w))
sum(amount_frac.vec)
# Make it a table 3 (weighted only)
(amount_table.df = data.frame("Check_dollar_amount" = amount_buckets.vec, "Share_of_checks_(w)" = percent(amount_frac.vec_w)))
write.table(format(amount_table.df, digits=3), file = "amount_table.txt", sep = ",", quote = F, row.names = F)
dir()# 

#######################

### Distribution of check payments w.r.t. 21 merchant types
# start Table 4 (merchant types)
# Remove trans with merch==NA
dim(check_trans_17_18)
check_trans_17_18_2 = check_trans_17_18[!is.na(check_trans_17_18$merch),]
dim(check_trans_17_18_2) # above 2 trans with merch==NA removed
#
# Volume by merchant UNweighted (as a reference, not for paper)
table(check_trans_17_18_2$merch)
sum(table(check_trans_17_18_2$merch))# 
(check_trans_by_merch = as.vector(table(check_trans_17_18_2$merch)))
(sum(check_trans_by_merch))
(check_frac_by_merch = check_trans_by_merch/sum(check_trans_by_merch))
percent(check_frac_by_merch)
sum(percent(check_frac_by_merch))
# 
# do the above (volume) weighted: Enters as volume share in Table
(check_trans_by_merch_w = check_trans_17_18_2 %>% group_by(merch) %>% summarize("vol_w" = sum(w1)))
str(check_trans_by_merch_w )
(vol_w_vec = as.vector(check_trans_by_merch_w$vol_w))
(check_frac_by_merch_w = vol_w_vec/sum(vol_w_vec))
(check_perc_by_merch_w = percent(check_frac_by_merch_w)) # column "vol share" in Table
sum(check_perc_by_merch_w)
#
# merchant by value (UNweighted)
(check_val_by_merch = check_trans_17_18_2 %>% group_by(merch) %>% summarize("val" = sum(amnt, na.rm = T)))
str(check_val_by_merch )
(val_vec = as.vector(check_val_by_merch$val))
(check_val_frac_by_merch = val_vec/sum(val_vec))
(check_val_perc_by_merch = percent(check_val_frac_by_merch)) # column "val share" in Table
sum(check_val_perc_by_merch)
#
# merchant by value (weighted)
(check_val_by_merch_w = check_trans_17_18_2 %>% group_by(merch) %>% summarize("val_w" = sum(amnt*w1, na.rm = T)))
str(check_val_by_merch_w )
(val_w_vec = as.vector(check_val_by_merch_w$val_w))
(check_val_frac_by_merch_w = val_w_vec/sum(val_w_vec))
(check_val_perc_by_merch_w = percent(check_val_frac_by_merch_w)) # column "val share" in Table
sum(check_val_perc_by_merch_w)
#
# merchant avg check value (weighted)
(check_avg_by_merch_w = check_trans_17_18_2 %>% group_by(merch) %>% summarize("avg_w" = weighted.mean(amnt, w1, na.rm = T)))
str(check_avg_by_merch_w)
(avg_w_vec =  as.vector(check_avg_by_merch_w$avg_w))# Column "Avg check val" in Table
#
# merchant median check value (weighted)
(check_med_by_merch_w = check_trans_17_18_2 %>% group_by(merch) %>% summarize("med_w" = weighted.median(amnt, w1, na.rm = T)))
str(check_med_by_merch_w)
(med_w_vec =  as.vector(check_med_by_merch_w$med_w))# Column "med check val" in Table
#
# making it a table (weighted for paper)
(check_by_merch_df = data.frame("Merchant_type" = 1:21, "Volume_share_(w)" = percent(check_frac_by_merch_w), "Value_share_(w)" = check_val_perc_by_merch_w, "Average_check_value_(w)" = round(avg_w_vec, digits = 2), "Median_check_value_(w)" = round(med_w_vec, digits = 2)))
write.table(check_by_merch_df, file = "check_by_merch.txt", sep = ",", quote = F, row.names = F)
dir()# 

# Comparing vol and val weighted vs. unweighted (just as reference, not for the paper)
(check_by_merch_compare_df = data.frame("Merchant_type" = 1:21, "Volume_share_(w)" = percent(check_frac_by_merch_w), "Volume_share" = percent(check_frac_by_merch), "Value_share_(w)" = check_val_perc_by_merch_w, "Value_share" = check_val_perc_by_merch ))
write.table(check_by_merch_compare_df, file = "check_by_merch_compare.txt", sep = ",", quote = F, row.names = F)

########

### Start Table: 21 merchant type: Share of check payments among all payments
# check_trans_by_merch # table of num check trans by merch type
# length(check_trans_by_merch) # should be 21 merchants
# (all_trans_by_merch = as.vector(table(all_trans_17_18$merch)))
# # Below, share % paid by check by merchant type
# percent(check_trans_by_merch/all_trans_by_merch)
# sum(percent(check_trans_by_merch/all_trans_by_merch))# Note: sums up to 314%
# # make it a table
# (share_check_by_merch_df = data.frame("Merchant_type" = 1:21, "Share_paid_by_check" = percent(check_trans_by_merch/all_trans_by_merch)))
# do the above weighted
vol_w_vec # weighted volume by merchant
dim(d2) # total num trans in the sample
d2_2 = d2[!is.na(d2$merch), ] # remove merch == NA
dim(d2_2) # num trans after removing merch == NA
(trans_by_merch_w = d2_2 %>% group_by(merch) %>% summarize("vol_all_w" = sum(w1)))
percent(vol_w_vec/trans_by_merch_w$vol_all_w)
# make it a table
(share_check_by_merch_df = data.frame("Merchant_type" = 1:21, "Share_paid_by_check_(w)" = percent(vol_w_vec/trans_by_merch_w$vol_all_w)))
write.table(share_check_by_merch_df, file = "share_check_by_merch.txt", sep = ",", quote = F, row.names = F)
dir()# 

### Age: Volume and value
# Age histogram
# table(check_trans_17_18$age)
# age_breaks = c(18,25,35,45,55,65,75,85,95)
# age_breaks = c(18,20,25,30,35,40,45,50,55,60,65,70,75,85,80,95)
# hist(check_trans_17_18$age, probability = F, col = "lightblue", main = "", xlab = "Age in years", breaks = age_breaks)

# Age table begins (see condensed table below)
age24 = subset(check_trans_17_18, age < 25)
dim(age24)
#
age34 = subset(check_trans_17_18, age >= 25 & age < 35)
dim(age34)
#
age44 = subset(check_trans_17_18, age >= 35 & age < 45)
dim(age44)
#
age54 = subset(check_trans_17_18, age >= 45 & age < 55)
dim(age54)
#
age64 = subset(check_trans_17_18, age >= 55 & age < 65)
dim(age64)
#
age74 = subset(check_trans_17_18, age >= 65 & age < 75)
dim(age74)
#
age84 = subset(check_trans_17_18, age >= 75 & age < 85)
dim(age84)
#
age94 = subset(check_trans_17_18, age >= 85 & age < 95)
dim(age94)
#
# age group names
(age_groups = c("[18_25)", "[25_35)", "[35_45)", "[45_55)", "[55_65)", "[65_75)", "[75_85)", "[85_95)"))
# vol by age
(age_vol = c(nrow(age24), nrow(age34), nrow(age44), nrow(age54), nrow(age64), nrow(age74), nrow(age84), nrow(age94)))
sum(age_vol)
(age_vol_perc = percent(age_vol/sum(age_vol)))
sum(age_vol_perc)
# total val by age
(age_val_24 = sum(filter(check_trans_17_18, age < 25)$amnt))
(age_val_34 = sum(filter(check_trans_17_18, age>= 25 & age < 35)$amnt, na.rm = T))
(age_val_44 = sum(filter(check_trans_17_18, age>= 35 & age < 45)$amnt, na.rm = T))
(age_val_54 = sum(filter(check_trans_17_18, age>= 45 & age < 55)$amnt, na.rm = T))
(age_val_64 = sum(filter(check_trans_17_18, age>= 55 & age < 65)$amnt, na.rm = T))
(age_val_74 = sum(filter(check_trans_17_18, age>= 65 & age < 75)$amnt, na.rm = T))
(age_val_84 = sum(filter(check_trans_17_18, age>= 75 & age < 85)$amnt, na.rm = T))
(age_val_94 = sum(filter(check_trans_17_18, age>= 85 & age < 95)$amnt, na.rm = T))
# percentage value by age
(age_val_perc_24 = age_val_24/sum(check_trans_17_18$amnt, na.rm = T))
(age_val_perc_34 = age_val_34/sum(check_trans_17_18$amnt, na.rm = T))
(age_val_perc_44 = age_val_44/sum(check_trans_17_18$amnt, na.rm = T))
(age_val_perc_54 = age_val_54/sum(check_trans_17_18$amnt, na.rm = T))
(age_val_perc_64 = age_val_64/sum(check_trans_17_18$amnt, na.rm = T))
(age_val_perc_74 = age_val_74/sum(check_trans_17_18$amnt, na.rm = T))
(age_val_perc_84 = age_val_84/sum(check_trans_17_18$amnt, na.rm = T))
(age_val_perc_94 = age_val_94/sum(check_trans_17_18$amnt, na.rm = T))
#
(age_val_perc = percent(c(age_val_perc_24, age_val_perc_34, age_val_perc_44, age_val_perc_54, age_val_perc_64, age_val_perc_74, age_val_perc_84, age_val_perc_94)))
# age avg check value by age
(age_avg_24 = mean(filter(check_trans_17_18, age < 25)$amnt))
(age_avg_34 = mean(filter(check_trans_17_18, age>= 25 & age < 35)$amnt, na.rm = T))
(age_avg_44 = mean(filter(check_trans_17_18, age>= 35 & age < 45)$amnt, na.rm = T))
(age_avg_54 = mean(filter(check_trans_17_18, age>= 45 & age < 55)$amnt, na.rm = T))
(age_avg_64 = mean(filter(check_trans_17_18, age>= 55 & age < 65)$amnt, na.rm = T))
(age_avg_74 = mean(filter(check_trans_17_18, age>= 65 & age < 75)$amnt, na.rm = T))
(age_avg_84 = mean(filter(check_trans_17_18, age>= 75 & age < 85)$amnt, na.rm = T))
(age_avg_94 = mean(filter(check_trans_17_18, age>= 85 & age < 95)$amnt, na.rm = T))
# 
(age_avg = c(age_avg_24, age_avg_34, age_avg_44, age_avg_54, age_avg_64, age_avg_74, age_avg_84, age_avg_94))
# 
# age median check value
(age_med_24 = median(filter(check_trans_17_18, age < 25)$amnt))
(age_med_34 = median(filter(check_trans_17_18, age>= 25 & age < 35)$amnt, na.rm = T))
(age_med_44 = median(filter(check_trans_17_18, age>= 35 & age < 45)$amnt, na.rm = T))
(age_med_54 = median(filter(check_trans_17_18, age>= 45 & age < 55)$amnt, na.rm = T))
(age_med_64 = median(filter(check_trans_17_18, age>= 55 & age < 65)$amnt, na.rm = T))
(age_med_74 = median(filter(check_trans_17_18, age>= 65 & age < 75)$amnt, na.rm = T))
(age_med_84 = median(filter(check_trans_17_18, age>= 75 & age < 85)$amnt, na.rm = T))
(age_med_94 = median(filter(check_trans_17_18, age>= 85 & age < 95)$amnt, na.rm = T))
# 
(age_med = c(age_med_24, age_med_34, age_med_44, age_med_54, age_med_64, age_med_74, age_med_84, age_med_94))
# 
# Adding share (of tranactions) in the sample by age
# Age table begins
age24_sample = subset(d2, age < 25)
dim(age24_sample)
#
age34_sample = subset(d2, age >= 25 & age < 35)
dim(age34_sample)
#
age44_sample = subset(d2, age >= 35 & age < 45)
dim(age44_sample)
#
age54_sample = subset(d2, age >= 45 & age < 55)
dim(age54_sample)
#
age64_sample = subset(d2, age >= 55 & age < 65)
dim(age64_sample)
#
age74_sample = subset(d2, age >= 65 & age < 75)
dim(age74_sample)
#
age84_sample = subset(d2, age >= 75 & age < 85)
dim(age84_sample)
#
age94_sample = subset(d2, age >= 85 & age < 95)
dim(age94_sample)
#
# now the above as shares of total num trans in sample
(age24_sample_perc = percent(nrow(age24_sample)/nrow(d2)))
(age34_sample_perc = percent(nrow(age34_sample)/nrow(d2)))
(age44_sample_perc = percent(nrow(age44_sample)/nrow(d2)))
(age54_sample_perc = percent(nrow(age54_sample)/nrow(d2)))
(age64_sample_perc = percent(nrow(age64_sample)/nrow(d2)))
(age74_sample_perc = percent(nrow(age74_sample)/nrow(d2)))
(age84_sample_perc = percent(nrow(age84_sample)/nrow(d2)))
(age94_sample_perc = percent(nrow(age94_sample)/nrow(d2)))
# make the above a vector
(age_sample_perc = c(age24_sample_perc, age34_sample_perc, age44_sample_perc, age54_sample_perc, age64_sample_perc, age74_sample_perc, age84_sample_perc, age94_sample_perc))
sum(age_sample_perc)

# Make it a table
(check_age_df = data.frame("Age_group" = age_groups, "Share_in_sample" = age_sample_perc, "Volume_share" = age_vol_perc, "Value_share" =  age_val_perc, "Average_check_value" = round(age_avg, digits = 2), "Median_check_value" = round(age_med, digits = 2)))
#
write.table(check_age_df, file = "check_age.txt", sep = ",", quote = F, row.names = F)
dir()# 

## Age condensed table begins: NOTE: Overwriting the full table, saving it as condensed
summary(check_trans_17_18$age)
nrow(check_trans_17_18)# num of checks
#
age44 = subset(check_trans_17_18, age < 45)
dim(age44)
#
age64 = subset(check_trans_17_18, age >= 45 & age < 65)
dim(age64)
#
age94 = subset(check_trans_17_18, age >= 65 & age < 95)
dim(age94)
#
# age group names
(age_groups = c("[18_44]", "[45_64]", "[65_95]"))
# vol by age
(age_vol = c(nrow(age44), nrow(age64), nrow(age94)))
sum(age_vol)
(age_vol_perc = percent(age_vol/sum(age_vol)))
sum(age_vol_perc)
# total val by age
(age_val_44 = sum(filter(check_trans_17_18, age < 45)$amnt, na.rm = T))
(age_val_64 = sum(filter(check_trans_17_18, age>= 45 & age < 65)$amnt, na.rm = T))
(age_val_94 = sum(filter(check_trans_17_18, age>= 65 & age < 95)$amnt, na.rm = T))
# percentage value by age
(age_val_perc_44 = age_val_44/sum(check_trans_17_18$amnt, na.rm = T))
(age_val_perc_64 = age_val_64/sum(check_trans_17_18$amnt, na.rm = T))
(age_val_perc_94 = age_val_94/sum(check_trans_17_18$amnt, na.rm = T))
#
(age_val_perc = percent(c(age_val_perc_44, age_val_perc_64, age_val_perc_94)))
sum(age_val_perc)
# age avg check value by age
(age_avg_44 = mean(filter(check_trans_17_18, age < 45)$amnt, na.rm = T))
(age_avg_64 = mean(filter(check_trans_17_18, age>= 45 & age < 65)$amnt, na.rm = T))
(age_avg_94 = mean(filter(check_trans_17_18, age>= 65 & age < 95)$amnt, na.rm = T))
# 
(age_avg = c(age_avg_44, age_avg_64, age_avg_94))
# 
# age median check value
(age_med_44 = median(filter(check_trans_17_18, age < 45)$amnt, na.rm = T))
(age_med_64 = median(filter(check_trans_17_18, age>= 45 & age < 65)$amnt, na.rm = T))
(age_med_94 = median(filter(check_trans_17_18, age>= 65 & age < 95)$amnt, na.rm = T))
# 
(age_med = c(age_med_44, age_med_64, age_med_94))
# 
# Adding share (of tranactions) in the sample by age
# Age table begins
age44_sample = subset(d2, age < 45)
dim(age44_sample)
#
age64_sample = subset(d2, age >= 45 & age < 65)
dim(age64_sample)
#
age94_sample = subset(d2, age >= 65 & age < 95)
dim(age94_sample)
#
# now the above as shares of total num trans in sample
(age44_sample_perc = percent(nrow(age44_sample)/nrow(d2)))
(age64_sample_perc = percent(nrow(age64_sample)/nrow(d2)))
(age94_sample_perc = percent(nrow(age94_sample)/nrow(d2)))
# make the above a vector
(age_sample_perc = c(age44_sample_perc, age64_sample_perc, age94_sample_perc))
sum(age_sample_perc)

# Make it a table
(check_age_3_df = data.frame("Age_group" = age_groups, "Share_in_sample" = age_sample_perc, "Volume_share" = age_vol_perc, "Value_share" =  age_val_perc, "Average_check_value" = round(age_avg, digits = 2), "Median_check_value" = round(age_med, digits = 2)))
#0
write.table(check_age_3_df, file = "check_age_3.txt", sep = ",", quote = F, row.names = F)
dir()# 
# End of condensed (3 age groups) age table

### HH Income begins (note 2017 used categories, 2018 actual numbers)
dim(check_trans_17_18) # num check payments in joint sample
summary(check_trans_17_18$income)
# creating buckets of income groups
income_50 = subset(check_trans_17_18, income < 50000)
income_100 = subset(check_trans_17_18, income >= 50000 & income < 100000)
income_inf = subset(check_trans_17_18, income >= 100000)
#
# income vol
(income_50_vol = nrow(income_50))
(income_100_vol = nrow(income_100))
(income_inf_vol = nrow(income_inf))
(income_vol = c(income_50_vol, income_100_vol, income_inf_vol))
#
(income_50_vol_perc = nrow(income_50)/(nrow(income_50)+nrow(income_100)+nrow(income_inf)))
(income_100_vol_perc = nrow(income_100)/(nrow(income_50)+nrow(income_100)+nrow(income_inf)))
(income_inf_vol_perc = nrow(income_inf)/(nrow(income_50)+nrow(income_100)+nrow(income_inf)))
income_50_vol_perc+income_100_vol_perc+income_inf_vol_perc# check sum to 1
#
# income vol weighted
(income_50_vol_w = sum(income_50$w1))
(income_100_vol_w = sum(income_100$w1))
(income_inf_vol_w = sum(income_inf$w1))
(income_vol_w = c(income_50_vol_w, income_100_vol_w, income_inf_vol_w))
#
(income_vol_perc_w = percent(c(income_50_vol_w/sum(income_vol_w), income_100_vol_w/sum(income_vol_w), income_inf_vol_w/sum(income_vol_w))))# "Vol share" column in table
#
# income val 
(income_50_val = sum(income_50$amnt, na.rm = T))
(income_100_val = sum(income_100$amnt, na.rm = T))
(income_inf_val = sum(income_inf$amnt, na.rm = T))
#
(income_50_val_perc = (income_50_val)/(sum(income_50_val + income_100_val + income_inf_val)))
(income_100_val_perc = (income_100_val)/(sum(income_50_val + income_100_val + income_inf_val)))
(income_inf_val_perc = (income_inf_val)/(sum(income_50_val + income_100_val + income_inf_val)))
income_50_val_perc+income_100_val_perc+income_inf_val_perc# check sum to 1
#
# income val weighted
(income_50_val_w = sum(income_50$amnt*income_50$w1, na.rm = T))
(income_100_val_w = sum(income_100$amnt*income_100$w1, na.rm = T))
(income_inf_val_w = sum(income_inf$amnt*income_inf$w1, na.rm = T))
(income_val_w = c(income_50_val_w, income_100_val_w, income_inf_val_w))
(income_val_perc_w = percent(c(income_50_val_w/sum(income_val_w), income_100_val_w/sum(income_val_w), income_inf_val_w/sum(income_val_w))))# "Val share" column in table
#
# income average check  value
(income_50_avg = mean(income_50$amnt, na.rm = T))
(income_100_avg = mean(income_100$amnt, na.rm = T))
(income_inf_avg = mean(income_inf$amnt, na.rm = T))
# income average check  value weighted
(income_50_avg_w = weighted.mean(income_50$amnt, income_50$w1, na.rm = T))
(income_100_avg_w = weighted.mean(income_100$amnt, income_100$w1, na.rm = T))
(income_inf_avg_w = weighted.mean(income_inf$amnt, income_inf$w1, na.rm = T))
#
# income median check  value
(income_50_med = median(income_50$amnt, na.rm = T))
(income_100_med = median(income_100$amnt, na.rm = T))
(income_inf_med = median(income_inf$amnt, na.rm = T))
# income median check  value weighted
(income_50_med_w = weighted.median(income_50$amnt, income_50$w1, na.rm = T))
(income_100_med_w = weighted.median(income_100$amnt, income_100$w1, na.rm = T))
(income_inf_med_w = weighted.median(income_inf$amnt, income_inf$w1, na.rm = T))
#
# # adding a column: Share trans in whole sample by incom
# income_17_50_sample = subset(d2, income_hh < 12 & year == 2017)
# income_17_100_sample = subset(d2, income_hh >= 12 & income_hh < 15 & year == 2017)
# income_17_inf_sample = subset(d2, income_hh >= 15 & year == 2017)
# income_18_50_sample = subset(d2, income_hh < 50000 & year == 2018)
# income_18_100_sample = subset(d2, income_hh >= 50000 & income_hh < 100000 & year == 2018)
# income_18_inf_sample = subset(d2, income_hh >= 100000 & year == 2018)
# #
# # combine the years
# income_50_sample = rbind(income_17_50_sample, income_18_50_sample)
# income_100_sample = rbind(income_17_100_sample, income_18_100_sample)
# income_inf_sample = rbind(income_17_inf_sample, income_18_inf_sample)
# nrow(d2)
# nrow(income_50_sample)+nrow(income_100_sample)+nrow(income_inf_sample)
# (income_50_sample_perc = nrow(income_50_sample)/nrow(d2))
# (income_100_sample_perc = nrow(income_100_sample)/nrow(d2))
# (income_inf_sample_perc = nrow(income_inf_sample)/nrow(d2))

# Construcing the income table (the vol and val weighted vectors have already been defined above)
(income_groups = c("[0_50k)", "[50k_100k)", "[100k_inf)"))
#(income_vol_perc = c(income_50_vol_perc, income_100_vol_perc, income_inf_vol_perc))
#(income_val_perc = c(income_50_val_perc, income_100_val_perc, income_inf_val_perc))
(income_avg_w = round(c(income_50_avg_w, income_100_avg_w, income_inf_avg_w), digits = 2))
(income_med_w = round(c(income_50_med_w, income_100_med_w, income_inf_med_w), digits = 2))
#(income_sample_perc = c(income_50_sample_perc, income_100_sample_perc, income_inf_sample_perc))

#
(income_df = data.frame("Income_group" = income_groups, "Volume_share_(w)" =  income_vol_perc_w, "Value_share_(w)" =  income_vol_perc_w, "Average_check_value_(w)" = income_avg_w, "Median_check_value" = income_med_w))
#
write.table(income_df, file = "check_income.txt", sep = ",", quote = F, row.names = F)
dir()# 

# ### New income table:  Comparison of shares in sample to volume shares of checks written
# nrow(subset(d2, pi==2 & payment==1))# total number of checks written as payments
# (income_share_check = percent(income_vol/nrow(subset(d2))))
# sum(income_share_check)# Less than 10% check payments out of all payments
# # make it a table
# (income_share_check_df = data.frame("Income_group" = income_groups, "Share_made_with_check"= income_share_check))
# write.table(income_share_check_df, file = "income_share_check.txt", sep = ",", quote = F, row.names = F)


### Day of the week
# # adding a day-of-the-week column (1:7)
# check_trans_17_18$wday = wday(check_trans_17_18$date)
# head(check_trans_17_18$wday)
# # vol by day
# (check_day_vol = check_trans_17_18 %>% group_by(wday) %>% summarise("vol"=n()))
# (check_day_vol = as.vector(check_day_vol$vol))
# (check_day_vol_frac = check_day_vol/sum(check_day_vol))
# sum(check_day_vol_frac)
# # val by day
# (check_day_val = check_trans_17_18 %>% group_by(wday) %>% summarise("val"= sum(amnt, na.rm = T)))
# (check_day_val = as.vector(check_day_val$val))
# (check_day_val_frac = check_day_val/sum(check_day_val))
# sum(check_day_val_frac)
# # avg check value al by day
# (check_day_avg = check_trans_17_18 %>% group_by(wday) %>% summarise("avg"= mean(amnt, na.rm = T)))
# (check_day_avg = as.vector(check_day_avg$avg))
# # med check value al by day
# (check_day_med = check_trans_17_18 %>% group_by(wday) %>% summarise("med"= median(amnt, na.rm = T)))
# (check_day_med = as.vector(check_day_med$med))
# #
# # Consutructing day-of-the-week table
# (day_group = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
# (check_day_df = data.frame("Day" = day_group, "Volume_share" = percent(check_day_vol_frac), "Value_share" = percent(check_day_val_frac), "Average_check_value" = round(check_day_avg, digits = 2), "Median_check_value" = round(check_day_med, digits = 2)))
# #
# write.table(check_day_df, file = "check_day.txt", sep = ",", quote = F, row.names = F)
# dir()# 

### check_190823.R adding new sections on income (types), Line 870
# Note: d1 has entire data including payment==0. d2 is restricted to payment==1 (not suitable for income analysis)
table(d1$payment)
nrow(d2)

i1 = subset(d1, type=="income")# subsets of income only
nrow(i1) # num of payment transactions
length(unique(i1$id)) # num respondents
#
sum(i1$weight_1)# weights are almost perfect
i1$w_1 = nrow(i1)*i1$weight_1/sum(i1$weight_1)# rescale weights
sum(i1$w_1)
names(i1)
table(i1$income_type)# type of income 
#
table(i1$income_howpaid) # means of payment, 4=paper check
nrow(i1[is.na(i1$income_howpaid), ]) # num missing means of payment (removed)
#
i1_2017 = subset(i1, year==2017)
nrow(i1_2017) # how many income payments received in 2017
i1_2018 = subset(i1, year==2018)
nrow(i1_2018) # how many income payments received in 2018
nrow(i1_2017[is.na(i1_2017$income_howpaid), ]) # num missing means of payment 2017
nrow(i1_2018[is.na(i1_2018$income_howpaid), ]) # num missing means of payment 2018
#
# remove NA from means of payment (how paid?)
i2 = i1[!is.na(i1$income_howpaid), ]
nrow(i2)# num income received after how paid NAs removed
sum(i2$w_1)
i2$w_1 = nrow(i2)*i2$weight_1/sum(i2$weight_1)# rescale weights
sum(i2$w_1)
#
(income_check_frac = nrow(subset(i2, income_howpaid=="4"))/nrow(i2))# frac check income unweighted
(income_check_frac_w = sum(subset(i2, income_howpaid=="4")$w_1)/nrow(i2))# frac check weighted
#
i2_checks = subset(i2, income_howpaid=="4") # income by check only
nrow(i2_checks)
sum(i2_checks$w_1)
table(i2_checks$income_type)
percent(prop.table(table(i2_checks$income_type)))
#
# constructing income table using i2 # page 181 2018 DCPC codebook
(Income_type = c("Employment", "Employer_retirement", "Self_employment", "Social_security", "Interest_dividends", "Rental_income", "Govt_assistance", "Alimony", "Child_support", "Other_retirement_fund", "Total"))
#
(All_income_num = i2 %>% group_by(income_type) %>% summarise(n()))# unweighted
(All_income_frac = All_income_num[,"n()"]/nrow(i2))# unweighted
(All_income_num_w = i2 %>% group_by(income_type) %>% summarise(sum(w_1)))# weighted
(All_income_frac_w = as.vector(All_income_num_w[,"sum(w_1)"]/sum(i2$w_1)))# weighted
(sum(All_income_frac_w))# verify sum to 1
(All_income_frac_w = pull(All_income_frac_w))# pull converts tibble (column) to vector
(All_income_frac_w = c(All_income_frac_w, sum(All_income_frac_w)))# adding total sum
#
(Paid_by_check_num = i2_checks %>% group_by(income_type, .drop=FALSE) %>% summarise(n()))# unweighted
(Paid_by_check_frac = Paid_by_check_num[,"n()"]/nrow(i2))# unweighted
(Paid_by_check_num_w = i2_checks %>% group_by(income_type, .drop=FALSE) %>% summarise(sum(w_1)))# weighted
(Paid_by_check_num_w = pull(Paid_by_check_num_w))
(Paid_by_check_frac_w = Paid_by_check_num_w/sum(i2$w_1))
(sum(Paid_by_check_frac_w))# correct, 14.5% = % of income paid with paper checks.
(Paid_by_check_frac_w = c(Paid_by_check_frac_w, sum(Paid_by_check_frac_w)))# adding total sum
#
(Min_check_val = i2_checks %>% group_by(income_type, .drop=FALSE) %>% summarise(min(amnt, na.rm = T)))
(Min_check_val = pull(Min_check_val))# pull converts tibble (column) to vector
(Min_check_val = c(Min_check_val, NA))# adding NA in Total row
(Max_check_val = i2_checks %>% group_by(income_type, .drop=FALSE) %>% summarise(max(amnt, na.rm = T)))
(Max_check_val = pull(Max_check_val))# pull converts tibble (column) to vector
(Max_check_val = c(Max_check_val, NA))# adding NA in Total row
(Med_check_val = i2_checks %>% group_by(income_type, .drop=FALSE) %>% summarise(median(amnt, na.rm = T)))
(Med_check_val = pull(Med_check_val))# pull converts tibble (column) to vector
(Med_check_val = c(Med_check_val, NA))# adding NA in Total row
(Avg_check_val = i2_checks %>% group_by(income_type, .drop=FALSE) %>% summarise(weighted.mean(amnt, w_1, na.rm = T)))
(Avg_check_val = pull(Avg_check_val))# pull converts tibble (column) to vector
(Avg_check_val = c(Avg_check_val, NA))# adding NA in Total row
#
# Finalizing the income table
(income_recv_df = data.frame(Income_type, "All_income" = percent(All_income_frac_w), "Paid_by_check" = percent(Paid_by_check_frac_w), "Min_check_val" = round(Min_check_val, digits = 2), "Max_check_val" = round(Max_check_val, digits = 2), "Med_check_val" = round(Med_check_val, digits = 2), "Avg_check_val" = round(Avg_check_val, digits = 2)  ))
names(income_recv_df)
write.table(income_recv_df, file = "check_income_recv.txt", sep = ",", quote = F, row.names = F)
dir()# 

### check_190917.R adding fraction of checks in total deposits
# Note: d1 has entire data including payment==0. d2 is restricted to payment==1 (not suitable for income analysis)
table(d1[d1$year == 2017, ]$check_dep_src)
table(d1[d1$year == 2018, ]$check_dep_src)
#
table(d1$check_dep_src)
percent(prop.table(table(d1$check_dep_src)))

### check_200305.R finding perc of cash for the NFTV blog with Claire
# recall from line 73
(check_total_18 = nrow(subset(d2, year==2018 & pi==2)))# total 2018 checks written
(check_frac_18 = check_total_18/num_trans_18)
# do the same for cash
(cash_total_18 = nrow(subset(d2, year==2018 & pi==1)))# total 2018 cash payments
(cash_frac_18 = cash_total_18/num_trans_18)

# total num payments by age group
(total_age_18 = nrow(subset(d2, year==2018 & age < 25)))
(total_age_25 = nrow(subset(d2, year==2018 & age >= 25 & age <= 34)))
(total_age_35 = nrow(subset(d2, year==2018 & age >= 35 & age <= 44)))
(total_age_45 = nrow(subset(d2, year==2018 & age >= 45 & age <= 54)))
(total_age_55 = nrow(subset(d2, year==2018 & age >= 55 & age <= 64)))
(total_age_65 = nrow(subset(d2, year==2018 & age >= 65)))

# check payments by age groups: 
(check_age_18 = nrow(subset(d2, year==2018 & pi==2 & age < 25)))
(check_age_25 = nrow(subset(d2, year==2018 & pi==2 & age >= 25 & age <= 34)))
(check_age_35 = nrow(subset(d2, year==2018 & pi==2 & age >= 35 & age <= 44)))
(check_age_45 = nrow(subset(d2, year==2018 & pi==2 & age >= 45 & age <= 54)))
(check_age_55 = nrow(subset(d2, year==2018 & pi==2 & age >= 55 & age <= 64)))
(check_age_65 = nrow(subset(d2, year==2018 & pi==2 & age >= 65)))

# cash payments by age groups: 
(cash_age_18 = nrow(subset(d2, year==2018 & pi==1 & age < 25)))
(cash_age_25 = nrow(subset(d2, year==2018 & pi==1 & age >= 25 & age <= 34)))
(cash_age_35 = nrow(subset(d2, year==2018 & pi==1 & age >= 35 & age <= 44)))
(cash_age_45 = nrow(subset(d2, year==2018 & pi==1 & age >= 45 & age <= 54)))
(cash_age_55 = nrow(subset(d2, year==2018 & pi==1 & age >= 55 & age <= 64)))
(cash_age_65 = nrow(subset(d2, year==2018 & pi==1 & age >= 65)))
#
(frac_check_18 = check_age_18/total_age_18)
(frac_check_25 = check_age_25/total_age_25)
(frac_check_35 = check_age_35/total_age_35)
(frac_check_45 = check_age_45/total_age_45)
(frac_check_55 = check_age_55/total_age_55)
(frac_check_65 = check_age_65/total_age_65)
#
round(100*frac_check_18, digits = 0)
round(100*frac_check_25, digits = 0)
round(100*frac_check_35, digits = 0)
round(100*frac_check_45, digits = 0)
round(100*frac_check_55, digits = 0)
round(100*frac_check_65, digits = 0)

(frac_cash_18 = cash_age_18/total_age_18)
(frac_cash_25 = cash_age_25/total_age_25)
(frac_cash_35 = cash_age_35/total_age_35)
(frac_cash_45 = cash_age_45/total_age_45)
(frac_cash_55 = cash_age_55/total_age_55)
(frac_cash_65 = cash_age_65/total_age_65)
#
round(100*frac_cash_18, digits = 0)
round(100*frac_cash_25, digits = 0)
round(100*frac_cash_35, digits = 0)
round(100*frac_cash_45, digits = 0)
round(100*frac_cash_55, digits = 0)
round(100*frac_cash_65, digits = 0)

# percentage of check payments during 2017-18
(check_total_17_18 = nrow(subset(d2, pi==2)))# total checks 2017-18
(check_frac_17_18 = check_total_17_18/num_trans_17_18)
# percentage of check payments during 2017-18
(cash_total_17_18 = nrow(subset(d2, pi==1)))# total cash 2017-18
(cash_frac_17_18 = cash_total_17_18/num_trans_17_18)
