# check_merge+2017_2018_190917.R adding trans var "check_dep_src" (source of checking accout deposit)
# check_merge+2017_2018_190823.R Adding transaction variables: income_type, income_howpaid, ind_payee, payee, purpose,
# check_merge+2017_2018_190718.R Adding weights_1 and weights_2 that fit the combined 2017-18 sample. 
#check_use_merge_2017_2018_190621.R Removing September and November obs 
#check_use_merge_2017_2018_190620.R Merging 2017 and 2018 data
### The following packages are used:
#library(formattable)# has percent function
#library(plotrix)# weighted histograms
library(dplyr)
library(lubridate)# needed to remove the months of 9 and 11
#library(xtable)# for LaTeX tables
#library(ggplot2)
setwd("~/Papers/non-academic/check/check_coding/check_merge_2017_2018") # Change working directory if needed!!!
dir()

### Reading RDS datasets
trans_18_2 = readRDS("dcpc_2018_tranlevel.rds")# read diary transation based
indiv_18_2 = readRDS("dcpc_2018_indlevel.rds")# read diary individual based
#day_18_2 = readRDS("dcpc_2018_daylevel.rds")# read diary day based
#scpc_18_2 = readRDS("scpc_2018.rds")# read survey data
objects()
trans_17_2 = readRDS("dcpc_2017_tranlevel.rds")# read diary transation based
indiv_17_2 = readRDS("dcpc_2017_indlevel.rds")# read diary individual based
#day_17_2 = readRDS("dcpc_2017_daylevel.rds")# read diary day based
#scpc_17_2 = readRDS("scpc_2017.rds")# read survey data
joint_weights = readRDS("joint_2017_2018_weights.rds")
objects()

dim(trans_17_2)# num trans in 2017
dim(trans_18_2)# num trans in 2018
length(unique(trans_17_2$prim_key))# num resp in 2017
length(unique(trans_18_2$uasid))# num resp in 2018
nrow(joint_weights)# total num rep in both years (joint weights)
names(joint_weights)
# renaming prime_key in 2017 to uasid to match the 2018 ID
trans_17_3 = trans_17_2
trans_18_3 = trans_18_2
colnames(trans_17_3)[colnames(trans_17_3)=="prim_key"] = "uasid"
#
# How many 2017 resp participated in the 2018 diary?
length(unique(trans_17_3[trans_17_3$uasid %in% trans_18_3$uasid,]$uasid))
# How many 2018 resp participated in the 2017 diary?
length(unique(trans_18_3[trans_18_3$uasid %in% trans_17_3$uasid,]$uasid))
# How many 2017 resp did not participate in the 2018 diary?
length(unique(trans_17_3[!(trans_17_3$uasid %in% trans_18_3$uasid),]$uasid))
# How many 2018 resp did not participate in the 2017 diary?
length(unique(trans_18_3[!(trans_18_3$uasid %in% trans_17_3$uasid),]$uasid))
#
## Selecting  and fixing transaction variables
names(trans_17_3)
trans_17_4 = subset(trans_17_3, select = c("uasid", "amnt", "pi", "merch", "in_person", "date", "time", "type", "bill", "payment", "income_type", "income_howpaid", "ind_payee", "payee", "purpose", "check_dep_src"))
trans_18_4 = subset(trans_18_3, select = c("uasid", "amnt", "pi", "merch", "in_person", "date", "time", "type", "bill", "payment", "income_type", "income_howpaid", "ind_payee", "payee", "purpose", "check_dep_src"))
str(trans_17_4)
trans_17_4$uasid = as.factor(trans_17_4$uasid)
str(trans_18_4)
trans_18_4$uasid = as.factor(trans_18_4$uasid)
#
table(trans_17_4$in_person)
trans_17_4$in_person = as.factor(trans_17_4$in_person)
table(trans_18_4$in_person)
trans_18_4$in_person = as.factor(trans_18_4$in_person)
#
table(trans_17_4$pi)
trans_17_4$pi = as.factor(trans_17_4$pi)
table(trans_18_4$pi) 
nrow(trans_18_4)
trans_18_4$pi = as.factor(trans_18_4$pi)
table(trans_18_4$pi)
#
table(trans_17_4$merch)
trans_17_4$merch = as.factor(trans_17_4$merch)
table(trans_18_4$merch)
trans_18_4$merch = as.factor(trans_18_4$merch)
#
str(trans_17_4)
table(trans_17_4$type)# Note: unreliable as some transfers are paymements
trans_17_4$type = as.factor(trans_17_4$type)
levels(trans_17_4$type)
levels(trans_17_4$type) = c("expenditure", "transfer", "income")
table(trans_17_4$type)
str(trans_18_4)
table(trans_18_4$type)
trans_18_4$type = as.factor(trans_18_4$type)
levels(trans_18_4$type)
levels(trans_18_4$type) = c("expenditure", "transfer", "income")
table(trans_18_4$type)
#
table(trans_17_4$payee)
trans_17_4$payee =  as.factor(trans_17_4$payee)
levels(trans_17_4$payee)
table(trans_17_4$payee)
table(trans_18_4$payee)
trans_18_4$payee =  as.factor(trans_18_4$payee)
levels(trans_18_4$payee)
levels(trans_18_4$payee) = levels(trans_17_4$payee)
table(trans_18_4$payee)
#
str(trans_17_4)
table(trans_17_4$bill)
trans_17_4$bill = as.factor(trans_17_4$bill)
str(trans_18_4)
table(trans_18_4$bill)
trans_18_4$bill = as.factor(trans_18_4$bill)
#
str(trans_17_4)
table(trans_17_4$payment)
trans_17_4$payment = as.factor(trans_17_4$payment)
str(trans_18_4)
table(trans_18_4$payment)
trans_18_4$payment = as.factor(trans_18_4$payment)
#
trans_17_4$time = as.integer(trans_17_4$time)
#
str(trans_17_4)
table(trans_17_4$income_type)
trans_17_4$income_type = as.factor(trans_17_4$income_type)
str(trans_18_4)
table(trans_18_4$income_type)
trans_18_4$income_type = as.factor(trans_18_4$income_type)
#
str(trans_17_4)
table(trans_17_4$purpose)
trans_17_4$purpose = as.factor(trans_17_4$purpose)
str(trans_18_4)
table(trans_18_4$purpose)
trans_18_4$purpose = as.factor(trans_18_4$purpose)
#
str(trans_17_4)
table(trans_17_4$income_howpaid)
trans_17_4$income_howpaid = as.factor(trans_17_4$income_howpaid)
str(trans_18_4)
table(trans_18_4$income_howpaid)
trans_18_4$income_howpaid = as.factor(trans_18_4$income_howpaid)
#
str(trans_17_4)
table(trans_17_4$ind_payee)
trans_17_4$ind_payee = as.factor(trans_17_4$ind_payee)
str(trans_18_4)
table(trans_18_4$ind_payee)
trans_18_4$ind_payee = as.factor(trans_18_4$ind_payee)
#
str(trans_17_4)
table(trans_17_4$payee)
trans_17_4$payee = as.factor(trans_17_4$payee)
str(trans_18_4)
table(trans_18_4$payee)
trans_18_4$payee = as.factor(trans_18_4$payee)
#
str(trans_17_4)
table(trans_17_4$purpose)
trans_17_4$purpose = as.factor(trans_17_4$purpose)
str(trans_18_4)
table(trans_18_4$purpose)
trans_18_4$purpose = as.factor(trans_18_4$purpose)

## Selecting individual data
names(indiv_17_2)
indiv_17_3 = indiv_17_2
colnames(indiv_17_3)[colnames(indiv_17_3)=="prim_key"]="uasid" #rename to match the 2018 resp ID
indiv_17_3 = subset(indiv_17_3, select = c("uasid", "ind_weight", "cc_num", "dc_num", "work_employed", "work_onleave", "work_temp_unemployed", "work_looking", "work_retired", "work_disabled", "work_oth", "work_occupation", "work_self", "marital_status", "highest_education", "hispaniclatino", "hispaniclatino_group", "race_white", "race_black", "race_asian", "race_other", "hh_size", "age", "income_hh", "gender"))
length(unique(indiv_17_3$uasid)) # num respondents indiv 2017
colnames(indiv_17_3)[colnames(indiv_17_3)=="work_oth"] = "work_other" # match 2017 "work_oth" with 2018 "work_other"
indiv_18_3 = subset(indiv_18_2, select = c("uasid", "ind_weight", "cc_num", "dc_num", "work_employed", "work_onleave", "work_temp_unemployed", "work_looking", "work_retired", "work_disabled", "work_other", "work_occupation", "work_self", "marital_status", "highest_education", "hispaniclatino", "hispaniclatino_group", "race_white", "race_black", "race_asian", "race_other", "hh_size", "age", "income_hh", "gender"))
length(unique(indiv_18_3$uasid)) # num respondents indiv 2018
#
## Adjusting factors to the indiv dataframes (equalizing definitions between 2017 and 2018), defining new variables (keep all originals)
indiv_17_4 = indiv_17_3 
indiv_18_4 = indiv_18_3 
str(indiv_17_4)
str(indiv_18_4)
#
indiv_17_4$uasid = as.factor(indiv_17_4$uasid)
indiv_18_4$uasid = as.factor(indiv_18_4$uasid)
#
table(indiv_17_4$work_employed)
indiv_17_4$employed = indiv_17_4$work_employed # new variable
indiv_17_4 = indiv_17_4 %>%  mutate(employed = replace(employed, employed != 0, "employed"))
indiv_17_4 = indiv_17_4 %>%  mutate(employed = replace(employed, employed == 0, "not_employed"))
indiv_17_4$employed = as.factor(indiv_17_4$employed)
table(indiv_17_4$employed)
table(indiv_18_4$work_employed)
indiv_18_4$employed = indiv_18_4$work_employed
levels(indiv_18_4$employed)
levels(indiv_18_4$employed) = c("not_employed", "employed")
table(indiv_18_4$employed)
#
table(indiv_17_4$marital_status)
str(indiv_17_4$marital_status)
indiv_17_4$married = indiv_17_4$marital_status # new variable initialized
indiv_17_4 = indiv_17_4 %>%  mutate(married = replace(married, married <= 2, "married"))
indiv_17_4 = indiv_17_4 %>%  mutate(married = replace(married, married >= 3 & married <= 6, "not_married"))
indiv_17_4$married =  as.factor(indiv_17_4$married)
table(indiv_17_4$married)
str(indiv_17_4$married)
head(indiv_17_4$married)
#
str(indiv_18_4$marital_status)
table(indiv_18_4$marital_status)
indiv_18_4$married = indiv_18_4$marital_status # new variable initialize
indiv_18_4 = indiv_18_4 %>%  mutate(married = replace(married, married <= 2, "married"))
indiv_18_4 = indiv_18_4 %>%  mutate(married = replace(married, married >= 3 & married <= 6, "not_married"))
indiv_18_4$married =  as.factor(indiv_18_4$married)
table(indiv_18_4$married)
str(indiv_18_4$married)
head(indiv_18_4$married)
#
table(indiv_17_4$gender)
str(indiv_17_4$gender)
indiv_17_4$gender = as.factor(indiv_17_4$gender)
levels(indiv_17_4$gender)
levels(indiv_17_4$gender) = c("female", "male")
table(indiv_17_4$gender)
levels(indiv_18_4$gender)
levels(indiv_18_4$gender) = c("female", "male")
table(indiv_18_4$gender)
#
indiv_17_4$education = indiv_17_4$highest_education
str(indiv_17_4$education)
indiv_17_4$education = as.factor(indiv_17_4$education)
table(indiv_17_4$education)
indiv_18_4$education = indiv_18_4$highest_education
str(indiv_18_4$education)
table(indiv_18_4$education)
levels(indiv_18_4$education)
levels(indiv_18_4$education) = c(1:16)
table(indiv_18_4$education)
#
indiv_17_4$hh_size = as.integer(indiv_17_4$hh_size)
table(indiv_17_4$hh_size)
str(indiv_18_4$hh_size)
table(indiv_18_4$hh_size)
# converting 2017 income to actual income (to match 2018)
str(indiv_17_4$income_hh)
table(indiv_17_4$income_hh)
indiv_17_4$income = indiv_17_4$income_hh # initialize 2017 income
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==1, 2500))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==2, (7500+5000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==3, (10000+7500)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==4, (12500+10000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==5, (12500+15000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==6, (20000+15000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==7, (20000+25000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==8, (25000+30000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==9, (30000+35000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==10, (35000+40000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==11, (40000+50000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==12, (50000+60000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==13, (60000+75000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==14, (75000+100000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==15, (100000+125000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==16, (125000+200000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==17, (200000+500000)/2))
indiv_17_4 = indiv_17_4 %>% mutate(income = replace(income, income==18, 500000))
str(indiv_17_4$income)
indiv_17_4$income = as.integer(indiv_17_4$income)
table(indiv_17_4$income)
str(indiv_18_4$income_hh)
indiv_18_4$income = indiv_18_4$income_hh
#
str(indiv_17_4)
str(indiv_18_4)
# Adding new joint weights to indiv datasets
indiv_17_4 = merge(x = indiv_17_4, y = joint_weights, all.x = T)
names(indiv_17_4)
indiv_18_4 = merge(x = indiv_18_4, y = joint_weights, all.x = T)
names(indiv_18_4)
#
# Merge indiv data into trans data
length(unique(trans_17_4$uasid))
length(unique(indiv_17_4$uasid)) # has more respondents than trans 
#
d_17 = merge(x=trans_17_4, y=indiv_17_4, all.x = T)
str(d_17)
nrow(d_17)
length(unique(d_17$uasid))
d_17$year = 2017 # year of the diary (note: also included in "date")
#
length(unique(trans_18_4$uasid))
length(unique(indiv_18_4$uasid)) # has more respondents than trans 
nrow(trans_18_4)
#
d_18 = merge(x=trans_18_4, y=indiv_18_4, all.x = T)
str(d_18)
nrow(d_18)
length(unique(d_18$uasid))
d_18$year = 2018 # year of the diary (note: also included in "date")
#
# Removing September and November transactions
nrow(filter(d_17, month(date)==9))# num trans in Sept 2017
nrow(filter(d_17, month(date)==11))# num trans in Nov 2017
d_17b = filter(d_17, month(date)!=9)
nrow(d_17b) -  nrow(d_17)
d_17c = filter(d_17b, month(date)!=11)
nrow(d_17c) -  nrow(d_17b)
nrow(d_17c) -  nrow(d_17)# total num trans removed from 9/17 and 11/17
#
nrow(filter(d_18, month(date)==9))# num trans in Sept 2018
nrow(filter(d_18, month(date)==11))# num trans in Nov 2018
d_18b = filter(d_18, month(date)!=9)
nrow(d_18b) -  nrow(d_18)
d_18c = filter(d_18b, month(date)!=11)
nrow(d_18c) -  nrow(d_18b)
nrow(d_18c) -  nrow(d_18)# total num trans removed from 9/18 and 11/18

### Merging the 2017 into 2018 transaction data: Combine d17 trans with d18 trans
dim(d_17c)
dim(d_18c)
names(d_17c)
names(d_18c)
d1 = rbind(d_17c, d_18c)
nrow(d1) # num trans in the 2017/8 merged datasets
length(unique(d1$uasid))# num resp in the merged dataset
# Below, testing if the sum of weights over unique ID equals approx to the number of unique ID

# Just checking if the sum of both weights sum up to the num of respondents
id = unique(d1$uasid)
head(id)
id.df = data.frame("uasid" = id)
dim(id.df)
head(id.df)
head(joint_weights)
dim(joint_weights)
test_weights =  merge(x=id.df, y=joint_weights, all.x = T)
dim(test_weights)
names(test_weights)
str(joint_weights)
sum(test_weights$weight_1)
sum(test_weights$weight_2)
# compare with 
length(unique(d1$uasid))# num resp in the merged dataset
#
# check sum of weights over all trans (will need to rescale as needed)
nrow(d1)
sum(d1$weight_1)
sum(d1$weight_2)
#
### Saving file to be used in this paper

saveRDS(d1, "check_merged_2017_2018_190917.rds")
# The coding for "check_use" paper  relies only on this .rds file
dir()

#### End of merging 2017 trans into 2018 diary with 2 new joint 2017-18 weights

