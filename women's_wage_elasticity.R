#====================
# Dynamic Modeling Project 
#====================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# for Lindsey:
# df <- read_csv("/Users/lindsey/Documents/Third\ Year/second\ quarter/dynamic\ modeling/cps_00006.csv")

# for sylvia 
df <- read_csv("~/Desktop/cps_00006.csv")
colnames(df) <- tolower(colnames(df)) 
#====================
# Section 1: Creating Subsample of the peeps need 
#====================

#  filtering out people who dont have march info (sylvia is not 100% if this kosher)
df <- filter(df, !is.na(marbasecidp)) 
# lindsey thinks we should just use the supplement
# since this has the march basic info we downloaded as well
# so this would be the code instead:
df <- filter(df, asecflag==1) 

# filtering out people with ages not in our range
listofpeepswithgoodages <- unique(df[(df$age <= 54 & 25 <= df$age ),]$cpsid) # all peeps with bad ages
df <- filter(df, cpsid %in% listofpeepswithgoodages) 

#====================
# Section 2: filtering out everyone besides married heads of houses and spouses
#====================

# marst=1:	Married, spouse present
# relate=0101: Head/householder
# relate= 0201: Spouse
df <- df %>%
  filter(marst==1) %>%
  filter(relate==101|relate==201)

#====================
# Section 3: filtering out everyone in the armed forces
#====================

# classwkr=26: Armed forces
df <- df %>%
  filter(!(classwkr==26)) 

#====================
# Section 4: Creating Columns for Spouse information 
#====================


#====================
# Section 5: Adjusting wage information for inflation ala 2000
#====================


#====================
# Section 6: Adjusting sample weight so every year has the same weight
#====================

newwt <- df %>% group_by(year) %>% 
  summarize(sumw = sum(as.numeric(wtsupp))) %>%
  mutate(scale = sumw[1]/sumw)
df <- df %>% left_join(newwt) %>%
  mutate(wtsupp2 = scale*as.numeric(wtsupp))

#====================
# Section 7: Imputing wages 
#====================
  
df <- df %>% mutate(period=ifelse(year<=1981,"79-81",ifelse(year<=1991,"89-91",ifelse(year<=2001,"99-01","09-11")))) %>%
  mutate(wkswork3 = ifelse(wkswork1<=20,0,1))

# the regression is going to be: 
# lm(hourwage ~ factor(period) + factor(sex) + factor(wkswork3), df)
sketchywage <- lm(hourwage ~ factor(period) + factor(sex) + factor(wkswork3), df)
# but we need a better measure of average hourly wage since hourwage isnt complete



