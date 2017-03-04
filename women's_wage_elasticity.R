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
# varriable to be converted 
df$hourwage <- as.numeric(df$hourwage)
df$wkswork1 <- as.numeric(df$wkswork1)
df$uhrsworkly <- as.numeric(df$uhrsworkly)
df$wtsupp <- as.numeric(df$wtsupp)



#====================
# Section 1: Creating Subsample of the peeps need 
#====================


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
# 1 dollar in 2000 was this much, 
inflation <- data.frame(year = c(1979, 1980, 1981, 1989, 1990, 1991, 1999, 
                                 2000, 2001, 2009, 2010, 2011) ,
                        price = c(2.37, 2.09, 1.89, 1.39, 1.32, 1.26, 1.03, 1.00, 
                                  0.97, 0.80, 0.79, 0.77))
df$hourwage <- (df$hourwage)







foo <- c(12, 22, 33)
names(foo) <- c("tic", "tac", "toe")
foo[["tic"]]



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



