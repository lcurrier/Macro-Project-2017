#====================
# Dynamic Modeling Project 
#====================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(stargazer)

# for Lindsey:
# df <- read_csv("/Users/lindsey/Documents/Third\ Year/second\ quarter/dynamic\ modeling/cps_00009.csv")

# for sylvia 
df <- read_csv("~/Desktop/cps_00009.csv")
colnames(df) <- tolower(colnames(df)) 
# varriable to be converted 
df$hourwage <- as.numeric(df$hourwage)
df$wkswork1 <- as.numeric(df$wkswork1)
df$uhrsworkly <- as.numeric(as.character(df$uhrsworkly))
df[(df$uhrsworkly >= 999 & !is.na(df$uhrsworkly)),]$uhrsworkly <- 0
df$wtsupp <- as.numeric(df$wtsupp)
df$incwage <- as.numeric(df$incwage)
df$sex <- df$sex - 1 
df$annualhours <- df$wkswork1*df$uhrsworkly
df$incomeI <-  pmax( (pmax(as.numeric(as.character(df$ftotval)), 0) -
                     pmax(as.numeric(as.character(df$inctot)), 0)), 0)

df$educ_4factor <- 0
df[(df$educ <= 072),]$educ_4factor <- 1
df[(df$educ %in% c(73:110, 120:122) ),]$educ_4factor <- 2 
df[(df$educ == 111 ),]$educ_4factor <- 3
df[(df$educ >= 123 ),]$educ_4factor <- 4
df$age2 <- df$age^2
df <- df %>% mutate(metro=ifelse(metro==0|9,1,metro)) 
df <- df %>% mutate(metro=ifelse(metro>=3,2,metro))
df$raceclean <- "other"
df <- df %>% mutate(raceclean =ifelse(race == 100,"white", raceclean))
df <- df %>% mutate(raceclean=ifelse(race == 200,"black", raceclean))
df <- df %>% mutate(raceclean=ifelse( !(hispan %in% c(000,901, 902)) ,"hispan", raceclean))

df <- df %>% mutate(period=ifelse(year<=1981,"79-81",ifelse(year<=1991,"89-91",ifelse(year<=2001,"99-01","09-11")))) %>%
  mutate(wkswork3 = ifelse(uhrsworkly<=20,0,1)) 
#wksweek3 == 0 when person works less than 20 hours per week


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
# sylvia note: jk this messes us up 
# classwkr=26: Armed forces
#df <- df %>%
#  filter(!(classwkr==26)) 

#====================
# Section 4: Calculate wages for 1979-81
#====================

# calcuate wage for the years 79-81 since we cps doesn't provide that
# also make infinities,NIUs,extreme values and self-employed people into NAs 
df <- df %>%
  mutate(hourwage_calcuated = incwage/(wkswork1*uhrsworkly)) %>%
  mutate(hourwage = ifelse(is.na(hourwage),hourwage_calcuated,hourwage)) %>%
  mutate(hourwage = ifelse(hourwage==Inf|hourwage==99.99|classwkr==10|classwkr==13|classwkr==14,NA,hourwage))
# note: still some na's because uhrsworkly is na for unemployed people

#====================
# Section 5: Adjusting wage information for inflation ala 2000
#====================
# 1 dollar in 2000 was this much, 

inflation = c(2.37, 2.09, 1.89, 1.39, 1.32, 1.26, 1.03, 1.00, 0.97, 0.80, 0.79, 0.77)

df$inf <- NA
df[(df$year == 1979),]$inf <- inflation[[1]]
df[(df$year == 1980),]$inf <- inflation[[2]]
df[(df$year == 1981),]$inf <- inflation[[3]]
df[(df$year == 1989),]$inf <- inflation[[4]]
df[(df$year == 1990),]$inf <- inflation[[5]]
df[(df$year == 1991),]$inf <- inflation[[6]]
df[(df$year == 1999),]$inf <- inflation[[7]]
df[(df$year == 2000),]$inf <- inflation[[8]]
df[(df$year == 2001),]$inf <- inflation[[9]]
df[(df$year == 2009),]$inf <- inflation[[10]]
df[(df$year == 2010),]$inf <- inflation[[11]]
df[(df$year == 2011),]$inf <- inflation[[12]]


df$hourwage <- df$hourwage*df$inf

# remove extreme/unlikely wages
df <- df %>% mutate(hourwage = ifelse(hourwage<2|hourwage>200,NA,hourwage))
#====================
# Section 7: Adjusting sample weight so every year has the same weight
#====================

newwt <- df %>% group_by(year) %>% 
  summarize(sumw = sum(as.numeric(wtsupp))) %>%
  mutate(scale = sumw[1]/sumw)
df <- df %>% left_join(newwt) %>%
  mutate(wtsupp2 = scale*as.numeric(wtsupp))


#====================
# Section 8: Getting Chars of Spouces  
#====================
df$spouseid <- paste(as.character(df$year), as.character(df$serial),
                     as.character(df$sploc), sep = "")
df$personid <- paste(as.character(df$year), as.character(df$serial),
                     as.character(df$pernum), sep = "")

bebespousedata <- data.frame("personid" = df$personid, 
                             "spouseid" = df$spouseid, "personwage" = df$hourwage, 
                             "age" = df$age, 
                             "raceclean" = df$raceclean,
                             "educ" = df$educ_4factor, 
                             "age2" = df$age2)

bebespousedata  <- bebespousedata[match(df$personid, bebespousedata$spouseid),]
df$spouseage <- bebespousedata$age
df$spouseage2 <- bebespousedata$age2
df$spouseraceclean <- as.character(bebespousedata$raceclean)
df$spouse_educ <- bebespousedata$educ

### lindsey suggests instead: 
bebespousedata <- df %>% 
  select(spouseid,hourwage,age,raceclean,educ_4factor,age2) %>%
  rename(spouseage=age,spouseage2=age2,spouseraceclean=raceclean,
         spouse_educ=educ_4factor,spousewage=hourwage)
df <- left_join(df, bebespousedata, c("personid" = "spouseid"))
# (produces exactly same result, so whatever really)
###

#====================
# Section 9: Imputing wages for non-workers
#====================

#======
# regression for wages  79_81
#======

fit79_81_low_fem <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                         factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                         factor(spouseraceclean) + factor(region) + factor(metro), 
                       filter(df, sex == 1, wkswork3 == 0, period == "79-81"),
                       weights = wtsupp2)

fit79_81_low_man <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                         factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                         factor(spouseraceclean) + factor(region) + factor(metro), 
                       filter(df, sex == 0, wkswork3 == 0, period == "79-81"),
                       weights = wtsupp2)

fit79_81_high_fem <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                         factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                         factor(spouseraceclean) + factor(region) + factor(metro), 
                       filter(df, sex == 1, wkswork3 == 1, period == "79-81"),
                       weights = wtsupp2)

fit79_81_high_man <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                         factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                         factor(spouseraceclean) + factor(region) + factor(metro), 
                       filter(df, sex == 0, wkswork3 == 1, period == "79-81"),
                       weights = wtsupp2)

#======
# regression for wages  89-91, 89_91
#======
fit89_91_low_fem <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                         factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                         factor(spouseraceclean) + factor(region) + factor(metro), 
                       filter(df, sex == 1, wkswork3 == 0, period == "89-91"),
                       weights = wtsupp2)

fit89_91_low_man <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                         factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                         factor(spouseraceclean) + factor(region) + factor(metro), 
                       filter(df, sex == 0, wkswork3 == 0, period == "89-91"),
                       weights = wtsupp2)

fit89_91_high_fem <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                          factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                          factor(spouseraceclean) + factor(region) + factor(metro), 
                        filter(df, sex == 1, wkswork3 == 1, period == "89-91"),
                        weights = wtsupp2)

fit89_91_high_man <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                          factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                          factor(spouseraceclean) + factor(region) + factor(metro), 
                        filter(df, sex == 0, wkswork3 == 1, period == "89-91"),
                        weights = wtsupp2)


#======
# regression for wages  99-01, 99_01
#======
fit99_01_low_fem <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                         factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                         factor(spouseraceclean) + factor(region) + factor(metro), 
                       filter(df, sex == 1, wkswork3 == 0, period == "99-01"),
                       weights = wtsupp2)

fit99_01_low_man <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                         factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                         factor(spouseraceclean) + factor(region) + factor(metro), 
                       filter(df, sex == 0, wkswork3 == 0, period == "99-01"),
                       weights = wtsupp2)

fit99_01_high_fem <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                          factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                          factor(spouseraceclean) + factor(region) + factor(metro), 
                        filter(df, sex == 1, wkswork3 == 1, period == "99-01"),
                        weights = wtsupp2)

fit99_01_high_man <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                          factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                          factor(spouseraceclean) + factor(region) + factor(metro), 
                        filter(df, sex == 0, wkswork3 == 1, period == "99-01"),
                        weights = wtsupp2)

#======
# regression for wages  09-11, 09_11
#======
fit09_11_low_fem <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                         factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                         factor(spouseraceclean) + factor(region) + factor(metro), 
                       filter(df, sex == 1, wkswork3 == 0, period == "09-11"),
                       weights = wtsupp2)

fit09_11_low_man <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                         factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                         factor(spouseraceclean) + factor(region) + factor(metro), 
                       filter(df, sex == 0, wkswork3 == 0, period == "09-11"),
                       weights = wtsupp2)

fit09_11_high_fem <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                          factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                          factor(spouseraceclean) + factor(region) + factor(metro), 
                        filter(df, sex == 1, wkswork3 == 1, period == "09-11"),
                        weights = wtsupp2)

fit09_11_high_man <- lm(hourwage ~ age + age2 + spouseage + spouseage2 + 
                          factor(educ_4factor) + factor(spouse_educ) + factor(raceclean) +
                          factor(spouseraceclean) + factor(region) + factor(metro), 
                        filter(df, sex == 0, wkswork3 == 1, period == "09-11"),
                        weights = wtsupp2)


#======
# prediciting 
#======
df$hourwage_predicted <- NA

# predict wages for the everyone with no wages and fill back in 

#======
# predict 79_81
#======
df[(df$sex == 1 & df$wkswork3 == 0 &
      df$period == "79-81"),]$hourwage_predicted <- predict(fit79_81_low_fem, 
                 df[(df$sex == 1 & df$wkswork3 == 0 & df$period == "79-81"),] ) 


df[(df$sex == 0 & df$wkswork3 == 0 &
      df$period == "79-81"),]$hourwage_predicted <- predict(fit79_81_low_man, 
                 df[(df$sex == 0 & df$wkswork3 == 0 & df$period == "79-81"),] ) 


df[(df$sex == 1 & df$wkswork3 == 1 &
      df$period == "79-81"),]$hourwage_predicted <- predict(fit79_81_high_fem, 
                df[(df$sex == 1 & df$wkswork3 == 1 & df$period == "79-81"),] ) 


df[(df$sex == 0 & df$wkswork3 == 1 &
      df$period == "79-81"),]$hourwage_predicted <- predict(fit79_81_high_man, 
                    df[(df$sex == 0 & df$wkswork3 == 1 & df$period == "79-81"),] ) 


#======
# predict 89-91, 89_91
#======

df[(df$sex == 1 & df$wkswork3 == 0 &
      df$period == "89-91"),]$hourwage_predicted <- predict(fit89_91_low_fem, 
                 df[(df$sex == 1 & df$wkswork3 == 0 & df$period == "89-91"),] ) 


df[(df$sex == 0 & df$wkswork3 == 0 &
      df$period == "89-91"),]$hourwage_predicted <- predict(fit89_91_low_man, 
                df[(df$sex == 0 & df$wkswork3 == 0 & df$period == "89-91"),] ) 


df[(df$sex == 1 & df$wkswork3 == 1 &
      df$period == "89-91"),]$hourwage_predicted <- predict(fit89_91_high_fem, 
            df[(df$sex == 1 & df$wkswork3 == 1 & df$period == "89-91"),] ) 


df[(df$sex == 0 & df$wkswork3 == 1 &
      df$period == "89-91"),]$hourwage_predicted <- predict(fit89_91_high_man, 
          df[(df$sex == 0 & df$wkswork3 == 1 & df$period == "89-91"),] ) 



#======
# predict 99-01, 99_01
#======
df[(df$sex == 1 & df$wkswork3 == 0 &
      df$period == "99-01"),]$hourwage_predicted <- predict(fit99_01_low_fem, 
      df[(df$sex == 1 & df$wkswork3 == 0 & df$period == "99-01"),] ) 


df[(df$sex == 0 & df$wkswork3 == 0 &
      df$period == "99-01"),]$hourwage_predicted <- predict(fit99_01_low_man, 
      df[(df$sex == 0 & df$wkswork3 == 0 & df$period == "99-01"),] ) 


df[(df$sex == 1 & df$wkswork3 == 1 &
      df$period == "99-01"),]$hourwage_predicted <- predict(fit99_01_high_fem, 
      df[(df$sex == 1 & df$wkswork3 == 1 & df$period == "99-01"),] ) 


df[(df$sex == 0 & df$wkswork3 == 1 &
      df$period == "99-01"),]$hourwage_predicted <- predict(fit99_01_high_man, 
      df[(df$sex == 0 & df$wkswork3 == 1 & df$period == "99-01"),] ) 

#======
# predict 09-11, 09_11
#======

df[(df$sex == 1 & df$wkswork3 == 0 &
      df$period == "09-11"),]$hourwage_predicted <- predict(fit09_11_low_fem, 
      df[(df$sex == 1 & df$wkswork3 == 0 & df$period == "09-11"),] ) 


df[(df$sex == 0 & df$wkswork3 == 0 &
      df$period == "09-11"),]$hourwage_predicted <- predict(fit09_11_low_man, 
      df[(df$sex == 0 & df$wkswork3 == 0 & df$period == "09-11"),] ) 


df[(df$sex == 1 & df$wkswork3 == 1 &
      df$period == "09-11"),]$hourwage_predicted <- predict(fit09_11_high_fem, 
       df[(df$sex == 1 & df$wkswork3 == 1 & df$period == "09-11"),] ) 


df[(df$sex == 0 & df$wkswork3 == 1 &
      df$period == "09-11"),]$hourwage_predicted <- predict(fit09_11_high_man, 
        df[(df$sex == 0 & df$wkswork3 == 1 & df$period == "09-11"),] ) 


sum(is.na(df$hourwage_predicted))

#====================
# Section 9: Imputing Wages for Spouses 
#====================

bebespousedata <- data.frame("personid" = df$personid, 
                             "spouseid" = df$spouseid, "personwage" = df$hourwage, 
                             "personwage_predicted" = df$hourwage_predicted)

bebespousedata  <- bebespousedata[match(df$personid, bebespousedata$spouseid),]
df$spousewage <- bebespousedata$personwage  # varriable the gives the wage of the spouce 
df$spousewage_predicted <- bebespousedata$personwage_predicted 

#====================
# Section 10: Running Basline Regressions 
#====================


df$logwage <- log( pmax(df$hourwage_predicted, 0) + .000001)
df$logwagegroup <- as.numeric(as.factor(cut(df$logwage, 10)))

mod1 <- lm(annualhours ~ incomeI + logwagegroup 
   + age + age2 + spouseage + spouseage2 + factor(metro) +
     factor(region) + factor(raceclean) + factor(spouseraceclean) +
     factor(year), data = filter(df, year %in% c(1979,1980, 1981), sex == 1),
   weights = wtsupp2)

mod2 <- lm(annualhours ~ incomeI + logwagegroup  
           + age + age2 + spouseage + spouseage2 + factor(metro) +
             factor(region) + factor(raceclean) + factor(spouseraceclean) +
             factor(year), data = filter(df, year %in% c(1989,1990, 1991), sex == 1),
           weights = wtsupp2)


mod3 <- lm(annualhours ~ incomeI + logwagegroup  
           + age + age2 + spouseage + spouseage2 + factor(metro) +
             factor(region) + factor(raceclean) + factor(spouseraceclean) +
             factor(year), data = filter(df, year %in% c(1999,2000, 2001), sex == 1), 
           weights = wtsupp2)

mod4 <- lm(annualhours ~ incomeI + logwagegroup  
           + age + age2 + spouseage + spouseage2 + factor(metro) +
             factor(region) + factor(raceclean) + factor(spouseraceclean) +
             factor(year), data = filter(df, year %in% c(2009,2010, 2011), sex == 1), 
           weights = wtsupp2)


stargazer(mod1, mod2, mod3, mod4)

mean(df[(df$period == "79-81" & df$sex == 1 ),]$annualhours)
mean(df[(df$period == "89-91" & df$sex == 1 ),]$annualhours)
mean(df[(df$period == "99-01" & df$sex == 1 ),]$annualhours)
mean(df[(df$period == "09-11" & df$sex == 1 ),]$annualhours)



plot( df[(df$period == "79-81"),]$hourwage_predicted,df[(df$period == "79-81"),]$hourwage )

max( df[(df$period == "79-81"),]$hourwage_predicted)
max(df[(df$period == "79-81"),]$hourwage, na.rm = T )
sum(df[(df$period == "79-81"),]$hourwage > 30, na.rm = T)


max( df[(df$period == "09-11"),]$hourwage_predicted)
max(df[(df$period == "09-11"),]$hourwage, na.rm = T )
sum(df[(df$period == "09-11"),]$hourwage > 30, na.rm = T)


hist(df[(df$period == "79-81"),]$hourwage_predicted)
hist(df[(df$period == "09-11"),]$hourwage_predicted)

max(df[(df$period == "79-81"),]$hourwage, na.rm = T)
hist(df[(df$period == "09-11"),]$hourwage_predicted)



