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
# Section 4: filtering out everyone who's spouse is not longer in the dataset
#====================

#====================
# Section 5: Creating Columns for Spouse information 
#====================



#====================
# Section 6: Adjusting wage information for inflation ala 2000
#====================








