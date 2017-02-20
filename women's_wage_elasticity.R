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
listofpeepswithgoodwages <- unique(df[(df$age <= 54 & 25 <= df$age ),]$cpsid) # all peeps with bad ages
df <- filter(df, cpsid %in% listofpeepswithgoodwages) 

# filtering out rows with children 
# idk how to do this :( 

#====================
# Section 2: Creating Columns for Spouse information 
#====================



#====================
# Section 3: Adjusting wage information for inflation ala 2000
#====================








