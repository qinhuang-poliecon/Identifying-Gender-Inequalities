#### Pathways to Political Participation 2004 Dataset
#### Date: 30/04/2023

rm(list = ls())

### Loading Libraries 

library(tidyverse)
library(haven)
library(dplyr)
library(QCA)
library(SetMethods)


### Loading the Dataset 

load("Data/2004.rda")
data_original <- da36529.0001


table(data_original$S58GEN) ## gender
table(data_original$STUDENTVOTEFINAL1) ## vote
table(data_original$S58GEN, data_original$STUDENTVOTEFINAL1) # female 68% voted, male 56% voted
table(data_original$ZSES)

table(data_original$SPTALK) ## with parents

table(data_original$SACTIVE)

table(data_original$S28COL) ## college

table(data_original$P34FRI) ## with friends


### Data Cleaning

data <- data_original %>%
  select(S58GEN, STUDENTVOTEFINAL1, ZSES, S28COL, P34FRI,SACTIVE
  )


#### Calibration ####

## Calibrate gender

table(data$S58GEN) 
data$S58GEN <- as.character(data$S58GEN)
unique(data$S58GEN)
data$Gender <- NA 
data$Gender[data$S58GEN == "(2) Male"] <- 1
data$Gender[data$S58GEN == "(1) Female"] <- 0
table(data$Gender)



## Calibrate election vote

table(data$STUDENTVOTEFINAL1) ## Voted in 2004
data$STUDENTVOTEFINAL1 <- as.character(data$STUDENTVOTEFINAL1)
unique(data$STUDENTVOTEFINAL1)
data$Vote <- NA 
data$Vote[data$STUDENTVOTEFINAL1 == "(1) no"] <- 0 
data$Vote[data$STUDENTVOTEFINAL1 == "(2) yes"] <- 1 
table(data$Vote)


## college education

table(data$S28COL) 
data$S28COL <- as.character(data$S28COL)
unique(data$S28COL)
data$College <- NA 
data$College[data$S28COL == "5"] <- 1
data$College[data$S28COL == "4"] <- 0
data$College[data$S28COL == "3"] <- 0
data$College[data$S28COL == "2"] <- 0
data$College[data$S28COL == "1"] <- 0
table(data$College)


## family background

table(data$ZSES) 
data$ZSES <- as.character(data$ZSES)
unique(data$ZSES)
data$FB <- NA 
data$FB[data$ZSES == "1.358000495515"] <- 1 
data$FB[data$ZSES == "0.929772914511"] <- 1 
data$FB[data$ZSES == "0.501545333508"] <- 0
data$FB[data$ZSES == "0.073317752505"] <- 0 
data$FB[data$ZSES == "-0.354909828498"] <- 0 
data$FB[data$ZSES == "-0.783137409501"] <- 0 
data$FB[data$ZSES == "-1.211364990505"] <- 0 
data$FB[data$ZSES == "-1.639592571508"] <- 0 
data$FB[data$ZSES == "-2.067820152511"] <- 0 
data$FB[data$ZSES == "-2.496047733514"] <- 0 

table(data$FB)


## socialization with friends

table(data$P34FRI) 
data$P34FRI<- as.character(data$P34FRI)
unique(data$P34FRI)
data$SocFr1 <- NA
data$SocFr1[data$P34FRI == "5"] <- 1 
data$SocFr1[data$P34FRI == "4"] <- 0 
data$SocFr1[data$P34FRI == "3"] <- 0 
data$SocFr1[data$P34FRI == "2"] <- 0 
data$SocFr1[data$P34FRI == "1"] <- 0 
table(data$SocFr1)



## activism

table(data$SACTIVE) 
data$SACTIVE <- as.character(data$SACTIVE)
unique(data$SACTIVE)
data$HighAc <- NA
data$HighAc[data$SACTIVE == "15"] <- 1
data$HighAc[data$SACTIVE == "14"] <- 1
data$HighAc[data$SACTIVE == "13"] <- 1
data$HighAc[data$SACTIVE == "12"] <- 1
data$HighAc[data$SACTIVE == "11"] <- 1
data$HighAc[data$SACTIVE == "10"] <- 1
data$HighAc[data$SACTIVE == "9"] <- 0
data$HighAc[data$SACTIVE == "8"] <- 0
data$HighAc[data$SACTIVE == "7"] <- 0
data$HighAc[data$SACTIVE == "6"] <- 0
data$HighAc[data$SACTIVE == "5"] <- 0
data$HighAc[data$SACTIVE == "4"] <- 0
data$HighAc[data$SACTIVE == "3"] <- 0

table(data$HighAc)


## Saving calibration


data_male <- data %>%
  filter(Gender == 1) %>%
  select(Vote, College, FB, SocFr1, HighAc) 
  
data_female <- data %>%
  filter(Gender == 0) %>%
  select(Vote, College, FB, SocFr1, HighAc) 



### Appendix 8: Male Model ####


data_male <- data %>%
  filter(Gender == 1) %>%
  select(Vote, College, FB, SocFr1, HighAc) %>%
  remove_missing()

## Necessity Analysis

QCAfit(data_male$College, 
       data_male$Vote,
       cond.lab = "College Attendance") 

QCAfit(data_male$FB, 
       data_male$Vote,
       cond.lab = "Family Background")

QCAfit(data_male$SocFr1, 
       data_male$Vote,
       cond.lab = "Socialization with Friends")

QCAfit(data_male$HighAc, 
       data_male$Vote,
       cond.lab = "School Activism")

QCAfit(1-data_male$College, 
       data_male$Vote,
       cond.lab = "~College Attendance") 

QCAfit(1-data_male$FB, 
       data_male$Vote,
       cond.lab = "~Family Background")

QCAfit(1-data_male$SocFr1, 
       data_male$Vote,
       cond.lab = "~Socialization with Friends")

QCAfit(1-data_male$HighAc, 
       data_male$Vote,
       cond.lab = "~School Activism")



## Sufficiency Analysis


Male80_tt <- truthTable(data = data_male, 
                           outcome='Vote', 
                           conditions = c("FB", "College", "SocFr1", "HighAc"), 
                           incl.cut = 0.80, #consistency cut
                           pri.cut = 0.51, #PRI cut
                           n.cut = 1, #cases in a row before is classified as logical remainder (rows without enough empirical evidence)
                           
                           complete = TRUE) #shows all rows in the TT

Male80_Solution <- minimize(input =  Male80_tt, 
                    use.tilde = TRUE,
                    details = TRUE)


x2 <- Male80_tt$tt %>%
  select(-cases, -PRI)

write.csv(x2, file = "Male80.csv", quote = F)

### Appendix 8: Female Model ####


data_female <- data %>%
  filter(Gender == 0) %>%
  select(Vote, College, FB, SocFr1, HighAc) %>%
  remove_missing()

## Necessity Analysis

QCAfit(data_female$College, 
       data_female$Vote,
       cond.lab = "College Attendance") 

QCAfit(data_female$FB, 
       data_female$Vote,
       cond.lab = "Family Background")

QCAfit(data_female$SocFr1, 
       data_female$Vote,
       cond.lab = "Socialization with Friends")

QCAfit(data_female$HighAc, 
       data_female$Vote,
       cond.lab = "School Activism")

QCAfit(1-data_male$College, 
       data_male$Vote,
       cond.lab = "~College Attendance") 

QCAfit(1-data_female$FB, 
       data_female$Vote,
       cond.lab = "~Family Background")

QCAfit(1-data_female$SocFr1, 
       data_female$Vote,
       cond.lab = "~Socialization with Friends")

QCAfit(1-data_female$HighAc, 
       data_male$Vote,
       cond.lab = "~School Activism")



## Sufficiency Analysis


FeMale80_tt <- truthTable(data = data_female, 
                        outcome='Vote', 
                        conditions = c("FB", "College", "SocFr1", "HighAc"), 
                        incl.cut = 0.80, #consistency cut
                        pri.cut = 0.51, #PRI cut
                        n.cut = 1, #cases in a row before is classified as logical remainder (rows without enough empirical evidence)
                        sort.by = c("OUT", "incl"), #sort by output and consistency
                        complete = TRUE) #shows all rows in the TT

FeMale80_Solution <- minimize(input =  FeMale80_tt, 
                            use.tilde = TRUE,
                            details = TRUE)

x1 <- FeMale80_tt$tt %>%
  select(-cases, -PRI)

write.csv(x1, file = "FeMale80.csv", quote = F)

