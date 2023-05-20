#### Pathways to Political Participation 1972 dataset
#### Date: 01/05/2023


rm(list = ls())

### Loading Libraries

library(tidyverse)
library(haven)
library(dplyr)
library(QCA)
library(SetMethods)

source("cluster_diag_hotfix.R")

### Merge two datasets

data_p <- read_dta("Data/PPanel.dta")
data_s <- read_dta("Data/SPanel.dta")
data_pk <-  select(data_p, "V5", "V41", "V44", "V195", "V258", "V408")
data_pk <- rename(data_pk, P_VTG = V41, P_GOVT = V44, P_HOHSES = V195, P_ANTCP = V258, P_LIBC = V408) ## 408 is for 1973 liberalism
data_m <- merge(data_s, data_pk, by = "V5") ## Now we have the dataset


### Calibration ####
## Calibrate vote

table(data_m$V502) ## Voted in 1972
data_m$Vote <- NA 
data_m$Vote[data_m$V502 == 5] <- 0 
data_m$Vote[data_m$V502 == 1] <- 1 
table(data_m$Vote)

## Calibrate college 

data_m$ColAtt <- NA 
data_m$ColAtt[data_m$V648 == 5] <- 0 # 
data_m$ColAtt[data_m$V648 == 1] <- 1 
summary(data_m$ColAtt) # 65.23% attended college
table(data_m$ColAtt)

## Calibrate Parents Edu  

data_m$EduF <- NA 
data_m$EduF[data_m$V207 >= 72 & data_m$V207 <= 87 ] <- 2 #
data_m$EduF[data_m$V207 == 71] <- 1 #
data_m$EduF[data_m$V207 < 71] <- 0


data_m$EduM <- NA 
data_m$EduM[data_m$V208 >= 72 & data_m$V208 <= 87 ] <- 2 #
data_m$EduM[data_m$V208 == 71] <- 1 #
data_m$EduM[data_m$V208 < 71] <- 0

data_m$EduP <- data_m$EduF + data_m$EduM  
summary(data_m$EduP)

data_m$EduPFS <- NA
data_m$EduPFS[data_m$EduP >= 1] <- 1
data_m$EduPFS[data_m$EduP <= 0] <- 0

table(data_m$EduPFS)


## Calibrate Gender
table(data_m$V231) # 1 male£º526 2 female: 538

data_m$male [data_m$V231 == 2] <- NA
data_m$male [data_m$V231 == 1] <- 1

data_m$female [data_m$V231 == 1] <- NA
data_m$female [data_m$V231 == 2] <- 1
table(data_m$male)


## Calibrate Socialization with Friends

data_m$SocFS11 <- NA
data_m$SocFS11[data_m$V77 == 1] <- 1
data_m$SocFS11[data_m$V77 >= 2] <- 0
table(data_m$SocFS11)

## Calibrate Socialization (mother's political interests)

data_m$SocFS111 <- NA #empty variable
data_m$SocFS111[data_m$V116 == 1] <- 1
data_m$SocFS111[data_m$V116 >= 2 | data_m$V116 == 0 ] <- 0
table(data_m$SocFS111)


## Calibrate Personality 

table(data_m$V151)
data_m$opi <- NA
data_m$opi[data_m$V151 == 1] <- 1 
data_m$opi[data_m$V151 == 5] <- 0 
table(data_m$opi)

## Political Efficacy

table(data_m$V59)
data_m$pe <- NA
data_m$pe[data_m$V59 == 1] <- 0
data_m$pe[data_m$V59 == 5] <- 1
table(data_m$pe)


### Table 3: Male + Strong Opinions ####

## Saving calibration

SPS1VSS <- data.frame(data_m$Vote, data_m$EduPFS, data_m$SocFS11, data_m$opi, data_m$ColAtt)
SPSVSS <- na.omit(SPS1VSS)
SPS1VSS <- data.frame(data_m$Vote, data_m$EduPFS, data_m$SocFS11, data_m$opi, data_m$ColAtt, data_m$male)
SPSMVSS <- na.omit(SPS1VSS)

## The Test of Necessity

QCAfit(SPSMVSS$data_m.EduPFS, 
       SPSMVSS$data_m.Vote,
       cond.lab = "Parents Education") 

QCAfit(SPSMVSS$data_m.ColAtt, 
       SPSMVSS$data_m.Vote,
       cond.lab = "College") 

QCAfit(SPSMVSS$data_m.SocFS11, 
       SPSMVSS$data_m.Vote,
       cond.lab = "SocializationFr") 

QCAfit(SPSMVSS$data_m.opi, 
       SPSMVSS$data_m.Vote,
       cond.lab = "Opinionated") 

QCAfit(1-SPSMVSS$data_m.EduPFS, 
       SPSMVSS$data_m.Vote,
       cond.lab = "~Parents Education") 

QCAfit(1-SPSMVSS$data_m.ColAtt, 
       SPSMVSS$data_m.Vote,
       cond.lab = "~College") 

QCAfit(1-SPSMVSS$data_m.SocFS11, 
       SPSMVSS$data_m.Vote,
       cond.lab = "~SocializationFr") 

QCAfit(1-SPSMVSS$data_m.opi, 
       SPSMVSS$data_m.Vote,
       cond.lab = "~Opinionated") 


## The Tests of Sufficiency

TT_EoPMVSS <- truthTable(data = SPSMVSS, 
                           outcome='data_m.Vote', 
                           conditions = c("data_m.EduPFS", "data_m.ColAtt", "data_m.SocFS11", "data_m.opi"), 
                           incl.cut = 0.80, #consistency cut
                           pri.cut = 0.51, #PRI cut
                           n.cut = 10, #cases in a row before is classified as logical remainder (rows without enough empirical evidence)
                           #sort.by = c("OUT", "incl"), #sort by output and consistency
                           complete = TRUE) #shows all rows in the TT


sMVSS <- minimize(input =  TT_EoPMVSS, 
                    use.tilde = TRUE,
                    details = TRUE)

sMVSS

x1 <- TT_EoPMVSS$tt %>%
  select(-cases, -PRI)

write.csv(x1, file = "72Male80.csv", quote = F)


### Table 3: Female + Strong Opinions####

## Saving calibration

SPS2VSS <- data.frame(data_m$Vote, data_m$EduPFS, data_m$SocFS11, data_m$opi, data_m$ColAtt, data_m$female)
SPSFVSS <- na.omit(SPS2VSS)


# The test of necessity
QCAfit(SPSFVSS$data_m.EduPFS, 
       SPSFVSS$data_m.Vote,
       cond.lab = "Parents Education") 

QCAfit(SPSFVSS$data_m.ColAtt, 
       SPSFVSS$data_m.Vote,
       cond.lab = "College") 

QCAfit(SPSFVSS$data_m.SocFS11, 
       SPSFVSS$data_m.Vote,
       cond.lab = "SocializationFr") 

QCAfit(SPSFVSS$data_m.opi, 
       SPSFVSS$data_m.Vote,
       cond.lab = "Opinionated") 

QCAfit(1-SPSFVSS$data_m.EduPFS, 
       SPSFVSS$data_m.Vote,
       cond.lab = "~Parents Education") 

QCAfit(1-SPSFVSS$data_m.ColAtt, 
       SPSFVSS$data_m.Vote,
       cond.lab = "~College") 

QCAfit(1-SPSFVSS$data_m.SocFS11, 
       SPSFVSS$data_m.Vote,
       cond.lab = "~SocializationFr") 

QCAfit(1-SPSFVSS$data_m.opi, 
       SPSFVSS$data_m.Vote,
       cond.lab = "~Opinionated") 


## The Tests of Sufficiency


TT_EoPF80VSS <- truthTable(data = SPSFVSS, 
                           outcome='data_m.Vote', 
                           conditions = c("data_m.EduPFS", "data_m.ColAtt", "data_m.SocFS11", "data_m.opi"), 
                           incl.cut = 0.80, #consistency cut
                           pri.cut = 0.51, #PRI cut
                           n.cut = 10, #cases in a row before is classified as logical remainder (rows without enough empirical evidence)
                           # sort.by = c("OUT", "incl"), #sort by output and consistency
                           complete = TRUE) #shows all rows in the TT


sF80VSS <- minimize(input =  TT_EoPF80VSS, 
                    use.tilde = TRUE,
                    details = TRUE)

sF80VSS

x2 <- TT_EoPF80VSS$tt %>%
  select(-cases, -PRI)

write.csv(x2, file = "72female80.csv", quote = F)


### Appendix 4: Family socialization ####

## males

SPS1VSS1 <- data.frame(data_m$Vote, data_m$EduPFS, data_m$SocFS111, data_m$opi, data_m$ColAtt, data_m$male)
SPSMVSS1 <- na.omit(SPS1VSS1)


TT_EoPMVSS1 <- truthTable(data = SPSMVSS1, 
                         outcome='data_m.Vote', 
                         conditions = c("data_m.EduPFS", "data_m.ColAtt", "data_m.SocFS111", "data_m.opi"), 
                         incl.cut = 0.85, #consistency cut
                         pri.cut = 0.51, #PRI cut
                         n.cut = 10, #cases in a row before is classified as logical remainder (rows without enough empirical evidence)
                         #sort.by = c("OUT", "incl"), #sort by output and consistency
                         complete = TRUE) #shows all rows in the TT


sMVSS1 <- minimize(input =  TT_EoPMVSS1, 
                  use.tilde = TRUE,
                  details = TRUE)

sMVSS1



## females

SPS2VSS1 <- data.frame(data_m$Vote, data_m$EduPFS, data_m$SocFS111, data_m$opi, data_m$ColAtt, data_m$female)
SPSFVSS1 <- na.omit(SPS2VSS1)

TT_EoPF80VSS1 <- truthTable(data = SPSFVSS1, 
                           outcome='data_m.Vote', 
                           conditions = c("data_m.EduPFS", "data_m.ColAtt", "data_m.SocFS111", "data_m.opi"), 
                           incl.cut = 0.85, #consistency cut
                           pri.cut = 0.51, #PRI cut
                           n.cut = 10, #cases in a row before is classified as logical remainder (rows without enough empirical evidence)
                           # sort.by = c("OUT", "incl"), #sort by output and consistency
                           complete = TRUE) #shows all rows in the TT


sF80VSS1 <- minimize(input =  TT_EoPF80VSS1, 
                    use.tilde = TRUE,
                    details = TRUE)

sF80VSS1



### Appendix 6: Male + Political Efficacy ####

## Saving calibration

SPS1VSSs <- data.frame(data_m$Vote, data_m$EduPFS, data_m$SocFS11, data_m$pe, data_m$ColAtt)
SPSVSSs <- na.omit(SPS1VSSs)

SPS1VSSs <- data.frame(data_m$Vote, data_m$EduPFS, data_m$SocFS11, data_m$pe, data_m$ColAtt, data_m$male)
SPSMVSSs <- na.omit(SPS1VSSs)


## The Test of Necessity

QCAfit(SPSMVSSs$data_m.pe, 
       SPSMVSSs$data_m.Vote,
       cond.lab = "Political Efficacy") 

QCAfit(1-SPSMVSSs$data_m.pe, 
       SPSMVSSs$data_m.Vote,
       cond.lab = "~Political Efficacy") 


## The Tests of Sufficiency


TT_EoPMVSSs <- truthTable(data = SPSMVSSs, 
                            outcome='data_m.Vote', 
                            conditions = c("data_m.EduPFS", "data_m.ColAtt", "data_m.SocFS11", "data_m.pe"), 
                            incl.cut = 0.80, #consistency cut
                            pri.cut = 0.51, #PRI cut
                            n.cut = 10, #cases in a row before is classified as logical remainder (rows without enough empirical evidence)
                            #sort.by = c("OUT", "incl"), #sort by output and consistency
                            complete = TRUE) #shows all rows in the TT


sMVSSs <- minimize(input =  TT_EoPMVSSs, 
                     use.tilde = TRUE,
                     details = TRUE)
sMVSSs



### Appendix 6: Female + Political Efficacy ####

## Saving calibration

SPS2VSSs <- data.frame(data_m$Vote, data_m$EduPFS, data_m$SocFS11, data_m$pe, data_m$ColAtt, data_m$female)
SPSFVSSs <- na.omit(SPS2VSSs)

## The Test of Neccessity

QCAfit(SPSFVSSs$data_m.pe, 
       SPSFVSSs$data_m.Vote,
       cond.lab = "Political Efficacy") 

QCAfit(1-SPSFVSSs$data_m.pe, 
       SPSFVSSs$data_m.Vote,
       cond.lab = "~Political Efficacy") 




## The Test of Sufficiency

TT_EoPFVSSs <- truthTable(data = SPSFVSSs, 
                            outcome='data_m.Vote', 
                            conditions = c("data_m.EduPFS", "data_m.ColAtt", "data_m.SocFS11", "data_m.pe"), 
                            incl.cut = 0.80, #consistency cut
                            pri.cut = 0.51, #PRI cut
                            n.cut = 10, #cases in a row before is classified as logical remainder (rows without enough empirical evidence)
                            sort.by = c("OUT", "incl"), #sort by output and consistency
                            complete = TRUE) #shows all rows in the TT


sFVSSs <- minimize(input =  TT_EoPFVSSs, 
                     use.tilde = TRUE,
                     details = TRUE)
sFVSSs


### Appendix 2: Skewness Tests ####

# Male

skew.check(SPSMVSS$data_m.Vote) # 74.84% in
skew.check(SPSMVSS$data_m.EduPFS) # 35.1% in
skew.check(SPSMVSS$data_m.SocFS11) # 30.02%
skew.check(SPSMVSSs$data_m.SocFS111) # 22.2%
skew.check(SPSMVSS$data_m.opi) #58.14
skew.check(SPSMVSS$data_m.ColAtt) #71.67


# Female
skew.check(SPSFVSS$data_m.Vote) # 71.25% in
skew.check(SPSFVSS$data_m.EduPFS) # 37.84% in
skew.check(SPSFVSS$data_m.SocFS11) # 27.06%
skew.check(SPSFVSSs$data_m.SocFS111) # 23.04%
skew.check(SPSFVSS$data_m.opi) # 61.31%
skew.check(SPSFVSS$data_m.ColAtt) # 63%

### Descriptive statistics ####

# Male
table(SPSMVSS$data_m.Vote) # 119 354
table(SPSMVSS$data_m.EduPFS) # 166
table(SPSMVSS$data_m.SocFS11) # 142
table(SPSMVSSs$data_m.SocFS111) # family 105
table(SPSMVSS$data_m.opi) #275
table(SPSMVSS$data_m.ColAtt) #339


# Female
table(SPSFVSS$data_m.Vote) # 337
table(SPSFVSS$data_m.EduPFS) # 179
table(SPSFVSS$data_m.SocFS11) # 128
table(SPSFVSSs$data_m.SocFS111) # family 109
table(SPSFVSS$data_m.opi) # 290
table(SPSFVSS$data_m.ColAtt) # 298


### Appendix 5: Robustness Tests for Male + Friends ####

## Raw data

M41 <- data.frame(data_m$Vote, data_m$EduPFS, data_m$ColAtt, data_m$SocFS11, data_m$V151, data_m$male)
M4M <- na.omit(M41)
table(M4M$data_m.V151)
summary(M4M$data_m.V151)


M412 <- data.frame(data_m$Vote, data_m$EduPFS, data_m$ColAtt, data_m$V77, data_m$opi, data_m$male)
M4M2 <- na.omit(M412)

## Sensitivity ranges:

rob.inclrange(
  data = SPSMVSS,
  step = 0.01, #specifies the value with which the particular threshold is increased and decreased
  max.runs = 30, #number of times the threshold is increased/decreased to find the upper and lower ranges
  outcome  = "data_m.Vote",
  conditions =  c("data_m.EduPFS", "data_m.ColAtt", "data_m.SocFS11", "data_m.opi"),
  incl.cut = 0.8,
  n.cut = 10,
  include = "?")  ##  Lower bound  0.79 Threshold  0.8 Upper bound  0.83 

rob.ncutrange(
  data = SPSMVSS,
  step = 1,
  max.runs = 30,
  outcome  = "data_m.Vote",
  conditions =   c("data_m.EduPFS", "data_m.ColAtt", "data_m.SocFS11", "data_m.opi"),
  incl.cut = 0.8,
  n.cut = 10,
  include = "?")   ## Lower bound  5 Threshold  10 Upper bound  13 


## Fit-oriented robustness

# the initial solution [10, 0.8]

RBT1 <- SPSMVSS

names(RBT1) <- toupper(names(RBT1))

IS <- minimize(data = RBT1,
               outcome  = "DATA_M.VOTE",
               conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS11", "DATA_M.OPI"),
               incl.cut = 0.8,
               n.cut = 10,
               include = "?",
               details = TRUE)
IS

# TS1 altering consistency
TS1 <- minimize(data = RBT1,
                outcome  = "DATA_M.VOTE",
                conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS11", "DATA_M.OPI"),
                incl.cut = 0.75,
                n.cut = 10,
                include = "?",
                details = TRUE)
TS1

# TS2 altering n.cut
TS2 <- minimize(data = RBT1,
                outcome  = "DATA_M.VOTE",
                conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS11", "DATA_M.OPI"),
                incl.cut = 0.8,
                n.cut = 1,
                include = "?",
                details = TRUE)
TS2

# Create the test set (TS) in a list:

TS_list <- list(TS1, TS2)

# Calculate robustness parameters:
RF <- rob.fit(test_sol = TS_list, initial_sol = IS, outcome = 'data_m.Vote')

RF    ### 0.968 0.985 0.954



## Cluster Diagnostics

# read file

CDM4 <- data.frame(data_m$V5, data_m$V7, data_m$Vote, data_m$EduPFS, data_m$SocFS11, data_m$opi, data_m$ColAtt, data_m$male)
CDM41 <- na.omit(CDM4)

RBT11 <- CDM41

names(RBT11) <- toupper(names(CDM41))

# Create parsimonious solution:

PS <- minimize(data = RBT11,
               outcome  = "DATA_M.VOTE",
               conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS11", "DATA_M.OPI"),
               incl.cut = 0.8,
               n.cut = 10,
               include = "?",
               details = TRUE,
               use.tilde = TRUE)
PS

# Cluster diagnostics:

CS <- cluster(results = PS, 
              data = RBT11, 
              outcome  = "DATA_M.VOTE", 
              unit_id = "DATA_M.V5", 
              cluster_id = "DATA_M.V7")
CS  ### Looks ok. 

# saveClust(CS, "Tables/M1")

cluster.plot(CS, labs = FALSE)


### Appendix 5: Robustness Tests for Female +Friend ####

RBT2 <- SPSFVSS

names(RBT2) <- toupper(names(RBT2))

M2_IS <- minimize(data = RBT2,
               outcome  = "DATA_M.VOTE",
               conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS11", "DATA_M.OPI"),
               incl.cut = 0.8,
               n.cut = 10,
               include = "?",
               details = TRUE)
M2_IS

# TS1 altering consistency
M2_TS1 <- minimize(data = RBT2,
                outcome  = "DATA_M.VOTE",
                conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS11", "DATA_M.OPI"),
                incl.cut = 0.75,
                n.cut = 10,
                include = "?",
                details = TRUE)
M2_TS1

# TS2 altering n.cut
M2_TS2 <- minimize(data = RBT2,
                outcome  = "DATA_M.VOTE",
                conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS11", "DATA_M.OPI"),
                incl.cut = 0.8,
                n.cut = 1,
                include = "?",
                details = TRUE)
M2_TS2

# Create the test set (TS) in a list:

TS_list2 <- list(M2_TS1, M2_TS2)

# Calculate robustness parameters:
M2_RF <- rob.fit(test_sol = TS_list2, initial_sol = M2_IS, outcome = 'DATA_M.VOTE')

M2_RF    ### 0.961 0.992 0.929



## Cluster Diagnostics

# read file


CDF4 <- data.frame(data_m$V5, data_m$V7, data_m$Vote, data_m$EduPFS, data_m$SocFS11, data_m$opi, data_m$ColAtt, data_m$female)
CDF41 <- na.omit(CDF4)

RBT22 <- CDF41

names(RBT22) <- toupper(names(RBT22))

# Create parsimonious solution:

M2_PS <- minimize(data = RBT22,
               outcome  = "DATA_M.VOTE",
               conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS11", "DATA_M.OPI"),
               incl.cut = 0.8,
               n.cut = 10,
               include = "?",
               details = TRUE,
               use.tilde = TRUE)
M2_PS

# Cluster diagnostics:

M2_CS <- cluster(results = M2_PS, 
              data = RBT22, 
              outcome  = "DATA_M.VOTE", 
              unit_id = "DATA_M.V5", 
              cluster_id = "DATA_M.V7")
M2_CS  ### Looks ok. 

cluster.plot(M2_CS ) 

# saveClust(M2_CS, "Tables/M2")
cluster.plot(M2_CS, labs = FALSE)

### Appendix 5: Robustness Tests for Male + Friend ####

RBT3 <- SPSMVSSs

names(RBT3) <- toupper(names(RBT3))

M3_IS <- minimize(data = RBT3,
                  outcome  = "DATA_M.VOTE",
                  conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS111", "DATA_M.OPI"),
                  incl.cut = 0.8,
                  n.cut = 10,
                  include = "?",
                  details = TRUE)
M3_IS

# TS1 altering consistency
M3_TS1 <- minimize(data = RBT3,
                   outcome  = "DATA_M.VOTE",
                   conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS111", "DATA_M.OPI"),
                   incl.cut = 0.75,
                   n.cut = 10,
                   include = "?",
                   details = TRUE)
M3_TS1

# TS2 altering n.cut
M3_TS2 <- minimize(data = RBT3,
                   outcome  = "DATA_M.VOTE",
                   conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS111", "DATA_M.OPI"),
                   incl.cut = 0.8,
                   n.cut = 1,
                   include = "?",
                   details = TRUE)
M3_TS2

# Create the test set (TS) in a list:

TS_list3 <- list(M3_TS1, M3_TS2)

# Calculate robustness parameters:
M3_RF <- rob.fit(test_sol = TS_list3, initial_sol = M3_IS, outcome = 'DATA_M.VOTE')

M3_RF    ### 0.989 0.984 0.973



## Cluster Diagnostics

# read file


M3_CDM <- data.frame(data_m$V5, data_m$V7, data_m$Vote, data_m$EduPFS, data_m$SocFS111, data_m$opi, data_m$ColAtt, data_m$male)
M3_CDM1 <- na.omit(M3_CDM)

RBT33 <- M3_CDM1

names(RBT33) <- toupper(names(RBT33))

# Create parsimonious solution:

M3_PS <- minimize(data = RBT33,
                  outcome  = "DATA_M.VOTE",
                  conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS111", "DATA_M.OPI"),
                  incl.cut = 0.8,
                  n.cut = 10,
                  include = "?",
                  details = TRUE,
                  use.tilde = TRUE)
M3_PS

# Cluster diagnostics:

M3_CS <- cluster(results = M3_PS, 
                 data = RBT33, 
                 outcome  = "DATA_M.VOTE", 
                 unit_id = "DATA_M.V5", 
                 cluster_id = "DATA_M.V7")
M3_CS  ### Looks ok. 

# saveClust(M3_CS, "Tables/M3")

cluster.plot(M3_CS, labs = FALSE)


### Appendix 5:  Robustness Tests for Female + Family ####

RBT4 <- SPSFVSSs

names(RBT4) <- toupper(names(RBT4))

M4_IS <- minimize(data = RBT4,
                  outcome  = "DATA_M.VOTE",
                  conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS111", "DATA_M.OPI"),
                  incl.cut = 0.8,
                  n.cut = 10,
                  include = "?",
                  details = TRUE)
M4_IS

# TS1 altering consistency
M4_TS1 <- minimize(data = RBT4,
                   outcome  = "DATA_M.VOTE",
                   conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS111", "DATA_M.OPI"),
                   incl.cut = 0.75,
                   n.cut = 10,
                   include = "?",
                   details = TRUE)
M4_TS1

# TS2 altering n.cut
M4_TS2 <- minimize(data = RBT4,
                   outcome  = "DATA_M.VOTE",
                   conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS111", "DATA_M.OPI"),
                   incl.cut = 0.8,
                   n.cut = 1,
                   include = "?",
                   details = TRUE)
M4_TS2

# Create the test set (TS) in a list:

TS_list4 <- list(M4_TS1, M4_TS2)

# Calculate robustness parameters:
M4_RF <- rob.fit(test_sol = TS_list4, initial_sol = M4_IS, outcome = 'DATA_M.VOTE')

M4_RF    ### 1 1 1



## Cluster Diagnostics

# read file


M4_CDM <- data.frame(data_m$V5, data_m$V7, data_m$Vote, data_m$EduPFS, data_m$SocFS111, data_m$opi, data_m$ColAtt, data_m$female)
M4_CDM1 <- na.omit(M4_CDM)

RBT44 <- M4_CDM1

names(RBT44) <- toupper(names(RBT44))

# Create parsimonious solution:

M4_PS <- minimize(data = RBT44,
                  outcome  = "DATA_M.VOTE",
                  conditions = c("DATA_M.EDUPFS", "DATA_M.COLATT", "DATA_M.SOCFS111", "DATA_M.OPI"),
                  incl.cut = 0.8,
                  n.cut = 10,
                  include = "?",
                  details = TRUE,
                  use.tilde = TRUE)
M4_PS

# Cluster diagnostics:

M4_CS <- cluster(results = M4_PS, 
                 data = RBT44, 
                 outcome  = "DATA_M.VOTE", 
                 unit_id = "DATA_M.V5", 
                 cluster_id = "DATA_M.V7")
M4_CS  ### Looks ok. 

# saveClust(M4_CS, "Tables/M4")

cluster.plot(M4_CS, labs = FALSE)















##------------------------------utility function

saveClust <- function(csObj, prefix){
  x <- csObj
  digits <- 3
  aux.pocos <- function(y) return(y$POCOS)
  aux.becos <- function(y) return(y$BECOS)
  aux.wicons <- function(y) return(y$WICONS)
  aux.dBP <- function(y) return(y$dBP)
  aux.dWP <- function(y) return(y$dWP)
  aux.pocvr <- function(y) return(y$Coverages$pooled)
  aux.becvr <- function(y) return(y$Coverages$between)
  aux.wicvr <- function(y) return(y$Coverages$within)
  pocos <- do.call(cbind, lapply(x$output, aux.pocos))
  becos <- do.call(cbind, lapply(x$output, aux.becos))
  wicons <- do.call(cbind, lapply(x$output, aux.wicons))
  dBP <- do.call(cbind, lapply(x$output, aux.dBP))
  dWP <- do.call(cbind, lapply(x$output, aux.dWP))
  pocvr <- do.call(cbind, lapply(x$output, aux.pocvr))
  becvr <- do.call(cbind, lapply(x$output, aux.becvr))
  wicvr <- do.call(cbind, lapply(x$output, aux.wicvr))
  te <- names(x$output)
  colnames(pocos) <- colnames(becos) <- colnames(wicons) <- te
  colnames(dWP) <- colnames(dBP) <- te
  rownames(pocos) <- rownames(pocvr) <- "Pooled"
  CNRC <- data.frame(table(x$cluster_ids))
  cnrc <- paste(as.character(CNRC[, 1]), " (", as.character(CNRC[, 
                                                                 2]), ") ", sep = "")
  CNRU <- data.frame(table(x$unit_ids))
  cnru <- paste(as.character(CNRU[, 1]), " (", as.character(CNRU[, 
                                                                 2]), ") ", sep = "")
  rownames(becos) <- rownames(becvr) <- paste("Between", cnrc)
  rownames(wicons) <- rownames(wicvr) <- paste("Within", cnru)
  rownames(dBP) <- "From Between to Pooled"
  rownames(dWP) <- "From Within to Pooled"
  if (x$wiconsprint == TRUE) {
    coses <- rbind(pocos, becos, wicons)
    dists <- rbind(dBP, dWP)
    cvres <- rbind(pocvr, becvr, wicvr)
  } else {
    coses <- rbind(pocos, becos)
    dists <- rbind(dBP)
    cvres <- rbind(pocvr, becvr)
  }
  conTable <- round(coses, digits)
  disTable <- round(dists, digits)
  covTable <- round(cvres, digits)
  
  write.csv(conTable, paste0(prefix, "_consistency.csv"), row.names = T, quote = F)
  write.csv(disTable, paste0(prefix, "_distance.csv"), row.names = T, quote = F)
  write.csv(covTable, paste0(prefix, "_coverage.csv"), row.names = T, quote = F)
}

