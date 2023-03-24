############################################################################################
############################################################################################
####
####   ＿＿＿＿＿＿＿__＿＿
####  |￣￣￣￣￣￣￣￣￣￣|
####  | Created by         |
####  |  Rainer Gabriel    |
####  | Have a             |
####  |      nice day      |
####  |＿＿＿＿＿＿＿__＿＿|
####  (\__/)||
####  (•ㅅ•)||
####  / 　 づ
#### Email: rainer.gabriel@zhaw.ch
#### --------------------------------
#### note: stable under R version 3.1.3. "wild kid" 
############################################################################################
############################################################################################


library(tidyverse)
library(haven)
library(eply)
library(tidyr)
library(knitr)
library(questionr)
library(ggplot2)
library(tidyverse)
library(sf)
library(rcartocolor)
library(readxl)
library(questionr)
library(ggthemes)
# wavelibrary(ggtext)
library(DescTools)
library(PropCIs)
library(svglite)
library(parsnip)
# library(tidymodels)
library(finalfit)
library(ggrepel)
library(stargazer)
library(mice)
# install.packages("effects")
# library(effects)



rm(list=ls())
load(file="data_sas22-variable-setup-done.Rdata")


# principle: since EL is so specific to marital status, i calculate generic variables for budget items, but they are always SPECIFIC to marital status 

############################################################################################
#### Part 1: Wealth 
############################################################################################

# fix the problem, that some have no value for realestate but for wealth, and vice versa. As the sum creates NA, i create this loop that takes the value of the 
# valid response if one is NA, or, if both values are valid, creates the sum 
sas.22$gross.wealth.sum <- ifelse(!is.na(sas.22$assetamount.merged) & !is.na(sas.22$realest.merged.val), sas.22$assetamount.merged+sas.22$realest.merged.val, 
                                 ifelse(is.na(sas.22$assetamount.merged) & !is.na(sas.22$realest.merged.val), sas.22$realest.merged.val, 
                                        ifelse(!is.na(sas.22$assetamount.merged) & is.na(sas.22$realest.merged.val), sas.22$assetamount.merged,NA))) 
length(which(is.na(sas.22$gross.wealth.sum)))


summary(sas.22$gross.wealth.sum )

sas.22$net.wealth.sum <- sas.22$gross.wealth.sum-sas.22$morgage.merged.amount #we substract the morgage amount to get the net wealth 

sas.22$adjusted.household.net.worth <- sas.22$net.wealth.sum # then we stick this into a variable which we will then adjust based on household composition

summary(sas.22$adjusted.household.net.worth)

# adjusting net wealth according to marital status and household composition 

levels <- levels(as.factor(sas.22$household.typology.superfine))
levels
table(sas.22$household.typology.superfine)

levels[1]
filter<-which(sas.22$household.typology.superfine==levels[1])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]/2

levels[2]
filter<-which(sas.22$household.typology.superfine==levels[2])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]/3

levels[3]
filter<-which(sas.22$household.typology.superfine==levels[3])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]/4

levels[4]
filter<-which(sas.22$household.typology.superfine==levels[4])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]

levels[5]
filter<-which(sas.22$household.typology.superfine==levels[5])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter] #no hypothesis possible on relationship between 3 aduls => hypothesis: family

levels[6]
filter<-which(sas.22$household.typology.superfine==levels[6])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]/3

levels[7]
filter<-which(sas.22$household.typology.superfine==levels[7])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]

levels[8]
filter<-which(sas.22$household.typology.superfine==levels[8])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]

levels[9]
filter<-which(sas.22$household.typology.superfine==levels[9])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]

levels[10]
filter<-which(sas.22$household.typology.superfine==levels[10])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]

levels[11]
filter<-which(sas.22$household.typology.superfine==levels[11])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]

levels[12]
filter<-which(sas.22$household.typology.superfine==levels[12])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]

levels[13]
filter<-which(sas.22$household.typology.superfine==levels[13])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]/2

levels[14]
filter<-which(sas.22$household.typology.superfine==levels[14])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]/2

levels[15]
filter<-which(sas.22$household.typology.superfine==levels[15])
sas.22$adjusted.household.net.worth[filter] <- sas.22$adjusted.household.net.worth[filter]/2

mean(sas.22$adjusted.household.net.worth, na.rm=TRUE)

# at this stage we have net household wealth, specific for married and non-married people

############################################################################################
#### PART 2: Financial Inflows ("Anrechenbares Einkommen")
############################################################################################

#calculate household income, but no based on OECD equivalence scales, because for EL ansprüche are calculated at the household level 

# adjusting net income according to marital status and household composition 

sas.22$el.specific.adjusted.hhincome.amount <- sas.22$income.amount.merged

levels <- levels(as.factor(sas.22$household.typology.superfine))
levels

levels[1]
filter<-which(sas.22$household.typology.superfine==levels[1])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]/2 #for 2 adults divide by 2... and so on... 

levels[2]
filter<-which(sas.22$household.typology.superfine==levels[2])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]/3

levels[3]
filter<-which(sas.22$household.typology.superfine==levels[3])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]/4

levels[4]
filter<-which(sas.22$household.typology.superfine==levels[4])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]

levels[5]
filter<-which(sas.22$household.typology.superfine==levels[5]) 
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]#no hypothesis possible on relationship between 3 aduls => hypothesis: family

levels[6]
filter<-which(sas.22$household.typology.superfine==levels[6])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]

levels[7]
filter<-which(sas.22$household.typology.superfine==levels[7])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]

levels[8]
filter<-which(sas.22$household.typology.superfine==levels[8])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]

levels[9]
filter<-which(sas.22$household.typology.superfine==levels[9])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]

levels[10]
filter<-which(sas.22$household.typology.superfine==levels[10])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]

levels[11]
filter<-which(sas.22$household.typology.superfine==levels[11])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]

levels[12]
filter<-which(sas.22$household.typology.superfine==levels[12])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]

levels[13]
filter<-which(sas.22$household.typology.superfine==levels[13])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]/2

levels[14]
filter<-which(sas.22$household.typology.superfine==levels[14])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]/2

levels[15]
filter<-which(sas.22$household.typology.superfine==levels[15])
sas.22$el.specific.adjusted.hhincome.amount[filter] <- sas.22$el.specific.adjusted.hhincome.amount[filter]/2

mean(sas.22$el.specific.adjusted.hhincome.amount, na.rm=TRUE)

plot(log(sas.22$income.amount.merged)) #

sas.22$income.amount.merged.month <- sas.22$income.amount.merged

# Calculate additional income from "Vermögensverzehr"--------------------------------------

sas.22$vermoegensverzehr.year <- 0

filter <- which(sas.22$el.anspruchstarif=="ehepaar"
                  & sas.22$adjusted.household.net.worth>50000)  
length(filter)
sas.22$vermoegensverzehr.year[filter] <- (sas.22$adjusted.household.net.worth[filter]-50000)*0.1
hist(sas.22$vermoegensverzehr.year)
mean(sas.22$vermoegensverzehr.year)

filter <- which(sas.22$el.anspruchstarif=="einzel"  & sas.22$adjusted.household.net.worth>30000)  
length(filter)
sas.22$vermoegensverzehr.year[filter] <- (sas.22$adjusted.household.net.worth[filter]-30000)*0.1

summary(sas.22$vermoegensverzehr.year)
hist(sas.22$vermoegensverzehr.year)
mean(sas.22$vermoegensverzehr.year)

sas.22$vermoegensverzehr.month <- sas.22$vermoegensverzehr.year/12
summary(  sas.22$vermoegensverzehr.month)

# create unified income variable at household level, specific to tariff------------------------------------------

summary(sas.22$income.amount.merged.month)

filter <- which((is.na(sas.22$income.amount.merged)) & sas.22$vermoegensverzehr.year>0)
length(filter) 

sas.22$total.income.amount.merged.month <- sas.22$income.amount.merged.month+sas.22$vermoegensverzehr.month

length(which(is.na(sas.22$total.income.amount.merged.month)))
  
sas.22 %>% count(total.income.amount.merged.month)
hist(log(sas.22$total.income.amount.merged.month))

#NOTE: AT THIS STAGE WE HAVE TYPOLOGY-SPECIFIC VARIABLES (WHICH ARE BASICALLY MARITAL-STATUS-SPECIFIC) INCOME LEVELS THAT HAVE TO BE TREATED SPECIFICALLY AS WELL! 




############################################################################################
#### Part 4: Expenses (aka "Anrechenbare Ausgaben") 
############################################################################################

# care costs 

summary(sas.22$cs_freqcomcare_22) # maority is NA because they did not get the question which had a filter if care was not needed
table(sas.22$cs_freqcomcare_22)
filter <- which(sas.22$cs_getcomcare_22==0)
sas.22$cs_freqcomcare_22[filter] <- 0
table(sas.22$cs_freqcomcare_22)

table(sas.22$ph_gali_22,sas.22$cs_freqcomcare_22)

table(sas.22$cs_freqcomcare_22)

sas.22$pflegekosten.month <- sas.22$cs_freqcomcare_22*15.35
summary(sas.22$pflegekosten.month)
table(sas.22$pflegekosten.month)
length(which(is.na(sas.22$pflegekosten.month)))




sas.22$overall.care.costs.month <- ifelse(!is.na(sas.22$pflegekosten.month),sas.22$pflegekosten.month,0)


hist(sas.22$overall.care.costs.month )
table(sas.22$overall.care.costs.month)

length(which(is.na(sas.22$overall.care.costs.month)))

filter <- which(is.na(sas.22$overall.care.costs.month))
sas.22$overall.care.costs.month[filter] <- 0 # hypothesis: those who have NA have zero care costs 


# variable with fixed expenses relative to EL-typologie (nursing home or not, etc.) ----------------------------------------------

#here i'm following nora meuli's documentation for the amounts, except for the KVG average for 2022 which i looked at through BAG 

sas.22$mietkosten <- 0

length(which(sas.22$env_BFStypo9_22>30))
sas.22 <- sas.22 %>% mutate(mietzinsregion=case_when(
  env_BFStypo9_22==11 | env_BFStypo9_22==12 ~ "stadt.grossregion", 
  env_BFStypo9_22>12 & env_BFStypo9_22<31 ~ "stadt", 
  env_BFStypo9_22>30 ~ "land"))
    table(sas.22$mietzinsregion)

    levels(as.factor(sas.22$household.typology.superfine))
    length(which(is.na(sas.22$household.typology.superfine)))
    
    sas.22 <- sas.22 %>% mutate(el.specific.hhtype = case_when(
      household.typology.superfine == "2 Erwachsene" |
      household.typology.superfine == "3 Erwachsene" | 
        household.typology.superfine == "4 Erwachsene" | 
        household.typology.superfine == "unreliable" |
        household.typology.superfine == "unverheiratet paarhaushalt" ~ "konkubinat",
      household.typology.superfine ==  "Einzelhaushalt" ~ "Alleinlebend", 
      household.typology.superfine == "Unverh.paar mit 1 Kind" | 
         household.typology.superfine == "Alleinerziehend" |
        household.typology.superfine == "Ehepaar + andere Erwachsene" |
        household.typology.superfine == "EhePaarhaushalt"~  "Ehepaar ohne kinder / alleinstehend mit kind",
      household.typology.superfine == "Ehepaar mit 1 Kind" |
        household.typology.superfine == "Elternteil + 2k" ~    "Ehepaar mit Kind / alleinstehend 2 kinder",
      household.typology.superfine == "Ehepaar mit 2 Kind" |
        household.typology.superfine ==  "Ehepaar mit 3 Kind" ~ "Ehepaar mit 2+Kindern / alleinstehend 2+kindern"))
        
    sas.22 %>% count(el.specific.hhtype)
    length(which(is.na(sas.22$el.specific.hhtype)))
    
             sas.22$mietkosten <- NA
     sas.22 <- sas.22 %>% mutate(mietkosten = case_when(
      
         mietzinsregion== "stadt.grossregion" & 
         el.specific.hhtype== "Alleinlebend" ~ 16440, 
      
         mietzinsregion== "stadt.grossregion" & 
         el.specific.hhtype== "Ehepaar ohne kinder / alleinstehend mit kind" ~ 19440, 
      
         mietzinsregion== "stadt.grossregion" & 
         el.specific.hhtype== "Ehepaar mit Kind / alleinstehend 2 kinder" ~ 21600, 
          
         mietzinsregion== "stadt.grossregion" & 
         el.specific.hhtype== "Ehepaar mit 2+Kindern / alleinstehend 2+kindern" ~ 23520, 
       mietzinsregion== "stadt.grossregion" & 
         el.specific.hhtype== "konkubinat" ~ 9720, 
      
         mietzinsregion== "stadt" & 
         el.specific.hhtype== "Alleinlebend" ~ 15900, 
      
         mietzinsregion== "stadt" & 
         el.specific.hhtype== "Ehepaar ohne kinder / alleinstehend mit kind" ~ 18900, 
      
         mietzinsregion== "stadt" & 
         el.specific.hhtype== "Ehepaar mit Kind / alleinstehend 2 kinder" ~ 20700, 
      
         mietzinsregion== "stadt" & 
         el.specific.hhtype== "Ehepaar mit 2+Kindern / alleinstehend 2+kindern" ~ 22500, 
       mietzinsregion== "stadt" & 
         el.specific.hhtype== "konkubinat" ~ 9450, 
      
         mietzinsregion== "land" & 
         el.specific.hhtype== "Alleinlebend" ~ 14520, 
      
         mietzinsregion== "land" & 
         el.specific.hhtype== "Ehepaar ohne kinder / alleinstehend mit kind" ~ 17520, 
      
         mietzinsregion== "land" & 
         el.specific.hhtype== "Ehepaar mit Kind / alleinstehend 2 kinder" ~ 19320, 
      
         mietzinsregion== "land" & 
         el.specific.hhtype== "Ehepaar mit 2+Kindern / alleinstehend 2+kindern" ~ 20760, 
       mietzinsregion== "land" & 
         el.specific.hhtype== "konkubinat" ~ 8760 ))
     

     sas.22$mietkosten.month <- sas.22$mietkosten/12
     
     sas.22$mietkosten.month <- ifelse(sas.22$homeowner.rcd=="Eigenheimbesitz",sas.22$mietkosten.month*0.8,sas.22$mietkosten.month)
     # grundbedarf
     
     sas.22$grundbedarf <- NA
     
     levels <- levels(as.factor(sas.22$household.typology.superfine))
     
     levels[1]
     filter<-which(sas.22$household.typology.superfine==levels[1])
     sas.22$grundbedarf[filter] <- 19610
     
     levels[2]
     filter<-which(sas.22$household.typology.superfine==levels[2])
     sas.22$grundbedarf[filter] <- 19610
     
     levels[3]
     filter<-which(sas.22$household.typology.superfine==levels[3])
     sas.22$grundbedarf[filter] <- 19610
     
     
     levels[4]
     filter<-which(sas.22$household.typology.superfine==levels[4])
     sas.22$grundbedarf[filter] <- 19610+7200
     
     levels[5]
     filter<-which(sas.22$household.typology.superfine==levels[5])
     sas.22$grundbedarf[filter] <- 29415
     
     levels[6]
     filter<-which(sas.22$household.typology.superfine==levels[6])
     sas.22$grundbedarf[filter] <- 29415+7200
     
     levels[7]
     filter<-which(sas.22$household.typology.superfine==levels[7])
     sas.22$grundbedarf[filter] <- 29415+7200+6000
     
     levels[8]
     filter<-which(sas.22$household.typology.superfine==levels[8])
     sas.22$grundbedarf[filter] <- 29415+7200+6000+5000
     
     levels[9]
     filter<-which(sas.22$household.typology.superfine==levels[9])
     sas.22$grundbedarf[filter] <- 29415
     
     levels[10]
     filter<-which(sas.22$household.typology.superfine==levels[10])
     sas.22$grundbedarf[filter] <- 19610
     
     levels[11]
     filter<-which(sas.22$household.typology.superfine==levels[11])
     sas.22$grundbedarf[filter] <- 19610+7200+6000
     
     levels[12]
     filter<-which(sas.22$household.typology.superfine==levels[12])
     sas.22$grundbedarf[filter] <- 19610
     
     levels[13]
     filter<-which(sas.22$household.typology.superfine==levels[13])
     sas.22$grundbedarf[filter] <- 19610+7200
     
     levels[14]
     filter<-which(sas.22$household.typology.superfine==levels[14])
     sas.22$grundbedarf[filter] <- 19610
     
    
     length(which(is.na(sas.22$grundbedarf)))
     
     mean(sas.22$grundbedarf, na.rm=TRUE)
    
     sas.22$grundbedarf.month <-sas.22$grundbedarf/12

     #durchschnittliche kvg prämie

     sas.22 <- sas.22 %>%  mutate(kvg.month.canton = case_when(
       canton_22=="AG"~	291,
     canton_22=="ARAI"~ 213,
      canton_22=="BE"~	323,
       canton_22=="BL"~	356,
     canton_22=="BS"~	410,
       canton_22=="FR"~	291,
     canton_22=="GE"~	299,
       canton_22=="GL"~	280,
     canton_22=="GR"~	275,
       canton_22=="JU"~	335,
     canton_22=="LU"~	271,
       canton_22=="NE"~	353,
     canton_22=="NW"~	256,
       canton_22=="OW"~	257,
     canton_22=="SG"~	275,
       canton_22=="SH"~	305,
     canton_22=="SO"~	313,
       canton_22=="SZ"~	269,
     canton_22=="TG"~	277,
       canton_22=="TI"~	362,
     canton_22=="UR"~	241,
       canton_22=="VD"~	348,
     canton_22=="VS"~	298,
       canton_22=="ZG"~	254,
     canton_22=="ZH"~	301))
     
sas.22$kvg.month <- ifelse(sas.22$el.anspruchstarif=="ehepaar",2*sas.22$kvg.month.canton,sas.22$kvg.month.canton) #gemäss BAG ch-durchschnittswert

length(which(is.na(sas.22$kvg.month )))
length(which(is.na(sas.22$overall.care.costs.month )))


# Overall expense variable ("Anrechenbare Ausgaben")------------------------------------------------

length(which(is.na(sas.22$overall.care.costs.month)))
length(which(is.na(sas.22$mietkosten.month)))
length(which(is.na(sas.22$grundbedarf.month)))
length(which(is.na(sas.22$kvg.month)))




sas.22$el.specific.overall.expenses.month <- sas.22$overall.care.costs.month+
  sas.22$mietkosten.month + 
  sas.22$grundbedarf.month + 
  sas.22$kvg.month

############################################################################################
#### Part 5: Comparison between income and expenses
############################################################################################

boxplot(sas.22$total.income.amount.merged.month, outline=FALSE)



length(which(is.na(sas.22$total.income.amount.merged.month)))
length(which(is.na(sas.22$el.specific.overall.expenses.month)))

boxplot(sas.22$el.specific.overall.expenses.month, outline=FALSE)


sas.22$difference.incomes.expenses <- sas.22$total.income.amount.merged.month-sas.22$el.specific.overall.expenses.month
boxplot(sas.22$difference.incomes.expenses, outline=FALSE)

length(which(is.na(sas.22$difference.incomes.expenses)))

length(which(sas.22$difference.incomes.expenses <0))

summary(sas.22$difference.incomes.expenses)

sas.22$absolute.difference.incomes.expenses <- ifelse(sas.22$difference.incomes.expenses <0, 
                                                      sas.22$difference.incomes.expenses * (-1), 
                                                      NA)
boxplot(sas.22$absolute.difference.incomes.expenses, outline=FALSE)


sas.22$monthly.expenses.surpass.income <- ifelse(sas.22$difference.incomes.expenses < (-100),1,0)

prop.table(table(sas.22$monthly.expenses.surpass.income))



############################################################################################
#### PART 6: Income Sources 
############################################################################################

# income from el 

sas.22$fi_incsourc_22_el

# # little fix regarding people who are 65 but no ahv -----------------------
# filter <- which(sas.22$fi_incsourc_22_el==1 & sas.22$fi_incsourc_22_1pill==0)
# length(filter)
# sas.22 <- sas.22[-filter,]

############################################################################################
#### PART 7: Final NTU variable 
############################################################################################


# calculate eligibility ---------------------------------------------------

sas.22$eligible.for.EL <- ifelse(sas.22$monthly.expenses.surpass.income==1,1,0)

prop.table(table(sas.22$eligible.for.EL))

sas.22$NTU.EL <- ifelse(sas.22$eligible.for.EL==1 & sas.22$fi_incsourc_22_el==0,1,0)

table(sas.22$NTU.EL)
table(sas.22$NTU.EL, sas.22$fi_incsourc_22_el)
prop.table(table(sas.22$NTU.EL)) #unweighted values 


# simulate ----------------------------------------------------------------

sas.22$theoretical.income.realized.el.anspruch <- sas.22$equiv.hh.income+sas.22$absolute.difference.incomes.expenses
length(which(is.na(sas.22$sas.22$equiv.hh.income)))
sas.22$poverty.bn.realized.el.anspruch <- ifelse(is.na(sas.22$theoretical.income.realized.el.anspruch),NA, 
                            ifelse(sas.22$theoretical.income.realized.el.anspruch<=2279,1,0) #based on 2020 SKOS / BFS richtlinien
)

dsurvey <- svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22)

svymean(~poverty.bn, dsurvey,na.rm=TRUE) 
svymean(~poverty.bn.realized.el.anspruch, dsurvey,na.rm=TRUE) 


svytable(~NTU.EL, dsurvey) # estimate on number of people affected by income poverty 




sas.22$poor.nothomeowner.less.30000.assets.realized.el.anspruch <- NA
filter <- which(sas.22$assetamount.merged<30000 & sas.22$poverty.bn.realized.el.anspruch==1 & sas.22$realest.merged.val==0 )
sas.22$poor.nothomeowner.less.30000.assets.realized.el.anspruch[filter] <- 1
filter <- which(sas.22$assetamount.merged>=30000)
sas.22$poor.nothomeowner.less.30000.assets.realized.el.anspruch[filter] <- 0
filter <- which(sas.22$poverty.bn.realized.el.anspruch==0)
sas.22$poor.nothomeowner.less.30000.assets.realized.el.anspruch[filter] <- 0
filter <- which(sas.22$realest.merged.val>1)
sas.22$poor.nothomeowner.less.30000.assets.realized.el.anspruch[filter] <- 0
sas.22 %>% count(poor.nothomeowner.less.30000.assets) 
sas.22 %>% count(poor.nothomeowner.less.30000.assets.realized.el.anspruch) 

svymean(~poor.nothomeowner.less.30000.assets.realized.el.anspruch, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight

length(which(is.na(sas.22$poor.nothomeowner.less.30000.assets)))

length(which(is.na(sas.22$poor.nothomeowner.less.30000.assets.realized.el.anspruch)))


svymean(~poor.nothomeowner.less.30000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight

svytable(~poor.nothomeowner.less.30000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight




# create survey objects ---------------------------------------------------


save(sas.22, file="data_sas22-variable-setup-NTUcalc-done.Rdata")

