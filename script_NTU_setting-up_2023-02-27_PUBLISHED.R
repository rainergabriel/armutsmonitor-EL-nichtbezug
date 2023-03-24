############################################################################################
############################################################################################
####
#### ---------------------------------------------------------------------------------------
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


# libraries ---------------------------------------------------------------

# if(!require(readxl)) {install.packages("readxl")}
# if(!require(sf)) {install.packages("sf",dependencies=TRUE)}
#   if(!require(eply)) {install.packages("eply")}
#     if(!require(ggthemes)) {install.packages("ggthemes")}
#       if(!require(rcartocolor)) {install.packages("rcartocolor")}
#         if(!require(ggtext)) {install.packages("ggtext")}
# if(!require(DescTools)) {install.packages("DescTools")}
# if(!require(PropCIs)) {install.packages("PropCIs")}
# if(!require(svglite)) {install.packages("svglite")}
# if(!require(Cairo)) {install.packages("Cairo",dependencies=TRUE)}
# if(!require(tidyverse)) {install.packages("tidyverse")}
# if(!require(questionr)) {install.packages("questionr")}
# if(!require(survey)) {install.packages("survey")}
# if(!require(parsnip)) {install.packages("parsnip")}
# if(!require(tidymodels)) {install.packages("tidymodels", dependencies=TRUE)}
# if(!require(finalfit)) {install.packages("finalfit", dependencies=TRUE)}
# if(!require(stargazer)) {install.packages("stargazer", dependencies=TRUE)}


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
library(survey)
library(ggtext)
library(DescTools)
library(PropCIs)
library(svglite)
library(parsnip)
# library(tidymodels)
library(finalfit)
library(ggrepel)
library(stargazer)


# loading data ------------------------------------------------------------

rm(list=ls())
load(file="/Users/gabn/switchdrive/Altersmonitoring und Alterssurvey PS-CH/4_Erhebung/datenbereinigung/data_sas22_ReleaseB_2022-11-22.Rdata") #workmachine
load(file="/Users/rainer/switchdrive2/Altersmonitoring und Alterssurvey PS-CH/4_Erhebung/datenbereinigung/data_sas22_ReleaseB_2022-11-22.Rdata") #homemachine



############################################################################################
############################################################################################
####SETTING UP 
############################################################################################
############################################################################################



# setup target variables --------------------------------------------------

# creating a variable based on the categorical response for incomes where there are virtual fixed amounts 
sas.22$fi_income_22_category
levels(as.factor(sas.22$fi_income_22_category))
sas.22$virtual.amount.income.cat <- ifelse(sas.22$fi_income_22_category==0, median(0:2000), 
                                           ifelse(sas.22$fi_income_22_category==1, median(2000:2300), 
                                                  ifelse(sas.22$fi_income_22_category==2, median(2300:3000), 
                                                         ifelse(sas.22$fi_income_22_category==3, median(3000:4000), 
                                                                ifelse(sas.22$fi_income_22_category==4, median(4000:4800), 
                                                                       ifelse(sas.22$fi_income_22_category==5, median(4800:6000), 
                                                                              ifelse(sas.22$fi_income_22_category==6, median(6000:8000), 
                                                                                     ifelse(sas.22$fi_income_22_category==7, median(8000:15000),
                                                                                            NA))))))))
sas.22 %>%  count(virtual.amount.income.cat)

#merge the "virtual-fixed-amount-for-the-categorical-responses" and income responses 
sas.22$income.amount.merged <- sas.22$fi_income_22_amount
filter <- which(is.na(sas.22$fi_income_22_amount))
sas.22$income.amount.merged[filter] <- sas.22$virtual.amount.income.cat[filter]
length(which(is.na(sas.22$income.amount.merged))) #about 10% missing across both variables which is acceptable
sas.22 %>%  count(income.amount.merged)
plot(log(sas.22$income.amount.merged))
sas.22 %>% filter(income.amount.merged<500) %>% count(income.amount.merged)
min(sas.22$income.amount.merged,na.rm=TRUE)
max(sas.22$income.amount.merged,na.rm=TRUE)


#creating the equivalizing factor based on modified OECD methodology 
sas.22 %>% count(pi_nbhoushold_22)
length(which(is.na(sas.22$pi_nbhoushold_22)))

sas.22$household.equiv.weighting.factor <- ifelse(sas.22$pi_nbhoushold_22==1,1,
                                                  ifelse(sas.22$pi_nbhoushold_22==2,(1+(1*0.5)),
                                                         ifelse(sas.22$pi_nbhoushold_22==3,(1+(2*0.5)),
                                                                ifelse(sas.22$pi_nbhoushold_22==4,(1+(3*0.5)),
                                                                       ifelse(sas.22$pi_nbhoushold_22==5,(1+(4*0.5)), 
                                                                              ifelse(sas.22$pi_nbhoushold_22==6,(1+(5*0.5)), 
                                                                                     ifelse(sas.22$pi_nbhoushold_22==7,(1+(6*0.5)), 
                                                                                            ifelse(sas.22$pi_nbhoushold_22==8,(1+(7*0.5)), 
                                                                                                   ifelse(sas.22$pi_nbhoushold_22==9,(1+(8*0.5)), 
                                                                                                          NA)))))))))
sas.22 %>% count(household.equiv.weighting.factor)


#correcting those who live with children (weighted with 0.3) and not with adults (weighted 0.5)
filter <- which(sas.22$pi_nbhoushold_22==2 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1.3
filter <- which(sas.22$pi_nbhoushold_22==3 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1+0.5+0.3
filter <- which(sas.22$pi_nbhoushold_22==4 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1+(2*0.5)+0.3
filter <- which(sas.22$pi_nbhoushold_22==5 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1+(3*0.5)+0.3
filter <- which(sas.22$pi_nbhoushold_22==6 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1+(4*0.5)+0.3
filter <- which(sas.22$pi_nbhoushold_22==7 & sas.22$pi_nbhoushold_22_child==1)
length(filter)
sas.22$household.equiv.weighting.factor[filter] <- 1+(5*0.5)+0.3

sas.22 %>% count(household.equiv.weighting.factor)

# calculating equivalized household income variable

sas.22$equiv.hh.income <- sas.22$income.amount.merged/(sas.22$household.equiv.weighting.factor)
length(which(is.na(sas.22$equiv.hh.income)))

boxplot(sas.22$equiv.hh.income, outline=FALSE)

# creating a binary poverty variable 

sas.22$poverty.bn <- ifelse(is.na(sas.22$equiv.hh.income),NA, 
                            ifelse(sas.22$equiv.hh.income<=2279,1,0) #based on 2020 SKOS / BFS richtlinien
                            )


# create armutsgefährdung variable 
 
rel.poverty.line <- 2506 #gemäss BFS 2020

sas.22$rel.poverty.bn <- ifelse(is.na(sas.22$equiv.hh.income),NA, 
                                ifelse(sas.22$equiv.hh.income<=rel.poverty.line,1,0) #based on 60% median income (silc)
)

# check the deprivation variable 
sas.22$fi_suddenexp_22
sas.22 %>% count(fi_suddenexp_22)
sas.22$fi_endsmeet_22

sas.22 <- sas.22 %>% mutate(
  cant.face.sudden.exp = case_when(
  fi_suddenexp_22 == 0 ~ 1,
  fi_suddenexp_22 == 1 ~ 0), 
  cant.holiday.away = case_when(
    mu_affordholiday_22 == 0 ~ 1,
    mu_affordholiday_22 == 1 ~ 0),
  difficulty.make.ends.meet = case_when(
    fi_endsmeet_22>=2 ~ 0, 
    fi_endsmeet_22<2 ~ 1)
  ) 
sas.22 %>% select(difficulty.make.ends.meet, cant.holiday.away, cant.face.sudden.exp)

# difficulties making ends meet 

prop.table(table(sas.22$fi_endsmeet_22))
sas.22$fi_endsmeet_22


#multi poverty

sas.22$poor.deprived.subjectivepoor <- NA
complete <- complete.cases(subset(sas.22, select=c(poverty.bn,fi_endsmeet_22, cant.face.sudden.exp)))
length(which(!is.na(complete)))
filter <- which(sas.22$poverty.bn==1 
                & sas.22$difficulty.make.ends.meet==1
                & sas.22$cant.face.sudden.exp==1
                )
length(filter)
sas.22$poor.deprived.subjectivepoor[filter] <- 1
filter <- which(sas.22$poverty.bn==0 
                | sas.22$difficulty.make.ends.meet==0
                | sas.22$cant.face.sudden.exp==0
)
sas.22$poor.deprived.subjectivepoor[filter] <- 0
sas.22 %>%  count(poor.deprived.subjectivepoor) 
sas.22 %>%  count(poor.deprived.subjectivepoor) %>% summarise(n/sum(n))
# einkommensquellen 

# 1pill
sas.22 %>% count(fi_incsourc_22_1pill)
prop.table( wtd.table(x=as.factor(sas.22$fi_incsourc_22_1pill), 
                      weights=sas.22$cross.design.weights.sample_22))

# 2pill
prop.table( wtd.table(x=as.factor(sas.22$fi_incsourc_22_2pill), 
                      weights=sas.22$cross.design.weights.sample_22))


# EL
sas.22 %>% count(fi_incsourc_22_el)
prop.table( wtd.table(x=as.factor(sas.22$fi_incsourc_22_el), 
            weights=sas.22$cross.design.weights.sample_22))


# kantonale beihilfen
sas.22 %>% count(fi_incsourc_22_hous) %>% summarise(n/sum(n))

# erwerb
sas.22 %>% count(fi_incsourc_22_employment) %>% summarise(n/sum(n))

#unterstützung durch freunde und familie 
sas.22 %>% count(fi_incsourc_22_friendfam) %>% summarise(n/sum(n))

#cashout 
prop.table(wtd.table(x = as.factor(sas.22$fi_cashout2pil_22),
                     weights = sas.22$cross.design.weights.sample_22))

prop.table(wtd.table(x = as.factor(sas.22$fi_2pil4home_22),
                     weights = sas.22$cross.design.weights.sample_22))

#typologie einkommensquellen 

sas.22 %>% count(fi_incsourc_22_2pill)
sas.22 %>% count(fi_incsourc_22_employment)

# first, merge AHV with EL
sas.22$fi_incsourc_22_1pill.corr <- sas.22$fi_incsourc_22_1pill
filter <- which(sas.22$fi_incsourc_22_el==1 &  sas.22$fi_incsourc_22_1pill==0)
length(filter)
sas.22$fi_incsourc_22_1pill.corr[filter] <- 1

sas.22 <- sas.22 %>% mutate(inc.source.type =case_when(
  fi_incsourc_22_1pill.corr == 1 & fi_incsourc_22_2pill == 0 & fi_incsourc_22_employment == 0~ "Nur 1. Säule",
  fi_incsourc_22_1pill.corr == 1 & fi_incsourc_22_2pill == 0 & fi_incsourc_22_employment == 1~ "1. Säule + Arbeit",
  fi_incsourc_22_1pill.corr == 1 & fi_incsourc_22_2pill == 1 &fi_incsourc_22_employment == 0~ "1. + 2. Säule",
  fi_incsourc_22_1pill.corr == 1 & fi_incsourc_22_2pill == 1 & fi_incsourc_22_employment == 1~ "1. + 2. Säule + Arbeit",
  fi_incsourc_22_1pill.corr == 0 & fi_incsourc_22_2pill == 1 & fi_incsourc_22_employment == 0~ "Nur 2. Säule",
  fi_incsourc_22_1pill.corr == 0 & fi_incsourc_22_2pill == 0 & fi_incsourc_22_employment == 1 ~ "Nur Arbeit", 
  fi_incsourc_22_1pill.corr == 0 & fi_incsourc_22_2pill == 0 & fi_incsourc_22_employment == 0 ~ "Andere"))




sas.22 %>% count(inc.source.type)

  sas.22$inc.source.type.rcd <- ifelse(
  sas.22$inc.source.type == "Andere" |  
  sas.22$inc.source.type =="Nur 2. Säule" | 
  sas.22$inc.source.type == "Nur Arbeit",
  "Andere",
  sas.22$inc.source.type)
  
  sas.22 %>% count(inc.source.type.rcd)
  


table(sas.22$inc.source.type, sas.22$pi_age.group_22)


# setup covariates


#sex
sas.22 %>% count(pi_sex_22)


# age group 
sas.22 %>% count(pi_age.group_22)

sas.22 <- sas.22 %>% mutate(age.group.3cat =  case_when(
   # pi_ageinterview_22>=55 & pi_ageinterview_22 <=64 ~"55-64",
   pi_ageinterview_22>=65 & pi_ageinterview_22 <=74   ~"65-74",
   pi_ageinterview_22>=75   ~"75+")) 

prop.table(table(sas.22$age.group.3cat))

# migration 

 sas.22 %>%  count(pi_swissnational_22)
 
 #civstat recode
 
 sas.22 <- sas.22 %>% mutate(civstat.rcd = case_when(
   pi_civstat_22 == 0 | pi_civstat_22 == 1 ~ "Verheiratet / Regist.Part.", 
   pi_civstat_22 == 2 ~ "Ledig", 
   pi_civstat_22 == 3 ~ "Geschieden", 
   pi_civstat_22 == 4 ~ "Verwitwet"
 )) %>%  mutate(civstat.rcd=factor(civstat.rcd, levels=c("Verheiratet / Regist.Part.", 
                                                         "Ledig", 
                                                         "Geschieden", 
                                                         "Verwitwet"), ordered=TRUE))

 sas.22 %>%  count(civstat.rcd)

  # education 
 sas.22$se_edu_22
 sas.22 %>% count(se_edu_22)
 sas.22 <- sas.22 %>% mutate(edu.rcd = case_when(
   se_edu_22<4 ~ "low", 
   se_edu_22>=3 & se_edu_22<=7 ~"avg", 
   se_edu_22>=8 ~ "high"
 ))
 sas.22 %>% count(edu.rcd)
 sas.22$edu.rcd <- as.factor(sas.22$edu.rcd)
 is.list(sas.22$edu.rcd)
 is.factor(sas.22$edu.rcd)
 
 

# children ----------------------------------------------------------------

table(sas.22$pi_nbchildren_22) 
 
sas.22$has.children <- ifelse(sas.22$pi_nbchildren_22>0,1,0)
 # household typology
 
 levels(as.factor(sas.22$household.typology))
 
 sas.22 <- sas.22 %>% mutate(household.typology = case_when(
   (pi_nbhoushold_22 == 2 &  pi_nbhoushold_22_part == 1) | 
     (pi_nbhoushold_22 == 2 & pi_nbhoushold_22_spous == 1 ) ~ "(Ehe)Paarhaushalt"  , 
   pi_nbhoushold_22 == 1 ~ "Einzelhaushalt", 
   (pi_nbhoushold_22 == 3 &  pi_nbhoushold_22_child == 1 &  pi_nbhoushold_22_part == 1) | 
     (pi_nbhoushold_22 == 3 &  pi_nbhoushold_22_child == 1 &  pi_nbhoushold_22_spous == 1) ~ "3-er Familienhaushalt",
   pi_nbhoushold_22 == 2 &  pi_nbhoushold_22_child == 1 ~ "Alleinerziehende", 
     pi_nbhoushold_22 >= 4 |
     (pi_nbhoushold_22 == 2 &  pi_nbhoushold_22_hmate  == 1)  |
     (pi_nbhoushold_22 == 2 &  pi_nbhoushold_22_part  == 0 &  pi_nbhoushold_22_part  == 0 &  pi_nbhoushold_22_child == 0)  |
     (pi_nbhoushold_22 == 3 &  pi_nbhoushold_22_child == 0)  |
     (pi_nbhoushold_22 == 3 &  pi_nbhoushold_22_child == 1)~ "Andere"
 ))  %>% mutate(household.typology=factor(household.typology, levels=c("Einzelhaushalt", 
                                                                   "Alleinerziehende",
                                                                      "(Ehe)Paarhaushalt", 
                                                                   "3-er Familienhaushalt",
                                                                      "Andere"), 
                                         ordered=TRUE))

sas.22 %>% count(household.typology)

#gemeindegrösse

sas.22 %>% count(env_municipalitysizecat_22)
levels(sas.22$env_municipalitysizecat_22)
sas.22 <- sas.22 %>% mutate(villagesize = case_when(
  env_municipalitysizecat_22 == "<1000"  |
    env_municipalitysizecat_22 == "1000-1999" ~ "<2000",
    env_municipalitysizecat_22 =="2000-4999" ~ "2000-4999", 
  env_municipalitysizecat_22 == "5000-9999"~ "5000-9999", 
    env_municipalitysizecat_22 == "10'000-19'999" ~ "10000-19999",
    env_municipalitysizecat_22 =="20'000-49'999" ~ "20000-49999",
  env_municipalitysizecat_22 == "50'000-99'999"   |
    env_municipalitysizecat_22 ==">100'000"  ~ ">50000"
)) %>% mutate(villagesize = factor(villagesize, ordered=TRUE, levels=c("<2000",
                                                                       "2000-4999",
                                                                       "5000-9999",
                                                                       "10000-19999",
                                                                       "20000-49999",
                                                                       ">50000")))
sas.22 %>%  count(villagesize)

# DEGURBA umcodieren

sas.22 <- sas.22 %>% mutate(degurba.rcd = case_when(
  env_degurba_22 == 1 ~ "Städtische Gemeinde", 
  env_degurba_22 == 2 ~ "Peri-urbane Gemeinde",
  env_degurba_22 == 3 ~ "Ländliche Gemeinde" 
)) 
sas.22 %>% count(degurba.rcd)

# Vermögen 

sas.22$fi_homeowner_22
sas.22 %>% count(fi_homeowner_22) 
sas.22 %>% count(fi_homeowner_22) %>% summarise(n/sum(n))

sas.22 <- sas.22 %>% mutate(homeowner.rcd = case_when(
  fi_homeowner_22 == 1 ~ "Eigenheimbesitz", 
  fi_homeowner_22!=1 ~ "Kein Eigenheimbesitz"
))

boxplot(log(sas.22$fi_valrealest_22))
filter <- which(sas.22$fi_valrealest_22<100000)
filter <- which(sas.22$fi_valrealest_22<5000)

length(filter) # Hypothese: diejenigen mit wert <100'000 haben das Haus wohl Geschenkt bekommen. Auschluss aber nicht plausibel und verändert Resultat nicht. 
length(which(!is.na(sas.22$fi_valrealest_22)))

# merge numerical direct-response variable and categorical in one "merged value variable" for realestate
sas.22$realest.merged.val <- sas.22$fi_valrealest_22
length(which(is.na(sas.22$realest.merged.val)))
length(which(!is.na(sas.22$realest.merged.val)))


sas.22$fi_valrestestcat_22
length(which(!is.na(sas.22$fi_valrestestcat_22)))

sas.22$virtual.realest.amount <- ifelse(sas.22$fi_valrestestcat_22==0,median(0:250000),
                                        ifelse(sas.22$fi_valrestestcat_22==1,median(250000:500000), 
                                               ifelse(sas.22$fi_valrestestcat_22==2,median(500000:1000000),
                                               ifelse(sas.22$fi_valrestestcat_22==3,median(1100000),NA))))
length(which(!is.na(sas.22$fi_valrestestcat_22))) == length(which(!is.na(sas.22$virtual.realest.amount)))
sas.22 %>% count(virtual.realest.amount)

filter <- which(is.na(sas.22$fi_valrealest_22))
length(filter)
sas.22$realest.merged.val[filter] <- sas.22$virtual.realest.amount[filter]
length(which(is.na(sas.22$realest.merged.val)))

filter <- which(is.na(sas.22$realest.merged.val))
length(filter)
sas.22$realest.merged.val[filter] <- 0 # this hypothesis is plausible: those who are not homeowners and have no value reported for real estate have value 0
hist(log(sas.22$realest.merged.val))
length(which(!is.na(sas.22$realest.merged.val)))

sas.22 %>% select(Respondent_ID,homeowner.rcd,fi_valrealest_22,virtual.realest.amount,realest.merged.val)

length(which(is.na(sas.22$realest.merged.val)))
length(which(sas.22$realest.merged.val==0))


#Assets

sas.22$fi_assetsamount_22
length(which(is.na(sas.22$fi_assetsamount_22)))
length(which(!is.na(sas.22$fi_assetsamount_22)))


sas.22$assetamount.merged <- sas.22$fi_assetsamount_22

sas.22$fi_assetscat_22
length(which(!is.na(sas.22$fi_assetscat_22)))
sas.22$virtual.asset.amount <- ifelse(sas.22$fi_assetscat_22==0,median(0:4000),
                                      ifelse(sas.22$fi_assetscat_22==1,median(4000:10000), 
                                             ifelse(sas.22$fi_assetscat_22==2,median(10000:37500), 
                                                    ifelse(sas.22$fi_assetscat_22==3,median(37500:60000),
                                                           ifelse(sas.22$fi_assetscat_22==4,median(60000:100000),
                                                                  ifelse(sas.22$fi_assetscat_22==5,median(100000:112500),
                                                                         ifelse(sas.22$fi_assetscat_22==6,median(112500:300000),
                                                                                ifelse(sas.22$fi_assetscat_22==7,median(300000:500000),
                                                                                       ifelse(sas.22$fi_assetscat_22==8,median(500000:1000000),
                                                                                              ifelse(sas.22$fi_assetscat_22==9,median(1000000:1500000),
                                                                                                     NA))))))))))
                                       
length(which(!is.na(sas.22$fi_assetscat_22))) == length(which(!is.na(sas.22$virtual.asset.amount)))

filter <- which(is.na(sas.22$fi_assetsamount_22))
length(filter)
sas.22$assetamount.merged[filter] <- sas.22$virtual.asset.amount[filter]
length(which(!is.na(sas.22$assetamount.merged)))


  
  
  # morgage
sas.22$fi_morgage_22
sas.22 %>% count(fi_morgage_22)

sas.22$fi_summorgage_22
length(which(!is.na(sas.22$fi_summorgage_22)))

#again, merge the one-shot and categorical (virtual) into one
sas.22$morgage.merged.amount <- sas.22$fi_summorgage_22

sas.22$virtual.morgage.amount <- ifelse(sas.22$fi_summorgagecat_22==0,median(0:250000),
                                        ifelse(sas.22$fi_summorgagecat_22==1,median(250000:500000), 
                                               ifelse(sas.22$fi_summorgagecat_22==2,median(500000:1000000),
                                                      ifelse(sas.22$fi_summorgagecat_22==3,median(1000001),NA))))
length(which(!is.na(sas.22$fi_summorgagecat_22))) == length(which(!is.na(sas.22$virtual.morgage.amount)))

filter <- which(sas.22$fi_morgage_22==1 & is.na(sas.22$fi_summorgage_22))
length(filter)
sas.22$morgage.merged.amount[filter] <- sas.22$virtual.morgage.amount[filter]
length(which(!is.na(sas.22$morgage.merged.amount)))

length(which(is.na(sas.22$morgage.merged.amount)))
filter <- which(sas.22$realest.merged.val==0 & is.na(sas.22$morgage.merged.amount))
length(filter)
sas.22$morgage.merged.amount[filter] <- 0
# create merged wealth variable(s)


sas.22$gross.wealth.sum <- sas.22$assetamount.merged + sas.22$realest.merged.val
sas.22$net.wealth.sum <- sas.22$gross.wealth.sum - sas.22$morgage.merged.amount
filter <- which(sas.22$net.wealth.sum<0)
length(filter)
sas.22$net.wealth.sum[filter] <- NA 
length(which(is.na(sas.22$net.wealth.sum)))
sas.22 %>% count(sas.22$net.wealth.sum)

# create wealth indicators mirroring those in BFS reports
sas.22$less.10000.assets <- NA
filter <- which(sas.22$assetamount.merged<10000)
sas.22$less.10000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=10000)
sas.22$less.10000.assets[filter] <- 0
svymean(~less.10000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight

sas.22$less.20000.assets <- NA
filter <- which(sas.22$assetamount.merged<20000)
sas.22$less.20000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=20000)
sas.22$less.20000.assets[filter] <- 0
svymean(~less.20000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight


sas.22$poor.less.20000.assets <- NA
filter <- which(sas.22$assetamount.merged<20000 & sas.22$poverty.bn==1)
sas.22$poor.less.20000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=20000)
sas.22$poor.less.20000.assets[filter] <- 0
filter <- which(sas.22$poverty.bn==0)
sas.22$poor.less.20000.assets[filter] <- 0
sas.22 %>% count(poor.less.20000.assets) 
svymean(~poor.less.20000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight



sas.22$less.30000.assets <- NA
filter <- which(sas.22$assetamount.merged<30000)
sas.22$less.30000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=30000)
sas.22$less.30000.assets[filter] <- 0
svymean(~less.30000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight


sas.22$poor.less.30000.assets.but.house <- NA
filter <- which(sas.22$assetamount.merged<30000 & sas.22$poverty.bn==1 & sas.22$fi_homeowner_22==1)
sas.22$poor.less.30000.assets.but.house[filter] <- 1
filter <- which(sas.22$assetamount.merged>=30000)
sas.22$poor.less.30000.assets.but.house[filter] <- 0
filter <- which(sas.22$poverty.bn==0)
sas.22$poor.less.30000.assets.but.house[filter] <- 0
filter <- which(sas.22$fi_homeowner_22==0)
sas.22$poor.less.30000.assets.but.house[filter] <- 0
svymean(~poor.less.30000.assets.but.house, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight
svytable(~poor.less.30000.assets.but.house, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight



sas.22$less.10000.assets <- NA
filter <- which(sas.22$assetamount.merged<10000)
sas.22$less.10000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=10000)
sas.22$less.10000.assets[filter] <- 0
sas.22 %>% count(less.10000.assets)



sas.22$over.100000.assets <- NA
filter <- which(sas.22$assetamount.merged<=100000)
sas.22$over.100000.assets[filter] <- 0
filter <- which(sas.22$assetamount.merged>100000)
sas.22$over.100000.assets[filter] <- 1
sas.22 %>% count(over.100000.assets)


sas.22$over.10e6.assets <- NA
filter <- which(sas.22$assetamount.merged<=1000000)
sas.22$over.10e6.assets[filter] <- 0
filter <- which(sas.22$assetamount.merged>1000000)
sas.22$over.10e6.assets[filter] <- 1
sas.22 %>% count(over.10e6.assets)
svytable(~over.10e6.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight

length(which(is.na(sas.22$net.wealth.sum)))
sas.22$over.10e6.netwealth <- NA
filter <- which(sas.22$net.wealth.sum<=1000000)
sas.22$over.10e6.netwealth[filter] <- 0
filter <- which(sas.22$assetamount.merged>1000000)
sas.22$over.10e6.netwealth[filter] <- 1
sas.22 %>% count(over.10e6.assets)
svytable(~over.10e6.netwealth, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight



sas.22$over.100.assets.and.houseowner <- NA
filter <- which(sas.22$assetamount.merged>100000 & sas.22$fi_homeowner_22==1)
sas.22$over.100.assets.and.houseowner[filter] <- 1
filter <- which(sas.22$assetamount.merged<=100000)
sas.22$over.100.assets.and.houseowner[filter] <- 0
filter <- which(sas.22$fi_homeowner_22==0)
sas.22$over.100.assets.and.houseowner[filter] <- 0
sas.22 %>% count(over.100.assets.and.houseowner) 
svymean(~over.100.assets.and.houseowner, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight
svytable(~over.100.assets.and.houseowner, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight


length(which(is.na(sas.22$realest.merged.val)))
length(which(sas.22$realest.merged.val==0))


sas.22$poor.nothomeowner.less.30000.assets <- NA
filter <- which(sas.22$assetamount.merged<30000 & sas.22$poverty.bn==1 & sas.22$realest.merged.val==0 )
sas.22$poor.nothomeowner.less.30000.assets[filter] <- 1
filter <- which(sas.22$assetamount.merged>=30000)
sas.22$poor.nothomeowner.less.30000.assets[filter] <- 0
filter <- which(sas.22$poverty.bn==0)
sas.22$poor.nothomeowner.less.30000.assets[filter] <- 0
filter <- which(sas.22$realest.merged.val>1)
sas.22$poor.nothomeowner.less.30000.assets[filter] <- 0
sas.22 %>% count(poor.nothomeowner.less.30000.assets) 
svymean(~poor.nothomeowner.less.30000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight
svytable(~poor.nothomeowner.less.30000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight





sas.22$multi.poor <- NA
filter <- which(
  sas.22$assetamount.merged<30000 & 
    sas.22$poverty.bn==1 & 
    sas.22$fi_homeowner_22==0 &
    sas.22$inc.source.type =="Nur 1. Säule" )
sas.22$multi.poor[filter] <- 1
filter <- which(sas.22$assetamount.merged>=30000)
sas.22$multi.poor[filter] <- 0
filter <- which(sas.22$poverty.bn==0)
sas.22$multi.poor[filter] <- 0
filter <- which(sas.22$fi_homeowner_22==1)
sas.22$multi.poor[filter] <- 0
filter <- which(sas.22$inc.source.type !="Nur 1. Säule")
sas.22$multi.poor[filter] <- 0
sas.22 %>% count(multi.poor) 
svymean(~multi.poor, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22), 
        na.rm=TRUE) # weight
svytable(~poor.nothomeowner.less.30000.assets, svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)) # weight




# multidimensional indicators wealth, specific to household constellation, marital status and EL-specific asset-levels

levels(as.factor(sas.22$civstat.rcd))

sas.22 <- sas.22 %>% mutate(less30kassets.livingalone.unmarried = case_when(
  civstat.rcd != "Verheiratet / Regist.Part." & pi_nbhoushold_22==1 & sas.22$assetamount.merged<30000 ~ 1, 
  civstat.rcd != "Verheiratet / Regist.Part." & pi_nbhoushold_22==1 & sas.22$assetamount.merged>=30000 ~ 0 
)) 

sas.22 %>% count(less30kassets.livingalone.unmarried) 

sas.22 <- sas.22 %>% mutate(less30kassets.poor.livingalone.unmarried = case_when(
  less30kassets.livingalone.unmarried == 1 & poverty.bn == 1 ~ 1, 
  (less30kassets.livingalone.unmarried == 1 & poverty.bn == 0) | 
    (less30kassets.livingalone.unmarried == 0 & poverty.bn == 1) | 
    (less30kassets.livingalone.unmarried == 0 & poverty.bn == 0) ~ 0))

sas.22 %>% count(less30kassets.poor.livingalone.unmarried)

sas.22 <- sas.22 %>% mutate(less50kassets.couplehhold.married = case_when(
  civstat.rcd == "Verheiratet / Regist.Part." & pi_nbhoushold_22==2 & sas.22$assetamount.merged<50000 ~ 1, 
  civstat.rcd == "Verheiratet / Regist.Part." & pi_nbhoushold_22==2 & sas.22$assetamount.merged>=50000 ~ 0 
))
sas.22 %>% count(less50kassets.couplehhold.married)


sas.22 <- sas.22 %>% mutate(less50kassets.poor.couplehhold.married = case_when(
  less50kassets.couplehhold.married == 1 & poverty.bn == 1 ~ 1, 
  (less50kassets.couplehhold.married == 1 & poverty.bn == 0) | 
    (less50kassets.couplehhold.married == 0 & poverty.bn) == 1| 
    (less50kassets.couplehhold.married == 0 & poverty.bn == 0) ~ 0))

sas.22 %>% count(less50kassets.poor.couplehhold.married) 



# other deprivation variables 

sas.22$mu_affordholiday_22

sas.22$difficulty.make.ends.meet



# explanatory variables 

sas.22$srh.bn <- ifelse(sas.22$ph_srh_22==0,1,0)
sas.22 %>% count(sas.22$srh.bn)

sas.22 <- sas.22 %>% mutate(srh.3cat = case_when(
  ph_srh_22 == 0 ~ "Schlechte Gesundheit", 
  ph_srh_22 == 1 ~ "Mittelmässige Gesundheit", 
  ph_srh_22 >1 ~ "Gute bis ausgezeichnete Gesundheit"
))
sas.22 %>% count(srh.3cat)

sas.22 %>% count(mh_3item.loneliness.score_22)
sas.22$lonely.3cat <- ifelse(sas.22$mh_3item.loneliness.score_22>6,"(Sehr) Einsam",
                           ifelse(sas.22$mh_3item.loneliness.score_22<=6 & sas.22$mh_3item.loneliness.score_22>3,"Etwas einsam",
                                  ifelse(is.na(sas.22$mh_3item.loneliness.score_22),NA,"Nicht einsam"))) 
sas.22$lonely.3cat <- factor(sas.22$lonely.3cat, levels=c("(Sehr) Einsam","Etwas einsam", "Nicht einsam"))
sas.22 %>% count(sas.22$lonely.3cat) 

sas.22$feeling.lonely <- NA
filter <- which(sas.22$mh_3item.loneliness.score_22>6)
sas.22$feeling.lonely[filter] <- 1
filter <- which(sas.22$mh_3item.loneliness.score_22<=6)
sas.22$feeling.lonely[filter] <- 0
sas.22 %>% count(feeling.lonely)

# genlifsat

levels(as.factor(sas.22$ls_genlifesatdiener_cat_22))
sas.22$low.life.sat.bn <- NA
filter <- which(sas.22$ls_genlifesatdiener_cat_22<3)
sas.22$low.life.sat.bn[filter] <- 1
filter <- which(sas.22$ls_genlifesatdiener_cat_22>=3)
sas.22$low.life.sat.bn[filter] <- 0
sas.22 %>% count(low.life.sat.bn)




# gali

sas.22$ph_gali_22

sas.22$cs_getcomcare_22
sas.22$cs_careneed_22



# variables specific for nichtbezug analyses ------------------------------


# Creating a typology variable for the calculation of EL eligibility --------
length(which(is.na(sas.22$pi_nbhoushold_22)))
table(sas.22$pi_nbhoushold_22)

length(which(sas.22$civstat.rcd=="Verheiratet / Regist.Part." & sas.22$pi_nbhoushold_22==1))
filter <- which(sas.22$civstat.rcd=="Verheiratet / Regist.Part." & sas.22$pi_nbhoushold_22==1)
sas.22 <- sas.22[-filter,]

sas.22$pi_nbhoushold_22 <- ifelse(sas.22$pi_livingalone_22==1,1,sas.22$pi_nbhoushold_22)

sas.22 <- sas.22 %>% mutate(household.typology.superfine = case_when(
  pi_nbhoushold_22 == 2   &  pi_nbhoushold_22_part == 1 ~ "unverheiratet paarhaushalt",
  pi_nbhoushold_22 == 2 & pi_nbhoushold_22_spous == 1  ~ "EhePaarhaushalt"  , 
  pi_nbhoushold_22 == 2 & pi_nbhoushold_22_hmate==1  | 
    pi_nbhoushold_22 == 2 & pi_nbhoushold_22_other==1 ~ "2 Erwachsene"  , 
  pi_nbhoushold_22 == 1 ~ "Einzelhaushalt", 
  pi_nbhoushold_22 == 2 &  
    pi_nbhoushold_22_child == 1 & 
    pi_nbhoushold_22_spous == 0 & 
    pi_nbhoushold_22_part == 0 &
    pi_nbhoushold_22_hmate == 0 &
    pi_nbhoushold_22_other== 0 ~  "Alleinerziehend", 
  pi_nbhoushold_22 == 3 & 
    pi_nbchildren_22==2 &
    pi_nbhoushold_22_child == 1 & 
    pi_nbhoushold_22_spous == 0 & 
    pi_nbhoushold_22_part == 0 &
    pi_nbhoushold_22_hmate == 0 &
    pi_nbhoushold_22_other== 0 ~  "Elternteil + 2k", 
  pi_nbhoushold_22 == 3 & 
    pi_nbchildren_22==1 &
    pi_nbhoushold_22_child == 1 & 
    pi_nbhoushold_22_spous == 1 & 
    pi_nbhoushold_22_part == 0 &
    pi_nbhoushold_22_hmate == 0 &
    pi_nbhoushold_22_other== 0 ~  "Ehepaar mit 1 Kind",
  pi_nbhoushold_22 == 3 & 
    pi_nbchildren_22==2 &
    pi_nbhoushold_22_child == 1 & 
    pi_nbhoushold_22_spous == 1 & 
    pi_nbhoushold_22_part == 0 &
    pi_nbhoushold_22_hmate == 0 &
    pi_nbhoushold_22_other== 0 ~  "Ehepaar mit 2 Kind",
  pi_nbhoushold_22 == 3 & 
    pi_nbchildren_22==3 &
    pi_nbhoushold_22_child == 1 & 
    pi_nbhoushold_22_spous == 1 & 
    pi_nbhoushold_22_part == 0 &
    pi_nbhoushold_22_hmate == 0 &
    pi_nbhoushold_22_other== 0 ~  "Ehepaar mit 3 Kind",
  pi_nbhoushold_22 == 3 & 
    pi_nbhoushold_22_child == 1 & 
    pi_nbhoushold_22_spous == 0 & 
    pi_nbhoushold_22_part == 1 &
    pi_nbhoushold_22_hmate == 0 &
    pi_nbhoushold_22_other== 0 ~  "Unverh.paar mit 1 Kind",
  pi_nbhoushold_22 == 3 & 
    pi_nbhoushold_22_child == 2 & 
    pi_nbhoushold_22_spous == 0 & 
    pi_nbhoushold_22_part == 1 &
    pi_nbhoushold_22_hmate == 0 &
    pi_nbhoushold_22_other== 0 ~  "Unverh.paar mit 2 Kind", 
  pi_nbhoushold_22 == 3 & 
    pi_nbhoushold_22_child == 3 & 
    pi_nbhoushold_22_spous == 0 & 
    pi_nbhoushold_22_part == 1 &
    pi_nbhoushold_22_hmate == 0 &
    pi_nbhoushold_22_other== 0 ~  "Unverh.paar mit 3 Kind" , 
  pi_nbhoushold_22 == 3 & 
    pi_nbhoushold_22_child == 0 & 
    pi_nbhoushold_22_spous == 1  ~ "Ehepaar + andere Erwachsene",
  pi_nbhoushold_22 ==3 & 
    pi_nbhoushold_22_child == 0 & 
    pi_nbhoushold_22_spous == 0  ~  "3 Erwachsene", 
  pi_nbhoushold_22 ==4 & 
    pi_nbhoushold_22_child == 0 & 
    pi_nbhoushold_22_spous == 0  ~  "4 Erwachsene"
)
)


sas.22$household.typology.superfine <-  
  ifelse(is.na(sas.22$household.typology.superfine) & sas.22$pi_nbhoushold_22==1,"Einzelhaushalt",
         ifelse(is.na(sas.22$household.typology.superfine) & sas.22$pi_nbhoushold_22==2,"2 Erwachsene",                             
                ifelse(is.na(sas.22$household.typology.superfine) & sas.22$pi_nbhoushold_22==3,"3 Erwachsene",
                       ifelse(is.na(sas.22$household.typology.superfine) & sas.22$pi_nbhoushold_22==4,"4 Erwachsene",
                              ifelse(is.na(sas.22$household.typology.superfine) & sas.22$pi_nbhoushold_22>5,"unreliable",sas.22$household.typology.superfine)))))



table(sas.22$household.typology.superfine, sas.22$pi_nbhoushold_22)

length(which(is.na((sas.22$household.typology.superfine))))

filter <- which(is.na((sas.22$household.typology.superfine)))
sas.22[filter,c("pi_nbhoushold_22", "pi_nbhoushold_22_child","pi_nbhoushold_22_spous")]

#create variable, which tells me what tariff I use to calculate el-eligibility 
sas.22$el.anspruchstarif <- NA

levels <- levels(as.factor(sas.22$household.typology.superfine))

levels[1]
filter<-which(sas.22$household.typology.superfine==levels[1])
sas.22$el.anspruchstarif[filter] <- "einzel"

levels[2]
filter<-which(sas.22$household.typology.superfine==levels[2])
sas.22$el.anspruchstarif[filter] <- "einzel"

levels[3]
filter<-which(sas.22$household.typology.superfine==levels[3])
sas.22$el.anspruchstarif[filter] <- "einzel"

levels[4]
filter<-which(sas.22$household.typology.superfine==levels[4])
sas.22$el.anspruchstarif[filter] <- "einzel"

levels[5]
filter<-which(sas.22$household.typology.superfine==levels[5])
sas.22$el.anspruchstarif[filter] <- "einzel"

levels[6]
filter<-which(sas.22$household.typology.superfine==levels[6])
sas.22$el.anspruchstarif[filter] <- "ehepaar"

levels[7]
filter<-which(sas.22$household.typology.superfine==levels[7])
sas.22$el.anspruchstarif[filter] <- "ehepaar"

levels[8]
filter<-which(sas.22$household.typology.superfine==levels[8])
sas.22$el.anspruchstarif[filter] <- "ehepaar"

levels[9]
filter<-which(sas.22$household.typology.superfine==levels[9])
sas.22$el.anspruchstarif[filter] <- "ehepaar"

levels[10]
filter<-which(sas.22$household.typology.superfine==levels[10])
sas.22$el.anspruchstarif[filter] <- "ehepaar"

levels[11]
filter<-which(sas.22$household.typology.superfine==levels[11])
sas.22$el.anspruchstarif[filter] <- "einzel"

levels[12]
filter<-which(sas.22$household.typology.superfine==levels[12])
sas.22$el.anspruchstarif[filter] <- "einzel"

levels[13]
filter<-which(sas.22$household.typology.superfine==levels[13])
sas.22$el.anspruchstarif[filter] <- "einzel"

levels[14]
filter<-which(sas.22$household.typology.superfine==levels[14])
sas.22$el.anspruchstarif[filter] <- "einzel"



# frequency seeing friends ------------------------------------------------


table(sas.22$sn_freqseefriend_22)

sas.22$friends.atleast.1pweek <- ifelse(sas.22$sn_freqseefriend_22>4,1,0)
table(sas.22$friends.atleast.1pweek)




# internet ----------------------------------------------------------------

table(sas.22$nt_useinternet_22)

sas.22$never.use.internet <- ifelse(sas.22$nt_useinternet_22==0,1,0)
table(sas.22$never.use.internet)




filter <- which(is.na(sas.22$nt_useinternet_22==0))
sas.22$nt_whyuseinternet_22_com[filter] <- 0
sas.22$useinternet.communicate.friends <- ifelse(sas.22$nt_whyuseinternet_22_com>0,1,0)



# el ----------------------------------------------------------------------

filter <- which(sas.22$fi_incsourc_22_el == 1)
length(filter)

sas.22$fi_knowel_22[filter] <- 1

table(sas.22$fi_knowel_22)

# reasons EL  -------------------------------------------------------------


sas.22 <- sas.22 %>% mutate(
  reason.getEL.owndecision=fi_reasonel_221,
  reason.getEL.familymember=fi_reasonel_222,
  reason.getEL.friend=fi_reasonel_223,
    reason.getEL.socialwork=fi_reasonel_224) 


# reasons no EL -----------------------------------------------------------

sas.22 <- sas.22 %>% mutate(
  reason.noEL.keinelast=fi_noelreason_221,
    reason.noEL.shame=fi_noelreason_222,
    reason.noEL.noneed=fi_noelreason_223, 
    reason.noEL.dontknowhow=fi_noelreason_224,
  reason.noEL.languagebarrier=fi_noelreason_225,
      reason.noEL.toocomplicated=fi_noelreason_226,
  reason.noEL.time.abroad=fi_noelreason_228
)





# saving this step's dataset ----------------------------------------------




save(sas.22, file="data_sas22-variable-setup-done.Rdata")







