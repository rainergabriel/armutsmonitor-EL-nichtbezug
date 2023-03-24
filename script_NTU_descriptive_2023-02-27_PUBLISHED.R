############################################################################################
############################################################################################
####
#### Purpose of script: Analyse SAS 22 data for use of EL and NTU of EL 
#### Initial creation Date: 2022-11-16
#### Version Release: draft/working version
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
####
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
# wavelibrary(ggtext)
library(DescTools)
library(PropCIs)
library(svglite)
library(parsnip)
# library(tidymodels)
library(finalfit)
library(ggrepel)
library(stargazer)
library(survey)

rm(list=ls())
load(file="data_sas22-variable-setup-NTUcalc-done.Rdata")

# subsetting nur pensionierte -------------------------------------------------------------

sas.22 <- sas.22 %>% filter(pi_ageinterview_22>=65)


  ############################################################################################
############################################################################################
#### ANALYSES
############################################################################################
############################################################################################


dsurvey.pop <- svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.pop_22)


# set survey design for survey package (and subsequent weighted tables)
dsurvey <- svydesign(ids = sas.22$Respondent_ID, data = sas.22, weights = sas.22$cross.design.weights.sample_22)




levels(sas.22$household.typology)

sas.22$household.typology <- factor(sas.22$household.typology, 
                                    levels=c(
                                      "Einzelhaushalt"  ,
                                      "Alleinerziehende" , 
                                      "(Ehe)Paarhaushalt"   ,
                                      "3-er Familienhaushalt",
                                      "Andere" ))

# amounts for incomes, expenses and EL 

p <- sas.22 %>% filter(!is.na(household.typology)) %>%  
  ggplot(aes(x=income.amount.merged.month, y=as.factor(household.typology))) + 
  geom_boxplot(outlier.shape = NA, position = 'dodge', alpha=0.8,  fill="#C8777B") +
coord_cartesian(xlim=c(0,30000)) +
  labs(
    # title = "Anrechenbare Einkommen gemäss Haushaltstyp", 
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("") + xlab("CHF") 
p

length(which(is.na(sas.22$income.amount.merged.month)))

sas.22 %>% group_by(household.typology) %>% summarize(mean(income.amount.merged.month, na.rm=TRUE))

ggsave(
  plot = print(p),
  filename="graph_boxplot-income.png",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 250,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p),
  filename="graph_boxplot-income.svg",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 120,
  units = "mm",
  dpi = 300
)



# amounts for incomes, expenses and EL 

p <- sas.22 %>% filter(!is.na(household.typology)) %>%  
  ggplot(aes(x=el.specific.overall.expenses.month, y=as.factor(household.typology))) + 
  geom_boxplot(outlier.shape = NA, position = 'dodge', alpha=0.8,  fill="#C8777B") +
  coord_cartesian(xlim=c(0,7500)) +
  labs(
    # title = "Anerkannte Ausgaben gemäss Haushaltstyp", 
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("") + xlab("CHF") 
p


sas.22 %>% group_by(household.typology) %>% summarize(mean(el.specific.overall.expenses.month, na.rm=TRUE))

ggsave(
  plot = print(p),
  filename="graph_boxplot-expenses.png",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 250,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p),
  filename="graph_boxplot-expenses.svg",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 120,
  units = "mm",
  dpi = 300
)



# amounts for incomes, expenses and EL 

p <- sas.22 %>% filter(!is.na(household.typology)) %>%  
  ggplot(aes(x=absolute.difference.incomes.expenses, y=as.factor(household.typology))) + 
  geom_boxplot(outlier.shape = NA, position = 'dodge', alpha=0.8,  fill="#C8777B") +
  coord_cartesian(xlim=c(0,7500)) +
  labs(
    # title = "Betrag des EL-Anspruchs gemäss Haushaltstyp", 
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("") + xlab("CHF") 
p

prop.table(table(sas.22$household.typology))
table(sas.22$household.typology)

sas.22 %>% group_by(household.typology) %>% summarize(mean(absolute.difference.incomes.expenses, na.rm=TRUE))
sas.22 %>% group_by(household.typology) %>% summarize(quantile(absolute.difference.incomes.expenses, na.rm=TRUE))


ggsave(
  plot = print(p),
  filename="graph_boxplot-EL-amount.png",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 250,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p),
  filename="graph_boxplot-EL-amount.svg",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 120,
  units = "mm",
  dpi = 300
)

sas.22 %>% filter(monthly.expenses.surpass.income==1) %>% summarize(mean(absolute.difference.incomes.expenses))


# other  ------------------------------------------------------------------

svymean(~NTU.EL, dsurvey.pop,na.rm=TRUE) 


# NTU
raw.table<-svymean(~NTU.EL, dsurvey,na.rm=TRUE) # weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- ""
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","name"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

merged.table <- result.table1
merged.table 


merged.table$category <- factor(merged.table$category, ordered=TRUE)

p <- 
  ggplot(merged.table, aes(x = forcats::fct_rev(category), y=est)) + 
  geom_bar(position = 'dodge', stat="identity", alpha=0.8,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, size=0.5) +
  coord_flip() +
  geom_text(aes(label={scales::percent(est, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 4)+
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    # title = "Nichtbezug von Ergänzungsleistungen von zuhause lebenden Personen \nab 65 Jahren in der Schweiz", 
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der zuhause lebenden Bevölkerung ab 65") + xlab("")
print(p)


ggsave(
  plot = print(p),
  filename="graph_NTU.absolutequote-sas.png",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 120,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p),
  filename="graph_NTU.absolutequote-sas.svg",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 120,
  units = "mm",
  dpi = 300
)


rm(merged.table,merged.table2,raw.table,raw.table1)

length(which(complete.cases(
  subset(sas.22, select=c(
    NTU.EL)))))
  



# Poverty  --------------------------------------------------------------

# income-poverty
raw.table1<-svyby(~poverty.bn,by=~NTU.EL, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Mehr als CHF 2279 Einkommen", "Weniger als CHF 2279 Einkommen")
raw.table1
raw.table1$category <- name
raw.table1

merged.table2<- raw.table1

svytable(~poverty.bn+NTU.EL, dsurvey.pop)

# 
# #multi-poor
# raw.table1<-svyby(~poor.nothomeowner.less.30000.assets,by=~NTU.EL, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
# raw.table1
# raw.table1 <- as_tibble(raw.table1)
# raw.table1
# names(raw.table1) <- c("category","sample.mean.pov","se")
# raw.table1
# name <- c("Nicht unkompensierbar armutsbetroffen", "Unkompensierbar armutsbetroffen")
# raw.table1
# raw.table1$category <- name
# raw.table1
# raw.table1 <- raw.table1[2,]
# 
# merged.table2 <- rbind(merged.table2, raw.table1)
# merged.table2
# 
# # 
# 
# #bvg rente
# raw.table1<-svyby(~fi_incsourc_22_el,by=~fi_incsourc_22_2pill, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
# raw.table1
# raw.table1 <- as_tibble(raw.table1)
# raw.table1
# names(raw.table1) <- c("category","sample.mean.pov","se")
# raw.table1
# name <- c("BVG-Rente","Keine BVG-Rente")
# raw.table1$category <- name
# raw.table1
# 
# merged.table2 <- rbind(merged.table2, raw.table1)
# merged.table2
# 
# 
# #cashout 
# raw.table1<-svyby(~fi_incsourc_22_el,by=~fi_cashout2pil_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
# raw.table1
# raw.table1 <- as_tibble(raw.table1)
# raw.table1
# names(raw.table1) <- c("category","sample.mean.pov","se")
# raw.table1
# name <- c("Auszahlung 2./3. Säule","Keine Auszahlung 2./3. Säule")
# raw.table1$category <- name
# raw.table1
# 
# merged.table2 <- rbind(merged.table2, raw.table1)
# merged.table2
# 
# 
# #multiple armutsbetroffen
# raw.table1<-svyby(~NTU.EL,by=~, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
# raw.table1
# raw.table1 <- as_tibble(raw.table1)
# raw.table1
# names(raw.table1) <- c("category","sample.mean.pov","se")
# raw.table1
# name <- c("Nicht unkompensierbar armutsbetroffen", "Unkompensierbar armutsbetroffen")
# raw.table1$category <- name
# raw.table1
# 
# merged.table2 <- rbind(merged.table2, raw.table1)
# merged.table2
# 
# 
# 
# merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
# merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se
# 
# merged.table2$se <-NULL
# 
# names(merged.table2)
# is.character(merged.table2$category)
# merged.table2$sample.mean.pov <- as.single(merged.table2$sample.mean.pov)
# is.double(merged.table2$sample.mean.pov)
# is.numeric(merged.table2$lwr.ci)
# is.numeric(merged.table2$upr.ci)
# 
# merged.table2



merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se


# grafik
raw.table<-svymean(~NTU.EL, dsurvey,na.rm=TRUE) # weighted tables using survey packages
elbezug.mean.line <- raw.table[1]


levels(as.factor(merged.table2$category))

levels=levels=c( "Weniger als CHF 2279 Einkommen","Mehr als CHF 2279 Einkommen")



labels=levels=c( "Weniger als CHF 2279 Einkommen","Mehr als CHF 2279 Einkommen")


length(labels)==length(levels)

merged.table2$category <- factor(merged.table2$category,
                                 levels=levels,
                                 labels=labels,
                                 ordered=TRUE)
merged.table2


p2 <-
  ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) +
  geom_bar(position = 'dodge', stat="identity", alpha=0.7,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.7, size=0.5) +
  coord_flip() +
  geom_hline(yintercept = elbezug.mean.line, color='red', lwd=0.5) +
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 3)+
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  labs(
    # title = "Nichtbezug von Ergänzungsleistungen und finanzielle Indikatoren",
       # subtitle = "Basierend auf dem monatlichen Äquivalenzeinkommen des Haushalts und der SKOS Armutsgrenze",
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der zuhause lebende Bevölkerung ab 65") + xlab("")
print(p2)

ggsave(
  plot = print(p2),
  filename="graph_NTU.poverty.png",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 150,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph_NTU.poverty.svg",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 150,
  units = "mm",
  dpi = 300
)


length(which(complete.cases(
  subset(sas.22, select=c(
    NTU.EL,
    civstat.rcd,
    degurba.rcd,
    edu.rcd,
    pi_swissnational_22)))))




# simulated poverty -------------------------------------------------------

# absolute poverty 
raw.table<-svymean(~poverty.bn, dsurvey,na.rm=TRUE) # weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "Armutsbetroffen"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","category"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

merged.table <- result.table1

# einkommensarm bei EL realisierung
raw.table<-svymean(~poverty.bn.realized.el.anspruch, dsurvey,na.rm=TRUE) # weighted tables using survey packages
est <- raw.table[1]
lwr.ci<-confint(raw.table,level = 0.95)[1]
upr.ci<-confint(raw.table,level = 0.95)[2]
category <- "Armutsbetroffen, wenn EL Anspruch realisiert"
result.table1 <- cbind(est,lwr.ci,upr.ci,category)
result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","name"))
result.table1$est <- as.numeric(result.table1$est)
result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
result.table1

merged.table <- rbind(merged.table, result.table1 )
merged.table 
# 
# 
# # auswegslos arm
# raw.table<-svymean(~poor.nothomeowner.less.30000.assets, dsurvey,na.rm=TRUE) # weighted tables using survey packages
# est <- raw.table[1]
# lwr.ci<-confint(raw.table,level = 0.95)[1]
# upr.ci<-confint(raw.table,level = 0.95)[2]
# category <- "Auswegslos arm"
# result.table1 <- cbind(est,lwr.ci,upr.ci,category)
# result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","name"))
# result.table1$est <- as.numeric(result.table1$est)
# result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
# result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
# result.table1
# 
# merged.table <- rbind(merged.table, result.table1 )
# merged.table 
# 
# 
# # auswegslos arm 
# raw.table<-svymean(~poor.nothomeowner.less.30000.assets.realized.el.anspruch, dsurvey,na.rm=TRUE) # weighted tables using survey packages
# est <- raw.table[1]
# lwr.ci<-confint(raw.table,level = 0.95)[1]
# upr.ci<-confint(raw.table,level = 0.95)[2]
# category <- "Auswegslos arm, wenn EL realisiert"
# result.table1 <- cbind(est,lwr.ci,upr.ci,category)
# result.table1 <- tidyr::as_tibble(result.table1, names=c("est","lwr.ci","upr.ci","name"))
# result.table1$est <- as.numeric(result.table1$est)
# result.table1$lwr.ci <- as.numeric(result.table1$lwr.ci)
# result.table1$upr.ci <- as.numeric(result.table1$upr.ci)
# result.table1
# 
# merged.table <- rbind(merged.table, result.table1 )
# merged.table 


merged.table$category <- factor(merged.table$category, ordered=TRUE)

p <- 
  ggplot(merged.table, aes(x = forcats::fct_rev(category), y=est)) + 
  geom_bar(position = 'dodge', stat="identity", alpha=0.8,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, linewidth=0.5) +
  coord_flip() +
  geom_text(aes(label={scales::percent(est, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 4)+
  scale_y_continuous(labels = scales::percent, limits = c(0,0.25)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  labs(
    # title = "Effekt von realisierten EL-Ansprüchen auf Armutsquoten", 
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der Bevölkerung ab 65") + xlab("")
print(p)


ggsave(
  plot = print(p),
  filename="graph_simulierte-armutsindikatoren.png",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 120,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p),
  filename="graph_simulierte-armutsindikatoren.svg",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 120,
  units = "mm",
  dpi = 300
)


# Amount EL  --------------------------------------------------------------

p <- ggplot(sas.22, aes(x=absolute.difference.incomes.expenses)) + 
  geom_boxplot(outlier.shape = NA) +coord_cartesian(ylim = quantile(sas.22$absolute.difference.incomes.expenses, c(0.1, 0.9), na.rm=TRUE))
p

  
  # classic sociodemographic factors ------------------------------------------------
  
  # sex 
  raw.table1<-svyby(~NTU.EL,by=~pi_sex_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
  raw.table1
  raw.table1 <- as_tibble(raw.table1)
  raw.table1
  names(raw.table1) <- c("category","sample.mean.pov","se")
  raw.table1
  name <- c("Männer","Frauen")
  raw.table1$category <- name
  raw.table1
  merged.table2 <- raw.table1
  
  #age group
  raw.table1<-svyby(~NTU.EL,by=~age.group.3cat, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
  raw.table1
  raw.table1 <- as_tibble(raw.table1)
  raw.table1
  names(raw.table1) <- c("category","sample.mean.pov","se")
  raw.table1
  merged.table2 <- rbind(merged.table2, raw.table1)
  merged.table2
  
  
  #civil status
  raw.table1<-svyby(~NTU.EL,by=~civstat.rcd, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
  raw.table1
  raw.table1 <- as_tibble(raw.table1)
  raw.table1
  names(raw.table1) <- c("category","sample.mean.pov","se")
  raw.table1
  raw.table1
  merged.table2 <- rbind(merged.table2, raw.table1)
  merged.table2
  
  # children 
  raw.table1<-svyby(~NTU.EL,by=~household.typology, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
  raw.table1
  raw.table1 <- as_tibble(raw.table1)
  raw.table1
  names(raw.table1) <- c("category","sample.mean.pov","se")
  raw.table1
  
  merged.table2 <- rbind(merged.table2, raw.table1)
  merged.table2
  
  
  
  merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
  merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se
  
  merged.table2$se <-NULL
  
  names(merged.table2)
  is.character(merged.table2$category)
  merged.table2$sample.mean.pov <- as.single(merged.table2$sample.mean.pov)
  is.double(merged.table2$sample.mean.pov)
  is.numeric(merged.table2$lwr.ci)
  is.numeric(merged.table2$upr.ci)
  
  merged.table2
  
  
  
  
  # grafik
  raw.table<-svymean(~NTU.EL, dsurvey,na.rm=TRUE) # weighted tables using survey packages
  elbezug.mean.line <- raw.table[1]
  
  
  levels(as.factor(merged.table2$category))
  
  levels=c(         "Frauen", 
                    "Männer", 
                    "65-74", 
                    "75+", 
                    "Verheiratet / Regist.Part.", 
                    "Ledig", 
                    "Geschieden", 
                    "Verwitwet",
                    "Einzelhaushalt",
                    "Alleinerziehende",
                    "(Ehe)Paarhaushalt", 
                    "3-er Familienhaushalt", 
                    "Andere" 
         )
                    
  
  labels=c(          "Frauen", 
                     "Männer", 
                     "65-74", 
                     "75+", 
                     "Verheiratet / Regist.Part.", 
                     "Ledig", 
                     "Geschieden", 
                     "Verwitwet",
                     "Einzelhaushalt",
                     "Alleinerziehende",
                     "(Ehe)Paarhaushalt", 
                     "3-er Familienhaushalt", 
                     "Andere Haushaltskonstellation" )
  
  length(labels)==length(levels)
  
  merged.table2$category <- factor(merged.table2$category, 
                                   levels=levels, 
                                   labels=labels, 
                                   ordered=TRUE)
  merged.table2
  
  
  p2 <- 
    ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) + 
    geom_bar(position = 'dodge', stat="identity", alpha=0.7,  fill="#C8777B") +
    geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.7, size=0.5) + 
    coord_flip() + 
    geom_hline(yintercept = elbezug.mean.line, color='red', lwd=0.5) +
    geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 3)+
    scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    labs(
      # title = "Nichtbezug von Ergänzungsleistungen und soziodemografische Faktoren", 
         # subtitle = "Basierend auf dem monatlichen Äquivalenzeinkommen des Haushalts und der SKOS Armutsgrenze",
         caption = "Quelle: Schweizer Alterssurvey 2022") +
    ylab("Prozent der zuhause lebenden Bevölkerung ab 65") + xlab("")
  print(p2)
  
  ggsave(
    plot = print(p2),
    filename="graph_NTU.demografie.png",
    path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
    scale = 0.5,
    width = 450,
    height = 450,
    units = "mm",
    dpi = 300
  )
  
  
  ggsave(
    plot = print(p2),
    filename="graph_NTU.demografie.svg",
    path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
    scale = 0.5,
    width = 450,
    height = 450,
    units = "mm",
    dpi = 300
  )
  
  
  length(which(complete.cases(
    subset(sas.22, select=c(
      NTU.EL, 
      civstat.rcd, 
      degurba.rcd, 
      edu.rcd, 
      pi_swissnational_22)))))



  
  # SEP ------------------------------------------------
  
  # bildung 
  
  raw.table1<-svyby(~NTU.EL,by=~edu.rcd, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
  raw.table1
  raw.table1 <- as_tibble(raw.table1)
  raw.table1
  names(raw.table1) <- c("category","sample.mean.pov","se")
  raw.table1
  merged.table2 <- raw.table1
  
  
  # csp 
  raw.table1<-svyby(~NTU.EL,by=~se_EGP5_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
  raw.table1
  raw.table1 <- as_tibble(raw.table1)
  raw.table1
  names(raw.table1) <- c("category","sample.mean.pov","se")
  raw.table1
    raw.table1
  merged.table2 <- rbind(merged.table2, raw.table1)
  merged.table2
  
  
  
  # migration 
  raw.table1<-svyby(~NTU.EL,by=~pi_swissnational_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
  raw.table1
  raw.table1 <- as_tibble(raw.table1)
  raw.table1
  names(raw.table1) <- c("category","sample.mean.pov","se")
  raw.table1
  name <- c("Ausländer:Innen","Schweizer:Innen")
  raw.table1$category <- name
  raw.table1
  merged.table2 <- rbind(merged.table2, raw.table1)
  merged.table2
  # 
  # # internet 
  # raw.table1<-svyby(~NTU.EL,by=~never.use.internet, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
  # raw.table1
  # raw.table1 <- as_tibble(raw.table1)
  # raw.table1
  # names(raw.table1) <- c("category","sample.mean.pov","se")
  # raw.table1
  # name <- c("Internetnutzer:in","Benutzt Internet nie")
  # raw.table1$category <- name
  # raw.table1
  # merged.table2 <- rbind(merged.table2, raw.table1)
  # merged.table2
  # 
  
  
  merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
  merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se
  
  merged.table2$se <-NULL
  
  names(merged.table2)
  is.character(merged.table2$category)
  merged.table2$sample.mean.pov <- as.single(merged.table2$sample.mean.pov)
  is.double(merged.table2$sample.mean.pov)
  is.numeric(merged.table2$lwr.ci)
  is.numeric(merged.table2$upr.ci)
  
  merged.table2
  
  
  
  
  # grafik
  raw.table<-svymean(~NTU.EL, dsurvey,na.rm=TRUE) # weighted tables using survey packages
  elbezug.mean.line <- raw.table[1]
  
  
  levels(as.factor(merged.table2$category))
  
  levels=c("low", 
                    "avg", 
                    "high", 
                    "I-III      White-collar workers",
                    "V+VI       Skilled workers" ,
                    "VII.a      Non-skilled workers", 
                    "Schweizer:Innen", 
                    "Ausländer:Innen" 
  )
  
  
  labels=c(
    "Obligatorische Schule", 
    "Sekundarstufe II", 
    "Tertiärstufe", 
    "Verwaltungs- und Führungsberufe",
    "Handwerkliche Berufe mit Ausbildung" ,
    "Handwerkliche Berufe ohne Ausbildung", 
    "Schweizer:Innen", 
    "Ausländer:Innen" )
    
    length(labels)==length(levels)
  
  merged.table2$category <- factor(merged.table2$category, 
                                   levels=levels, 
                                   labels=labels, 
                                   ordered=TRUE)
  merged.table2
  
  p2 <- 
    ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) + 
    geom_bar(position = 'dodge', stat="identity", alpha=0.7,  fill="#C8777B") +
    geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.7, size=0.5) + 
    coord_flip() + 
    geom_hline(yintercept = elbezug.mean.line, color='red', lwd=0.5) +
    geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 3)+
    scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    labs(
      # title = "Nichtbezug von Ergänzungsleistungen und Indikatoren der sozioökonomischen Position", 
         # subtitle = "Basierend auf dem monatlichen Äquivalenzeinkommen des Haushalts und der SKOS Armutsgrenze",
         caption = "Quelle: Schweizer Alterssurvey 2022") +
    ylab("Prozent der zuhause lebenden Bevölkerung ab 65") + xlab("")
  print(p2)
  
  ggsave(
    plot = print(p2),
    filename="graph_NTU.sep.png",
    path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
    scale = 0.5,
    width = 450,
    height = 350,
    units = "mm",
    dpi = 300
  )
  
  
  ggsave(
    plot = print(p2),
    filename="graph_NTU.sep.svg",
    path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
    scale = 0.5,
    width = 450,
    height = 350,
    units = "mm",
    dpi = 300
  )
  
  
  length(which(complete.cases(
    subset(sas.22, select=c(
      NTU.EL, 
      civstat.rcd, 
      degurba.rcd, 
      edu.rcd, 
      pi_swissnational_22)))))
  
  
  
  
  
  
  # räumliche  ------------------------------------------------
  
  
  #grösse 
  raw.table1<-svyby(~NTU.EL,by=~env_BFStypo9_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
  raw.table1
  raw.table1 <- as_tibble(raw.table1)
  raw.table1
  names(raw.table1) <- c("category","sample.mean.pov","se")
  raw.table1
  raw.table1
  merged.table2 <- raw.table1
  merged.table2
  
  
  
  
  merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
  merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se
  
  merged.table2$se <-NULL
  
  names(merged.table2)
  is.character(merged.table2$category)
  merged.table2$sample.mean.pov <- as.single(merged.table2$sample.mean.pov)
  is.double(merged.table2$sample.mean.pov)
  is.numeric(merged.table2$lwr.ci)
  is.numeric(merged.table2$upr.ci)
  
  merged.table2
  
  
  
  
  # grafik
  raw.table<-svymean(~NTU.EL, dsurvey,na.rm=TRUE) # weighted tables using survey packages
  elbezug.mean.line <- raw.table[1]
  
  
  levels(as.factor(merged.table2$category))
  
  levels=c(         
                    "11", 
                    "12", 
                    "13",
                    "21", 
                    "22",
                    "23",
                    "31",
                    "32",
                    "33")
  
  
  labels=c(
    
    "BFS Gemeindetypologie: Städtische Gemeinde \n einer grossen Agglomeration", 
    "Städtische Gemeinde \n einer mittelgrossen Agglomeration", 
    " Städtische Gemeinde \n einer kleinen Agglomeration oder ausserhalb", 
    "Periurbane Gemeinde hoher Dichte", 
    "Periurbane Gemeinde mittlerer dichte", 
    "Periurbane Gemeinde geringer dichte", 
    "Ländliche Zentrumsgemeinde",
    "Ländliche zentral gelegene Gemeinde",
    "Ländliche periphere Gemeinde")
  
  length(labels)==length(levels)
  
  merged.table2$category <- factor(merged.table2$category, 
                                   levels=levels, 
                                   labels=labels, 
                                   ordered=TRUE)
  merged.table2
  
  
  p2 <- 
    ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) + 
    geom_bar(position = 'dodge', stat="identity", alpha=0.7,  fill="#C8777B") +
    geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.7, size=0.5) + 
    coord_flip() + 
    geom_hline(yintercept = elbezug.mean.line, color='red', lwd=0.5) +
    geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 3)+
    scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
    labs(
      # title = "Nichtbezug von Ergänzungsleistungen und räumliche Indikatoren", 
         # subtitle = "Basierend auf dem monatlichen Äquivalenzeinkommen des Haushalts und der SKOS Armutsgrenze",
         caption = "Quelle: Schweizer Alterssurvey 2022") +
    ylab("Prozent der zuhause lebenden Bevölkerung ab 65") + xlab("")
  print(p2)
  
  ggsave(
    plot = print(p2),
    filename="graph_NTU.raum.png",
    path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
    scale = 0.5,
    width = 450,
    height = 450,
    units = "mm",
    dpi = 300
  )
  
  
  ggsave(
    plot = print(p2),
    filename="graph_NTU.raum.png",
    path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
    scale = 0.5,
    width = 450,
    height = 450,
    units = "mm",
    dpi = 300
  )
  
  
  length(which(complete.cases(
    subset(sas.22, select=c(
      NTU.EL, 
      civstat.rcd, 
      degurba.rcd, 
      edu.rcd, 
      pi_swissnational_22)))))
  
  
  


# soziale einbettung -------------------------------------------------------

  
  # loneliness 
  raw.table1<-svyby(~NTU.EL,by=~feeling.lonely, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
  raw.table1
  raw.table1 <- as_tibble(raw.table1)
  raw.table1
  names(raw.table1) <- c("category","sample.mean.pov","se")
  raw.table1
  name <- c("Fühlt sich nicht einsam","Fühlt sich einsam")
  raw.table1$category <- name
  raw.table1
  merged.table2 <- raw.table1
  merged.table2

#see friends
raw.table1<-svyby(~NTU.EL,by=~friends.atleast.1pweek, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Trifft Freunde alle 2. Wochen oder seltener", "Trifft Freunde & Bekannte  \n 1 Mal/Woche oder häufiger")
raw.table1$category <- name
raw.table1

merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2

#internet to communicate
raw.table1<-svyby(~NTU.EL,by=~useinternet.communicate.friends, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Verwendet Internet gar nicht oder nicht für Kommunikation","Kommuniziert mit Freunden und Bekannten über das Internet")
raw.table1$category <- name
raw.table1

merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2




merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se

merged.table2$se <-NULL

names(merged.table2)
is.character(merged.table2$category)
merged.table2$sample.mean.pov <- as.single(merged.table2$sample.mean.pov)
is.double(merged.table2$sample.mean.pov)
is.numeric(merged.table2$lwr.ci)
is.numeric(merged.table2$upr.ci)

merged.table2




# grafik
raw.table<-svymean(~NTU.EL, dsurvey,na.rm=TRUE) # weighted tables using survey packages
elbezug.mean.line <- raw.table[1]


levels(as.factor(merged.table2$category))

levels=c(   "Fühlt sich einsam", 
            "Fühlt sich nicht einsam" , 
                        "Verwendet Internet gar nicht oder nicht für Kommunikation",
             "Kommuniziert mit Freunden und Bekannten über das Internet", 
           "Trifft Freunde & Bekannte  \n 1 Mal/Woche oder häufiger", 
             "Trifft Freunde alle 2. Wochen oder seltener"  )             

labels=c(   "Fühlt sich einsam", 
            "Fühlt sich nicht einsam" , 
            "Verwendet Internet gar nicht oder nicht für Kommunikation",
            "Kommuniziert mit Freunden und Bekannten über das Internet", 
            "Trifft Freunde & Bekannte  \n 1 Mal/Woche oder häufiger", 
            "Trifft Freunde alle 2. Wochen oder seltener"  )      

length(labels)==length(levels)

merged.table2$category <- factor(merged.table2$category,
                                 levels=levels,
                                 labels=labels,
                                 ordered=TRUE)
merged.table2


p2 <-
  ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) +
  geom_bar(position = 'dodge', stat="identity", alpha=0.7,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.7, size=0.5) +
  coord_flip() +
  geom_hline(yintercept = elbezug.mean.line, color='red', lwd=0.5) +
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 3)+
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  labs(
    # title = "EL-Bezug und soziale Kontakte",
       # subtitle = "Basierend auf dem monatlichen Äquivalenzeinkommen des Haushalts und der SKOS Armutsgrenze",
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der zuhause lebende Bevölkerung ab 65") + xlab("")
print(p2)

ggsave(
  plot = print(p2),
  filename="graph_NTU.sozialekontakte.png",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 350,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph_NTU.sozialekontakte.svg",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 350,
  units = "mm",
  dpi = 300
)


length(which(complete.cases(
  subset(sas.22, select=c(
    NTU.EL,
    civstat.rcd,
    degurba.rcd,
    edu.rcd,
    pi_swissnational_22)))))





# gesundheit -------------------------------------------------------

# gali
raw.table1<-svyby(~NTU.EL,by=~ph_gali_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Aktivitäten stark eingeschränkt und EL-Bezug",
          "Aktivitäten etwas eingeschränkt und EL-Bezug",
          "Nicht eingeschränkt und EL-Bezug")
raw.table1$category <- name
raw.table1
merged.table2 <- raw.table1

#getcomcare
raw.table1<-svyby(~NTU.EL,by=~cs_getcomcare_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("Anspruchnahme von Pflegeleistungen & EL-Bezug",
          "Keine Anspruchnahme von Pflegeleistungen & EL-Bezug")
raw.table1$category <- name
raw.table1

merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2

# 
# #getinfcare
# raw.table1<-svyby(~NTU.EL,by=~cs_getinfcare_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
# raw.table1
# raw.table1 <- as_tibble(raw.table1)
# raw.table1
# names(raw.table1) <- c("category","sample.mean.pov","se")
# raw.table1
# name <- c("Erhält keine informelle Pflegeleistungen und EL-Bezug","Erhält informelle kostenpflichtigen Pflegeleistungen & EL-Bezug")
# raw.table1$category <- name
# raw.table1
# 
# merged.table2 <- rbind(merged.table2, raw.table1)
# merged.table2



merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se

merged.table2$se <-NULL

names(merged.table2)
is.character(merged.table2$category)
merged.table2$sample.mean.pov <- as.single(merged.table2$sample.mean.pov)
is.double(merged.table2$sample.mean.pov)
is.numeric(merged.table2$lwr.ci)
is.numeric(merged.table2$upr.ci)

merged.table2




# grafik
raw.table<-svymean(~NTU.EL, dsurvey,na.rm=TRUE) # weighted tables using survey packages
elbezug.mean.line <- raw.table[1]


levels(as.factor(merged.table2$category))
                                     
                              

levels=c(
  "Aktivitäten stark eingeschränkt und EL-Bezug" ,
  "Aktivitäten etwas eingeschränkt und EL-Bezug",
  "Nicht eingeschränkt und EL-Bezug" ,
  "Anspruchnahme von Pflegeleistungen & EL-Bezug" ,
  "Keine Anspruchnahme von Pflegeleistungen & EL-Bezug" 


  )    

labels=c(
  "Aktivitäten stark eingeschränkt" ,
  "Aktivitäten etwas eingeschränkt",
  "Nicht körperlich eingeschränkt" ,
  "Anspruchnahme von Pflegeleistungen" ,
  "Keine Anspruchnahme von Pflegeleistungen" )    

length(labels)==length(levels)

merged.table2$category <- factor(merged.table2$category,
                                 levels=levels,
                                 labels=labels,
                                 ordered=TRUE)
merged.table2


p2 <-
  ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) +
  geom_bar(position = 'dodge', stat="identity", alpha=0.7,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.7, size=0.5) +
  coord_flip() +
  geom_hline(yintercept = elbezug.mean.line, color='red', lwd=0.5) +
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 3)+
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+
  labs(
    # title = "Nichtbezug von Ergänzungsleistungen und Gesundheit",
       # subtitle = "Basierend auf dem monatlichen Äquivalenzeinkommen des Haushalts und der SKOS Armutsgrenze",
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der zuhause lebenden Bevölkerung ab 65") + xlab("")
print(p2)

ggsave(
  plot = print(p2),
  filename="graph_NTU.health.png",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 250,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph_NTU.health.svg",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 250,
  units = "mm",
  dpi = 300
)


length(which(complete.cases(
  subset(sas.22, select=c(
    NTU.EL,
    civstat.rcd,
    degurba.rcd,
    edu.rcd,
    pi_swissnational_22)))))




# Map  ------------------------------------------------------

t<-svyby(~NTU.EL,by=~canton_22, dsurvey, svymean, na.rm=TRUE, ci=FALSE)
t<-t[,-3]
t

t <- as.data.frame(t)
t$NTU.EL <- as.numeric(t$NTU.EL)
x <- c("AI",t[2,2]) 
t <- rbind(t,x)
t
t[2,1]<-"AR"
t


names(t) <- c("Canton", "incidence")
t
is.numeric(t$incidence)
t$incidence <- as.numeric(t$incidence)
t$incidence <- t$incidence*100
t

swiss_lakes <- st_read("g2s22.shp")
swiss_cantons <- st_read("G1K22.shp")

ggplot()+
  geom_sf(data = swiss_cantons)

levels(as.factor(swiss_cantons$KTNAME))

swiss_cantons <- swiss_cantons %>% select(KTNAME) %>% 
  mutate(KTNAME = case_when(
    KTNAME=="Aargau" ~ "AG", 
    KTNAME=="Basel-Landschaft" ~ "BL",
    KTNAME=="Fribourg / Freiburg" ~ "FR",
    KTNAME=="Graubünden / Grigioni / Grischun" ~ "GR",
    KTNAME=="Neuchâtel" ~ "NE",
    KTNAME=="Schaffhausen" ~ "SH",
    KTNAME=="St. Gallen" ~ "SG",
    KTNAME=="Uri" ~ "UR",
    KTNAME=="Zug" ~ "ZG",
    KTNAME=="Appenzell Ausserrhoden" ~ "AR",
    KTNAME=="Basel-Stadt" ~ "BS",
    KTNAME=="Genève" ~ "GE",
    KTNAME=="Jura" ~ "JU",
    KTNAME=="Nidwalden" ~ "NW",
    KTNAME=="Schwyz" ~ "SZ",
    KTNAME=="Thurgau" ~ "TG",
    KTNAME=="Valais / Wallis" ~ "VS",
    KTNAME=="Zürich" ~ "ZH",
    KTNAME=="Appenzell Innerrhoden" ~ "AI",
    KTNAME=="Bern / Berne" ~ "BE",
    KTNAME=="Zürich" ~ "ZH",
    KTNAME=="Glarus" ~ "GL",
    KTNAME=="Luzern" ~ "LU",
    KTNAME=="Obwalden" ~ "OW",
    KTNAME=="Solothurn" ~ "SO",
    KTNAME=="Ticino" ~ "TI",
    KTNAME=="Vaud" ~ "VD"
  ))
levels(as.factor(swiss_cantons$KTNAME))
length(swiss_cantons$KTNAME)==length(t$Canton)
zz <- match(as.character(swiss_cantons$KTNAME),as.character(t$Canton))
which(is.na(zz))
t$Canton
head(swiss_cantons)
swiss_cantons <- swiss_cantons %>% 
  left_join(t, c("KTNAME" = "Canton"))
swiss_cantons %>%  select(KTNAME,incidence)

ggplot()+
  geom_sf(data = swiss_cantons, fill = NA) +
  geom_sf(data = swiss_lakes,  fill = "#d1eeea", color = "#d1eeea") +
  theme_void()

table(t$incidence)

swiss_cantons <- swiss_cantons %>% 
  mutate(incidence_cat = case_when(
    incidence <=10 ~ "<10%",
    incidence >10 & incidence<=20 ~ "10-20%",
    incidence >20 ~ ">20%"
  )) %>% 
  mutate(incidence_cat = factor(incidence_cat, levels = c("<10%", "10-20%",">20%")))

swiss_cantons %>%  select(KTNAME, incidence_cat)

ggplot(swiss_cantons) +
  geom_sf(aes(fill = incidence_cat), size = 0.3) +
  scale_fill_carto_d(palette = "BrwnYl") +
  geom_sf(data = swiss_lakes, fill = "#d1eeea", color = "#d1eeea")+
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") 

ggplot(swiss_cantons) +
  geom_sf(aes(fill = incidence_cat), size = 0.3) +
  scale_fill_carto_d(palette = "BrwnYl",
                     guide = guide_legend(direction = "horizontal",
                                          keyheight = unit(2, units = "mm"),
                                          keywidth = unit(70 / 5, units = "mm"),
                                          title.position = 'top',
                                          title.hjust = 0.5,
                                          label.hjust = 0.5,
                                          nrow = 1,
                                          byrow = T,
                                          label.position = "bottom")) +
  geom_sf(data = swiss_lakes, fill = "#d1eeea", color = "#d1eeea")+
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") 



p2 <- ggplot(swiss_cantons) +
  geom_sf(aes(fill = incidence_cat), size = 0.3) +
  scale_fill_carto_d(palette = "Earth",
                     guide = guide_legend(direction = "horizontal",
                                          keyheight = unit(2, units = "mm"),
                                          keywidth = unit(70 / 5, units = "mm"),
                                          title.position = 'top',
                                          title.hjust = 0.5,
                                          label.hjust = 0.5,
                                          nrow = 1,
                                          byrow = T,
                                          label.position = "bottom")) +
  geom_sf(data = swiss_lakes, fill = "#d1eeea", color = "#d1eeea")+
  ggrepel::geom_label_repel(
    data = swiss_cantons,
    aes(label = paste0(KTNAME,":",round(incidence, digits = 1)), 
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0.2,
    colour = "#541f3f",
    size = 3,
    segment.alpha = 0.5
  ) +
  labs(
    # title = "<b style='color:#541f3f'> Nichtbezugsquote Ergänzungsleistungen zuhause lebende Bevölkerung 65+ </b>",
       # subtitle = "<span style='font-size:10pt'>Basierend auf den Äquivalenzeinkommen der Haushalte </span>",
       caption = "Source: Schweizer Alterssurvey | 2022") +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown()) 
print(p2)

ggsave(
  plot = print(p2),
  filename="graph_CHkarte.png",
  path="/Users/gabn/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 350,
  height = 350,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph_CHkarte.svg",
  path="/Users/gabn/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 350,
  height = 350,
  units = "mm",
  dpi = 300
)




# rangliste ---------------------------------------------------------------


# cantonal differences 


raw.table1<-svyby(~NTU.EL,by=~canton_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
raw.table1

merged.table2 <- raw.table1
merged.table2


merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se

merged.table2

order <- order(merged.table2$sample.mean.pov, decreasing = FALSE)
merged.table2 <- merged.table2[order,]

line <- median(merged.table2$sample.mean.pov)
line 
p <- ggplot(merged.table2, aes( y = sample.mean.pov, x = reorder(category, +sample.mean.pov))) + geom_col(fill="skyblue",alpha=0.5 ) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,0.4)) + scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
  geom_hline(yintercept = line, color="#541F3F", lwd=0.7, alpha=0.4) +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, size=0.5)+ 
  ylab("EL-Nichtbezugsquote") + xlab("Kantone") + labs(
    # title = "Kantonale Nichtbezugsquoten Ergänzungsleistungen", 
                                                        # subtitle = "Basierend auf dem monatlichen Äquivalenzeinkommen des Haushalts und der SKOS Armutsgrenze"
                                                       )+
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-4, size = 4)
print(p)


ggsave(
  plot = print(p),
  filename="graph-rangliste_CHkarte-NTU.png",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 700,
  height = 350,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p),
  filename="graph-rangliste_CHkarte-NTU.svg",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 350,
  height = 500,
  units = "mm",
  dpi = 300
)


#  thoughts about EL  -------------------------------------------


#  Information of EL -------------------------------------------


# know 
raw.table1<-svyby(~NTU.EL,by=~fi_knowel_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("no-know-EL","know-EL")
raw.table1$category <- name
raw.table1
merged.table2 <- raw.table1
merged.table2



svytable(~fi_knowel_22, dsurvey.pop)
prop.table(svytable(~fi_knowel_22, dsurvey))


svytable(~fi_knowel_22+NTU.EL, dsurvey.pop)


#eligible el
raw.table1<-svyby(~NTU.EL,by=~fi_eligibleel_22, dsurvey, svymean, na.rm=TRUE, ci=TRUE)
raw.table1
raw.table1 <- as_tibble(raw.table1)
raw.table1
names(raw.table1) <- c("category","sample.mean.pov","se")
raw.table1
name <- c("think-not-eligible","think-eligible")
raw.table1$category <- name

merged.table2 <- rbind(merged.table2, raw.table1)
merged.table2


merged.table2$lwr.ci <- merged.table2$sample.mean.pov-merged.table2$se
merged.table2$upr.ci <- merged.table2$sample.mean.pov+merged.table2$se

merged.table2


levels(as.factor(merged.table2$category))

levels=c("know-EL",
         "no-know-EL" , 
         "think-eligible" , 
         "think-not-eligible") 



labels=c("EL bekannt" , 
         "EL nicht bekannt"  ,
         "Denkt, dass Anspruch auf EL",
         "Denkt, dass keinen Anspruch auf EL")



length(labels)==length(levels)

merged.table2$category <- factor(merged.table2$category, 
                                 levels=levels, 
                                 labels=labels, 
                                 ordered=TRUE)
merged.table2

raw.table<-svymean(~NTU.EL, dsurvey,na.rm=TRUE) # weighted tables using survey packages


elbezug.mean.line <- raw.table[1]

p2 <- 
  ggplot(merged.table2, aes(x = forcats::fct_rev(category), y=sample.mean.pov)) + 
  geom_bar(position = 'dodge', stat="identity", alpha=0.8,  fill="#C8777B") +
  geom_errorbar( aes(x=category, ymin=lwr.ci, ymax=upr.ci), width=0.4, colour="black", alpha=0.9, size=0.5) + 
  geom_hline(yintercept = elbezug.mean.line, color='red', lwd=0.5) +
  coord_flip() +
  geom_text(aes(label={scales::percent(sample.mean.pov, accuracy=0.1)}), position=position_dodge(width=0.9), vjust=-2, size = 3)+
  scale_y_continuous(labels = scales::percent) + scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
  labs(
    # title = "Nichtbezug und Wissen um EL ", 
       caption = "Quelle: Schweizer Alterssurvey 2022") +
  ylab("Prozent der zuhause lebenden Bevölkerung ab 65") + xlab("")
print(p2)





ggsave(
  plot = print(p2),
  filename="graph_NTU.wissen.png",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 200,
  units = "mm",
  dpi = 300
)


ggsave(
  plot = print(p2),
  filename="graph_NTU.wissen.svg",
  path="/Users/rainer/Dropbox/Documents/Work/ACTIVE/ZHAW/3_Research-Projects/Altersmonitoring Pro Senectute/Studie EL und Nichtbezug/auswertungen-el-NTU-PS/PICTURES-GRAPHS-FINAL",
  scale = 0.5,
  width = 450,
  height = 120,
  units = "mm",
  dpi = 300
)








