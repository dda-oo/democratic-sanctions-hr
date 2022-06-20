############# Democratic Sanctions & HR##############
####### Date: 19 Jun 2022 #########


library("readxl")
library("plm")

rm(list = ls())


## load data
alldata <- read.csv("Dataset- Democratic Sanctions-HR.csv")

## To test results via OLS method
simplereg = lm(civilrights ~ democratic_sanction, data= alldata)
summary(simplereg)

## Fixed effects on Civil Rights
FE_civilrights = plm (civilrights ~ democratic_sanction + coup1 + coup2 + coup3 + coup4 + conflict + one_sided_violence +
                ln_GDPgrowth + ln_GDPpc + ln_population + factor(year), index="Country", data= alldata)
summary(FE_civilrights)

## Fixed effects on Personal Integrity
FE_PTS = plm (PTS ~ democratic_sanction + coup1 + coup2 + coup3 + coup4 + conflict + one_sided_violence +
                ln_GDPgrowth + ln_GDPpc + ln_population + factor(year), index="Country", data= alldata)
summary(FE_PTS)


## Applying FE on Specific case of Russia- Under Democratic sanctions imposed by the EU&US (2012-2015)- Civil Rights
alldata_filter <- alldata %>%
  filter(grepl('Russia', Country))

FE_Russia_civilrights <- plm(civilrights ~ democratic_sanction + coup1 + coup2 + coup3 + coup4 + conflict + one_sided_violence +
                         ln_GDPgrowth + ln_GDPpc + ln_population + factor(year), index="Country", data= alldata_filter)
summary(FE_Russia_civilrights)
