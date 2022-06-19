############# Democratic Sanctions & HR##############
####### Date: 19 Jun 2022 #########


library("readxl")
library("plm")

rm(list = ls())


## load data
alldata <- data.frame(read_excel("Dataset- Democratic Sanctions-HR.xlsx"))

## To get an overview results via OLS
simplereg = lm(civilrights ~ democratic_sanction, data= alldata)
summary(simplereg)

## Fixed effects on Civil Rights
FE = plm (civilrights + PTS ~ democratic_sanction + coup1 + coup2 + coup3 + coup4 + conflict + one_sided_violence +
                ln_GDPgrowth + ln_GDPpc + ln_population + factor(year), index="Country", data= alldata)

summary(FE)

##Applying FE on Specific case of Russia- Under Democratic sanctions imposed by the EU&US (2012-2015)
alldata_filter <- alldata %>%
  filter(grepl('Russia', Country))

fixed_intercept <- plm(civilrights ~ democratic_sanction + coup1 + coup2 + coup3 + coup4 + conflict + one_sided_violence +
                         ln_GDPgrowth + ln_GDPpc + ln_population + factor(year), index="Country", data= alldata_filter)
summary(fixed_intercept)
