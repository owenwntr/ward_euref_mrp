if (!"tidyverse" %in% installed.packages()[,1]){
  install.packages("tidyverse")
}

library(tidyverse)
rm(list=ls())

###
i <- 1
j <- 23

print(paste("PACKAGES LOADED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Age by Education Data for England and Wales

age_edu <- read.csv("oa_data/data/AGE_HIQUAL_OASA_England_Northern_Ireland_Scotland_Wales_Descriptions.csv")
age_edu <- age_edu[which((!startsWith(age_edu$GEO_CODE,"S"))&(!startsWith(age_edu$GEO_CODE,"N"))),]

colnames(age_edu)[5:52] <- c("a1624_eTotal",
                          "a1624_eNone",
                          "a1624_eOne",
                          "a1624_eTwo",
                          "a1624_eThree",
                          "a1624_eFourPlus",
                          "a1624_eApprentice",
                          "a1624_eOther",
                          "a16plus_eTotal",
                          "a16plus_eNone",
                          "a16plus_eOne",
                          "a16plus_eTwo",
                          "a16plus_eThree",
                          "a16plus_eFourPlus",
                          "a16plus_eApprentice",
                          "a16plus_eOther",
                          "a2534_eTotal",
                          "a2534_eNone",
                          "a2534_eOne",
                          "a2534_eTwo",
                          "a2534_eThree",
                          "a2534_eFourPlus",
                          "a2534_eApprentice",
                          "a2534_eOther",
                          "a3549_eTotal",
                          "a3549_eNone",
                          "a3549_eOne",
                          "a3549_eTwo",
                          "a3549_eThree",
                          "a3549_eFourPlus",
                          "a3549_eApprentice",
                          "a3549_eOther",
                          "a5064_eTotal",
                          "a5064_eNone",
                          "a5064_eOne",
                          "a5064_eTwo",
                          "a5064_eThree",
                          "a5064_eFourPlus",
                          "a5064_eApprentice",
                          "a5064_eOther",
                          "a65plus_eTotal",
                          "a65plus_eNone",
                          "a65plus_eOne",
                          "a65plus_eTwo",
                          "a65plus_eThree",
                          "a65plus_eFourPlus",
                          "a65plus_eApprentice",
                          "a65plus_eOther")
###
i <- 2

print(paste("ENGLAND DATA Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

age_edu_scot <- read.csv('oa_data/data/AGE_HIQUAL_SEX_OASA_England_Northern_Ireland_Scotland_Wales_Descriptions.csv')
age_edu_scot <- age_edu_scot[which(startsWith(age_edu_scot$GEO_CODE,"S")),1:40]

colnames(age_edu_scot)[5:40] <- c("a1624_eTotal",
                             "a1624_eNone",
                             "a1624_eOne",
                             "a1624_eTwo",
                             "a1624_eThree",
                             "a1624_eFourPlus",
                             "a16plus_eTotal",
                             "a16plus_eNone",
                             "a16plus_eOne",
                             "a16plus_eTwo",
                             "a16plus_eThree",
                             "a16plus_eFourPlus",
                             "a2534_eTotal",
                             "a2534_eNone",
                             "a2534_eOne",
                             "a2534_eTwo",
                             "a2534_eThree",
                             "a2534_eFourPlus",
                             "a3549_eTotal",
                             "a3549_eNone",
                             "a3549_eOne",
                             "a3549_eTwo",
                             "a3549_eThree",
                             "a3549_eFourPlus",
                             "a5064_eTotal",
                             "a5064_eNone",
                             "a5064_eOne",
                             "a5064_eTwo",
                             "a5064_eThree",
                             "a5064_eFourPlus",
                             "a65plus_eTotal",
                             "a65plus_eNone",
                             "a65plus_eOne",
                             "a65plus_eTwo",
                             "a65plus_eThree",
                             "a65plus_eFourPlus")

###
i <- 3

print(paste("SCOTLAND DATA Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

underage <- read.csv("oa_data/data/Data_AGE_UNIT.csv")

age_edu$underage <- underage$age1617[match(age_edu$GEO_CODE,underage$GEO_CODE)]
age_edu$underage_pct <- age_edu$underage/(age_edu$a1624_eNone+age_edu$a1624_eOne+age_edu$a1624_eTwo+age_edu$a1624_eOther)
age_edu$underage_pct[which(is.na(age_edu$underage_pct))] <- 0
age_edu$underage_pct[which(age_edu$underage_pct>1)] <- 1

age_edu_scot$underage <- underage$age1617[match(age_edu_scot$GEO_CODE,underage$GEO_CODE)]
age_edu_scot$underage_pct <- age_edu_scot$underage/(age_edu_scot$a1624_eNone+age_edu_scot$a1624_eOne)
age_edu_scot$underage_pct[which(is.na(age_edu_scot$underage_pct))] <- 0
age_edu_scot$underage_pct[which(age_edu_scot$underage_pct>1)] <- 1

rm(underage)

age_edu$a1824_eTotal <- age_edu$a1624_eTotal - age_edu$underage_pct*(age_edu$a1624_eNone+age_edu$a1624_eOne+age_edu$a1624_eTwo+age_edu$a1624_eOther)
age_edu$a1824_eNone <- age_edu$a1624_eNone*(1-age_edu$underage_pct)
age_edu$a1824_eOne <- age_edu$a1624_eOne*(1-age_edu$underage_pct)
age_edu$a1824_eTwo <- age_edu$a1624_eTwo*(1-age_edu$underage_pct)
age_edu$a1824_eThree <- age_edu$a1624_eThree
age_edu$a1824_eFourPlus <- age_edu$a1624_eFourPlus
age_edu$a1824_eApprentice <- age_edu$a1624_eApprentice
age_edu$a1824_eOther <- age_edu$a1624_eOther*(1-age_edu$underage_pct)

age_edu_scot$a1824_eTotal <- age_edu_scot$a1624_eTotal - age_edu_scot$underage_pct*(age_edu_scot$a1624_eNone+age_edu_scot$a1624_eOne)
age_edu_scot$a1824_eNone <- age_edu_scot$a1624_eNone*(1-age_edu_scot$underage_pct)
age_edu_scot$a1824_eOne <- age_edu_scot$a1624_eOne*(1-age_edu_scot$underage_pct)
age_edu_scot$a1824_eTwo <- age_edu_scot$a1624_eTwo*(1-age_edu_scot$underage_pct)
age_edu_scot$a1824_eThree <- age_edu_scot$a1624_eThree*(1-age_edu_scot$underage_pct)
age_edu_scot$a1824_eFourPlus <- age_edu_scot$a1624_eFourPlus*(1-age_edu_scot$underage_pct)

###
i <- 4

print(paste("UNDERAGE REMOVED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

lookup <- read_csv("oa_data/data/onspd/Data/ONSPD_FEB_2021_UK.csv")
lookup <- lookup[which(!is.na(lookup$oa11)),]

age_edu[,c("lsoa","msoa","ward","pcon","la","rgn","easting","northing")] <- lookup[match(age_edu$GEO_CODE,lookup$oa11),c("lsoa11","msoa11","osward","pcon","oslaua","rgn","oseast1m","osnrth1m")]
age_edu_scot[,c("lsoa","msoa","ward","pcon","la","rgn","easting","northing")] <- lookup[match(age_edu_scot$GEO_CODE,lookup$oa11),c("lsoa11","msoa11","osward","pcon","oslaua","rgn","oseast1m","osnrth1m")]

code_plus_one <- function(oldcode){
  new_number <- as.numeric(substr(oldcode, 2, 9))+1
  zeroes <- 8-nchar(as.character(new_number))
  if (zeroes==1){
    new_code <- paste(substr(missing, 1, 1),"0",new_number,sep="")
  } else if (zeroes==2){
    new_code <- paste(substr(missing, 1, 1),"00",new_number,sep="")
  } else if (zeroes==3){
    new_code <- paste(substr(missing, 1, 1),"000",new_number,sep="")
  } else if (zeroes==4){
    new_code <- paste(substr(missing, 1, 1),"0000",new_number,sep="")
  }
  return(new_code)
}

for (missing in age_edu$GEO_CODE[which(is.na(age_edu$pcon))]){

  new_code <- code_plus_one(missing)
  
  while (!(new_code %in% age_edu$GEO_CODE[which(!is.na(age_edu$pcon))])){
    print(new_code)
    print("Add Another")
    new_code <- code_plus_one(new_code)
  }
  
  age_edu[which(age_edu$GEO_CODE==missing),c("lsoa","msoa","ward","pcon","la","rgn","easting","northing")] <- age_edu[which(age_edu$GEO_CODE==new_code),c("lsoa","msoa","ward","pcon","la","rgn","easting","northing")]
  
}

for (missing in age_edu_scot$GEO_CODE[which(is.na(age_edu_scot$pcon))]){
  
  new_code <- code_plus_one(missing)
  
  while (!(new_code %in% age_edu_scot$GEO_CODE[which(!is.na(age_edu_scot$pcon))])){
    print(new_code)
    print("Add Another")
    new_code <- code_plus_one(new_code)
  }
  
  age_edu_scot[which(age_edu_scot$GEO_CODE==missing),c("lsoa","msoa","ward","pcon","la","rgn","easting","northing")] <- age_edu_scot[which(age_edu_scot$GEO_CODE==new_code),c("lsoa","msoa","ward","pcon","la","rgn","easting","northing")]
  
}

age_edu$northing <- as.numeric(age_edu$northing)
age_edu_scot$northing <- as.numeric(age_edu_scot$northing)

###
i <- 5

print(paste("GEOGRAPHIC DATA ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

age_edu_long <- pivot_longer(age_edu[,c(1:4,22:28,30:36,38:44,46:52,56:70)], cols=c(5:39))
age_edu_scot_long <- pivot_longer(age_edu_scot[,c(1:4,18:22,24:28,30:34,36:40,44:56)], cols=c(5:29))

ps_frame <- rbind(age_edu_long,age_edu_scot_long)

ps_frame$agegroup <- NA
ps_frame$agegroup[which(startsWith(ps_frame$name,"a1824"))] <- 1
ps_frame$agegroup[which(startsWith(ps_frame$name,"a2534"))] <- 2
ps_frame$agegroup[which(startsWith(ps_frame$name,"a3549"))] <- 3
ps_frame$agegroup[which(startsWith(ps_frame$name,"a5064"))] <- 4
ps_frame$agegroup[which(startsWith(ps_frame$name,"a65plus"))] <- 5

ps_frame$edlevel <- NA
ps_frame$edlevel[which(endsWith(ps_frame$name,"eNone"))] <- "None"
ps_frame$edlevel[which(endsWith(ps_frame$name,"eOne"))] <- "1"
ps_frame$edlevel[which(endsWith(ps_frame$name,"eTwo"))] <- "2"
ps_frame$edlevel[which(endsWith(ps_frame$name,"eThree"))] <- "3"
ps_frame$edlevel[which(endsWith(ps_frame$name,"eFourPlus"))] <- "4"
ps_frame$edlevel[which(endsWith(ps_frame$name,"eApprentice"))] <- "Apprentice"
ps_frame$edlevel[which(endsWith(ps_frame$name,"eOther"))] <- "Other"
ps_frame$edlevel[which((endsWith(ps_frame$name,"eOne"))&(ps_frame$rgn=="S99999999"))] <- "1 Scotland"
ps_frame$edlevel[which((endsWith(ps_frame$name,"eTwo"))&(ps_frame$rgn=="S99999999"))] <- "2 Scotland"
ps_frame$edlevel[which((endsWith(ps_frame$name,"eThree"))&(ps_frame$rgn=="S99999999"))] <- "3 Scotland"
ps_frame$edlevel[which((endsWith(ps_frame$name,"eFourPlus"))&(ps_frame$rgn=="S99999999"))] <- "4 Scotland"

###
i <- 6

print(paste("LONG VERSION Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Sex

sex <- read.csv("oa_data/data/Data_SEX_UNIT.csv")
sex$female_pct <- sex$Sex...Females...Unit...Persons/(sex$Sex...Males...Unit...Persons+sex$Sex...Females...Unit...Persons)
ps_frame$female_pct <- sex$female_pct[match(ps_frame$GEO_CODE,sex$GEO_CODE)]

rm(sex)

###
i <- 7

print(paste("SEX ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Cars

cars <- read.csv("oa_data/data/Data_cars.csv")
cars$nocar_pct <- cars[,6]/(cars[,6]+cars[,7]+cars[,8])
cars$onecar_pct <- cars[,7]/(cars[,6]+cars[,7]+cars[,8])
cars$twopluscar_pct <- cars[,8]/(cars[,6]+cars[,7]+cars[,8])
ps_frame$nocar_pct <- cars$nocar_pct[match(ps_frame$GEO_CODE,cars$GEO_CODE)]
ps_frame$onecar_pct <- cars$onecar_pct[match(ps_frame$GEO_CODE,cars$GEO_CODE)]
ps_frame$twopluscar_pct <- cars$twopluscar_pct[match(ps_frame$GEO_CODE,cars$GEO_CODE)]

rm(cars)

###
i <- 8

print(paste("CARS ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Accomodation Type

accom_type <- read.csv("oa_data/data/Data_ACCTYP_UNIT.csv")
accom_type$total <- accom_type$Accommodation.type...Whole.house.or.bungalow..Detached...Unit...Household.spaces +
  accom_type$Accommodation.type...Whole.house.or.bungalow..Semi.detached...Unit...Household.spaces +
  accom_type$Accommodation.type...Whole.house.or.bungalow..Terraced..including.end.terrace....Unit...Household.spaces +
  accom_type$Accommodation.type...Flat..maisonette.or.apartment..Purpose.built.block.of.flats.or.tenement...Unit...Household.spaces +
  accom_type$Accommodation.type...Flat..maisonette.or.apartment..Part.of.a.converted.or.shared.house..including.bed.sits....Unit...Household.spaces +
  accom_type$Accommodation.type...Flat..maisonette.or.apartment..In.a.commercial.building...Unit...Household.spaces +
  accom_type$Accommodation.type...Caravan.or.other.mobile.or.temporary.structure...Unit...Household.spaces

accom_type$detachedhouse_pct <- accom_type$Accommodation.type...Whole.house.or.bungalow..Detached...Unit...Household.spaces/accom_type$total
accom_type$semidetachedhouse_pct <- accom_type$Accommodation.type...Whole.house.or.bungalow..Semi.detached...Unit...Household.spaces/accom_type$total
accom_type$terracedhouse_pct <- accom_type$Accommodation.type...Whole.house.or.bungalow..Terraced..including.end.terrace....Unit...Household.spaces/accom_type$total
accom_type$flat_apartment_pct <- (accom_type$Accommodation.type...Flat..maisonette.or.apartment..Purpose.built.block.of.flats.or.tenement...Unit...Household.spaces +
  accom_type$Accommodation.type...Flat..maisonette.or.apartment..Part.of.a.converted.or.shared.house..including.bed.sits....Unit...Household.spaces +
  accom_type$Accommodation.type...Flat..maisonette.or.apartment..In.a.commercial.building...Unit...Household.spaces)/accom_type$total

ps_frame$detachedhouse_pct <- accom_type$detachedhouse_pct[match(ps_frame$GEO_CODE,accom_type$GEO_CODE)]
ps_frame$semidetachedhouse_pct <- accom_type$semidetachedhouse_pct[match(ps_frame$GEO_CODE,accom_type$GEO_CODE)]
ps_frame$terracedhouse_pct <- accom_type$terracedhouse_pct[match(ps_frame$GEO_CODE,accom_type$GEO_CODE)]
ps_frame$flat_apartment_pct <- accom_type$flat_apartment_pct[match(ps_frame$GEO_CODE,accom_type$GEO_CODE)]

rm(accom_type)

###
i <- 9

print(paste("ACCOM TYPE ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Country of Birth

cob <- read.csv("oa_data/data/Data_ADLFA_AGE_UNIT_URESPOP.csv")

cob$bornUK_pct <- cob$Country.of.birth..UK.harmonised....Europe..United.Kingdom...Unit...Persons/cob$Country.of.birth..UK.harmonised....Total..Country.of.birth.UK...Unit...Persons
cob$bornSco_pct <- cob$Country.of.birth..UK.harmonised....Europe..United.Kingdom..Scotland...Unit...Persons/cob$Country.of.birth..UK.harmonised....Total..Country.of.birth.UK...Unit...Persons
cob$bornWal_pct <- cob$Country.of.birth..UK.harmonised....Europe..United.Kingdom..Wales...Unit...Persons/cob$Country.of.birth..UK.harmonised....Total..Country.of.birth.UK...Unit...Persons
cob$bornEng_pct <- cob$Country.of.birth..UK.harmonised....Europe..United.Kingdom..England...Unit...Persons/cob$Country.of.birth..UK.harmonised....Total..Country.of.birth.UK...Unit...Persons
cob$bornEU_pct <- (cob$Country.of.birth..UK.harmonised....Europe..Other.Europe..EU.Countries...Unit...Persons+cob$Country.of.birth..UK.harmonised....Europe..Republic.of.Ireland.Includes..Ireland.otherwise.not.specified..for.England.and.Wales....Unit...Persons)/cob$Country.of.birth..UK.harmonised....Total..Country.of.birth.UK...Unit...Persons

ps_frame$bornUK_pct <- cob$bornUK_pct[match(ps_frame$GEO_CODE,cob$GEO_CODE)]
ps_frame$bornSco_pct <- cob$bornSco_pct[match(ps_frame$GEO_CODE,cob$GEO_CODE)]
ps_frame$bornWal_pct <- cob$bornWal_pct[match(ps_frame$GEO_CODE,cob$GEO_CODE)]
ps_frame$bornEng_pct <- cob$bornEng_pct[match(ps_frame$GEO_CODE,cob$GEO_CODE)]
ps_frame$bornEU_pct <- cob$bornEU_pct[match(ps_frame$GEO_CODE,cob$GEO_CODE)]

rm(cob)

###
i <- 10

print(paste("COUNTRY OF BIRTH ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Economic Activity

econ <- read.csv("oa_data/data/Data_economic_activity.csv")

econ$fulltimestudent_pct <- econ$Age...Age.18.to.74...Economic.activity...Economically.inactive..Full.time.students...Unit...Persons/econ$Age...Age.16.to.74...Economic.activity...Total..Economic.activity...Unit...Persons
econ$econ_active_pct <- econ$Age...Age.16.to.74...Economic.activity...Economically.active...Unit...Persons/econ$Age...Age.16.to.74...Economic.activity...Total..Economic.activity...Unit...Persons
econ$retired_pct <- econ$Age...Age.16.to.74...Economic.activity...Economically.inactive..Retired...Unit...Persons/econ$Age...Age.16.to.74...Economic.activity...Total..Economic.activity...Unit...Persons
econ$unemployed_pct <- econ$Age...Age.16.to.24...Economic.activity...Unemployed...Unit...Persons/econ$Age...Age.16.to.74...Economic.activity...Total..Economic.activity...Unit...Persons
econ$longterm_unemployed_pct <- econ$Age...Age.16.to.74...Economic.activity...Long.term.unemployed..where..Year.last.worked..is.2009.or.earlier....Unit...Persons/econ$Age...Age.16.to.74...Economic.activity...Total..Economic.activity...Unit...Persons

ps_frame$fulltimestudent_pct  <- econ$fulltimestudent_pct[match(ps_frame$GEO_CODE,econ$GEO_CODE)]
ps_frame$econ_active_pct  <- econ$econ_active_pct[match(ps_frame$GEO_CODE,econ$GEO_CODE)]
ps_frame$retired_pct  <- econ$retired_pct[match(ps_frame$GEO_CODE,econ$GEO_CODE)]
ps_frame$unemployed_pct  <- econ$unemployed_pct[match(ps_frame$GEO_CODE,econ$GEO_CODE)]
ps_frame$longterm_unemployed_pct  <- econ$longterm_unemployed_pct[match(ps_frame$GEO_CODE,econ$GEO_CODE)]

rm(econ)

###
i <- 11

print(paste("ECONOMIC ACTIVITY ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Ethnic group

ethnicity <- read.csv("oa_data/data/Data_ethnic_group.csv")

ethnicity$white_pct <- ethnicity$Ethnic.group..UK.harmonised....White...Unit...Persons/ethnicity$Ethnic.group..UK.harmonised....Total..Ethnic.Group.UK...Unit...Persons
ethnicity$gypsytraveller_pct <- ethnicity$Ethnic.group..UK.harmonised....Gypsy.Traveller.Irish.Traveller...Unit...Persons/ethnicity$Ethnic.group..UK.harmonised....Total..Ethnic.Group.UK...Unit...Persons
ethnicity$mixedrace_pct <- ethnicity$Ethnic.group..UK.harmonised....Mixed.Multiple.Ethnic.Groups...Unit...Persons/ethnicity$Ethnic.group..UK.harmonised....Total..Ethnic.Group.UK...Unit...Persons
ethnicity$indian_pct <- ethnicity$Ethnic.group..UK.harmonised....Asian.Asian.British..Indian...Unit...Persons/ethnicity$Ethnic.group..UK.harmonised....Total..Ethnic.Group.UK...Unit...Persons
ethnicity$pakistani_pct <- ethnicity$Ethnic.group..UK.harmonised....Asian.Asian.British..Pakistani...Unit...Persons/ethnicity$Ethnic.group..UK.harmonised....Total..Ethnic.Group.UK...Unit...Persons
ethnicity$bangladeshi_pct <- ethnicity$Ethnic.group..UK.harmonised....Asian.Asian.British..Bangladeshi...Unit...Persons/ethnicity$Ethnic.group..UK.harmonised....Total..Ethnic.Group.UK...Unit...Persons
ethnicity$chinese_pct <- ethnicity$Ethnic.group..UK.harmonised....Asian.Asian.British..Chinese...Unit...Persons/ethnicity$Ethnic.group..UK.harmonised....Total..Ethnic.Group.UK...Unit...Persons
ethnicity$other_asian_pct <- ethnicity$Ethnic.group..UK.harmonised....Asian.Asian.British..Other.Asian...Unit...Persons/ethnicity$Ethnic.group..UK.harmonised....Total..Ethnic.Group.UK...Unit...Persons
ethnicity$black_pct <- ethnicity$Ethnic.group..UK.harmonised....Black.African.Caribbean.Black.British...Unit...Persons/ethnicity$Ethnic.group..UK.harmonised....Total..Ethnic.Group.UK...Unit...Persons
ethnicity$other_ethnicity_pct <- ethnicity$Ethnic.group..UK.harmonised....Other.Ethnic.Group...Unit...Persons/ethnicity$Ethnic.group..UK.harmonised....Total..Ethnic.Group.UK...Unit...Persons

ps_frame$white_pct  <- ethnicity$white_pct[match(ps_frame$GEO_CODE,ethnicity$GEO_CODE)]
ps_frame$gypsytraveller_pct  <- ethnicity$gypsytraveller_pct[match(ps_frame$GEO_CODE,ethnicity$GEO_CODE)]
ps_frame$mixedrace_pct  <- ethnicity$mixedrace_pct[match(ps_frame$GEO_CODE,ethnicity$GEO_CODE)]
ps_frame$indian_pct  <- ethnicity$indian_pct[match(ps_frame$GEO_CODE,ethnicity$GEO_CODE)]
ps_frame$pakistani_pct  <- ethnicity$pakistani_pct[match(ps_frame$GEO_CODE,ethnicity$GEO_CODE)]
ps_frame$bangladeshi_pct  <- ethnicity$bangladeshi_pct[match(ps_frame$GEO_CODE,ethnicity$GEO_CODE)]
ps_frame$chinese_pct  <- ethnicity$chinese_pct[match(ps_frame$GEO_CODE,ethnicity$GEO_CODE)]
ps_frame$other_asian_pct  <- ethnicity$other_asian_pct[match(ps_frame$GEO_CODE,ethnicity$GEO_CODE)]
ps_frame$black_pct  <- ethnicity$black_pct[match(ps_frame$GEO_CODE,ethnicity$GEO_CODE)]
ps_frame$other_ethnicity_pct  <- ethnicity$other_ethnicity_pct[match(ps_frame$GEO_CODE,ethnicity$GEO_CODE)]

rm(ethnicity)

###
i <- 12

print(paste("ETHNICITY ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Health

health <- read.csv("oa_data/data/Data_health.csv")

health$health_verybad_pct <- health$General.health...Very.bad.health...Unit...Persons/health$General.health...Total..General.health...Unit...Persons
health$health_bad_pct <- health$General.health...Bad.health...Unit...Persons/health$General.health...Total..General.health...Unit...Persons
health$health_fair_pct <- health$General.health...Fair.health...Unit...Persons/health$General.health...Total..General.health...Unit...Persons
health$health_good_pct <- health$General.health...Good.health...Unit...Persons/health$General.health...Total..General.health...Unit...Persons
health$health_verygood_pct <- health$General.health...Very.good.health...Unit...Persons/health$General.health...Total..General.health...Unit...Persons

health$health_mean <- (1*health$health_bad_pct + 2*health$health_fair_pct + 3*health$health_good_pct + 4*health$health_verygood_pct)/1

ps_frame$health_verybad_pct  <- health$health_verybad_pct[match(ps_frame$GEO_CODE,health$GEO_CODE)]
ps_frame$health_bad_pct  <- health$health_bad_pct[match(ps_frame$GEO_CODE,health$GEO_CODE)]
ps_frame$health_fair_pct  <- health$health_fair_pct[match(ps_frame$GEO_CODE,health$GEO_CODE)]
ps_frame$health_good_pct  <- health$health_good_pct[match(ps_frame$GEO_CODE,health$GEO_CODE)]
ps_frame$health_verygood_pct  <- health$health_verygood_pct[match(ps_frame$GEO_CODE,health$GEO_CODE)]
ps_frame$health_mean  <- health$health_mean[match(ps_frame$GEO_CODE,health$GEO_CODE)]

rm(health)

###
i <- 13

print(paste("HEALTH ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Dependent Children

children <- read.csv("oa_data/data/Data_dependent_children.csv")

children$dep_child_pct <- (children$Household.type..E..S..W....Married.couple.household..With.dependent.children...Unit...Households+
  children$Household.type..E..S..W....Cohabiting.couple.household..With.dependent.children...Unit...Households+
  children$Household.type..E..S..W....Lone.parent.household..With.dependent.children...Unit...Households)/children$Household.type..E..S..W....Total..Household.type...Unit...Households

ps_frame$dep_child_pct <- children$dep_child_pct[match(ps_frame$GEO_CODE,children$GEO_CODE)]

rm(children)

###
i <- 14

print(paste("DEPENDENT CHILDREN ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Industry

industry <- read.csv("oa_data/data/Data_industry.csv")

industry$age1674 <- age_edu$a16plus_eTotal[match(industry$GEO_CODE,age_edu$GEO_CODE)]
industry$age1674[which(startsWith(industry$GEO_CODE,"S"))] <- age_edu_scot$a16plus_eTotal[match(industry$GEO_CODE[which(startsWith(industry$GEO_CODE,"S"))],age_edu_scot$GEO_CODE)]

industry$ind_agriculture_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...A.Agriculture..forestry.and.fishing...Unit...Persons/industry$age1674
industry$ind_mining_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...B.Mining.and.quarrying...Unit...Persons/industry$age1674
industry$ind_manufacturing_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...C.Manufacturing...Unit...Persons/industry$age1674
industry$ind_utilities_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...D.Electricity..gas..steam.and.air.conditioning.supply...Unit...Persons/industry$age1674
industry$ind_construction_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...F.Construction...Unit...Persons/industry$age1674
industry$ind_wholesale_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...G.Wholesale.and.retail.trade..repair.of.motor.vehicles.and.motor.cycles...Unit...Persons/industry$age1674
industry$ind_transport_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...H.Transport.and.storage...Unit...Persons/industry$age1674
industry$ind_accom_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...I.Accommodation.and.food.service.activities...Unit...Persons/industry$age1674
industry$ind_ict_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...J.Information.and.communication...Unit...Persons/industry$age1674
industry$ind_finance_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...K.Financial.and.insurance.activities...Unit...Persons/industry$age1674
industry$ind_realestate_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...L.Real.estate.activities...Unit...Persons/industry$age1674
industry$ind_professional_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...M.Professional..scientific.and.technical.activities...Unit...Persons/industry$age1674
industry$ind_adminpct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...N.Administrative.and.support.service.activities...Unit...Persons/industry$age1674
industry$ind_publicadmin_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...O.Public.administration.and.defence..compulsory.social.security...Unit...Persons/industry$age1674
industry$ind_education_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...P.Education...Unit...Persons/industry$age1674
industry$ind_socialwork_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...Q.Human.health.and.social.work.activities...Unit...Persons/industry$age1674
industry$ind_watersupply_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...E.Water.supply..sewerage..waste.management.and.remediation.activities...Unit...Persons/industry$age1674
industry$ind_arts_pct <- industry$Age...Age.16.to.74...Economic.activity...In.employment.the.week.before.the.census...Industry...R.S.Arts..entertainment.and.recreation..other.service.activities...Unit...Persons/industry$age1674

ps_frame$ind_agriculture_pct <- industry$ind_agriculture_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_mining_pct <- industry$ind_mining_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_manufacturing_pct <- industry$ind_manufacturing_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_utilities_pct <- industry$ind_utilities_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_construction_pct <- industry$ind_construction_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_wholesale_pct <- industry$ind_wholesale_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_transport_pct <- industry$ind_transport_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_accom_pct <- industry$ind_accom_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_ict_pct <- industry$ind_ict_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_finance_pct <- industry$ind_finance_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_professional_pct <- industry$ind_professional_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_adminpct <- industry$ind_adminpct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_publicadmin_pct <- industry$ind_publicadmin_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_education_pct <- industry$ind_education_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_socialwork_pct <- industry$ind_socialwork_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_watersupply_pct <- industry$ind_watersupply_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]
ps_frame$ind_arts_pct <- industry$ind_arts_pct[match(ps_frame$GEO_CODE,industry$GEO_CODE)]

rm(industry)

###
i <- 15

print(paste("INDUSTRY ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Welsh

welsh <- read.csv("oa_data/data/Data_welsh_lang.csv")

welsh$welshlang_pct <- welsh$Age...Age.16.and.over...General.health...Total..General.health...Language..ability.to.speak.Welsh..W....Can.speak.Welsh...Unit...Persons/welsh$Age...Age.16.and.over...General.health...Total..General.health...Language..ability.to.speak.Welsh..W....Total..Ability.to.speak.Welsh...Unit...Persons

ps_frame$welshlang_pct <- welsh$welshlang_pct[match(ps_frame$GEO_CODE,welsh$GEO_CODE)]
ps_frame$welshlang_pct[which(is.na(ps_frame$welshlang_pct))] <- 0

rm(welsh)

###
i <- 16

print(paste("WELSH LANGUAGE ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Marriage

marriage <- read.csv("oa_data/data/Data_marriage.csv")

marriage$married_pct <- (marriage$Age...Age.16.and.over...Marital.and.civil.partnership.status...Married...Unit...Persons+marriage$Age...Age.16.and.over...Marital.and.civil.partnership.status...In.a.registered.same.sex.civil.partnership...Unit...Persons)/marriage$Age...Age.16.and.over...Marital.and.civil.partnership.status...Total..Marital.and.civil.partnership.status...Unit...Persons
marriage$civilparternship_pct <- marriage$Age...Age.16.and.over...Marital.and.civil.partnership.status...In.a.registered.same.sex.civil.partnership...Unit...Persons/marriage$Age...Age.16.and.over...Marital.and.civil.partnership.status...Total..Marital.and.civil.partnership.status...Unit...Persons
marriage$nevermarried_pct <- marriage$Age...Age.16.and.over...Marital.and.civil.partnership.status...Single..never.married.or.never.registered.a.same.sex.civil.partnership....Unit...Persons/marriage$Age...Age.16.and.over...Marital.and.civil.partnership.status...Total..Marital.and.civil.partnership.status...Unit...Persons
marriage$divorced_pct <- marriage$Age...Age.16.and.over...Marital.and.civil.partnership.status...Divorced.or.formerly.in.a.same.sex.civil.partnership.which.is.now.legally.dissolved...Unit...Persons/marriage$Age...Age.16.and.over...Marital.and.civil.partnership.status...Total..Marital.and.civil.partnership.status...Unit...Persons

ps_frame$married_pct <- marriage$married_pct[match(ps_frame$GEO_CODE,marriage$GEO_CODE)]
ps_frame$nevermarried_pct <- marriage$nevermarried_pct[match(ps_frame$GEO_CODE,marriage$GEO_CODE)]
ps_frame$divorced_pct <- marriage$divorced_pct[match(ps_frame$GEO_CODE,marriage$GEO_CODE)]

rm(marriage)

###
i <- 17

print(paste("MARRIAGE ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#National ID (Scottish + English)

national <- read.csv("oa_data/data/Data_national_id.csv")

national$english_only_pct <- national$National.identity...National.identity.English..English.only.identity...Unit...Persons/national$National.identity...Total..National.identity.Scottish...Unit...Persons
national$scottish_only_pct <- national$National.identity...National.identity.Scottish..Scottish.only.identity...Unit...Persons/national$National.identity...Total..National.identity.Scottish...Unit...Persons
national$british_only_pct <- national$National.identity...National.identity.British..British.only.identity...Unit...Persons/national$National.identity...Total..National.identity.Scottish...Unit...Persons

ps_frame$english_only_pct <- national$english_only_pct[match(ps_frame$GEO_CODE,national$GEO_CODE)]
ps_frame$scottish_only_pct <- national$scottish_only_pct[match(ps_frame$GEO_CODE,national$GEO_CODE)]
ps_frame$british_only_pct <- national$british_only_pct[match(ps_frame$GEO_CODE,national$GEO_CODE)]

rm(national)

###
i <- 18

print(paste("NATIONAL IDENTITY ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Deprivation

deprivation <- read.csv("oa_data/data/Data_deprivation.csv")

deprivation$notdeprived_pct <- deprivation$Deprivation..classification.of.household..E..S..W....Household.is.not.deprived.in.any.dimension...Unit...Households/deprivation$Deprivation..classification.of.household..E..S..W....Total..Classification.of.household.deprivation...Unit...Households
deprivation$deprived1_pct <- deprivation$Deprivation..classification.of.household..E..S..W....Household.is.deprived.in.1.dimension...Unit...Households/deprivation$Deprivation..classification.of.household..E..S..W....Total..Classification.of.household.deprivation...Unit...Households
deprivation$deprived2_pct <- deprivation$Deprivation..classification.of.household..E..S..W....Household.is.deprived.in.2.dimensions...Unit...Households/deprivation$Deprivation..classification.of.household..E..S..W....Total..Classification.of.household.deprivation...Unit...Households
deprivation$deprived3_pct <- deprivation$Deprivation..classification.of.household..E..S..W....Household.is.deprived.in.3.dimensions...Unit...Households/deprivation$Deprivation..classification.of.household..E..S..W....Total..Classification.of.household.deprivation...Unit...Households
deprivation$deprived4_pct <- deprivation$Deprivation..classification.of.household..E..S..W....Household.is.deprived.in.4.dimensions...Unit...Households/deprivation$Deprivation..classification.of.household..E..S..W....Total..Classification.of.household.deprivation...Unit...Households

deprivation$deprivation_mean <- (1*deprivation$deprived1_pct + 2*deprivation$deprived2_pct + 3*deprivation$deprived3_pct + 4*deprivation$deprived4_pct)

ps_frame$notdeprived_pct <- deprivation$notdeprived_pct[match(ps_frame$GEO_CODE,deprivation$GEO_CODE)]
ps_frame$deprived1_pct <- deprivation$deprived1_pct[match(ps_frame$GEO_CODE,deprivation$GEO_CODE)]
ps_frame$deprived2_pct <- deprivation$deprived2_pct[match(ps_frame$GEO_CODE,deprivation$GEO_CODE)]
ps_frame$deprived3_pct <- deprivation$deprived3_pct[match(ps_frame$GEO_CODE,deprivation$GEO_CODE)]
ps_frame$deprived4_pct <- deprivation$deprived4_pct[match(ps_frame$GEO_CODE,deprivation$GEO_CODE)]
ps_frame$deprivation_mean <- deprivation$deprivation_mean[match(ps_frame$GEO_CODE,deprivation$GEO_CODE)]

rm(deprivation)

###
i <- 19

print(paste("DEPRIVATION ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#NSSEC

nssec <- read.csv("oa_data/data/Data_AGE_NSSEC_UNIT.csv")

nssec$higher_manager_pct <- nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....1..Higher.managerial..administrative.and.professional.occupations...Unit...Persons/nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....Total..NS.SeC..National.Statistics.Socio.economic.Classification....Unit...Persons
nssec$lower_manager_pct <- nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....2..Lower.managerial..administrative.and.professional.occupations...Unit...Persons/nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....Total..NS.SeC..National.Statistics.Socio.economic.Classification....Unit...Persons
nssec$intermediate_nssec_pct <- nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....3..Intermediate.occupations...Unit...Persons/nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....Total..NS.SeC..National.Statistics.Socio.economic.Classification....Unit...Persons
nssec$small_employers_pct <- nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....4..Small.employers.and.own.account.workers...Unit...Persons/nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....Total..NS.SeC..National.Statistics.Socio.economic.Classification....Unit...Persons
nssec$lower_supervisor_pct <- nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....5..Lower.supervisory.and.technical.occupations...Unit...Persons/nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....Total..NS.SeC..National.Statistics.Socio.economic.Classification....Unit...Persons
nssec$semiroutine_pct <- nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....6..Semi.routine.occupations...Unit...Persons/nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....Total..NS.SeC..National.Statistics.Socio.economic.Classification....Unit...Persons
nssec$routine_pct <- nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....7..Routine.occupations...Unit...Persons/nssec$Age...Age.16.to.74...NS.SeC..National.Statistics.Socio.economic.Classification....Total..NS.SeC..National.Statistics.Socio.economic.Classification....Unit...Persons

ps_frame$higher_manager_pct <- nssec$higher_manager_pct[match(ps_frame$GEO_CODE,nssec$GEO_CODE)]
ps_frame$lower_manager_pct <- nssec$lower_manager_pct[match(ps_frame$GEO_CODE,nssec$GEO_CODE)]
ps_frame$intermediate_nssec_pct <- nssec$intermediate_nssec_pct[match(ps_frame$GEO_CODE,nssec$GEO_CODE)]
ps_frame$small_employers_pct <- nssec$small_employers_pct[match(ps_frame$GEO_CODE,nssec$GEO_CODE)]
ps_frame$lower_supervisor_pct <- nssec$lower_supervisor_pct[match(ps_frame$GEO_CODE,nssec$GEO_CODE)]
ps_frame$semiroutine_pct <- nssec$semiroutine_pct[match(ps_frame$GEO_CODE,nssec$GEO_CODE)]
ps_frame$routine_pct <- nssec$routine_pct[match(ps_frame$GEO_CODE,nssec$GEO_CODE)]

rm(nssec)

###
i <- 20

print(paste("NSSEC ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Religion

religion <- read.csv("oa_data/data/Data_religion.csv")

religion$christian_pct <- religion$Religion..E..S..W....Christian...Unit...Persons/religion$Religion..E..S..W....Total..Religion...Unit...Persons
religion$buddhist_pct <- religion$Religion..E..S..W....Buddhist...Unit...Persons/religion$Religion..E..S..W....Total..Religion...Unit...Persons
religion$hindu_pct <- religion$Religion..E..S..W....Hindu...Unit...Persons/religion$Religion..E..S..W....Total..Religion...Unit...Persons
religion$jewish_pct <- religion$Religion..E..S..W....Jewish...Unit...Persons/religion$Religion..E..S..W....Total..Religion...Unit...Persons
religion$muslim_pct <- religion$Religion..E..S..W....Muslim...Unit...Persons/religion$Religion..E..S..W....Total..Religion...Unit...Persons
religion$sikh_pct <- religion$Religion..E..S..W....Sikh...Unit...Persons/religion$Religion..E..S..W....Total..Religion...Unit...Persons
religion$noreligion_pct <- religion$Religion..E..S..W....No.religion...Unit...Persons/religion$Religion..E..S..W....Total..Religion...Unit...Persons

ps_frame$christian_pct <- religion$christian_pct[match(ps_frame$GEO_CODE,religion$GEO_CODE)]
ps_frame$buddhist_pct <- religion$buddhist_pct[match(ps_frame$GEO_CODE,religion$GEO_CODE)]
ps_frame$hindu_pct <- religion$hindu_pct[match(ps_frame$GEO_CODE,religion$GEO_CODE)]
ps_frame$jewish_pct <- religion$jewish_pct[match(ps_frame$GEO_CODE,religion$GEO_CODE)]
ps_frame$muslim_pct <- religion$muslim_pct[match(ps_frame$GEO_CODE,religion$GEO_CODE)]
ps_frame$sikh_pct <- religion$sikh_pct[match(ps_frame$GEO_CODE,religion$GEO_CODE)]
ps_frame$noreligion_pct <- religion$noreligion_pct[match(ps_frame$GEO_CODE,religion$GEO_CODE)]

rm(religion)

###
i <- 21

print(paste("RELIGION ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

#Tenure

tenure <- read.csv("oa_data/data/Data_tenure.csv")

tenure$owned_outright_pct <- tenure$Tenure...Owned..Owned.outright...Unit...Persons/tenure$Tenure...Total..Tenure...Unit...Persons
tenure$owned_mortgage_pct <- tenure$Tenure...Owned..Owned.with.a.mortgage.or.loan...Unit...Persons/tenure$Tenure...Total..Tenure...Unit...Persons
tenure$social_rented_pct <- tenure$Tenure...Social.rented...Unit...Persons/tenure$Tenure...Total..Tenure...Unit...Persons
tenure$private_rented_pct <- tenure$Tenure...Private.rented...Unit...Persons/tenure$Tenure...Total..Tenure...Unit...Persons

ps_frame$owned_outright_pct <- tenure$owned_outright_pct[match(ps_frame$GEO_CODE,tenure$GEO_CODE)]
ps_frame$owned_mortgage_pct <- tenure$owned_mortgage_pct[match(ps_frame$GEO_CODE,tenure$GEO_CODE)]
ps_frame$social_rented_pct <- tenure$social_rented_pct[match(ps_frame$GEO_CODE,tenure$GEO_CODE)]
ps_frame$private_rented_pct <- tenure$private_rented_pct[match(ps_frame$GEO_CODE,tenure$GEO_CODE)]

rm(age_edu_scot)

###
i <- 22

print(paste("TENURE ADDED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

write.csv(ps_frame, "oa_data/data/ps_frame.csv")

###
i <- 23

print(paste("CSV WRITTEN Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

rm(list=ls())
