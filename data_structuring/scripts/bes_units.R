library(haven)
library(creditmodel)
library(tidyverse)
library(pano)
library(labelled)

###
i <- 1
j <- 7

print(paste("PACKAGES LOADED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

ps_frame <- read.csv("oa_data/data/ps_frame.csv")

###
i <- 2

print(paste("PS_FRAME LOADED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

ps_frame$bes_unit <- paste(ps_frame$msoa,ps_frame$pcon)

bes_units1 <- ps_frame %>% group_by(bes_unit) %>%
  summarise_at(vars(msoa,pcon:rgn), funs(first(.)))

bes_units2 <- ps_frame %>% group_by(bes_unit,name) %>%
  summarise_at(vars(agegroup,edlevel), funs(first(.)))

bes_units3 <- ps_frame %>% group_by(bes_unit,name) %>%
  summarise_at(vars(value), funs(sum(.)))

bes_units4 <- ps_frame %>% group_by(bes_unit) %>%
  summarise_at(vars(female_pct:private_rented_pct), funs(weighted.mean(.,w=value)))

bes_units <- left_join(bes_units2,bes_units3,by=c("bes_unit","name"))

bes_units <- reduce(list(bes_units,bes_units1,bes_units4), left_join, by="bes_unit")

###
i <- 3

print(paste("BES_UNITS AGGREGATED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

source("utils/bes_path.R")

bes <- read_dta(bes_path)

irrelevant_waves <- c("W1","W2","W3","W4","W5","W6","W7","W8","W9","W11",
                      "W12","W13","W14","W15","W16","W17","W18","W19","W20")

current <- endsWith(colnames(bes),"W1")

for (wave in irrelevant_waves){
  new <- endsWith(colnames(bes),wave)
  current <- rowAny(cbind(current,new))
}

bes <- bes[,!current]

###
i <- 4

print(paste("BES DATA LOADED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

bes$onscodeW10 <- constid(bes$panoW10, "pa_id", "ons_id", warn = TRUE)
bes$msoacodeW10 <- to_character(bes$p_msoa11W10)

bes$bes_unit <- paste(bes$msoacodeW10, bes$onscodeW10)
bes$onscodeW10[which(!bes$bes_unit %in% bes_units$bes_unit)] <- bes_units$pcon[match(bes$msoacodeW10[which(!bes$bes_unit %in% bes_units$bes_unit)],bes_units$msoa)]
bes$bes_unit <- paste(bes$msoacodeW10, bes$onscodeW10)

bes <- bes[which((!is.na(bes$msoacodeW10))&(!is.na(bes$onscodeW10))),]

###
i <- 5

print(paste("BES DATA CLEANED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###


bes$agegroup <- NA
bes$agegroup[which((bes$ageW10>17)&(bes$ageW10<25))] <- 1
bes$agegroup[which((bes$ageW10>24)&(bes$ageW10<35))] <- 2
bes$agegroup[which((bes$ageW10>34)&(bes$ageW10<50))] <- 3
bes$agegroup[which((bes$ageW10>49)&(bes$ageW10<65))] <- 4
bes$agegroup[which((bes$ageW10>64))] <- 5

bes$edlevel <- NA  
bes$edlevel[which(bes$p_educationW10 %in% c(1))] <- "0"
bes$edlevel[which(bes$p_educationW10 %in% c(2,8,10))] <- "1"
bes$edlevel[which(bes$p_educationW10 %in% c(4,18))] <- "Other"
bes$edlevel[which(bes$p_educationW10 %in% c(3))] <- "Apprentice"
bes$edlevel[which(bes$p_educationW10 %in% c(5,9))] <- "2"
bes$edlevel[which(bes$p_educationW10 %in% c(6,11,7,12))] <- "3"
bes$edlevel[which(bes$p_educationW10 %in% c(15,13,14,16,17))] <- "4"
bes$edlevel[which(bes$p_educationW10 %in% c(19,20))] <- "DK"

bes$edlevel[which((bes$p_educationW10 %in% c(2,4,5,8,9,10))&(bes$gorW10==11))] <- "1 Scotland"
bes$edlevel[which((bes$p_educationW10 %in% c(3,6,7,11,12))&(bes$gorW10==11))] <- "2 Scotland"
bes$edlevel[which((bes$p_educationW10 %in% c(15))&(bes$gorW10==11))] <- "3 Scotland"
bes$edlevel[which((bes$p_educationW10 %in% c(13,14,16,17))&(bes$gorW10==11))] <- "3 Scotland"
bes$edlevel[which((bes$p_educationW10 %in% c(18,19,20))&(bes$gorW10==11))] <- "DK"

###
i <- 6

print(paste("HARMONISED AGE GROUP AND EDUCATION Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

bes[,c(colnames(bes_units)[5:92])] <- bes_units[match(bes$bes_unit,bes_units$bes_unit),5:92]

###
i <- 7

print(paste("CENSUS DATA ADDED MERGED WITH BES Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

rm(bes_units)
rm(bes_units1)
rm(bes_units2)
rm(bes_units3)
rm(bes_units4)


