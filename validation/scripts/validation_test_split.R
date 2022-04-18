library(readxl)

###
i <- 1
j <- 4

print(paste("PACKAGES LOADED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

valid <- read_excel("validation/data/true-ward-results.xlsx")
valid$turnout <- NA
valid$turnout[which(!is.na(valid$Postals))] <- valid$Remain[which(!is.na(valid$Postals))] + valid$Leave[which(!is.na(valid$Postals))]

validation_test_wards <- valid$WardCode[which(!is.na(valid$Postals))]

###
i <- 2

print(paste("TRUE WARD RESULTS LOADED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

set.seed(101)
sample <- sample.int(n = length(validation_test_wards),
                     size = floor(.75*length(validation_test_wards)),
                     replace = F, useHash=F)

validation_wards <- validation_test_wards[sample]
test_wards <- validation_test_wards[-sample]

###
i <- 3

print(paste("DATA SPLIT Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###

write.csv(validation_wards,"validation/validation.csv")
write.csv(test_wards,"validation/test.csv")

###
i <- 4

print(paste("SPLITS SAVED Task - ",i,", PCT COMPLETE - ",100*i/j,"%",sep=""))
###
