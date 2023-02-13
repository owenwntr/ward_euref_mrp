
la_results <- read.csv("model/scaling/data/EU-referendum-result-data.csv")
la_results$Area_Code[which(la_results$Area_Code=="S12000046")] <- "S12000049"
la_results$Area_Code[which(la_results$Area_Code=="S12000044")] <- "S12000050"
la_results$Area_Code[which(la_results$Area_Code=="S12000015")] <- "S12000047"
la_results$Area_Code[which(la_results$Area_Code=="S12000024")] <- "S12000048"

la_results[nrow(la_results)+1,] <- c(NA,NA,NA,"E06000058",
                                     NA,NA,NA,NA,NA,NA,208424,
                                     89996,118428,NA,NA,NA,NA,NA,NA,NA,NA)
la_results[nrow(la_results)+1,] <- c(NA,NA,NA,"E06000059",
                                     NA,NA,NA,NA,NA,NA,232855,
                                     101766,131089,NA,NA,NA,NA,NA,NA,NA,NA)
la_results[nrow(la_results)+1,] <- c(NA,NA,NA,"E07000244",
                                     NA,NA,NA,NA,NA,NA,144830,
                                     61574,83256,NA,NA,NA,NA,NA,NA,NA,NA)
la_results[nrow(la_results)+1,] <- c(NA,NA,NA,"E07000245",
                                     NA,NA,NA,NA,NA,NA,90161,
                                     36777,53384,NA,NA,NA,NA,NA,NA,NA,NA)
la_results[nrow(la_results)+1,] <- c(NA,NA,NA,"E07000246",
                                     NA,NA,NA,NA,NA,NA,87467,
                                     39510,47957,NA,NA,NA,NA,NA,NA,NA,NA)
la_results[nrow(la_results)+1,] <- c(NA,NA,NA,"E07000246",
                                     NA,NA,NA,NA,NA,NA,87467,
                                     39510,47957,NA,NA,NA,NA,NA,NA,NA,NA)
la_results[nrow(la_results)+1,] <- c(NA,NA,NA,"E06000060",
                                     NA,NA,NA,NA,NA,NA,300951,
                                     154456,146495,NA,NA,NA,NA,NA,NA,NA,NA)

la_results$Valid_Votes <- as.numeric(la_results$Valid_Votes)
la_results$Leave <- as.numeric(la_results$Leave)

la_preds <- msoas %>% group_by(la) %>% summarise(pred_turnout = weighted.mean(pred_turnout,value),
                                                 population = sum(value))

la_results$pred_turnout <- la_preds$pred_turnout[match(la_results$Area_Code,la_preds$la)]
la_results$population <- la_preds$population[match(la_results$Area_Code,la_preds$la)]

la_results$pred_votes <- la_results$pred_turnout*la_results$population

la_results$scaling <- la_results$Valid_Votes/la_results$pred_votes

msoas$turnout_scaling <- la_results$scaling[match(msoas$la,la_results$Area_Code)]

msoas$pred_turnout_scaled <- msoas$pred_turnout*msoas$turnout_scaling

msoas$voters <- msoas$value*msoas$pred_turnout_scaled

wards$turnout_scaling <- la_results$scaling[match(wards$la,la_results$Area_Code)]

wards$pred_turnout_scaled <- wards$pred_turnout*wards$turnout_scaling

wards$voters <- wards$value*wards$pred_turnout_scaled

