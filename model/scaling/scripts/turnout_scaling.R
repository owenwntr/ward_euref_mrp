
la_results <- read.csv("model/scaling/data/EU-referendum-result-data.csv")

la_preds <- wards %>% group_by(la) %>% summarise(pred_turnout = weighted.mean(pred_turnout,value,na.rm=TRUE),
                                                 population = sum(value))

la_results$pred_turnout <- la_preds$pred_turnout[match(la_results$Area_Code,la_preds$la)]
la_results$population <- la_preds$population[match(la_results$Area_Code,la_preds$la)]

la_results$pred_votes <- la_results$pred_turnout*la_results$population

la_results$scaling <- la_results$Valid_Votes/la_results$pred_votes

wards$turnout_scaling <- la_results$scaling[match(wards$la,la_results$Area_Code)]

wards$pred_turnout_scaled <- wards$pred_turnout*wards$turnout_scaling

wards$voters <- wards$value*wards$pred_turnout_scaled

