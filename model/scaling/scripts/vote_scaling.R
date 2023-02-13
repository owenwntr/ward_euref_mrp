
la_vote_preds <- msoas %>% group_by(la) %>% summarise(pred_vote = weighted.mean(pred_leave,voters),
                                                 voters = sum(voters))
la_results$pred_leave <- la_vote_preds$pred_vote[match(la_results$Area_Code,la_preds$la)]

la_results$pred_leave_votes <- la_results$pred_leave*la_results$Valid_Votes

la_results$vote_scaling <- la_results$Leave/la_results$pred_leave_votes

msoas$vote_scaling <- la_results$vote_scaling[match(msoas$la,la_results$Area_Code)]
msoas$pred_leave_scaled <- msoas$pred_leave*msoas$vote_scaling

msoa_results <- msoas %>% group_by(msoa,la) %>%
  summarise(pred_leave = weighted.mean(pred_leave_scaled,voters))

write.csv(msoa_results,"msoa_results.csv")
