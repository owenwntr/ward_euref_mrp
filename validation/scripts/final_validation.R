for (package in c("ggplot2","scales")){
  if (!package %in% installed.packages()[,1]){
    install.packages(package)
  }
}

library(ggplot2)

wards$pred_turnout <- NA

wards$pred_turnout[which((wards$ward %in% test_wards)|
                           (wards$ward %in% validation_wards))] <- 
  predict(turnout_model,
          newdata=wards[which((wards$ward %in% test_wards)|
                                (wards$ward %in% validation_wards)),],
          type="response",allow.new.levels=TRUE)

wards$turnout_scaling <- la_results$scaling[match(wards$la,la_results$Area_Code)]
wards$pred_turnout_scaled <- wards$pred_turnout*wards$turnout_scaling
wards$voters <- wards$pred_turnout_scaled*wards$value

wards$pred_leave <- NA
wards$pred_leave[which((wards$ward %in% test_wards)|
                         (wards$ward %in% validation_wards))] <- predict(vote_model,
                                                               newdata=wards[which((wards$ward %in% test_wards)|
                                                                                     (wards$ward %in% validation_wards)),],
                                                               type="response",
                                                               allow.new.levels=TRUE)

wards$vote_scaling <- la_results$vote_scaling[match(wards$la,la_results$Area_Code)]
wards$pred_leave_scaled <- wards$pred_leave*wards$vote_scaling

wards_sum <- wards[which((wards$ward %in% test_wards)|
                           (wards$ward %in% validation_wards)),] %>% 
  group_by(ward) %>%
  summarise(pop = sum(value),
            pred_turnout_pct = weighted.mean(pred_turnout,value),
            pred_turnout_pct_scaled = weighted.mean(pred_turnout_scaled,value),
            pred_leave_pct = weighted.mean(pred_leave,voters),
            pred_leave_pct_scaled = weighted.mean(pred_leave_scaled,voters))

#wards_sum <- left_join(wards_sum, wards4, by="ward")

wards_sum$true_turnout <- valid$turnout[match(wards_sum$ward,valid$WardCode)]

wards_sum$true_turnout_pct <- wards_sum$true_turnout/wards_sum$pop

valid$leave_pct <- valid$Leave/valid$turnout

wards_sum$true_leave_pct <- valid$leave_pct[match(wards_sum$ward,valid$WardCode)]

wards_sum$valid_test <- NA
wards_sum$valid_test[which(wards_sum$ward %in% validation_wards)] <- "Validation"
wards_sum$valid_test[which(wards_sum$ward %in% test_wards)] <- "Test"

wards_sum$valid_test <- factor(wards_sum$valid_test,levels=c("Validation","Test"))

# MSE Scores

#Validation Turnout
sqrt(mean((wards_sum$true_turnout_pct[which(wards_sum$valid_test=="Validation")]
           -wards_sum$pred_turnout_pct[which(wards_sum$valid_test=="Validation")])^2,na.rm=TRUE))
#Validation Turnout Scaled
sqrt(mean((wards_sum$true_turnout_pct[which(wards_sum$valid_test=="Validation")]-
             wards_sum$pred_turnout_pct_scaled[which(wards_sum$valid_test=="Validation")])^2,na.rm=TRUE))
#Test Turnout
sqrt(mean((wards_sum$true_turnout_pct[which(wards_sum$valid_test=="Test")]
           -wards_sum$pred_turnout_pct[which(wards_sum$valid_test=="Test")])^2,na.rm=TRUE))
#Test Turnout Scaled
sqrt(mean((wards_sum$true_turnout_pct[which(wards_sum$valid_test=="Test")]-
             wards_sum$pred_turnout_pct_scaled[which(wards_sum$valid_test=="Test")])^2,na.rm=TRUE))

#Validation Leave
sqrt(mean((wards_sum$true_leave_pct[which(wards_sum$valid_test=="Validation")]
           -wards_sum$pred_leave_pct[which(wards_sum$valid_test=="Validation")])^2,na.rm=TRUE))
#Validation Leave Scaled
sqrt(mean((wards_sum$true_leave_pct[which(wards_sum$valid_test=="Validation")]-
             wards_sum$pred_leave_pct_scaled[which(wards_sum$valid_test=="Validation")])^2,na.rm=TRUE))
#Test Leave
sqrt(mean((wards_sum$true_leave_pct[which(wards_sum$valid_test=="Test")]
           -wards_sum$pred_leave_pct[which(wards_sum$valid_test=="Test")])^2,na.rm=TRUE))
#Test Leave Scaled
sqrt(mean((wards_sum$true_leave_pct[which(wards_sum$valid_test=="Test")]-
             wards_sum$pred_leave_pct_scaled[which(wards_sum$valid_test=="Test")])^2,na.rm=TRUE))

# MAE Scores

#Validation Turnout
mean(abs(wards_sum$true_turnout_pct[which(wards_sum$valid_test=="Validation")]
           -wards_sum$pred_turnout_pct[which(wards_sum$valid_test=="Validation")]),na.rm=TRUE)
#Validation Turnout Scaled
mean(abs(wards_sum$true_turnout_pct[which(wards_sum$valid_test=="Validation")]-
             wards_sum$pred_turnout_pct_scaled[which(wards_sum$valid_test=="Validation")]),na.rm=TRUE)
#Test Turnout
mean(abs(wards_sum$true_turnout_pct[which(wards_sum$valid_test=="Test")]
           -wards_sum$pred_turnout_pct[which(wards_sum$valid_test=="Test")]),na.rm=TRUE)
#Test Turnout Scaled
mean(abs(wards_sum$true_turnout_pct[which(wards_sum$valid_test=="Test")]-
             wards_sum$pred_turnout_pct_scaled[which(wards_sum$valid_test=="Test")]),na.rm=TRUE)

#Validation Leave
mean(abs(wards_sum$true_leave_pct[which(wards_sum$valid_test=="Validation")]
           -wards_sum$pred_leave_pct[which(wards_sum$valid_test=="Validation")]),na.rm=TRUE)
#Validation Leave Scaled
mean(abs(wards_sum$true_leave_pct[which(wards_sum$valid_test=="Validation")]-
             wards_sum$pred_leave_pct_scaled[which(wards_sum$valid_test=="Validation")]),na.rm=TRUE)
#Test Leave
mean(abs(wards_sum$true_leave_pct[which(wards_sum$valid_test=="Test")]
           -wards_sum$pred_leave_pct[which(wards_sum$valid_test=="Test")]),na.rm=TRUE)
#Test Leave Scaled
mean(abs(wards_sum$true_leave_pct[which(wards_sum$valid_test=="Test")]-
             wards_sum$pred_leave_pct_scaled[which(wards_sum$valid_test=="Test")]),na.rm=TRUE)

wards_long <- wards_sum %>% pivot_longer(cols=3:6)
wards_long$name[which(wards_long$name=="pred_leave_pct")] <- "Predicted Leave Vote"
wards_long$name[which(wards_long$name=="pred_leave_pct_scaled")] <- "Predicted Leave Vote (Scaled)"
wards_long$name[which(wards_long$name=="pred_turnout_pct")] <- "Predicted Turnout"
wards_long$name[which(wards_long$name=="pred_turnout_pct_scaled")] <- "Predicted Turnout (Scaled)"

ggplot(wards_long[which(startsWith(wards_long$name,"Predicted L")),],
       aes(x=true_leave_pct)) +
  geom_point(aes(y=value),size=1) +
  geom_smooth(method="lm",aes(y=value)) +
  scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  geom_abline(slope=1,intercept=0,lty=2) +
  facet_wrap(~valid_test + name) +
  labs(x="True",y="Predicted",colour="")

ggplot(wards_long[which(startsWith(wards_long$name,"Predicted T")),],
       aes(x=true_turnout_pct)) +
  geom_point(aes(y=value),size=1) +
  geom_smooth(method="lm",aes(y=value)) +
  scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  geom_abline(slope=1,intercept=0,lty=2) +
  facet_wrap(~valid_test + name) +
  labs(x="True",y="Predicted",colour="")
